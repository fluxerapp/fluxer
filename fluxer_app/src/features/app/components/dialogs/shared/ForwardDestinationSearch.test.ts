// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardResultType} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {
	buildForwardSearchBoosters,
	type ForwardChannelCandidate,
	type ForwardGroupDMCandidate,
	type ForwardSearchBoosters,
	type ForwardSearchResult,
	type ForwardUserCandidate,
	searchForwardDestinations,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSearch';
import {loadUnicodeConfusables} from '@app/features/search/utils/SearchTextMatching';
import {beforeAll, describe, expect, it} from 'vitest';

const ALL_TYPES: ReadonlyArray<ForwardResultType> = ['user', 'text_channel', 'voice_channel', 'group_dm'];
const NO_BOOSTERS = buildForwardSearchBoosters({dmUserIds: [], frequent: [], friendIds: []});
let confusables: ReadonlyMap<string, string> = new Map();

beforeAll(async () => {
	confusables = await loadUnicodeConfusables();
});

interface SearchOverrides {
	readonly blacklist?: ReadonlySet<string>;
	readonly boosters?: ForwardSearchBoosters;
	readonly channels?: ReadonlyArray<ForwardChannelCandidate>;
	readonly confusables?: ReadonlyMap<string, string>;
	readonly groupDMs?: ReadonlyArray<ForwardGroupDMCandidate>;
	readonly limit?: number;
	readonly resultTypes?: ReadonlyArray<ForwardResultType>;
	readonly users?: ReadonlyArray<ForwardUserCandidate>;
}

function search(query: string, overrides: SearchOverrides = {}): ReadonlyArray<ForwardSearchResult> {
	return searchForwardDestinations({
		blacklist: new Set(),
		boosters: NO_BOOSTERS,
		channels: [],
		confusables,
		groupDMs: [],
		limit: 20,
		resultTypes: ALL_TYPES,
		users: [],
		...overrides,
		query,
	});
}

function channel(id: string, name: string, overrides: Partial<ForwardChannelCandidate> = {}): ForwardChannelCandidate {
	return {canAccess: true, guildName: null, hasFrecency: false, id, kind: 'text', name, parentName: null, ...overrides};
}

function user(id: string, username: string, overrides: Partial<ForwardUserCandidate> = {}): ForwardUserCandidate {
	return {friendNickname: null, globalName: null, id, nicknames: [], username, ...overrides};
}

function groupDM(id: string, name: string, memberFields: ReadonlyArray<string> = []): ForwardGroupDMCandidate {
	return {id, memberFields, name};
}

function tier(results: ReadonlyArray<ForwardSearchResult>): number | null {
	return results.length === 0 ? null : results[0].score / 1000;
}

function channelTier(
	query: string,
	candidate: ForwardChannelCandidate,
	resultTypes: ReadonlyArray<ForwardResultType> = ['text_channel'],
): number | null {
	return tier(search(query, {channels: [candidate], resultTypes}));
}

function userTier(query: string, candidate: ForwardUserCandidate, overrides: SearchOverrides = {}): number | null {
	return tier(search(query, {resultTypes: ['user'], users: [candidate], ...overrides}));
}

function summarize(results: ReadonlyArray<ForwardSearchResult>): ReadonlyArray<string> {
	return results.map((result) => `${result.type}:${result.id}:${result.score}`);
}

describe('forward destination channel search', () => {
	const general = channel('general', 'general');

	it('scores exact, prefix, contains and fuzzy name matches', () => {
		expect(channelTier('general', general)).toBe(10);
		expect(channelTier('gen', general)).toBe(7);
		expect(channelTier('eral', general)).toBe(5);
		expect(channelTier('gnrl', general)).toBe(1);
	});

	it('adds the frecency bonus up to the tier cap', () => {
		expect(channelTier('gen', channel('general', 'general', {hasFrecency: true}))).toBe(10);
		expect(channelTier('eral', channel('general', 'general', {hasFrecency: true}))).toBe(7);
	});

	it('matches a full phrase across a dash', () => {
		expect(channelTier('general chat', channel('general-chat', 'general-chat'))).toBe(7);
	});

	it('rejects words in the wrong order', () => {
		expect(channelTier('chat general', channel('general-chat', 'general-chat'))).toBeNull();
	});

	it('lets leftover words match the guild name at half weight, capped at 6', () => {
		expect(channelTier('general foo', channel('general', 'general', {guildName: 'Foo Server'}))).toBe(6);
	});

	it('rejects a query that leaves more than one word unmatched', () => {
		expect(channelTier('gen my server', channel('general', 'general', {guildName: 'My Server'}))).toBeNull();
	});

	it('needs the channel name itself to match', () => {
		expect(channelTier('foo', channel('general', 'general', {guildName: 'foo'}))).toBeNull();
	});

	it('caps a query with a stray space at 6', () => {
		const inGuild = channel('general', 'general', {guildName: 'Foo Server'});
		expect(channelTier('general ', inGuild)).toBe(6);
		expect(channelTier(' general', inGuild)).toBe(6);
	});

	it('penalises voice channels found by the text search only', () => {
		const lounge = channel('lounge', 'lounge', {kind: 'voice'});
		expect(search('lounge', {channels: [lounge], resultTypes: ['text_channel']})).toEqual([
			{comparator: 'lounge', id: 'lounge', score: 9000, type: 'voice_channel'},
		]);
		expect(channelTier('lounge', lounge, ['voice_channel'])).toBe(10);
	});

	it('keeps the penalised text-search copy of a voice channel when both searches find it', () => {
		const lounge = channel('lounge', 'lounge', {kind: 'voice'});
		const boosters = buildForwardSearchBoosters({
			dmUserIds: [],
			frequent: [{id: 'lounge', kind: 'voice', score: 10}],
			friendIds: [],
		});
		expect(summarize(search('lounge', {boosters, channels: [lounge], resultTypes: ['voice_channel']}))).toEqual([
			'voice_channel:lounge:20000',
		]);
		expect(summarize(search('lounge', {boosters, channels: [lounge]}))).toEqual(['voice_channel:lounge:9000']);
	});

	it('does not fold diacritics in channel names', () => {
		expect(channelTier('cafe', channel('cafe', 'café'))).toBeNull();
	});

	it('skips channels the reader cannot access', () => {
		expect(channelTier('general', channel('general', 'general', {canAccess: false}))).toBeNull();
	});

	it('keeps candidate order between channels with equal scores', () => {
		const channels = [channel('z', 'general-z'), channel('a', 'general-a'), channel('m', 'general')];
		expect(search('general', {channels, resultTypes: ['text_channel']}).map((result) => result.id)).toEqual([
			'm',
			'z',
			'a',
		]);
	});

	it('truncates each category to the limit before merging', () => {
		const channels = Array.from({length: 25}, (_, index) => channel(`c${index}`, `general-${index}`));
		const results = search('general', {channels, limit: 20});
		expect(results.map((result) => result.id)).toEqual(channels.slice(0, 20).map((candidate) => candidate.id));
	});
});

describe('forward destination user search', () => {
	it('ranks a case-insensitive prefix at 10 and a substring at 5', () => {
		const bobby = user('1', 'bobby');
		expect(userTier('Bob', bobby)).toBe(10);
		expect(userTier('ob', bobby)).toBe(5);
	});

	it('folds diacritics in the field but not in the query', () => {
		expect(userTier('jose', user('1', 'José'))).toBe(1);
		expect(userTier('josé', user('1', 'jose99'))).toBeNull();
	});

	it('matches look-alike characters once the confusables table is loaded', () => {
		expect(userTier('b0b', user('1', 'bob'))).toBe(1);
		expect(userTier('b0b', user('1', 'bob'), {confusables: new Map()})).toBeNull();
	});

	it('does not trim the query', () => {
		expect(userTier('john ', user('1', 'john'))).toBeNull();
	});

	it('matches the exact user id', () => {
		expect(userTier('123456789012345678', user('123456789012345678', 'ada'))).toBe(10);
	});

	it('matches community nicknames, friend nicknames and global names', () => {
		expect(userTier('mods', user('1', 'ada', {nicknames: ['Mods Team']}))).toBe(10);
		expect(userTier('count', user('1', 'ada', {friendNickname: 'Countess'}))).toBe(10);
		expect(userTier('love', user('1', 'ada', {globalName: 'Ada Lovelace'}))).toBe(5);
	});

	it('reports the first field that produced the best score', () => {
		const [strongest] = search('love', {
			users: [user('1', 'ada', {globalName: 'Lovelace', nicknames: ['Ada Lovelace']})],
		});
		expect(strongest.comparator).toBe('Lovelace');
		const [first] = search('ada', {users: [user('1', 'ada_x', {globalName: 'Ada'})]});
		expect(first.comparator).toBe('ada_x');
	});

	it('skips blacklisted users', () => {
		expect(userTier('ada', user('me', 'ada'), {blacklist: new Set(['me'])})).toBeNull();
	});

	it('multiplies by the user booster', () => {
		const boosters = buildForwardSearchBoosters({dmUserIds: [], frequent: [], friendIds: ['1']});
		expect(userTier('bob', user('1', 'bob'), {boosters})).toBeCloseTo(12);
	});

	it('breaks ties alphabetically by the matched name', () => {
		const users = [user('1', 'Bob_b'), user('2', 'bob_a'), user('3', 'bob_c')];
		expect(search('bob', {users}).map((result) => result.id)).toEqual(['2', '1', '3']);
	});
});

describe('forward destination group DM search', () => {
	it('folds diacritics and look-alikes in the name', () => {
		expect(tier(search('cafe', {groupDMs: [groupDM('g', 'Café Crew')]}))).toBe(7);
		expect(tier(search('rnovie', {groupDMs: [groupDM('g', 'movie night')]}))).toBe(7);
	});

	it('caps member matches at 5', () => {
		expect(tier(search('alice', {groupDMs: [groupDM('g', 'Weekend', ['alice', 'Alice A.'])]}))).toBe(5);
	});

	it('prefers a stronger name match over a member match', () => {
		expect(tier(search('weekend', {groupDMs: [groupDM('g', 'Weekend', ['weekend_fan'])]}))).toBe(10);
	});
});

describe('forward destination search merge', () => {
	it('returns nothing for a blank query', () => {
		expect(search('   ', {channels: [channel('general', 'general')]})).toEqual([]);
	});

	it('only searches the requested result types', () => {
		const results = search('general', {
			channels: [channel('general', 'general')],
			groupDMs: [groupDM('g', 'general')],
			resultTypes: ['user'],
			users: [user('1', 'general')],
		});
		expect(summarize(results)).toEqual(['user:1:10000']);
	});

	it('orders equal scores as users, group DMs, text channels, then voice channels', () => {
		const results = search('general', {
			channels: [channel('v', 'general', {kind: 'voice'}), channel('t', 'general')],
			groupDMs: [groupDM('g', 'general')],
			users: [user('1', 'general')],
		});
		expect(summarize(results)).toEqual([
			'user:1:10000',
			'group_dm:g:10000',
			'text_channel:t:10000',
			'voice_channel:v:9000',
		]);
	});

	it('sorts higher scores first across types', () => {
		const results = search('gen', {
			channels: [channel('t', 'general', {hasFrecency: true})],
			users: [user('1', 'xgen')],
		});
		expect(summarize(results)).toEqual(['text_channel:t:10000', 'user:1:5000']);
	});
});

describe('buildForwardSearchBoosters', () => {
	it('scales frecency against the highest score and adds friend and open DM boosts', () => {
		const boosters = buildForwardSearchBoosters({
			dmUserIds: ['user-1', 'user-3'],
			frequent: [
				{id: 'guild-1', kind: 'other', score: 200},
				{id: 'dm-1', kind: 'dm', recipientId: 'user-1', score: 100},
				{id: 'text-1', kind: 'text', score: 50},
				{id: 'voice-1', kind: 'voice', score: 150},
				{id: 'gdm-1', kind: 'group_dm', score: 20},
			],
			friendIds: ['user-1', 'user-2'],
		});
		expect(boosters.users.get('user-1')).toBeCloseTo(1.8);
		expect(boosters.users.get('user-2')).toBeCloseTo(1.2);
		expect(boosters.users.get('user-3')).toBeCloseTo(1.1);
		expect(boosters.users.has('dm-1')).toBe(false);
		expect(boosters.textChannels.get('text-1')).toBe(1.25);
		expect(boosters.voiceChannels.get('voice-1')).toBe(1.75);
		expect(boosters.groupDMs.get('gdm-1')).toBeCloseTo(1.1);
		expect(boosters.textChannels.has('guild-1')).toBe(false);
	});

	it('falls back to a boost of 1 when every frecency score is zero', () => {
		const boosters = buildForwardSearchBoosters({
			dmUserIds: [],
			frequent: [{id: 'text-1', kind: 'text', score: 0}],
			friendIds: [],
		});
		expect(boosters.textChannels.get('text-1')).toBe(1);
	});

	it('boosts text channels only in the text search and voice channels only in the voice search', () => {
		const boosters = buildForwardSearchBoosters({
			dmUserIds: [],
			frequent: [
				{id: 't', kind: 'text', score: 10},
				{id: 'v', kind: 'voice', score: 10},
			],
			friendIds: [],
		});
		const channels = [channel('t', 'general'), channel('v', 'general', {kind: 'voice'})];
		expect(summarize(search('general', {boosters, channels, resultTypes: ['text_channel']}))).toEqual([
			'text_channel:t:20000',
			'voice_channel:v:9000',
		]);
		expect(summarize(search('general', {boosters, channels, resultTypes: ['voice_channel']}))).toEqual([
			'voice_channel:v:20000',
		]);
	});
});
