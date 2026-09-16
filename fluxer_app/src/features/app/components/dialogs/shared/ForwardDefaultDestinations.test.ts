// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildForwardDefaultDestinations,
	type ForwardDestination,
	type ForwardDestinationRow,
	type ForwardRowIdentity,
	filterForwardSearchRows,
	resolveForwardOrigin,
} from '@app/features/app/components/dialogs/shared/ForwardDefaultDestinations';
import type {ForwardResultType} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const DM_RECIPIENTS: ReadonlyMap<string, string> = new Map([['dm-1', 'user-1']]);
const KNOWN_USERS: ReadonlySet<string> = new Set(['user-1', 'user-9']);
const INVALID_IDS: ReadonlySet<string> = new Set(['text-blocked']);

function channelType(channelId: string): ForwardResultType | null {
	if (channelId.startsWith('text-')) return 'text_channel';
	if (channelId.startsWith('voice-')) return 'voice_channel';
	if (channelId.startsWith('gdm-')) return 'group_dm';
	return null;
}

function resolveChannel(channelId: string): ForwardRowIdentity | null {
	const recipientId = DM_RECIPIENTS.get(channelId);
	if (recipientId !== undefined) return {id: recipientId, type: 'user'};
	const type = channelType(channelId);
	return type == null ? null : {id: channelId, type};
}

function resolveDestination(destination: ForwardDestination): ForwardRowIdentity | null {
	if (destination.type === 'channel') return resolveChannel(destination.id);
	return KNOWN_USERS.has(destination.id) ? {id: destination.id, type: 'user'} : null;
}

function isValid(row: ForwardRowIdentity): boolean {
	return !INVALID_IDS.has(row.id);
}

interface DefaultsOverrides {
	readonly frequentIds?: ReadonlyArray<string>;
	readonly history?: ReadonlyArray<string>;
	readonly mode?: ForwardResultType | null;
	readonly origin?: ForwardDestination | null;
	readonly pinned?: ReadonlyArray<ForwardDestination>;
	readonly selected?: ReadonlyArray<ForwardDestination>;
}

function defaults(overrides: DefaultsOverrides): ReadonlyArray<string> {
	const rows = buildForwardDefaultDestinations({
		frequentIds: [],
		history: [],
		isValid,
		mode: null,
		origin: null,
		pinned: [],
		resolveChannel,
		resolveDestination,
		selected: [],
		...overrides,
	});
	return rows.map((row) => `${row.type}:${row.id}`);
}

function channelIds(prefix: string, count: number): ReadonlyArray<string> {
	return Array.from({length: count}, (_, index) => `${prefix}-${index}`);
}

describe('buildForwardDefaultDestinations', () => {
	it('lists pinned destinations, then history, then frecency, keeping the first copy of each', () => {
		expect(
			defaults({
				frequentIds: ['voice-1', 'guild-1', 'text-1', 'gdm-1'],
				history: ['text-1', 'text-2', 'dm-1'],
				pinned: [
					{id: 'text-2', type: 'channel'},
					{id: 'user-9', type: 'user'},
				],
			}),
		).toEqual([
			'text_channel:text-2',
			'user:user-9',
			'text_channel:text-1',
			'user:user-1',
			'voice_channel:voice-1',
			'group_dm:gdm-1',
		]);
	});

	it('drops unknown users, unresolvable ids and invalid rows', () => {
		expect(
			defaults({
				frequentIds: ['guild-1', 'text-blocked', 'text-1'],
				pinned: [{id: 'user-unknown', type: 'user'}],
			}),
		).toEqual(['text_channel:text-1']);
	});

	it('hides the origin channel unless it is selected', () => {
		const origin: ForwardDestination = {id: 'text-1', type: 'channel'};
		expect(defaults({history: ['text-1', 'text-2'], origin})).toEqual(['text_channel:text-2']);
		expect(defaults({history: ['text-1', 'text-2'], origin, selected: [origin]})).toEqual([
			'text_channel:text-1',
			'text_channel:text-2',
		]);
	});

	it('hides the recipient of a DM origin', () => {
		const origin = resolveForwardOrigin('dm-1', {recipientIds: ['user-1'], type: ChannelTypes.DM});
		expect(origin).toEqual({id: 'user-1', type: 'user'});
		expect(defaults({history: ['dm-1', 'text-1'], origin})).toEqual(['text_channel:text-1']);
	});

	it('caps the list at 15 rows', () => {
		expect(defaults({frequentIds: channelIds('text', 20)})).toEqual(
			channelIds('text', 15).map((id) => `text_channel:${id}`),
		);
	});

	it('filters to the mode without a cap', () => {
		const frequentIds = [...channelIds('text', 20), 'dm-1', 'voice-1'];
		expect(defaults({frequentIds, mode: 'text_channel'})).toEqual(
			channelIds('text', 20).map((id) => `text_channel:${id}`),
		);
		expect(defaults({frequentIds, mode: 'user'})).toEqual(['user:user-1']);
	});

	it('reads at most 8 history entries and 100 frecency entries', () => {
		const history = channelIds('text-h', 10);
		const frequentIds = channelIds('text-f', 120);
		const rows = defaults({frequentIds, history, mode: 'text_channel'});
		expect(rows).toHaveLength(108);
		expect(rows).not.toContain('text_channel:text-h-8');
		expect(rows).not.toContain('text_channel:text-f-100');
	});

	it('maps each row to the destination the selection stores', () => {
		const rows = buildForwardDefaultDestinations({
			frequentIds: ['dm-1', 'gdm-1', 'voice-1'],
			history: [],
			isValid,
			mode: null,
			origin: null,
			pinned: [],
			resolveChannel,
			resolveDestination,
			selected: [],
		});
		expect(rows.map((row) => row.destination)).toEqual([
			{id: 'user-1', type: 'user'},
			{id: 'gdm-1', type: 'channel'},
			{id: 'voice-1', type: 'channel'},
		]);
	});
});

describe('resolveForwardOrigin', () => {
	it('uses the channel for anything but a DM', () => {
		expect(resolveForwardOrigin('text-1', {recipientIds: [], type: ChannelTypes.GUILD_TEXT})).toEqual({
			id: 'text-1',
			type: 'channel',
		});
		expect(resolveForwardOrigin('gdm-1', {recipientIds: ['user-1', 'user-9'], type: ChannelTypes.GROUP_DM})).toEqual({
			id: 'gdm-1',
			type: 'channel',
		});
		expect(resolveForwardOrigin('text-1', null)).toEqual({id: 'text-1', type: 'channel'});
	});
});

describe('filterForwardSearchRows', () => {
	it('drops invalid rows and duplicate ids without a cap or origin exclusion', () => {
		const results: ReadonlyArray<ForwardRowIdentity> = [
			{id: 'user-1', type: 'user'},
			{id: 'text-blocked', type: 'text_channel'},
			{id: 'text-1', type: 'text_channel'},
			{id: 'text-1', type: 'voice_channel'},
			...channelIds('text-x', 30).map((id): ForwardRowIdentity => ({id, type: 'text_channel'})),
		];
		const rows: ReadonlyArray<ForwardDestinationRow> = filterForwardSearchRows(results, isValid);
		expect(rows).toHaveLength(32);
		expect(rows.slice(0, 2)).toEqual([
			{destination: {id: 'user-1', type: 'user'}, id: 'user-1', type: 'user'},
			{destination: {id: 'text-1', type: 'channel'}, id: 'text-1', type: 'text_channel'},
		]);
	});
});
