// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
	TEST_GUILD_ID,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

type EntryFixture = Parameters<typeof makeEntry>[0];
type ChangeFixture = NonNullable<EntryFixture['changes']>[number];

const CHANNEL_ID = '1400000000000000200';
const CATEGORY_ID = '1400000000000000201';
const OTHER_CATEGORY_ID = '1400000000000000202';
const ROLE_ID = '1400000000000000300';
const MEMBER_ID = '1400000000000000400';
const UNKNOWN_PERMISSION_BIT = 1n << 19n;
const NAMES = {[TEST_ACTOR_ID]: 'Hampus', [MEMBER_ID]: 'ender'};
const ACTOR = {kind: 'user', id: TEST_ACTOR_ID};
const VOICE_DEFAULTS = {type: 2, name: 'Lounge', bitrate: 64000, user_limit: 0, voice_connection_limit: 5};

const ROLE_OVERWRITE = {target_id: ROLE_ID, options: {channel_id: CHANNEL_ID, id: ROLE_ID, type: 0, role_name: 'Mods'}};
const MEMBER_OVERWRITE = {target_id: MEMBER_ID, options: {channel_id: CHANNEL_ID, id: MEMBER_ID, type: 1}};
const EVERYONE_OVERWRITE = {target_id: TEST_GUILD_ID, options: {channel_id: CHANNEL_ID, id: TEST_GUILD_ID, type: 0}};

interface OverwriteTarget {
	target_id: string;
	options: Record<string, unknown>;
}

function present(fixture: EntryFixture) {
	const presentation = presentAuditLogEntry(makeEntry(fixture), fakeContext());
	return {...resultToText(presentation, NAMES), expandable: presentation.expandable};
}

function summaryValues(fixture: EntryFixture) {
	return presentAuditLogEntry(makeEntry(fixture), fakeContext()).summary.values;
}

function change(key: string, oldValue: unknown, newValue: unknown): ChangeFixture {
	return {key, old_value: oldValue, new_value: newValue};
}

function mask(...flags: Array<bigint>): string {
	return flags.reduce((all, flag) => all | flag, 0n).toString();
}

function channelSnapshot(values: Record<string, unknown>): Record<string, unknown> {
	return {
		channel_id: CHANNEL_ID,
		type: 0,
		name: 'general',
		topic: null,
		parent_id: null,
		position: 12,
		nsfw: null,
		content_warning_level: 0,
		content_warning_text: null,
		rate_limit_per_user: 0,
		user_limit: 0,
		voice_connection_limit: null,
		bitrate: 0,
		rtc_region: null,
		permission_overwrite_count: 0,
		...values,
	};
}

function createFixture(values: Record<string, unknown> = {}): EntryFixture {
	const snapshot = channelSnapshot(values);
	return {
		action_type: AuditLogActionType.CHANNEL_CREATE,
		target_id: CHANNEL_ID,
		options: {type: snapshot.type},
		changes: Object.entries(snapshot).map(([key, value]) => ({key, new_value: value})),
	};
}

function updateFixture(changes: Array<ChangeFixture>, type?: unknown): EntryFixture {
	return {action_type: AuditLogActionType.CHANNEL_UPDATE, target_id: CHANNEL_ID, options: {type}, changes};
}

function deleteFixture(values: Record<string, unknown> = {}): EntryFixture {
	const snapshot = channelSnapshot(values);
	return {
		action_type: AuditLogActionType.CHANNEL_DELETE,
		target_id: CHANNEL_ID,
		options: {type: snapshot.type},
		changes: Object.entries(snapshot).map(([key, value]) => ({key, old_value: value})),
	};
}

function overwriteCreated(target: OverwriteTarget, allow: unknown, deny: unknown): EntryFixture {
	return {
		action_type: AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
		...target,
		changes: [
			{key: 'id', new_value: target.target_id},
			{key: 'type', new_value: String(target.options.type)},
			{key: 'allow', new_value: allow},
			{key: 'deny', new_value: deny},
		],
	};
}

function overwriteUpdated(target: OverwriteTarget, changes: Array<ChangeFixture>): EntryFixture {
	return {action_type: AuditLogActionType.CHANNEL_OVERWRITE_UPDATE, ...target, changes};
}

function overwriteDeleted(target: OverwriteTarget, allow: unknown, deny: unknown): EntryFixture {
	return {
		action_type: AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
		...target,
		changes: [
			{key: 'id', old_value: target.target_id},
			{key: 'type', old_value: String(target.options.type)},
			{key: 'allow', old_value: allow},
			{key: 'deny', old_value: deny},
		],
	};
}

describe('presentChannelCreate', () => {
	it('shows a text channel made from the create modal as a summary with nothing to expand', () => {
		expect(present(createFixture())).toEqual({
			summary: 'Hampus created the text channel #general.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each<[string, Record<string, unknown>]>([
		['Hampus created the text channel #general.', {type: 0}],
		[`Hampus created the text channel #general in #${CATEGORY_ID}.`, {type: 0, parent_id: CATEGORY_ID}],
		['Hampus created the voice channel #Lounge.', VOICE_DEFAULTS],
		[`Hampus created the voice channel #Lounge in #${CATEGORY_ID}.`, {...VOICE_DEFAULTS, parent_id: CATEGORY_ID}],
		['Hampus created the link channel #general.', {type: 998, url: null}],
		[`Hampus created the link channel #general in #${CATEGORY_ID}.`, {type: 998, url: null, parent_id: CATEGORY_ID}],
		['Hampus created the channel #general.', {type: 13}],
		[`Hampus created the channel #general in #${CATEGORY_ID}.`, {type: 13, parent_id: CATEGORY_ID}],
		['Hampus created the category #Info.', {type: 4, name: 'Info'}],
		['Hampus created the category #Info.', {type: 4, name: 'Info', parent_id: CATEGORY_ID}],
	])('gives "%s"', (summary, values) => {
		expect(present(createFixture(values))).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('reads the type from the options when the changes lack it, including dispatch strings', () => {
		const {changes = []} = createFixture();
		expect(
			present({
				action_type: AuditLogActionType.CHANNEL_CREATE,
				target_id: CHANNEL_ID,
				options: {type: '998'},
				changes: changes.filter((candidate) => candidate.key !== 'type'),
			}).summary,
		).toBe('Hampus created the link channel #general.');
		expect(present({...createFixture({type: 2}), options: {type: 0}}).summary).toBe(
			'Hampus created the voice channel #general.',
		);
		expect(present({...createFixture({type: 'voice'}), options: undefined}).summary).toBe(
			'Hampus created the channel #general.',
		);
	});

	it('records the channel name and points the category chip at the category', () => {
		expect(summaryValues(createFixture({parent_id: CATEGORY_ID}))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: 'general', fallback: 'channel'},
			category: {kind: 'channel', id: CATEGORY_ID, recordedName: null, fallback: 'category'},
		});
		expect(summaryValues(createFixture({type: 4, name: 'Info'}))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: 'Info', fallback: 'category'},
		});
	});

	it('names the system as the actor of a channel it created', () => {
		expect(present({...createFixture(), user_id: '0'}).summary).toBe('System created the text channel #general.');
	});

	it.each<[string, Record<string, unknown>, Array<string>]>([
		['the link of a link channel', {type: 998, url: 'https://fluxer.app'}, ['~ Set the link to https://fluxer.app.']],
		['no link on a text channel', {type: 0, url: 'https://fluxer.app'}, []],
		['no blank link', {type: 998, url: '   '}, []],
		['the topic of a text channel', {type: 0, topic: 'Rules here'}, ['~ Set the topic to Rules here.']],
		['the topic of a voice channel', {...VOICE_DEFAULTS, topic: 'Hang out'}, ['~ Set the topic to Hang out.']],
		['the topic of a link channel', {type: 998, topic: 'Docs'}, ['~ Set the topic to Docs.']],
		['no topic on a category', {type: 4, topic: 'Info'}, []],
		['no blank topic', {topic: '  '}, []],
		['a mature channel', {nsfw: true}, ['+ Marked the channel as containing mature content.']],
		['a channel marked not mature', {nsfw: false}, ['- Marked the channel as not containing mature content.']],
		['no maturity when it follows the community', {nsfw: null}, []],
		[
			'a content warning with text',
			{content_warning_level: 1, content_warning_text: 'Spoilers'},
			['+ Added a content warning with the text Spoilers.'],
		],
		['a content warning without text', {content_warning_level: 1}, ['+ Added a content warning.']],
		[
			'a content warning with blank text',
			{content_warning_level: 1, content_warning_text: '  '},
			['+ Added a content warning.'],
		],
		['no content warning at level 0', {content_warning_level: 0, content_warning_text: 'Spoilers'}, []],
		['no content warning on a category', {type: 4, content_warning_level: 1}, []],
		['slowmode on a text channel', {rate_limit_per_user: 30}, ['~ Set slowmode to 30 seconds.']],
		['slowmode on a voice channel', {...VOICE_DEFAULTS, rate_limit_per_user: 3600}, ['~ Set slowmode to 1 hour.']],
		['no slowmode on a link channel', {type: 998, rate_limit_per_user: 30}, []],
		['no slowmode on a category', {type: 4, rate_limit_per_user: 30}, []],
		['no slowmode at 0', {rate_limit_per_user: 0}, []],
		['the audio quality', {...VOICE_DEFAULTS, bitrate: 96000}, ['~ Set the audio quality to 96 kbps.']],
		['no audio quality at 0', {...VOICE_DEFAULTS, bitrate: 0}, []],
		['no audio quality on a text channel', {bitrate: 96000}, []],
		['a user limit of 1', {...VOICE_DEFAULTS, user_limit: 1}, ['~ Limited the channel to 1 user.']],
		['a user limit of 10', {...VOICE_DEFAULTS, user_limit: 10}, ['~ Limited the channel to 10 users.']],
		['no user limit on a text channel', {user_limit: 10}, []],
		[
			'1 connection per member',
			{...VOICE_DEFAULTS, voice_connection_limit: 1},
			['~ Allowed up to 1 connection per member.'],
		],
		[
			'10 connections per member',
			{...VOICE_DEFAULTS, voice_connection_limit: 10},
			['~ Allowed up to 10 connections per member.'],
		],
		['no connection limit when it is null', {...VOICE_DEFAULTS, voice_connection_limit: null}, []],
		['no connection limit on a text channel', {voice_connection_limit: 10}, []],
		['the voice region', {...VOICE_DEFAULTS, rtc_region: 'rotterdam'}, ['~ Set the voice region to rotterdam.']],
		['no voice region on a text channel', {rtc_region: 'rotterdam'}, []],
	])('shows %s', (_label, values, rows) => {
		const presentation = present(createFixture(values));
		expect({rows: presentation.rows, blocks: presentation.blocks, expandable: presentation.expandable}).toEqual({
			rows,
			blocks: [],
			expandable: rows.length > 0,
		});
	});

	it('gives no rows for the voice defaults', () => {
		expect(present(createFixture(VOICE_DEFAULTS))).toEqual({
			summary: 'Hampus created the voice channel #Lounge.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('lists every voice channel row in order whatever order the changes arrive in, after the reason', () => {
		const fixture = createFixture({
			...VOICE_DEFAULTS,
			parent_id: CATEGORY_ID,
			topic: 'Hang out',
			nsfw: true,
			content_warning_level: 1,
			content_warning_text: 'Loud',
			rate_limit_per_user: 300,
			bitrate: 128000,
			user_limit: 10,
			voice_connection_limit: 2,
			rtc_region: 'rotterdam',
		});
		expect(present({...fixture, changes: [...(fixture.changes ?? [])].reverse(), reason: 'setup'})).toEqual({
			summary: `Hampus created the voice channel #Lounge in #${CATEGORY_ID}.`,
			rows: [
				'~ Set the topic to Hang out.',
				'+ Marked the channel as containing mature content.',
				'+ Added a content warning with the text Loud.',
				'~ Set slowmode to 5 minutes.',
				'~ Set the audio quality to 128 kbps.',
				'~ Limited the channel to 10 users.',
				'~ Allowed up to 2 connections per member.',
				'~ Set the voice region to rotterdam.',
			],
			blocks: [{kind: 'reason', text: 'setup'}],
			expandable: true,
		});
	});

	it('puts the link before the topic of a link channel', () => {
		expect(present(createFixture({type: 998, url: 'https://fluxer.app', topic: 'Docs'})).rows).toEqual([
			'~ Set the link to https://fluxer.app.',
			'~ Set the topic to Docs.',
		]);
	});

	it('becomes expandable for a reason alone', () => {
		expect(present({...createFixture(), reason: 'Bot%20setup'})).toEqual({
			summary: 'Hampus created the text channel #general.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Bot setup'}],
			expandable: true,
		});
	});

	it('reads dispatch string values', () => {
		expect(
			present(
				createFixture({
					type: '2',
					name: 'Lounge',
					nsfw: 'true',
					bitrate: '96000',
					user_limit: '10',
					voice_connection_limit: '5',
				}),
			),
		).toEqual({
			summary: 'Hampus created the voice channel #Lounge.',
			rows: [
				'+ Marked the channel as containing mature content.',
				'~ Set the audio quality to 96 kbps.',
				'~ Limited the channel to 10 users.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('prints no row whose value fails its reader', () => {
		expect(
			present({
				action_type: AuditLogActionType.CHANNEL_CREATE,
				target_id: 'not-a-snowflake',
				options: {type: 'voice'},
				changes: [
					{key: 'type', new_value: {}},
					{key: 'name', new_value: 42},
					{key: 'parent_id', new_value: 1400000000},
					{key: 'topic', new_value: ['Rules']},
					{key: 'nsfw', new_value: 'yes'},
					{key: 'content_warning_level', new_value: 'on'},
					{key: 'rate_limit_per_user', new_value: 'fast'},
				],
			}),
		).toEqual({summary: 'Hampus created the channel #.', rows: [], blocks: [], expandable: false});
	});
});

describe('presentChannelUpdate', () => {
	it('shows the bot rename of a link channel as one sentence with nothing to expand', () => {
		expect(present(updateFixture([change('name', 'User count: 417,017', 'User count: 417,018')], 998))).toEqual({
			summary: 'Hampus renamed the link channel User count: 417,017 to User count: 417,018.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each<[string, unknown]>([
		['Hampus renamed the text channel old-name to new-name.', 0],
		['Hampus renamed the voice channel old-name to new-name.', 2],
		['Hampus renamed the link channel old-name to new-name.', 998],
		['Hampus renamed the category old-name to new-name.', 4],
		['Hampus renamed the category old-name to new-name.', '4'],
		['Hampus renamed the channel old-name to new-name.', 13],
		['Hampus renamed the channel old-name to new-name.', undefined],
	])('gives "%s" for a rename alone', (summary, type) => {
		expect(present(updateFixture([change('name', 'old-name', 'new-name')], type))).toEqual({
			summary,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('keeps a rename alone expandable when it has a reason', () => {
		expect(present({...updateFixture([change('name', 'old-name', 'new-name')], 0), reason: 'cleanup'})).toEqual({
			summary: 'Hampus renamed the text channel old-name to new-name.',
			rows: [],
			blocks: [{kind: 'reason', text: 'cleanup'}],
			expandable: true,
		});
	});

	it.each<[string, unknown]>([
		[`Hampus changed the permission overrides of the text channel #${CHANNEL_ID}.`, 0],
		[`Hampus changed the permission overrides of the voice channel #${CHANNEL_ID}.`, 2],
		[`Hampus changed the permission overrides of the link channel #${CHANNEL_ID}.`, 998],
		[`Hampus changed the permission overrides of the category #${CHANNEL_ID}.`, 4],
		[`Hampus changed the permission overrides of the channel #${CHANNEL_ID}.`, 13],
	])('gives "%s" for a legacy overwrite count change alone', (summary, type) => {
		expect(present(updateFixture([change('permission_overwrite_count', 2, 3)], type))).toEqual({
			summary,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each<[string, unknown]>([
		[`Hampus updated the text channel #${CHANNEL_ID}.`, 0],
		[`Hampus updated the voice channel #${CHANNEL_ID}.`, 2],
		[`Hampus updated the link channel #${CHANNEL_ID}.`, 998],
		[`Hampus updated the category #${CHANNEL_ID}.`, 4],
		[`Hampus updated the channel #${CHANNEL_ID}.`, 13],
	])('gives "%s" for other changes', (summary, type) => {
		expect(present(updateFixture([change('topic', null, 'Welcome')], type))).toEqual({
			summary,
			rows: ['+ Set the topic to Welcome.'],
			blocks: [],
			expandable: true,
		});
	});

	it.each<[string, Array<ChangeFixture>]>([
		['no changes', []],
		[
			'only keys it never shows',
			[change('channel_id', CHANNEL_ID, CHANNEL_ID), change('type', 0, 2), change('position', 3, 4)],
		],
		['values that read as equal', [change('topic', '  ', null), change('rtc_region', '', null)]],
		['an overwrite count that did not change', [change('permission_overwrite_count', 3, 3)]],
		['an overwrite count it cannot read', [change('permission_overwrite_count', 'three', 4)]],
	])('gives the plain summary with nothing to expand for %s', (_label, changes) => {
		expect(present(updateFixture(changes, 0))).toEqual({
			summary: `Hampus updated the text channel #${CHANNEL_ID}.`,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('leaves the channel chip without a recorded name when the channel was not renamed', () => {
		expect(summaryValues(updateFixture([change('topic', null, 'Welcome')], 0))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
		});
		expect(summaryValues(updateFixture([change('permission_overwrite_count', 0, 1)], 4))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'category'},
		});
	});

	it('uses the new name and a name row when a rename comes with other changes', () => {
		expect(
			present(updateFixture([change('name', 'old-name', 'new-name'), change('permission_overwrite_count', 1, 2)], 0)),
		).toEqual({
			summary: 'Hampus updated the text channel #new-name.',
			rows: ['~ Changed the name from old-name to new-name.'],
			blocks: [],
			expandable: true,
		});
		expect(
			summaryValues(updateFixture([change('name', 'old-name', 'new-name'), change('nsfw', null, true)], 0)),
		).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: 'new-name', fallback: 'channel'},
		});
	});

	it.each<[string, Array<ChangeFixture>, Array<string>]>([
		[
			'a move into a category',
			[change('parent_id', null, CATEGORY_ID)],
			[`~ Moved the channel into the category #${CATEGORY_ID}.`],
		],
		[
			'a move out of a category',
			[change('parent_id', CATEGORY_ID, null)],
			[`~ Removed the channel from the category #${CATEGORY_ID}.`],
		],
		[
			'a move between categories',
			[change('parent_id', CATEGORY_ID, OTHER_CATEGORY_ID)],
			[`~ Moved the channel from the category #${CATEGORY_ID} to the category #${OTHER_CATEGORY_ID}.`],
		],
		['a link that was set', [change('url', null, 'https://fluxer.app')], ['+ Set the link to https://fluxer.app.']],
		[
			'a link that changed',
			[change('url', 'https://fluxer.app', 'https://docs.fluxer.app')],
			['~ Changed the link from https://fluxer.app to https://docs.fluxer.app.'],
		],
		['a link that was removed', [change('url', 'https://fluxer.app', null)], ['- Removed the link.']],
		['a topic that was set', [change('topic', null, 'Welcome')], ['+ Set the topic to Welcome.']],
		['a topic that was set from blank', [change('topic', ' ', 'Welcome')], ['+ Set the topic to Welcome.']],
		['a topic that was removed', [change('topic', 'Welcome', null)], ['- Removed the topic Welcome.']],
		['a topic that changed', [change('topic', 'Welcome', 'Hello')], ['~ Changed the topic from Welcome to Hello.']],
		[
			'a channel marked mature from the community setting',
			[change('nsfw', null, true)],
			['+ Marked the channel as containing mature content.'],
		],
		[
			'a channel marked mature from not mature',
			[change('nsfw', false, true)],
			['+ Marked the channel as containing mature content.'],
		],
		[
			'a channel marked not mature from the community setting',
			[change('nsfw', null, false)],
			['- Marked the channel as not containing mature content.'],
		],
		[
			'a channel marked not mature from mature',
			[change('nsfw', true, false)],
			['- Marked the channel as not containing mature content.'],
		],
		[
			'a mature channel that follows the community again',
			[change('nsfw', true, null)],
			['~ Made the channel inherit its mature content setting.'],
		],
		[
			'a not mature channel that follows the community again',
			[change('nsfw', false, null)],
			['~ Made the channel inherit its mature content setting.'],
		],
		['no maturity row for a value it cannot read', [change('nsfw', true, 'yes')], []],
		['slowmode that was turned on', [change('rate_limit_per_user', 0, 30)], ['+ Set slowmode to 30 seconds.']],
		[
			'slowmode that changed',
			[change('rate_limit_per_user', 30, 3600)],
			['~ Changed slowmode from 30 seconds to 1 hour.'],
		],
		['slowmode that was turned off', [change('rate_limit_per_user', 3600, 0)], ['- Turned off slowmode.']],
		[
			'slowmode sent as dispatch strings',
			[change('rate_limit_per_user', '0', '10')],
			['+ Set slowmode to 10 seconds.'],
		],
		['no slowmode row for a value it cannot read', [change('rate_limit_per_user', 30, 'fast')], []],
		[
			'an audio quality change',
			[change('bitrate', 64000, 96000)],
			['~ Changed the audio quality from 64 kbps to 96 kbps.'],
		],
		['no audio quality row from 0', [change('bitrate', 0, 64000)], []],
		['a user limit of 1', [change('user_limit', 0, 1)], ['~ Limited the channel to 1 user.']],
		['a user limit of 10', [change('user_limit', 0, 10)], ['~ Limited the channel to 10 users.']],
		['a user limit that changed', [change('user_limit', 10, 20)], ['~ Changed the user limit from 10 to 20.']],
		['a user limit that was removed', [change('user_limit', 10, 0)], ['~ Removed the user limit.']],
		[
			'a connection limit that changed',
			[change('voice_connection_limit', 5, 10)],
			['~ Changed the connections per member from 5 to 10.'],
		],
		['no connection limit row from null', [change('voice_connection_limit', null, 5)], []],
		[
			'a voice region that was set',
			[change('rtc_region', null, 'rotterdam')],
			['~ Set the voice region to rotterdam.'],
		],
		[
			'a voice region back to automatic',
			[change('rtc_region', 'rotterdam', null)],
			['~ Set the voice region to automatic.'],
		],
		[
			'a voice region that changed',
			[change('rtc_region', 'rotterdam', 'frankfurt')],
			['~ Changed the voice region from rotterdam to frankfurt.'],
		],
	])('shows %s', (_label, changes, rows) => {
		expect(present(updateFixture(changes, 2))).toEqual({
			summary: `Hampus updated the voice channel #${CHANNEL_ID}.`,
			rows,
			blocks: [],
			expandable: rows.length > 0,
		});
	});

	it.each<[string, Array<ChangeFixture>, Array<string>]>([
		[
			'a content warning added with text',
			[change('content_warning_level', 0, 1), change('content_warning_text', null, 'Spoilers')],
			['+ Added a content warning with the text Spoilers.'],
		],
		[
			'a content warning added without a text change',
			[change('content_warning_level', 0, 1)],
			['+ Added a content warning.'],
		],
		[
			'a content warning added with blank text',
			[change('content_warning_level', 0, 1), change('content_warning_text', null, '  ')],
			['+ Added a content warning.'],
		],
		[
			'a content warning added from a level it cannot read',
			[change('content_warning_level', null, 1)],
			['+ Added a content warning.'],
		],
		[
			'a content warning removed with its text',
			[change('content_warning_level', 1, 0), change('content_warning_text', 'Spoilers', null)],
			['- Removed the content warning.'],
		],
		['a content warning removed alone', [change('content_warning_level', 1, 0)], ['- Removed the content warning.']],
		[
			'content warning text that was set',
			[change('content_warning_text', null, 'Spoilers')],
			['~ Set the content warning text to Spoilers.'],
		],
		[
			'content warning text that changed',
			[change('content_warning_text', 'Spoilers', 'Gore')],
			['~ Changed the content warning text from Spoilers to Gore.'],
		],
		[
			'content warning text that was removed',
			[change('content_warning_text', 'Spoilers', null)],
			['~ Removed the content warning text.'],
		],
		['no row for an unknown level alone', [change('content_warning_level', 0, 2)], []],
		[
			'a text change next to an unknown level',
			[change('content_warning_level', 0, 2), change('content_warning_text', null, 'Spoilers')],
			['~ Set the content warning text to Spoilers.'],
		],
	])('shows %s', (_label, changes, rows) => {
		expect(present(updateFixture(changes, 0))).toEqual({
			summary: `Hampus updated the text channel #${CHANNEL_ID}.`,
			rows,
			blocks: [],
			expandable: rows.length > 0,
		});
	});

	it('lists the rows of an overview save in order', () => {
		expect(
			present(
				updateFixture(
					[change('rate_limit_per_user', 0, 10), change('topic', null, 'Hi'), change('name', 'old-name', 'new-name')],
					0,
				),
			),
		).toEqual({
			summary: 'Hampus updated the text channel #new-name.',
			rows: [
				'~ Changed the name from old-name to new-name.',
				'+ Set the topic to Hi.',
				'+ Set slowmode to 10 seconds.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('orders every row kind the same way whatever order the changes arrive in', () => {
		const changes = [
			change('name', 'old-name', 'new-name'),
			change('parent_id', null, CATEGORY_ID),
			change('url', null, 'https://fluxer.app'),
			change('topic', 'Welcome', null),
			change('nsfw', null, true),
			change('content_warning_level', 1, 0),
			change('rate_limit_per_user', 30, 0),
			change('bitrate', 64000, 96000),
			change('user_limit', 0, 10),
			change('voice_connection_limit', 5, 10),
			change('rtc_region', null, 'rotterdam'),
			change('permission_overwrite_count', 0, 2),
			change('position', 1, 2),
		];
		expect(present(updateFixture([...changes].reverse(), 2))).toEqual({
			summary: 'Hampus updated the voice channel #new-name.',
			rows: [
				'~ Changed the name from old-name to new-name.',
				`~ Moved the channel into the category #${CATEGORY_ID}.`,
				'+ Set the link to https://fluxer.app.',
				'- Removed the topic Welcome.',
				'+ Marked the channel as containing mature content.',
				'- Removed the content warning.',
				'- Turned off slowmode.',
				'~ Changed the audio quality from 64 kbps to 96 kbps.',
				'~ Limited the channel to 10 users.',
				'~ Changed the connections per member from 5 to 10.',
				'~ Set the voice region to rotterdam.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('gives a rename with an old name it cannot read the plain summary with the new name', () => {
		expect(present(updateFixture([change('name', 42, 'new-name')], 0))).toEqual({
			summary: 'Hampus updated the text channel #new-name.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('presentChannelDelete', () => {
	it.each<[string, Record<string, unknown>]>([
		['Hampus deleted the text channel #general.', {type: 0}],
		['Hampus deleted the voice channel #Lounge.', VOICE_DEFAULTS],
		['Hampus deleted the link channel #Docs.', {type: 998, name: 'Docs', url: 'https://fluxer.app'}],
		['Hampus deleted the category #Info.', {type: 4, name: 'Info'}],
		['Hampus deleted the channel #general.', {type: 13}],
	])('gives "%s" with no rows', (summary, values) => {
		expect(present(deleteFixture(values))).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('reads the type from the changes when the options lack it, and prefers the options', () => {
		expect(present({...deleteFixture(VOICE_DEFAULTS), options: undefined}).summary).toBe(
			'Hampus deleted the voice channel #Lounge.',
		);
		expect(present({...deleteFixture({type: 0, name: 'Info'}), options: {type: '4'}}).summary).toBe(
			'Hampus deleted the category #Info.',
		);
	});

	it('records the deleted name and picks the deleted label by type', () => {
		expect(summaryValues(deleteFixture({type: 4, name: 'Info'}))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: 'Info', fallback: 'category'},
		});
		expect(summaryValues(deleteFixture({name: null}))).toEqual({
			actor: ACTOR,
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
		});
	});

	it('becomes expandable for a reason alone', () => {
		expect(present({...deleteFixture(), reason: 'Old channel'})).toEqual({
			summary: 'Hampus deleted the text channel #general.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Old channel'}],
			expandable: true,
		});
	});
});

describe('channel permission override presenters', () => {
	it.each<[string, EntryFixture]>([
		[
			`Hampus added a permission override for the role @Mods in #${CHANNEL_ID}.`,
			overwriteCreated(ROLE_OVERWRITE, '0', '0'),
		],
		[`Hampus added a permission override for ender in #${CHANNEL_ID}.`, overwriteCreated(MEMBER_OVERWRITE, '0', '0')],
		[
			`Hampus added a permission override for @everyone in #${CHANNEL_ID}.`,
			overwriteCreated(EVERYONE_OVERWRITE, '0', '0'),
		],
		[
			`Hampus updated the permission override for the role @Mods in #${CHANNEL_ID}.`,
			overwriteUpdated(ROLE_OVERWRITE, []),
		],
		[`Hampus updated the permission override for ender in #${CHANNEL_ID}.`, overwriteUpdated(MEMBER_OVERWRITE, [])],
		[
			`Hampus updated the permission override for @everyone in #${CHANNEL_ID}.`,
			overwriteUpdated(EVERYONE_OVERWRITE, []),
		],
		[
			`Hampus removed the permission override for the role @Mods from #${CHANNEL_ID}.`,
			overwriteDeleted(ROLE_OVERWRITE, '0', '0'),
		],
		[
			`Hampus removed the permission override for ender from #${CHANNEL_ID}.`,
			overwriteDeleted(MEMBER_OVERWRITE, '0', '0'),
		],
		[
			`Hampus removed the permission override for @everyone from #${CHANNEL_ID}.`,
			overwriteDeleted(EVERYONE_OVERWRITE, '0', '0'),
		],
	])('gives "%s" with no rows for an empty override', (summary, fixture) => {
		expect(present(fixture)).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('passes the role, member and channel chips', () => {
		const channel = {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'};
		expect(summaryValues(overwriteCreated(ROLE_OVERWRITE, '0', '0'))).toEqual({
			actor: ACTOR,
			role: {kind: 'role', id: ROLE_ID, recordedName: 'Mods'},
			channel,
		});
		expect(summaryValues(overwriteUpdated(MEMBER_OVERWRITE, []))).toEqual({
			actor: ACTOR,
			user: {kind: 'user', id: MEMBER_ID},
			channel,
		});
		expect(summaryValues(overwriteDeleted(EVERYONE_OVERWRITE, '0', '0'))).toEqual({actor: ACTOR, channel});
	});

	it('gives a role without a recorded name an empty recorded name', () => {
		const fixture = overwriteCreated({target_id: ROLE_ID, options: {channel_id: CHANNEL_ID, type: 0}}, '0', '0');
		expect(summaryValues(fixture)).toEqual({
			actor: ACTOR,
			role: {kind: 'role', id: ROLE_ID, recordedName: null},
			channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
		});
		expect(present(fixture).summary).toBe(
			`Hampus added a permission override for the role @${ROLE_ID} in #${CHANNEL_ID}.`,
		);
	});

	it('reads the target kind from the changes when the options lack it', () => {
		expect(present({...overwriteCreated(MEMBER_OVERWRITE, '0', '0'), options: {channel_id: CHANNEL_ID}}).summary).toBe(
			`Hampus added a permission override for ender in #${CHANNEL_ID}.`,
		);
		expect(present({...overwriteDeleted(MEMBER_OVERWRITE, '0', '0'), options: {channel_id: CHANNEL_ID}}).summary).toBe(
			`Hampus removed the permission override for ender from #${CHANNEL_ID}.`,
		);
		expect(
			present({
				action_type: AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
				target_id: MEMBER_ID,
				options: {channel_id: CHANNEL_ID},
				changes: [change('type', '0', '1')],
			}).summary,
		).toBe(`Hampus updated the permission override for ender in #${CHANNEL_ID}.`);
	});

	it('treats a missing target kind as a role, and a member on the community id as a member', () => {
		expect(
			present({
				action_type: AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
				target_id: ROLE_ID,
				options: {channel_id: CHANNEL_ID},
			}).summary,
		).toBe(`Hampus added a permission override for the role @${ROLE_ID} in #${CHANNEL_ID}.`);
		expect(
			present(overwriteCreated({target_id: TEST_GUILD_ID, options: {channel_id: CHANNEL_ID, type: '1'}}, '0', '0'))
				.summary,
		).toBe(`Hampus added a permission override for @${TEST_GUILD_ID} in #${CHANNEL_ID}.`);
	});

	it('gives an empty channel chip when the options lack the channel', () => {
		const fixture = {...overwriteDeleted(ROLE_OVERWRITE, '0', '0'), options: {type: 0, role_name: 'Mods'}};
		expect(summaryValues(fixture).channel).toEqual({kind: 'channel', id: '', recordedName: null, fallback: 'channel'});
		expect(present(fixture).summary).toBe('Hampus removed the permission override for the role @Mods from #.');
	});

	it('keeps the reason block from the request header', () => {
		expect(
			present({...overwriteCreated(EVERYONE_OVERWRITE, '0', mask(Permissions.VIEW_CHANNEL)), reason: 'setup'}),
		).toEqual({
			summary: `Hampus added a permission override for @everyone in #${CHANNEL_ID}.`,
			rows: ['- Denied View channel.'],
			blocks: [{kind: 'reason', text: 'setup'}],
			expandable: true,
		});
	});

	it.each<[string, EntryFixture, Array<string>]>([
		[
			'allowed permissions',
			overwriteCreated(ROLE_OVERWRITE, mask(Permissions.VIEW_CHANNEL, Permissions.SEND_MESSAGES), '0'),
			['+ Allowed View channel and Send messages.'],
		],
		[
			'denied permissions',
			overwriteCreated(ROLE_OVERWRITE, '0', mask(Permissions.SEND_MESSAGES)),
			['- Denied Send messages.'],
		],
		[
			'allowed and denied permissions in that order',
			overwriteCreated(
				ROLE_OVERWRITE,
				mask(Permissions.VIEW_CHANNEL),
				mask(Permissions.ATTACH_FILES, Permissions.SEND_MESSAGES),
			),
			['+ Allowed View channel.', '- Denied Send messages and Attach files.'],
		],
		[
			'numeric allow and deny',
			overwriteCreated(ROLE_OVERWRITE, Number(Permissions.VIEW_CHANNEL), Number(Permissions.SEND_MESSAGES)),
			['+ Allowed View channel.', '- Denied Send messages.'],
		],
		['unknown bits only', overwriteCreated(ROLE_OVERWRITE, UNKNOWN_PERMISSION_BIT.toString(), '0'), []],
		[
			'known bits next to unknown bits',
			overwriteCreated(ROLE_OVERWRITE, (UNKNOWN_PERMISSION_BIT | Permissions.VIEW_CHANNEL).toString(), '0'),
			['+ Allowed View channel.'],
		],
		[
			'a readable deny next to an allow it cannot read',
			overwriteCreated(ROLE_OVERWRITE, 'everything', mask(Permissions.SEND_MESSAGES)),
			['- Denied Send messages.'],
		],
		['no allow or deny at all', {...overwriteCreated(ROLE_OVERWRITE, '0', '0'), changes: []}, []],
	])('shows %s on an added override', (_label, fixture, rows) => {
		expect(present(fixture)).toEqual({
			summary: `Hampus added a permission override for the role @Mods in #${CHANNEL_ID}.`,
			rows,
			blocks: [],
			expandable: rows.length > 0,
		});
	});

	it('caps a long permission list at seven names and a count', () => {
		const flags = [
			Permissions.ADD_REACTIONS,
			Permissions.VIEW_CHANNEL,
			Permissions.SEND_MESSAGES,
			Permissions.MANAGE_MESSAGES,
			Permissions.EMBED_LINKS,
			Permissions.ATTACH_FILES,
			Permissions.READ_MESSAGE_HISTORY,
			Permissions.CONNECT,
			Permissions.SPEAK,
		];
		const presentation = presentAuditLogEntry(
			makeEntry(overwriteCreated(MEMBER_OVERWRITE, mask(...flags), '0')),
			fakeContext(),
		);
		expect(presentation.rows.map((row) => row.sentence.values)).toEqual([{permissions: {kind: 'permissions', flags}}]);
		expect(resultToText(presentation, NAMES).rows).toEqual([
			'+ Allowed Add reactions, View channel, Send messages, Manage messages, Embed links, Attach files, Read message history, and 2 more permissions.',
		]);
	});

	it.each<[string, Array<ChangeFixture>, Array<string>]>([
		[
			'a newly allowed permission',
			[change('allow', mask(Permissions.VIEW_CHANNEL), mask(Permissions.VIEW_CHANNEL, Permissions.SEND_MESSAGES))],
			['+ Allowed Send messages.'],
		],
		['a newly denied permission', [change('deny', '0', mask(Permissions.ATTACH_FILES))], ['- Denied Attach files.']],
		[
			'an allowed permission back to default',
			[change('allow', mask(Permissions.VIEW_CHANNEL, Permissions.SEND_MESSAGES), mask(Permissions.VIEW_CHANNEL))],
			['~ Removed the override for Send messages.'],
		],
		[
			'a denied permission back to default',
			[change('deny', mask(Permissions.ATTACH_FILES), '0')],
			['~ Removed the override for Attach files.'],
		],
		[
			'a flip from allowed to denied',
			[change('allow', mask(Permissions.SEND_MESSAGES), '0'), change('deny', '0', mask(Permissions.SEND_MESSAGES))],
			['- Denied Send messages.'],
		],
		[
			'a flip from denied to allowed',
			[change('allow', '0', mask(Permissions.SEND_MESSAGES)), change('deny', mask(Permissions.SEND_MESSAGES), '0')],
			['+ Allowed Send messages.'],
		],
		[
			'every kind of change in order',
			[
				change('deny', mask(Permissions.EMBED_LINKS), mask(Permissions.SEND_MESSAGES)),
				change('allow', mask(Permissions.ATTACH_FILES), mask(Permissions.VIEW_CHANNEL)),
			],
			[
				'+ Allowed View channel.',
				'- Denied Send messages.',
				'~ Removed the override for Embed links and Attach files.',
			],
		],
		['numeric values', [change('allow', 0, Number(Permissions.VIEW_CHANNEL))], ['+ Allowed View channel.']],
		['a change to unknown bits only', [change('allow', '0', UNKNOWN_PERMISSION_BIT.toString())], []],
		['a value it cannot read', [change('allow', mask(Permissions.VIEW_CHANNEL), 'everything')], []],
	])('shows %s on an updated override', (_label, changes, rows) => {
		expect(present(overwriteUpdated(ROLE_OVERWRITE, changes))).toEqual({
			summary: `Hampus updated the permission override for the role @Mods in #${CHANNEL_ID}.`,
			rows,
			blocks: [],
			expandable: rows.length > 0,
		});
	});

	it('gives a type flip alone the summary only', () => {
		expect(present(overwriteUpdated(MEMBER_OVERWRITE, [change('type', '0', '1')]))).toEqual({
			summary: `Hampus updated the permission override for ender in #${CHANNEL_ID}.`,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each<[string, EntryFixture, string, Array<string>]>([
		[
			'allowed and denied permissions',
			overwriteDeleted(ROLE_OVERWRITE, mask(Permissions.VIEW_CHANNEL), mask(Permissions.SEND_MESSAGES)),
			`Hampus removed the permission override for the role @Mods from #${CHANNEL_ID}.`,
			['~ The override allowed View channel.', '~ The override denied Send messages.'],
		],
		[
			'allowed permissions',
			overwriteDeleted(MEMBER_OVERWRITE, mask(Permissions.VIEW_CHANNEL, Permissions.SEND_MESSAGES), '0'),
			`Hampus removed the permission override for ender from #${CHANNEL_ID}.`,
			['~ The override allowed View channel and Send messages.'],
		],
		[
			'numeric denied permissions',
			overwriteDeleted(EVERYONE_OVERWRITE, 0, Number(Permissions.VIEW_CHANNEL)),
			`Hampus removed the permission override for @everyone from #${CHANNEL_ID}.`,
			['~ The override denied View channel.'],
		],
		[
			'unknown bits only',
			overwriteDeleted(EVERYONE_OVERWRITE, UNKNOWN_PERMISSION_BIT.toString(), '0'),
			`Hampus removed the permission override for @everyone from #${CHANNEL_ID}.`,
			[],
		],
	])('shows %s on a removed override', (_label, fixture, summary, rows) => {
		expect(present(fixture)).toEqual({summary, rows, blocks: [], expandable: rows.length > 0});
	});
});

describe('channel presenters on junk input', () => {
	it.each<[string, AuditLogActionType]>([
		['CHANNEL_CREATE', AuditLogActionType.CHANNEL_CREATE],
		['CHANNEL_UPDATE', AuditLogActionType.CHANNEL_UPDATE],
		['CHANNEL_DELETE', AuditLogActionType.CHANNEL_DELETE],
		['CHANNEL_OVERWRITE_CREATE', AuditLogActionType.CHANNEL_OVERWRITE_CREATE],
		['CHANNEL_OVERWRITE_UPDATE', AuditLogActionType.CHANNEL_OVERWRITE_UPDATE],
		['CHANNEL_OVERWRITE_DELETE', AuditLogActionType.CHANNEL_OVERWRITE_DELETE],
	])('never throws for %s', (_name, actionType) => {
		const fixtures: Array<EntryFixture> = [
			{action_type: actionType},
			{action_type: actionType, target_id: null, user_id: null, options: {}, changes: []},
			{
				action_type: actionType,
				target_id: 'not-a-snowflake',
				options: {type: {}, channel_id: 5, role_name: 7},
				changes: [
					{key: 'name'},
					{key: 'type', old_value: [], new_value: {}},
					{key: 'allow', old_value: -1, new_value: 1.5},
					{key: 'deny', new_value: 'NaN'},
					{key: 'parent_id', old_value: 1, new_value: true},
					{key: 'nsfw', new_value: 0},
					{key: 'content_warning_level', old_value: '1', new_value: 'x'},
					{key: 'user_limit', old_value: '10', new_value: -3},
					{key: 'bitrate', old_value: Number.NaN, new_value: '64k'},
				],
			},
		];
		for (const fixture of fixtures) {
			expect(() => present(fixture)).not.toThrow();
		}
	});
});
