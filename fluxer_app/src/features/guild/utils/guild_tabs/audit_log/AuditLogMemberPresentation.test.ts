// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	presentMemberBanAdd,
	presentMemberBanRemove,
	presentMemberDisconnect,
	presentMemberMove,
	presentMemberRoleUpdate,
	presentMemberUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberPresentation';
import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
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
type PresentedText = ReturnType<typeof resultToText> & {expandable: boolean};

const TARGET_ID = '1400000000000000100';
const MODERATOR_ID = '1400000000000000101';
const BOT_ID = '1400000000000000102';
const ROLE_A = '1400000000000000200';
const ROLE_B = '1400000000000000201';
const ROLE_C = '1400000000000000202';
const CHANNEL_A = '1400000000000000300';
const CHANNEL_B = '1400000000000000301';
const EARLIER_PAST = '2026-08-30T12:00:00.000Z';
const PAST = '2026-08-31T12:00:00.000Z';
const ENTRY_TIME = '2026-09-01T12:00:00.000Z';
const BAN_TIME = '2026-09-01T12:00:00.007Z';
const FUTURE = '2026-09-02T12:00:00.000Z';
const LATER_FUTURE = '2026-09-03T18:30:00.000Z';
const USER_NAMES: Readonly<Record<string, string>> = {
	[TEST_ACTOR_ID]: 'Hampus',
	[TARGET_ID]: 'ender',
	[MODERATOR_ID]: 'Mira',
	[BOT_ID]: 'MyBot',
};

function present(fixture: EntryFixture): PresentedText {
	const presentation = presentAuditLogEntry(makeEntry(fixture), fakeContext());
	return {...resultToText(presentation, USER_NAMES), expandable: presentation.expandable};
}

function change(key: string, oldValue: unknown, newValue: unknown): ChangeFixture {
	return {key, old_value: oldValue, new_value: newValue};
}

function memberUpdate(changes: Array<ChangeFixture>, targetId: string = TARGET_ID): EntryFixture {
	return {action_type: AuditLogActionType.MEMBER_UPDATE, target_id: targetId, changes};
}

function banAdd(changes: Array<ChangeFixture>, extra: Partial<EntryFixture> = {}): EntryFixture {
	return {action_type: AuditLogActionType.MEMBER_BAN_ADD, target_id: TARGET_ID, changes, ...extra};
}

function banSnapshot(fields: {
	bannedAt?: string;
	expiresAt?: string | null;
	reason?: string | null;
}): Array<ChangeFixture> {
	return [
		{key: 'user_id', new_value: TARGET_ID},
		{key: 'moderator_id', new_value: TEST_ACTOR_ID},
		{key: 'banned_at', new_value: fields.bannedAt ?? BAN_TIME},
		{key: 'expires_at', new_value: fields.expiresAt ?? null},
		{key: 'reason', new_value: fields.reason ?? null},
	];
}

function banRemove(changes: Array<ChangeFixture>, extra: Partial<EntryFixture> = {}): EntryFixture {
	return {action_type: AuditLogActionType.MEMBER_BAN_REMOVE, target_id: TARGET_ID, changes, ...extra};
}

function roleUpdate(changes: Array<ChangeFixture>, extra: Partial<EntryFixture> = {}): EntryFixture {
	return {action_type: AuditLogActionType.MEMBER_ROLE_UPDATE, target_id: TARGET_ID, changes, ...extra};
}

describe('presentMemberKick', () => {
	it('summarizes a kick with no rows', () => {
		expect(present({action_type: AuditLogActionType.MEMBER_KICK, target_id: TARGET_ID})).toEqual({
			summary: 'Hampus kicked ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('shows the decoded header reason as the only detail', () => {
		expect(
			present({action_type: AuditLogActionType.MEMBER_KICK, target_id: TARGET_ID, reason: 'Posting%20scam%20links'}),
		).toEqual({
			summary: 'Hampus kicked ender.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Posting scam links'}],
			expandable: true,
		});
	});
});

describe('presentMemberPrune', () => {
	it('summarizes a prune and ignores stray prune options and changes', () => {
		expect(
			present({
				action_type: AuditLogActionType.MEMBER_PRUNE,
				options: {members_removed: 3, delete_member_days: '7'},
				changes: [{key: 'prune_delete_days', new_value: 7}],
			}),
		).toEqual({summary: 'Hampus pruned inactive members.', rows: [], blocks: [], expandable: false});
	});
});

describe('presentMemberBanAdd', () => {
	it('shows the screenshot 1 legacy row as a bare summary', () => {
		expect(present(banAdd(banSnapshot({}), {options: {delete_member_days: '0'}}))).toEqual({
			summary: 'Hampus banned ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('shows a body reason that the API resolved into the entry reason once', () => {
		expect(
			present(banAdd(banSnapshot({reason: 'Spamming invites'}), {reason: 'Spamming invites', options: {}})),
		).toEqual({
			summary: 'Hampus banned ender.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Spamming invites'}],
			expandable: true,
		});
	});

	it('shows a body-only reason as a ban reason block when the entry has no reason', () => {
		expect(present(banAdd(banSnapshot({reason: 'Spamming invites'})))).toEqual({
			summary: 'Hampus banned ender.',
			rows: [],
			blocks: [{kind: 'ban_reason', text: 'Spamming invites'}],
			expandable: true,
		});
	});

	it('adds a ban reason block after the Reason block when the two reasons differ', () => {
		expect(present(banAdd(banSnapshot({reason: 'Spamming invites'}), {reason: 'Raid%20cleanup'}))).toEqual({
			summary: 'Hampus banned ender.',
			rows: [],
			blocks: [
				{kind: 'reason', text: 'Raid cleanup'},
				{kind: 'ban_reason', text: 'Spamming invites'},
			],
			expandable: true,
		});
	});

	it('adds no ban reason block when both reasons decode to the same text', () => {
		expect(present(banAdd(banSnapshot({reason: ' Raid%20cleanup '}), {reason: 'Raid cleanup'}))).toEqual({
			summary: 'Hampus banned ender.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Raid cleanup'}],
			expandable: true,
		});
	});

	it('adds no ban reason block for a blank ban reason', () => {
		expect(present(banAdd(banSnapshot({reason: '   '}))).blocks).toEqual([]);
	});

	it.each([
		{name: 'a 3-day ban', expiresAt: '2026-09-04T12:00:00.000Z', summary: 'Hampus banned ender for 3 days.'},
		{name: 'a 12-hour ban', expiresAt: '2026-09-02T00:00:00.000Z', summary: 'Hampus banned ender for 12 hours.'},
		{name: 'a 90-minute ban', expiresAt: '2026-09-01T13:30:00.000Z', summary: 'Hampus banned ender for 90 minutes.'},
	])('gives the length of $name measured from banned_at', ({expiresAt, summary}) => {
		expect(present(banAdd(banSnapshot({expiresAt})))).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('measures a temporary ban from the entry time when banned_at is missing', () => {
		expect(present(banAdd([change('expires_at', null, FUTURE)])).summary).toBe('Hampus banned ender for 1 day.');
	});

	it.each([
		{name: 'an unparseable expiry', expiresAt: 'soon'},
		{name: 'an expiry equal to banned_at', expiresAt: BAN_TIME},
		{name: 'an expiry before banned_at', expiresAt: PAST},
	])('treats $name as a permanent ban', ({expiresAt}) => {
		expect(present(banAdd(banSnapshot({expiresAt}))).summary).toBe('Hampus banned ender.');
	});

	it.each([
		{
			name: 'numeric delete_message_seconds',
			options: {delete_message_seconds: 3600},
			row: "- Deleted 1 hour of the member's recent message history.",
		},
		{
			name: 'dispatch string delete_message_seconds',
			options: {delete_message_seconds: '43200'},
			row: "- Deleted 12 hours of the member's recent message history.",
		},
		{
			name: 'delete_message_seconds over legacy days',
			options: {delete_message_seconds: 21600, delete_member_days: '7'},
			row: "- Deleted 6 hours of the member's recent message history.",
		},
		{
			name: 'legacy delete_member_days "7"',
			options: {delete_member_days: '7'},
			row: "- Deleted 7 days of the member's recent message history.",
		},
		{
			name: 'legacy days when delete_message_seconds is 0',
			options: {delete_message_seconds: 0, delete_member_days: '2'},
			row: "- Deleted 2 days of the member's recent message history.",
		},
	])('lists deleted messages from $name', ({options, row}) => {
		expect(present(banAdd(banSnapshot({}), {options}))).toEqual({
			summary: 'Hampus banned ender.',
			rows: [row],
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{delete_member_days: '0'},
		{delete_member_days: 'x'},
		{delete_member_days: '1.5'},
		{delete_member_days: '-2'},
		{delete_member_days: ''},
		{delete_message_seconds: 'lots'},
		{},
	])('adds no deletion row for options %j', (options) => {
		expect(present(banAdd(banSnapshot({}), {options})).rows).toEqual([]);
	});

	it('combines a temporary ban, both reasons and a deletion', () => {
		expect(
			present(
				banAdd(banSnapshot({expiresAt: '2026-09-08T12:00:00.000Z', reason: 'Spamming invites'}), {
					reason: 'Raid cleanup',
					options: {delete_message_seconds: 86400},
				}),
			),
		).toEqual({
			summary: 'Hampus banned ender for 7 days.',
			rows: ["- Deleted 1 day of the member's recent message history."],
			blocks: [
				{kind: 'reason', text: 'Raid cleanup'},
				{kind: 'ban_reason', text: 'Spamming invites'},
			],
			expandable: true,
		});
	});

	it('passes the ban length and deletion window as durations', () => {
		const result = presentMemberBanAdd(
			makeEntry(
				banAdd(banSnapshot({expiresAt: '2026-09-04T12:00:00.000Z'}), {options: {delete_message_seconds: 3600}}),
			),
		);
		expect(result.summary.values.duration).toEqual({kind: 'duration', seconds: 259200});
		expect(result.rows[0]?.sentence.values.duration).toEqual({kind: 'duration', seconds: 3600});
	});
});

describe('presentMemberBanRemove', () => {
	it.each([
		{
			name: 'a ban by another moderator with its date',
			changes: [
				{key: 'moderator_id', old_value: MODERATOR_ID},
				{key: 'banned_at', old_value: '2026-08-20T08:30:00.000Z'},
			],
			rows: ['~ Originally banned by Mira on 2026-08-20T08:30:00.000Z.'],
		},
		{
			name: 'a ban by the same moderator with its date',
			changes: [
				{key: 'moderator_id', old_value: TEST_ACTOR_ID},
				{key: 'banned_at', old_value: '2026-08-20T08:30:00.000Z'},
			],
			rows: ['~ Originally banned on 2026-08-20T08:30:00.000Z.'],
		},
		{
			name: 'a ban with its date and no moderator',
			changes: [{key: 'banned_at', old_value: '2026-08-20T08:30:00.000Z'}],
			rows: ['~ Originally banned on 2026-08-20T08:30:00.000Z.'],
		},
		{
			name: 'a ban by another moderator without a date',
			changes: [{key: 'moderator_id', old_value: MODERATOR_ID}],
			rows: ['~ Originally banned by Mira.'],
		},
	])('describes the origin of $name', ({changes, rows}) => {
		expect(present(banRemove(changes))).toEqual({
			summary: 'Hampus unbanned ender.',
			rows,
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{name: 'the same moderator without a date', changes: [{key: 'moderator_id', old_value: TEST_ACTOR_ID}]},
		{name: 'nothing recorded', changes: []},
		{
			name: 'junk values',
			changes: [
				{key: 'moderator_id', old_value: 1400000000000000},
				{key: 'banned_at', old_value: 'yesterday'},
				{key: 'expires_at', old_value: 42},
				{key: 'reason', old_value: 7},
			],
		},
	])('shows only the summary for $name', ({changes}) => {
		expect(present(banRemove(changes))).toEqual({
			summary: 'Hampus unbanned ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each([
		{name: 'a future expiry', expiresAt: LATER_FUTURE, row: `~ The ban was scheduled to expire on ${LATER_FUTURE}.`},
		{name: 'a past expiry', expiresAt: PAST, row: `~ The ban had already expired on ${PAST}.`},
		{
			name: 'an expiry at the entry time',
			expiresAt: ENTRY_TIME,
			row: `~ The ban had already expired on ${ENTRY_TIME}.`,
		},
	])('describes $name', ({expiresAt, row}) => {
		expect(present(banRemove([{key: 'expires_at', old_value: expiresAt}])).rows).toEqual([row]);
	});

	it('shows the full removed snapshot with the unban reason and the original ban reason', () => {
		expect(
			present(
				banRemove(
					[
						{key: 'user_id', old_value: TARGET_ID},
						{key: 'moderator_id', old_value: MODERATOR_ID},
						{key: 'banned_at', old_value: EARLIER_PAST},
						{key: 'expires_at', old_value: FUTURE},
						{key: 'reason', old_value: 'Spamming%20invites'},
					],
					{reason: 'Appeal accepted'},
				),
			),
		).toEqual({
			summary: 'Hampus unbanned ender.',
			rows: [`~ Originally banned by Mira on ${EARLIER_PAST}.`, `~ The ban was scheduled to expire on ${FUTURE}.`],
			blocks: [
				{kind: 'reason', text: 'Appeal accepted'},
				{kind: 'ban_reason', text: 'Spamming invites'},
			],
			expandable: true,
		});
	});

	it('shows a ban reason block even without other rows', () => {
		expect(present(banRemove([{key: 'reason', old_value: 'Spam'}]))).toEqual({
			summary: 'Hampus unbanned ender.',
			rows: [],
			blocks: [{kind: 'ban_reason', text: 'Spam'}],
			expandable: true,
		});
	});

	it('passes the original moderator as a user and the dates as dates', () => {
		const result = presentMemberBanRemove(
			makeEntry(
				banRemove([
					{key: 'moderator_id', old_value: MODERATOR_ID},
					{key: 'banned_at', old_value: EARLIER_PAST},
				]),
			),
		);
		expect(result.rows[0]?.sentence.values).toEqual({
			user: {kind: 'user', id: MODERATOR_ID},
			date: {kind: 'date', timestamp: Date.parse(EARLIER_PAST)},
		});
	});
});

describe('presentMemberUpdate', () => {
	it.each([
		{
			name: 'a nickname set for another member',
			targetId: TARGET_ID,
			changes: [change('nick', null, 'Ender')],
			summary: 'Hampus set the nickname of ender to Ender.',
		},
		{
			name: 'a blank nickname replaced for another member',
			targetId: TARGET_ID,
			changes: [change('nick', '  ', 'Ender')],
			summary: 'Hampus set the nickname of ender to Ender.',
		},
		{
			name: 'a nickname changed for another member',
			targetId: TARGET_ID,
			changes: [change('nick', 'Ender', 'Endy')],
			summary: 'Hampus changed the nickname of ender from Ender to Endy.',
		},
		{
			name: 'a nickname removed from another member',
			targetId: TARGET_ID,
			changes: [change('nick', 'Endy', null)],
			summary: 'Hampus removed the nickname Endy from ender.',
		},
		{
			name: 'an own nickname set',
			targetId: TEST_ACTOR_ID,
			changes: [change('nick', null, 'Hamp')],
			summary: 'Hampus set their nickname to Hamp.',
		},
		{
			name: 'an own nickname changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('nick', 'Hamp', 'H')],
			summary: 'Hampus changed their nickname from Hamp to H.',
		},
		{
			name: 'an own nickname removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('nick', 'H', null)],
			summary: 'Hampus removed their nickname H.',
		},
		{
			name: 'a timeout set',
			targetId: TARGET_ID,
			changes: [change('communication_disabled_until', null, FUTURE)],
			summary: `Hampus timed out ender until ${FUTURE}.`,
		},
		{
			name: 'a timeout set over an expired one',
			targetId: TARGET_ID,
			changes: [change('communication_disabled_until', PAST, FUTURE)],
			summary: `Hampus timed out ender until ${FUTURE}.`,
		},
		{
			name: 'a timeout set on the actor',
			targetId: TEST_ACTOR_ID,
			changes: [change('communication_disabled_until', null, FUTURE)],
			summary: `Hampus timed out Hampus until ${FUTURE}.`,
		},
		{
			name: 'a running timeout changed',
			targetId: TARGET_ID,
			changes: [change('communication_disabled_until', FUTURE, LATER_FUTURE)],
			summary: `Hampus changed ender's block on sending messages, reacting and joining voice to end at ${LATER_FUTURE}.`,
		},
		{
			name: 'a running timeout removed',
			targetId: TARGET_ID,
			changes: [change('communication_disabled_until', FUTURE, null)],
			summary: 'Hampus removed the timeout for ender.',
		},
		{
			name: 'a running timeout ended with a past time',
			targetId: TARGET_ID,
			changes: [change('communication_disabled_until', FUTURE, PAST)],
			summary: 'Hampus removed the timeout for ender.',
		},
		{
			name: 'a community mute applied',
			targetId: TARGET_ID,
			changes: [change('mute', false, true)],
			summary: "Hampus turned off ender's microphone in this community.",
		},
		{
			name: 'a community mute removed',
			targetId: TARGET_ID,
			changes: [change('mute', true, false)],
			summary: "Hampus turned ender's microphone back on in this community.",
		},
		{
			name: 'a community mute applied to the actor',
			targetId: TEST_ACTOR_ID,
			changes: [change('mute', false, true)],
			summary: "Hampus turned off Hampus's microphone in this community.",
		},
		{
			name: 'a community deafen applied',
			targetId: TARGET_ID,
			changes: [change('deaf', false, true)],
			summary: 'Hampus stopped ender from hearing others in this community.',
		},
		{
			name: 'a community deafen removed',
			targetId: TARGET_ID,
			changes: [change('deaf', true, false)],
			summary: 'Hampus let ender hear others in this community again.',
		},
		{
			name: 'a community deafen removed from the actor',
			targetId: TEST_ACTOR_ID,
			changes: [change('deaf', true, false)],
			summary: 'Hampus let Hampus hear others in this community again.',
		},
		{
			name: 'one role added',
			targetId: TARGET_ID,
			changes: [change('roles', [ROLE_A], [ROLE_A, ROLE_B])],
			summary: `Hampus gave ender the role @${ROLE_B}.`,
		},
		{
			name: 'one role removed',
			targetId: TARGET_ID,
			changes: [change('roles', [ROLE_A, ROLE_B], [ROLE_B])],
			summary: `Hampus removed the role @${ROLE_A} from ender.`,
		},
		{
			name: 'one role added to the actor',
			targetId: TEST_ACTOR_ID,
			changes: [change('roles', [], [ROLE_A])],
			summary: `Hampus gave Hampus the role @${ROLE_A}.`,
		},
		{
			name: 'an own community avatar set',
			targetId: TEST_ACTOR_ID,
			changes: [change('avatar_hash', null, 'a1')],
			summary: 'Hampus set their community profile avatar.',
		},
		{
			name: 'an own community avatar changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('avatar_hash', 'a1', 'a2')],
			summary: 'Hampus changed their community profile avatar.',
		},
		{
			name: 'an own community avatar removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('avatar_hash', 'a2', null)],
			summary: 'Hampus removed their community profile avatar.',
		},
		{
			name: 'an own community banner set',
			targetId: TEST_ACTOR_ID,
			changes: [change('banner_hash', null, 'b1')],
			summary: 'Hampus set their community profile banner.',
		},
		{
			name: 'an own community banner changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('banner_hash', 'b1', 'b2')],
			summary: 'Hampus changed their community profile banner.',
		},
		{
			name: 'an own community banner removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('banner_hash', 'b2', null)],
			summary: 'Hampus removed their community profile banner.',
		},
		{
			name: 'an own community bio set',
			targetId: TEST_ACTOR_ID,
			changes: [change('bio', null, 'Hello there')],
			summary: 'Hampus set their community profile bio to Hello there.',
		},
		{
			name: 'an own community bio changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('bio', 'Hello there', 'Back soon')],
			summary: 'Hampus changed their community profile bio to Back soon.',
		},
		{
			name: 'an own community bio removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('bio', 'Back soon', null)],
			summary: 'Hampus removed their community profile bio.',
		},
		{
			name: 'own community pronouns set',
			targetId: TEST_ACTOR_ID,
			changes: [change('pronouns', null, 'he/him')],
			summary: 'Hampus set their community profile pronouns to he/him.',
		},
		{
			name: 'own community pronouns changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('pronouns', 'he/him', 'they/them')],
			summary: 'Hampus set their community profile pronouns to they/them.',
		},
		{
			name: 'own community pronouns removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('pronouns', 'they/them', null)],
			summary: 'Hampus removed their community profile pronouns.',
		},
		{
			name: 'an own community accent color set',
			targetId: TEST_ACTOR_ID,
			changes: [change('accent_color', null, 16711680)],
			summary: 'Hampus set their community profile accent color to #FF0000.',
		},
		{
			name: 'an own community accent color changed',
			targetId: TEST_ACTOR_ID,
			changes: [change('accent_color', 16711680, 255)],
			summary: 'Hampus set their community profile accent color to #0000FF.',
		},
		{
			name: 'an own community accent color removed',
			targetId: TEST_ACTOR_ID,
			changes: [change('accent_color', 255, null)],
			summary: 'Hampus removed their community profile accent color.',
		},
	])('gives a specific summary for $name', ({targetId, changes, summary}) => {
		expect(present(memberUpdate(changes, targetId))).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it.each([
		{
			name: 'a community avatar set',
			changes: [change('avatar_hash', null, 'a1')],
			row: "+ Added the member's community profile avatar.",
		},
		{
			name: 'a community avatar changed',
			changes: [change('avatar_hash', 'a1', 'a2')],
			row: "~ Changed the member's community profile avatar.",
		},
		{
			name: 'a community avatar removed',
			changes: [change('avatar_hash', 'a2', null)],
			row: "- Removed the member's community profile avatar.",
		},
		{
			name: 'a community banner set',
			changes: [change('banner_hash', null, 'b1')],
			row: "+ Added the member's community profile banner.",
		},
		{
			name: 'a community banner changed',
			changes: [change('banner_hash', 'b1', 'b2')],
			row: "~ Changed the member's community profile banner.",
		},
		{
			name: 'a community banner removed',
			changes: [change('banner_hash', 'b2', null)],
			row: "- Removed the member's community profile banner.",
		},
		{
			name: 'a community bio set',
			changes: [change('bio', null, 'Hello there')],
			row: "+ Set the member's community profile bio to Hello there.",
		},
		{
			name: 'a community bio changed',
			changes: [change('bio', 'Hello there', 'Back soon')],
			row: "~ Changed the member's community profile bio to Back soon.",
		},
		{
			name: 'a community bio removed',
			changes: [change('bio', 'Back soon', null)],
			row: "- Removed the member's community profile bio.",
		},
		{
			name: 'community pronouns set',
			changes: [change('pronouns', null, 'he/him')],
			row: "~ Set the member's community profile pronouns to he/him.",
		},
		{
			name: 'community pronouns changed',
			changes: [change('pronouns', 'he/him', 'they/them')],
			row: "~ Set the member's community profile pronouns to they/them.",
		},
		{
			name: 'community pronouns removed',
			changes: [change('pronouns', 'they/them', null)],
			row: "- Removed the member's community profile pronouns.",
		},
		{
			name: 'a community accent color set',
			changes: [change('accent_color', null, 16711680)],
			row: "~ Set the member's community profile accent color to #FF0000.",
		},
		{
			name: 'a community accent color changed',
			changes: [change('accent_color', 16711680, 255)],
			row: "~ Set the member's community profile accent color to #0000FF.",
		},
		{
			name: 'a community accent color removed',
			changes: [change('accent_color', 255, null)],
			row: "- Removed the member's community profile accent color.",
		},
	])('uses the generic summary and a row for $name on another member', ({changes, row}) => {
		expect(present(memberUpdate(changes))).toEqual({
			summary: 'Hampus updated ender.',
			rows: [row],
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{name: 'a nickname set', groupChange: change('nick', null, 'Ender'), row: '+ Set the nickname to Ender.'},
		{
			name: 'a nickname changed',
			groupChange: change('nick', 'Ender', 'Endy'),
			row: '~ Changed the nickname from Ender to Endy.',
		},
		{name: 'a nickname removed', groupChange: change('nick', 'Endy', null), row: '- Removed the nickname Endy.'},
		{name: 'a role added', groupChange: change('roles', [], [ROLE_A]), row: `+ Added the role @${ROLE_A}.`},
		{name: 'a role removed', groupChange: change('roles', [ROLE_A], []), row: `- Removed the role @${ROLE_A}.`},
		{
			name: 'a timeout set',
			groupChange: change('communication_disabled_until', null, FUTURE),
			row: `+ Blocked the member from sending messages, reacting and joining voice until ${FUTURE}.`,
		},
		{
			name: 'a timeout changed',
			groupChange: change('communication_disabled_until', FUTURE, LATER_FUTURE),
			row: `~ Changed the block on sending messages, reacting and joining voice to end at ${LATER_FUTURE}.`,
		},
		{
			name: 'a timeout removed',
			groupChange: change('communication_disabled_until', FUTURE, null),
			row: '- Removed the timeout.',
		},
		{
			name: 'a community mute applied',
			groupChange: change('mute', false, true),
			row: "+ Turned off the member's microphone in this community.",
		},
		{
			name: 'a community mute removed',
			groupChange: change('mute', true, false),
			row: "- Turned the member's microphone back on in this community.",
		},
		{
			name: 'a community deafen applied',
			groupChange: change('deaf', false, true),
			row: '+ Stopped the member from hearing others in this community.',
		},
		{
			name: 'a community deafen removed',
			groupChange: change('deaf', true, false),
			row: '- Let the member hear others in this community again.',
		},
	])('renders the row for $name under the generic summary', ({groupChange, row}) => {
		expect(present(memberUpdate([change('accent_color', null, 255), groupChange]))).toEqual({
			summary: 'Hampus updated ender.',
			rows: [row, "~ Set the member's community profile accent color to #0000FF."],
			blocks: [],
			expandable: true,
		});
	});

	it('orders every group row under the generic summary regardless of the stored key order', () => {
		expect(
			present(
				memberUpdate([
					change('temporary', true, false),
					change('accent_color', null, 255),
					change('pronouns', null, 'he/him'),
					change('bio', null, 'Hello'),
					change('banner_hash', 'b1', null),
					change('avatar_hash', 'a1', 'a2'),
					change('deaf', false, true),
					change('mute', true, false),
					change('communication_disabled_until', null, FUTURE),
					change('roles', [ROLE_A], [ROLE_B, ROLE_C]),
					change('nick', null, 'Ender'),
					change('user_id', TARGET_ID, TARGET_ID),
				]),
			),
		).toEqual({
			summary: 'Hampus updated ender.',
			rows: [
				'+ Set the nickname to Ender.',
				`+ Added the role @${ROLE_B}.`,
				`+ Added the role @${ROLE_C}.`,
				`- Removed the role @${ROLE_A}.`,
				`+ Blocked the member from sending messages, reacting and joining voice until ${FUTURE}.`,
				"- Turned the member's microphone back on in this community.",
				'+ Stopped the member from hearing others in this community.',
				"~ Changed the member's community profile avatar.",
				"- Removed the member's community profile banner.",
				"+ Set the member's community profile bio to Hello.",
				"~ Set the member's community profile pronouns to he/him.",
				"~ Set the member's community profile accent color to #0000FF.",
				'~ The member is no longer temporary.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('summarizes an own profile save with several changes as a community profile update', () => {
		expect(
			present(
				memberUpdate(
					[
						change('nick', 'Hampus', 'Hamp'),
						change('avatar_hash', null, 'a1'),
						change('bio', null, 'Building Fluxer'),
						change('pronouns', null, 'he/him'),
						change('accent_color', null, 16711680),
					],
					TEST_ACTOR_ID,
				),
			),
		).toEqual({
			summary: 'Hampus updated their community profile.',
			rows: [
				'~ Changed the nickname from Hampus to Hamp.',
				"+ Added the member's community profile avatar.",
				"+ Set the member's community profile bio to Building Fluxer.",
				"~ Set the member's community profile pronouns to he/him.",
				"~ Set the member's community profile accent color to #FF0000.",
			],
			blocks: [],
			expandable: true,
		});
	});

	it('uses the roles summary with a row per role when several roles change', () => {
		expect(present(memberUpdate([change('roles', [ROLE_A], [ROLE_B, ROLE_C])]))).toEqual({
			summary: 'Hampus updated the roles of ender.',
			rows: [`+ Added the role @${ROLE_B}.`, `+ Added the role @${ROLE_C}.`, `- Removed the role @${ROLE_A}.`],
			blocks: [],
			expandable: true,
		});
	});

	it('names a deleted role by its id and never by a role_name option', () => {
		const entry = makeEntry({...memberUpdate([change('roles', [], [ROLE_A])]), options: {role_name: 'Moderators'}});
		expect(presentMemberUpdate(entry).summary.values.role).toEqual({kind: 'role', id: ROLE_A, recordedName: null});
		expect(
			present({...memberUpdate([change('roles', [], [ROLE_A])]), options: {role_name: 'Moderators'}}).summary,
		).toBe(`Hampus gave ender the role @${ROLE_A}.`);
	});

	it('adds the temporary row to a specific summary', () => {
		expect(present(memberUpdate([change('roles', [], [ROLE_A]), change('temporary', true, false)]))).toEqual({
			summary: `Hampus gave ender the role @${ROLE_A}.`,
			rows: ['~ The member is no longer temporary.'],
			blocks: [],
			expandable: true,
		});
	});

	it('adds the temporary row after the role rows of the roles summary', () => {
		expect(present(memberUpdate([change('roles', [ROLE_A], [ROLE_B]), change('temporary', true, false)]))).toEqual({
			summary: 'Hampus updated the roles of ender.',
			rows: [`+ Added the role @${ROLE_B}.`, `- Removed the role @${ROLE_A}.`, '~ The member is no longer temporary.'],
			blocks: [],
			expandable: true,
		});
	});

	it('shows a lone temporary change under the generic summary and ignores the reverse direction', () => {
		expect(present(memberUpdate([change('temporary', true, false)]))).toEqual({
			summary: 'Hampus updated ender.',
			rows: ['~ The member is no longer temporary.'],
			blocks: [],
			expandable: true,
		});
		expect(present(memberUpdate([change('temporary', false, true)])).rows).toEqual([]);
	});

	it('treats a timeout that moved from one past time to another as no change', () => {
		expect(present(memberUpdate([change('communication_disabled_until', EARLIER_PAST, PAST)]))).toEqual({
			summary: 'Hampus updated ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each([
		{name: 'another member', targetId: TARGET_ID, summary: 'Hampus updated ender.'},
		{name: 'the actor', targetId: TEST_ACTOR_ID, summary: 'Hampus updated their community profile.'},
	])('gives the generic summary for an empty update of $name', ({targetId, summary}) => {
		const empty = {summary, rows: [], blocks: [], expandable: false};
		expect(present({action_type: AuditLogActionType.MEMBER_UPDATE, target_id: targetId})).toEqual(empty);
		expect(present(memberUpdate([], targetId))).toEqual(empty);
	});

	it('shows the timeout reason from the entry reason and never reads options', () => {
		expect(
			present({
				...memberUpdate([change('communication_disabled_until', null, FUTURE)]),
				reason: 'Cool off',
				options: {timeout_reason: 'Ignored', communication_disabled_until: LATER_FUTURE},
			}),
		).toEqual({
			summary: `Hampus timed out ender until ${FUTURE}.`,
			rows: [],
			blocks: [{kind: 'reason', text: 'Cool off'}],
			expandable: true,
		});
	});

	it('omits groups whose values fail their readers or do not differ', () => {
		expect(
			present(
				memberUpdate([
					change('nick', 'Ender', ' Ender '),
					change('nick', null, 'ignored duplicate'),
					change('roles', 'R1', [ROLE_A, 5]),
					change('communication_disabled_until', 'soon', 'later'),
					change('mute', 'yes', true),
					change('deaf', true, true),
					change('avatar_hash', 5, null),
					change('banner_hash', 'b1', 'b1'),
					change('bio', null, '   '),
					change('pronouns', ['he/him'], null),
					change('accent_color', null, 'red'),
					change('temporary', 'true', 'false'),
					change('user_id', TARGET_ID, MODERATOR_ID),
				]),
			),
		).toEqual({
			summary: 'Hampus updated ender.',
			rows: ['~ The member is no longer temporary.'],
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{name: 'a fraction', value: 1.5},
		{name: 'a negative number', value: -1},
		{name: 'a value above white', value: 0x1000000},
		{name: 'a string', value: 'red'},
	])('ignores an accent color that is $name', ({value}) => {
		expect(present(memberUpdate([change('accent_color', null, value)])).rows).toEqual([]);
	});

	it('ignores role ids that are not snowflakes', () => {
		expect(present(memberUpdate([change('roles', ['everyone'], ['everyone', 'admins'])]))).toEqual({
			summary: 'Hampus updated ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('passes nicknames as names, profile text as boxed text, colors and dates to the renderer', () => {
		const values = (changes: Array<ChangeFixture>) =>
			presentMemberUpdate(makeEntry(memberUpdate(changes, TEST_ACTOR_ID))).summary.values;
		expect(values([change('nick', null, 'Hamp')]).nick).toEqual({kind: 'name', value: 'Hamp'});
		expect(values([change('bio', null, 'Hello')]).text).toEqual({kind: 'text', value: 'Hello'});
		expect(values([change('pronouns', null, 'he/him')]).text).toEqual({kind: 'text', value: 'he/him'});
		expect(values([change('accent_color', null, '255')]).color).toEqual({kind: 'color', value: 255});
		expect(values([change('communication_disabled_until', null, FUTURE)]).date).toEqual({
			kind: 'date',
			timestamp: Date.parse(FUTURE),
		});
	});
});

describe('presentMemberRoleUpdate', () => {
	it('names the added role by its recorded name', () => {
		expect(
			present(roleUpdate([change('roles', [ROLE_A], [ROLE_A, ROLE_B])], {options: {role_name: 'Moderators'}})),
		).toEqual({summary: 'Hampus gave ender the role @Moderators.', rows: [], blocks: [], expandable: false});
	});

	it('names the removed role by its recorded name', () => {
		expect(
			present(roleUpdate([change('roles', [ROLE_A, ROLE_B], [ROLE_A])], {options: {role_name: 'Moderators'}})),
		).toEqual({summary: 'Hampus removed the role @Moderators from ender.', rows: [], blocks: [], expandable: false});
	});

	it('uses the system sentences when the system gave or removed a role', () => {
		expect(
			present(roleUpdate([change('roles', [], [ROLE_A])], {user_id: '0', options: {role_name: 'Visionary'}})).summary,
		).toBe('The system gave ender the role @Visionary.');
		expect(
			present(roleUpdate([change('roles', [ROLE_A], [])], {user_id: '0', options: {role_name: 'Visionary'}})).summary,
		).toBe('The system removed the role @Visionary from ender.');
	});

	it('falls back to the role id when no role name was recorded', () => {
		const fixture = roleUpdate([change('roles', [], [ROLE_A])], {options: {role_name: '   '}});
		expect(presentMemberRoleUpdate(makeEntry(fixture)).summary.values.role).toEqual({
			kind: 'role',
			id: ROLE_A,
			recordedName: null,
		});
		expect(present(fixture).summary).toBe(`Hampus gave ender the role @${ROLE_A}.`);
	});

	it('gives the roles summary for an empty legacy row and ignores its role_id and action options', () => {
		expect(present(roleUpdate([], {options: {role_id: ROLE_A, action: 'add'}}))).toEqual({
			summary: 'Hampus updated the roles of ender.',
			rows: [],
			blocks: [],
			expandable: false,
		});
		expect(present({action_type: AuditLogActionType.MEMBER_ROLE_UPDATE, target_id: TARGET_ID}).summary).toBe(
			'Hampus updated the roles of ender.',
		);
	});

	it('adds the temporary row to a single role change', () => {
		expect(
			present(
				roleUpdate([change('roles', [], [ROLE_A]), change('temporary', true, false)], {
					options: {role_name: 'Moderators'},
				}),
			),
		).toEqual({
			summary: 'Hampus gave ender the role @Moderators.',
			rows: ['~ The member is no longer temporary.'],
			blocks: [],
			expandable: true,
		});
	});

	it('lists each role by id when several roles differ and ignores role_name', () => {
		expect(
			present(
				roleUpdate([change('roles', [ROLE_A], [ROLE_B, ROLE_C]), change('temporary', true, false)], {
					user_id: '0',
					options: {role_name: 'Moderators'},
				}),
			),
		).toEqual({
			summary: 'System updated the roles of ender.',
			rows: [
				`+ Added the role @${ROLE_B}.`,
				`+ Added the role @${ROLE_C}.`,
				`- Removed the role @${ROLE_A}.`,
				'~ The member is no longer temporary.',
			],
			blocks: [],
			expandable: true,
		});
	});
});

describe('presentMemberMove', () => {
	const move = (targetId: string, changes: Array<ChangeFixture> | undefined, options?: Record<string, unknown>) =>
		present({action_type: AuditLogActionType.MEMBER_MOVE, target_id: targetId, changes, options});

	it.each([
		{
			name: 'another member between two channels',
			targetId: TARGET_ID,
			changes: [change('channel_id', CHANNEL_A, CHANNEL_B)],
			options: {channel_id: CHANNEL_B, count: 1},
			summary: `Hampus moved ender from #${CHANNEL_A} to #${CHANNEL_B}.`,
		},
		{
			name: 'another member from an unknown channel',
			targetId: TARGET_ID,
			changes: [{key: 'channel_id', new_value: CHANNEL_B}],
			options: undefined,
			summary: `Hampus moved ender to #${CHANNEL_B}.`,
		},
		{
			name: 'another member to the options channel',
			targetId: TARGET_ID,
			changes: undefined,
			options: {channel_id: CHANNEL_B, count: '1'},
			summary: `Hampus moved ender to #${CHANNEL_B}.`,
		},
		{
			name: 'another member with no channels',
			targetId: TARGET_ID,
			changes: undefined,
			options: undefined,
			summary: 'Hampus moved ender to a different voice channel.',
		},
		{
			name: 'another member away from a known channel only',
			targetId: TARGET_ID,
			changes: [{key: 'channel_id', old_value: CHANNEL_A}],
			options: undefined,
			summary: 'Hampus moved ender to a different voice channel.',
		},
		{
			name: 'the actor between two channels',
			targetId: TEST_ACTOR_ID,
			changes: [change('channel_id', CHANNEL_A, CHANNEL_B)],
			options: {channel_id: CHANNEL_B},
			summary: `Hampus moved from #${CHANNEL_A} to #${CHANNEL_B}.`,
		},
		{
			name: 'the actor from an unknown channel',
			targetId: TEST_ACTOR_ID,
			changes: [{key: 'channel_id', new_value: CHANNEL_B}],
			options: undefined,
			summary: `Hampus moved to #${CHANNEL_B}.`,
		},
		{
			name: 'the actor with no channels',
			targetId: TEST_ACTOR_ID,
			changes: undefined,
			options: undefined,
			summary: 'Hampus moved to a different voice channel.',
		},
	])('summarizes a move of $name', ({targetId, changes, options, summary}) => {
		expect(move(targetId, changes, options)).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('passes both channels as channel chips that fall back to a deleted channel', () => {
		const result = presentMemberMove(
			makeEntry({
				action_type: AuditLogActionType.MEMBER_MOVE,
				target_id: TARGET_ID,
				changes: [change('channel_id', CHANNEL_A, CHANNEL_B)],
			}),
		);
		expect(result.summary.values.oldChannel).toEqual({
			kind: 'channel',
			id: CHANNEL_A,
			recordedName: null,
			fallback: 'channel',
		});
		expect(result.summary.values.newChannel).toEqual({
			kind: 'channel',
			id: CHANNEL_B,
			recordedName: null,
			fallback: 'channel',
		});
	});
});

describe('presentMemberDisconnect', () => {
	const disconnect = (targetId: string, changes: Array<ChangeFixture> | undefined, options?: Record<string, unknown>) =>
		present({action_type: AuditLogActionType.MEMBER_DISCONNECT, target_id: targetId, changes, options});

	it.each([
		{
			name: 'another member from a known channel',
			targetId: TARGET_ID,
			changes: [{key: 'channel_id', old_value: CHANNEL_A}],
			options: {channel_id: CHANNEL_A, count: 1},
			summary: `Hampus disconnected ender from #${CHANNEL_A}.`,
		},
		{
			name: 'another member from the options channel',
			targetId: TARGET_ID,
			changes: undefined,
			options: {channel_id: CHANNEL_A},
			summary: `Hampus disconnected ender from #${CHANNEL_A}.`,
		},
		{
			name: 'another member from an unknown channel',
			targetId: TARGET_ID,
			changes: undefined,
			options: undefined,
			summary: 'Hampus disconnected ender from voice.',
		},
		{
			name: 'the actor from a known channel',
			targetId: TEST_ACTOR_ID,
			changes: [{key: 'channel_id', old_value: CHANNEL_A}],
			options: undefined,
			summary: `Hampus disconnected from #${CHANNEL_A}.`,
		},
		{
			name: 'the actor from an unknown channel',
			targetId: TEST_ACTOR_ID,
			changes: [{key: 'channel_id', old_value: 'voice'}],
			options: undefined,
			summary: 'Hampus disconnected from voice.',
		},
	])('summarizes a disconnect of $name', ({targetId, changes, options, summary}) => {
		expect(disconnect(targetId, changes, options)).toEqual({summary, rows: [], blocks: [], expandable: false});
	});

	it('passes the channel as a channel chip', () => {
		const result = presentMemberDisconnect(
			makeEntry({
				action_type: AuditLogActionType.MEMBER_DISCONNECT,
				target_id: TARGET_ID,
				changes: [{key: 'channel_id', old_value: CHANNEL_A}],
			}),
		);
		expect(result.summary.values.channel).toEqual({
			kind: 'channel',
			id: CHANNEL_A,
			recordedName: null,
			fallback: 'channel',
		});
	});
});

describe('presentBotAdd', () => {
	it.each([
		{name: 'temporary false', options: {temporary: false}, summary: 'Hampus added the bot MyBot.'},
		{name: 'no options', options: undefined, summary: 'Hampus added the bot MyBot.'},
		{name: 'a junk temporary value', options: {temporary: 'yes'}, summary: 'Hampus added the bot MyBot.'},
		{name: 'temporary true', options: {temporary: true}, summary: 'Hampus added the bot MyBot as a temporary member.'},
		{
			name: 'the dispatch string "true"',
			options: {temporary: 'true'},
			summary: 'Hampus added the bot MyBot as a temporary member.',
		},
	])('summarizes a bot add with $name', ({options, summary}) => {
		expect(present({action_type: AuditLogActionType.BOT_ADD, target_id: BOT_ID, options})).toEqual({
			summary,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('member presenters', () => {
	it('render a target that is missing or not a snowflake as an unknown user chip', () => {
		for (const target_id of [null, 'not-a-user']) {
			const presentation = presentAuditLogEntry(
				makeEntry({action_type: AuditLogActionType.MEMBER_KICK, target_id}),
				fakeContext(),
			);
			expect(presentation.summary.values.target).toEqual({kind: 'user', id: ''});
		}
	});

	it('never treat a missing actor and a missing target as the same member', () => {
		expect(
			present({
				action_type: AuditLogActionType.MEMBER_UPDATE,
				user_id: null,
				target_id: null,
				changes: [change('nick', null, 'Ender')],
			}).summary,
		).toBe('@ set the nickname of @ to Ender.');
	});
});
