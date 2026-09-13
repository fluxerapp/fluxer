// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentMemberBanAdd} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberPresentation';
import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import type {
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogSentence,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	CENTERED_LABEL,
	UNKNOWN_ACTION_SUMMARY,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
	TEST_GUILD_ID,
	toText,
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

vi.mock('@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberPresentation', async (importOriginal) => {
	const actual =
		await importOriginal<typeof import('@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberPresentation')>();
	return {...actual, presentMemberBanAdd: vi.fn(actual.presentMemberBanAdd)};
});

const ACTION_TYPES = Object.values(AuditLogActionType).filter(
	(value): value is AuditLogActionType => typeof value === 'number',
);
const UNKNOWN_ACTION_TYPE = 999 as AuditLogActionType;
const ACTOR: AuditLogPlaceholder = {kind: 'user', id: TEST_ACTOR_ID};

function sentence(message: string, values: AuditLogSentence['values']): AuditLogSentence {
	return {descriptor: {id: message, message}, values};
}

function domainResult(overrides: Partial<AuditLogDomainResult>): AuditLogDomainResult {
	return {summary: sentence('{actor} did a test action.', {actor: ACTOR}), rows: [], blocks: [], ...overrides};
}

describe('presentAuditLogEntry', () => {
	it('gives every action type a summary other than the unknown action summary', () => {
		const unpresented = ACTION_TYPES.filter(
			(actionType) =>
				presentAuditLogEntry(makeEntry({action_type: actionType}), fakeContext()).summary.descriptor.message ===
				UNKNOWN_ACTION_SUMMARY.message,
		).map((actionType) => AuditLogActionType[actionType]);
		expect(unpresented).toEqual([]);
	});

	it('formats the summary of every action type with placeholders that match its message', () => {
		for (const actionType of ACTION_TYPES) {
			const presentation = presentAuditLogEntry(makeEntry({action_type: actionType}), fakeContext());
			expect(() => resultToText(presentation)).not.toThrow();
		}
	});

	it('uses the unknown action summary for an action type this version does not know', () => {
		const presentation = presentAuditLogEntry(makeEntry({action_type: UNKNOWN_ACTION_TYPE}), fakeContext());
		expect(presentation.summary.descriptor).toBe(UNKNOWN_ACTION_SUMMARY);
		expect({...resultToText(presentation), expandable: presentation.expandable}).toEqual({
			summary: `@${TEST_ACTOR_ID} made a change that this version of the app cannot show.`,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('names the system as the actor of an unknown action it wrote', () => {
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: UNKNOWN_ACTION_TYPE, user_id: '0'}),
			fakeContext(),
		);
		expect(resultToText(presentation).summary).toBe('System made a change that this version of the app cannot show.');
	});

	it('passes the entry and context to the presenter for its action type', () => {
		const entry = makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD, target_id: '1400000000000000100'});
		const context = fakeContext();
		presentAuditLogEntry(entry, context);
		expect(presentMemberBanAdd).toHaveBeenLastCalledWith(entry, context);
	});

	it('puts the decoded Reason block before the domain blocks and becomes expandable', () => {
		vi.mocked(presentMemberBanAdd).mockReturnValueOnce(
			domainResult({blocks: [{kind: 'ban_reason', text: 'Raiding the server'}]}),
		);
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD, reason: '  Raid%20cleanup  '}),
			fakeContext(),
		);
		expect(presentation.blocks).toEqual([
			{kind: 'reason', text: 'Raid cleanup'},
			{kind: 'ban_reason', text: 'Raiding the server'},
		]);
		expect(presentation.expandable).toBe(true);
	});

	it('adds no Reason block for a blank reason', () => {
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: UNKNOWN_ACTION_TYPE, reason: '   '}),
			fakeContext(),
		);
		expect(presentation.blocks).toEqual([]);
		expect(presentation.expandable).toBe(false);
	});

	it('is expandable when the domain returns rows and no blocks', () => {
		vi.mocked(presentMemberBanAdd).mockReturnValueOnce(
			domainResult({
				rows: [{id: 'row', tone: 'remove', sentence: sentence('Deleted a test row.', {})}],
			}),
		);
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD}),
			fakeContext(),
		);
		expect({...resultToText(presentation), expandable: presentation.expandable}).toEqual({
			summary: `@${TEST_ACTOR_ID} did a test action.`,
			rows: ['- Deleted a test row.'],
			blocks: [],
			expandable: true,
		});
	});

	it('is expandable when the domain returns only blocks', () => {
		vi.mocked(presentMemberBanAdd).mockReturnValueOnce(domainResult({blocks: [{kind: 'ban_reason', text: 'Spam'}]}));
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD}),
			fakeContext(),
		);
		expect(presentation.blocks).toEqual([{kind: 'ban_reason', text: 'Spam'}]);
		expect(presentation.expandable).toBe(true);
	});

	it('is not expandable with no rows and no blocks', () => {
		vi.mocked(presentMemberBanAdd).mockReturnValueOnce(domainResult({}));
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD}),
			fakeContext(),
		);
		expect(presentation.expandable).toBe(false);
	});
});

describe('fakeContext', () => {
	it('resolves only the names it was given', () => {
		const context = fakeContext({webhooks: {'1': 'Deploy bot'}, emojis: {'2': 'blobcat'}, stickers: {'3': 'wave'}});
		expect(context.guildId).toBe(TEST_GUILD_ID);
		expect(context.getWebhookName('1')).toBe('Deploy bot');
		expect(context.getEmojiName('2')).toBe('blobcat');
		expect(context.getStickerName('3')).toBe('wave');
		expect(context.getWebhookName('2')).toBeNull();
		expect(fakeContext().getEmojiName('2')).toBeNull();
	});
});

describe('toText', () => {
	it('renders users by id or by the given names, and the system by its label', () => {
		const text = sentence('{actor} kicked {target}.', {actor: {kind: 'system'}, target: {kind: 'user', id: '42'}});
		expect(toText(text)).toBe('System kicked @42.');
		expect(toText(text, {'42': 'ender'})).toBe('System kicked ender.');
	});

	it('renders channels and roles from the recorded name, else the id, and @everyone for the guild id', () => {
		const text = sentence('{channel} {category} {role} {deletedRole} {everyone}', {
			channel: {kind: 'channel', id: '10', recordedName: 'general', fallback: 'channel'},
			category: {kind: 'channel', id: '11', recordedName: null, fallback: 'category'},
			role: {kind: 'role', id: '20', recordedName: 'Moderators'},
			deletedRole: {kind: 'role', id: '21', recordedName: null},
			everyone: {kind: 'role', id: TEST_GUILD_ID, recordedName: '@everyone'},
		});
		expect(toText(text, {'10': 'ignored', '20': 'ignored'})).toBe('#general #11 @Moderators @21 @everyone');
	});

	it('renders names, text, emojis, dates, colors and labels', () => {
		const text = sentence('{name}|{text}|{emoji}|{date}|{color}|{shortColor}|{label}', {
			name: {kind: 'name', value: 'Deploy bot'},
			text: {kind: 'text', value: 'Be nice'},
			emoji: {kind: 'emoji', id: null, name: 'blobcat'},
			date: {kind: 'date', timestamp: Date.parse('2026-09-01T12:00:00.000Z')},
			color: {kind: 'color', value: 0x5865f2},
			shortColor: {kind: 'color', value: 0xff},
			label: {kind: 'label', descriptor: CENTERED_LABEL},
		});
		expect(toText(text)).toBe('Deploy bot|Be nice|:blobcat:|2026-09-01T12:00:00.000Z|#5865F2|#0000FF|Centered');
	});

	it('renders durations in the largest whole unit', () => {
		const text = (seconds: number) => toText(sentence('{duration}', {duration: {kind: 'duration', seconds}}));
		expect(text(300)).toBe('5 minutes');
		expect(text(60)).toBe('1 minute');
		expect(text(90)).toBe('90 seconds');
		expect(text(3600)).toBe('1 hour');
		expect(text(259199.993)).toBe('3 days');
	});

	it('renders permission titles as a list and caps lists longer than eight', () => {
		const text = (flags: Array<bigint>) =>
			toText(sentence('Allowed {permissions}.', {permissions: {kind: 'permissions', flags}}));
		expect(text([Permissions.VIEW_CHANNEL])).toBe('Allowed View channel.');
		expect(text([Permissions.VIEW_CHANNEL, Permissions.SEND_MESSAGES])).toBe('Allowed View channel and Send messages.');
		const flags = Object.values(Permissions);
		expect(text(flags.slice(0, 8))).toBe(
			'Allowed Create invite links, Kick members, Ban members, Administrator, Manage channels, Manage community, Add reactions, and View activity log.',
		);
		expect(text(flags.slice(0, 9))).toBe(
			'Allowed Create invite links, Kick members, Ban members, Administrator, Manage channels, Manage community, Add reactions, and 2 more permissions.',
		);
	});

	it('passes numbers through to ICU plurals', () => {
		const text = (count: number) =>
			toText(sentence('{actor} deleted {count, plural, one {# message} other {# messages}}.', {actor: ACTOR, count}));
		expect(text(1)).toBe(`@${TEST_ACTOR_ID} deleted 1 message.`);
		expect(text(5)).toBe(`@${TEST_ACTOR_ID} deleted 5 messages.`);
	});

	it('throws when the passed placeholders do not match the message', () => {
		expect(() =>
			toText(
				sentence('{actor} pinned a message.', {
					actor: ACTOR,
					channel: {kind: 'channel', id: '1', recordedName: null, fallback: 'channel'},
				}),
			),
		).toThrow(/Not in the message: \[channel\]/);
		expect(() => toText(sentence('{actor} pinned a message in {channel}.', {actor: ACTOR}))).toThrow(
			/Not passed: \[channel\]/,
		);
	});
});

describe('resultToText', () => {
	it('marks rows by tone and keeps blocks', () => {
		expect(
			resultToText(
				domainResult({
					rows: [
						{id: 'add', tone: 'add', sentence: sentence('Added {name}.', {name: {kind: 'name', value: 'a'}})},
						{id: 'remove', tone: 'remove', sentence: sentence('Removed {name}.', {name: {kind: 'name', value: 'b'}})},
						{id: 'neutral', tone: 'neutral', sentence: sentence('Changed {name}.', {name: {kind: 'name', value: 'c'}})},
					],
					blocks: [{kind: 'reason', text: 'Cleanup'}],
				}),
				{[TEST_ACTOR_ID]: 'Hampus'},
			),
		).toEqual({
			summary: 'Hampus did a test action.',
			rows: ['+ Added a.', '- Removed b.', '~ Changed c.'],
			blocks: [{kind: 'reason', text: 'Cleanup'}],
		});
	});
});
