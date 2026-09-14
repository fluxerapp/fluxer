// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentGuildUpdate} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogGuildPresentation';
import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
	TEST_GUILD_ID,
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

interface ChangeFixture {
	key: string;
	old_value?: unknown;
	new_value?: unknown;
}

interface RowCase {
	title: string;
	changes: Array<ChangeFixture>;
	rows: Array<string>;
}

const NEW_OWNER_ID = '1400000000000000200';
const FIRST_CHANNEL_ID = '1400000000000000300';
const SECOND_CHANNEL_ID = '1400000000000000301';
const USER_NAMES = {[TEST_ACTOR_ID]: 'Hampus', [NEW_OWNER_ID]: 'ender'};
const SETTINGS_SUMMARY = 'Hampus updated the community settings.';
const STAFF_FEATURES = ['VANITY_URL', 'VERIFIED'];

function change(key: string, oldValue: unknown, newValue: unknown): ChangeFixture {
	return {key, old_value: oldValue, new_value: newValue};
}

function featureChange(previous: Array<string>, next: Array<string>): ChangeFixture {
	return change('features', [...STAFF_FEATURES, ...previous], [...STAFF_FEATURES, ...next]);
}

const REQUIRE_2FA_CHANGE = change('mfa_level', 0, 1);
const REQUIRE_2FA_ROW = '+ Required 2FA for moderation actions.';

function present(changes: Array<ChangeFixture> | undefined, fixture: {user_id?: string; reason?: string} = {}) {
	const presentation = presentAuditLogEntry(
		makeEntry({action_type: AuditLogActionType.GUILD_UPDATE, target_id: TEST_GUILD_ID, changes, ...fixture}),
		fakeContext(),
	);
	return {...resultToText(presentation, USER_NAMES), expandable: presentation.expandable};
}

function settingsUpdate(...rows: Array<string>) {
	return {summary: SETTINGS_SUMMARY, rows, blocks: [], expandable: true};
}

function summaryOnly(summary: string) {
	return {summary, rows: [], blocks: [], expandable: false};
}

const ROW_CASES: Array<RowCase> = [
	{
		title: 'row 1 renames the community',
		changes: [REQUIRE_2FA_CHANGE, change('name', 'Old Name', 'New Name')],
		rows: ['~ Renamed the community from Old Name to New Name.', REQUIRE_2FA_ROW],
	},
	{
		title: 'row 2 adds a community icon',
		changes: [change('icon_hash', null, 'a_1f2e')],
		rows: ['+ Added a community icon.'],
	},
	{
		title: 'row 2 changes the community icon',
		changes: [change('icon_hash', '1f2e', 'a_3c4d')],
		rows: ['~ Changed the community icon.'],
	},
	{
		title: 'row 2 removes the community icon',
		changes: [change('icon_hash', '1f2e', null)],
		rows: ['- Removed the community icon.'],
	},
	{
		title: 'row 3 adds a community banner',
		changes: [
			change('banner_hash', null, '5b6a'),
			change('banner_width', null, 960),
			change('banner_height', null, 540),
		],
		rows: ['+ Added a community banner.'],
	},
	{
		title: 'row 3 changes the community banner',
		changes: [change('banner_hash', '5b6a', '6c7b')],
		rows: ['~ Changed the community banner.'],
	},
	{
		title: 'row 3 removes the community banner',
		changes: [
			change('banner_hash', '5b6a', null),
			change('banner_width', 960, null),
			change('banner_height', 540, null),
		],
		rows: ['- Removed the community banner.'],
	},
	{
		title: 'row 4 turns on the detached banner',
		changes: [featureChange([], ['DETACHED_BANNER'])],
		rows: ['+ Turned on the detached banner.'],
	},
	{
		title: 'row 4 turns off the detached banner',
		changes: [featureChange(['DETACHED_BANNER'], [])],
		rows: ['- Turned off the detached banner.'],
	},
	{
		title: 'row 5 adds an invite background',
		changes: [change('splash_hash', null, '7c8d')],
		rows: ['+ Added an invite background.'],
	},
	{
		title: 'row 5 changes the invite background',
		changes: [change('splash_hash', '7c8d', '9e0f')],
		rows: ['~ Changed the invite background.'],
	},
	{
		title: 'row 5 removes the invite background',
		changes: [change('splash_hash', '7c8d', null)],
		rows: ['- Removed the invite background.'],
	},
	{
		title: 'row 6 changes the invite background alignment away from centered',
		changes: [change('splash_card_alignment', 0, 1)],
		rows: ['~ Changed the invite background alignment from Centered to Left aligned.'],
	},
	{
		title: 'row 6 changes the invite background alignment back to centered',
		changes: [change('splash_card_alignment', 2, 0)],
		rows: ['~ Changed the invite background alignment from Right aligned to Centered.'],
	},
	{
		title: 'row 7 adds a chat embed background',
		changes: [change('embed_splash_hash', null, '2a3b')],
		rows: ['+ Added a chat embed background.'],
	},
	{
		title: 'row 7 changes the chat embed background',
		changes: [change('embed_splash_hash', '2a3b', '4c5d')],
		rows: ['~ Changed the chat embed background.'],
	},
	{
		title: 'row 7 removes the chat embed background',
		changes: [change('embed_splash_hash', '2a3b', null)],
		rows: ['- Removed the chat embed background.'],
	},
	{
		title: 'row 8 sets the system channel',
		changes: [change('system_channel_id', null, FIRST_CHANNEL_ID)],
		rows: [`+ Set the system channel to #${FIRST_CHANNEL_ID}.`],
	},
	{
		title: 'row 8 changes the system channel',
		changes: [change('system_channel_id', FIRST_CHANNEL_ID, SECOND_CHANNEL_ID)],
		rows: [`~ Changed the system channel from #${FIRST_CHANNEL_ID} to #${SECOND_CHANNEL_ID}.`],
	},
	{
		title: 'row 8 removes the system channel',
		changes: [change('system_channel_id', FIRST_CHANNEL_ID, null)],
		rows: ['- Removed the system channel.'],
	},
	{
		title: 'row 9 hides join messages',
		changes: [change('system_channel_flags', 0, 1)],
		rows: ['~ Hid join messages in the system channel.'],
	},
	{
		title: 'row 9 stops hiding join messages',
		changes: [change('system_channel_flags', 1, 0)],
		rows: ['~ Stopped hiding join messages in the system channel.'],
	},
	{
		title: 'row 10 changes the default notifications to mentions only',
		changes: [change('default_message_notifications', 0, 1)],
		rows: ['~ Changed the default notification setting from All messages to Mentions only.'],
	},
	{
		title: 'row 10 changes the default notifications to all messages',
		changes: [change('default_message_notifications', 1, 0)],
		rows: ['~ Changed the default notification setting from Mentions only to All messages.'],
	},
	{
		title: 'row 11 sets the AFK channel',
		changes: [change('afk_channel_id', null, FIRST_CHANNEL_ID)],
		rows: [`+ Set the AFK channel to #${FIRST_CHANNEL_ID}.`],
	},
	{
		title: 'row 11 changes the AFK channel',
		changes: [change('afk_channel_id', FIRST_CHANNEL_ID, SECOND_CHANNEL_ID)],
		rows: [`~ Changed the AFK channel from #${FIRST_CHANNEL_ID} to #${SECOND_CHANNEL_ID}.`],
	},
	{
		title: 'row 11 removes the AFK channel',
		changes: [change('afk_channel_id', FIRST_CHANNEL_ID, null)],
		rows: ['- Removed the AFK channel.'],
	},
	{
		title: 'row 12 lengthens the AFK timeout',
		changes: [change('afk_timeout', 300, 900)],
		rows: ['~ Changed the AFK timeout from 5 minutes to 15 minutes.'],
	},
	{
		title: 'row 12 shortens the AFK timeout',
		changes: [change('afk_timeout', 900, 300)],
		rows: ['~ Changed the AFK timeout from 15 minutes to 5 minutes.'],
	},
	{
		title: 'row 13 allows flexible text channel names',
		changes: [featureChange([], ['TEXT_CHANNEL_FLEXIBLE_NAMES'])],
		rows: ['+ Allowed flexible text channel names.'],
	},
	{
		title: 'row 13 stops allowing flexible text channel names',
		changes: [featureChange(['TEXT_CHANNEL_FLEXIBLE_NAMES'], [])],
		rows: ['- Stopped allowing flexible text channel names.'],
	},
	{
		title: 'row 14 hides the owner crown',
		changes: [featureChange([], ['HIDE_OWNER_CROWN'])],
		rows: ['~ Hid the community owner crown.'],
	},
	{
		title: 'row 14 stops hiding the owner crown',
		changes: [featureChange(['HIDE_OWNER_CROWN'], [])],
		rows: ['~ Stopped hiding the community owner crown.'],
	},
	{
		title: 'row 15 sets the message history threshold',
		changes: [change('message_history_cutoff', null, '2026-08-01T00:00:00.000Z')],
		rows: ['+ Set the message history threshold to 2026-08-01T00:00:00.000Z.'],
	},
	{
		title: 'row 15 changes the message history threshold',
		changes: [change('message_history_cutoff', '2026-08-01T00:00:00.000Z', '2026-08-15T09:30:00.000Z')],
		rows: ['~ Changed the message history threshold from 2026-08-01T00:00:00.000Z to 2026-08-15T09:30:00.000Z.'],
	},
	{
		title: 'row 15 removes the message history threshold',
		changes: [change('message_history_cutoff', '2026-08-01T00:00:00.000Z', null)],
		rows: ['- Removed the message history threshold.'],
	},
	{
		title: 'row 16 pauses invites',
		changes: [REQUIRE_2FA_CHANGE, featureChange([], ['INVITES_DISABLED'])],
		rows: ['- Paused invites.', REQUIRE_2FA_ROW],
	},
	{
		title: 'row 16 resumes invites',
		changes: [REQUIRE_2FA_CHANGE, featureChange(['INVITES_DISABLED'], [])],
		rows: ['+ Resumed invites.', REQUIRE_2FA_ROW],
	},
	{
		title: 'row 17 allows others to clone emojis',
		changes: [featureChange([], ['CLONE_EMOJI_ENABLED'])],
		rows: ["+ Allowed others to clone this community's emojis."],
	},
	{
		title: 'row 17 stops allowing others to clone emojis',
		changes: [featureChange(['CLONE_EMOJI_ENABLED'], [])],
		rows: ["- Stopped allowing others to clone this community's emojis."],
	},
	{
		title: 'row 18 allows others to clone stickers',
		changes: [featureChange([], ['CLONE_STICKER_ENABLED'])],
		rows: ["+ Allowed others to clone this community's stickers."],
	},
	{
		title: 'row 18 stops allowing others to clone stickers',
		changes: [featureChange(['CLONE_STICKER_ENABLED'], [])],
		rows: ["- Stopped allowing others to clone this community's stickers."],
	},
	{
		title: 'row 19 raises the verification level',
		changes: [change('verification_level', 0, 2)],
		rows: ['~ Changed the verification level from None to Medium.'],
	},
	{
		title: 'row 19 lowers the verification level',
		changes: [change('verification_level', 4, 1)],
		rows: ['~ Changed the verification level from Very high to Low.'],
	},
	{
		title: 'row 20 turns on the explicit content filter',
		changes: [change('explicit_content_filter', 0, 1)],
		rows: ['~ Changed the explicit content filter from Off to Filter members without roles.'],
	},
	{
		title: 'row 20 turns off the explicit content filter',
		changes: [change('explicit_content_filter', 2, 0)],
		rows: ['~ Changed the explicit content filter from Filter everyone to Off.'],
	},
	{
		title: 'row 21 turns on mature content',
		changes: [change('nsfw_level', 0, 3), change('nsfw', false, true)],
		rows: ['+ Turned on mature content.'],
	},
	{
		title: 'row 21 turns off mature content',
		changes: [change('nsfw_level', 3, 0), change('nsfw', true, false)],
		rows: ['- Turned off mature content.'],
	},
	{
		title: 'row 22 turns on the content warning',
		changes: [change('content_warning_level', 0, 1)],
		rows: ['+ Turned on the content warning.'],
	},
	{
		title: 'row 22 turns off the content warning',
		changes: [change('content_warning_level', 1, 0)],
		rows: ['- Turned off the content warning.'],
	},
	{
		title: 'row 23 sets the custom warning text',
		changes: [change('content_warning_text', null, 'Spoilers ahead')],
		rows: ['+ Set the custom warning text to Spoilers ahead.'],
	},
	{
		title: 'row 23 changes the custom warning text',
		changes: [change('content_warning_text', 'Spoilers ahead', 'Graphic content')],
		rows: ['~ Changed the custom warning text from Spoilers ahead to Graphic content.'],
	},
	{
		title: 'row 23 removes the custom warning text',
		changes: [change('content_warning_text', 'Spoilers ahead', null)],
		rows: ['- Removed the custom warning text.'],
	},
	{title: 'row 24 requires 2FA for moderation', changes: [REQUIRE_2FA_CHANGE], rows: [REQUIRE_2FA_ROW]},
	{
		title: 'row 24 stops requiring 2FA for moderation',
		changes: [change('mfa_level', 1, 0)],
		rows: ['- Stopped requiring 2FA for moderation actions.'],
	},
	{
		title: 'row 25 transfers ownership next to another change',
		changes: [change('owner_id', TEST_ACTOR_ID, NEW_OWNER_ID), REQUIRE_2FA_CHANGE],
		rows: [REQUIRE_2FA_ROW, '~ Transferred community ownership to ender.'],
	},
	{
		title: 'row 26 sets the vanity URL next to another change',
		changes: [change('vanity_url_code', null, 'fluxer'), REQUIRE_2FA_CHANGE],
		rows: [REQUIRE_2FA_ROW, '+ Set the vanity URL to fluxer.'],
	},
	{
		title: 'row 26 changes the vanity URL next to another change',
		changes: [change('vanity_url_code', 'fluxer', 'fluxerhq'), REQUIRE_2FA_CHANGE],
		rows: [REQUIRE_2FA_ROW, '~ Changed the vanity URL from fluxer to fluxerhq.'],
	},
	{
		title: 'row 26 removes the vanity URL next to another change',
		changes: [change('vanity_url_code', 'fluxer', null), REQUIRE_2FA_CHANGE],
		rows: [REQUIRE_2FA_ROW, '- Removed the vanity URL fluxer.'],
	},
];

const EVERY_ROW_CHANGES: Array<ChangeFixture> = [
	change('name', 'Old Name', 'New Name'),
	change('icon_hash', null, 'a_1f2e'),
	change('banner_hash', '5b6a', null),
	featureChange(
		[],
		[
			'CLONE_EMOJI_ENABLED',
			'CLONE_STICKER_ENABLED',
			'DETACHED_BANNER',
			'HIDE_OWNER_CROWN',
			'INVITES_DISABLED',
			'TEXT_CHANNEL_FLEXIBLE_NAMES',
		],
	),
	change('splash_hash', '7c8d', '9e0f'),
	change('splash_card_alignment', 0, 1),
	change('embed_splash_hash', null, '2a3b'),
	change('system_channel_id', null, FIRST_CHANNEL_ID),
	change('system_channel_flags', 0, 1),
	change('default_message_notifications', 0, 1),
	change('afk_channel_id', FIRST_CHANNEL_ID, SECOND_CHANNEL_ID),
	change('afk_timeout', 300, 900),
	change('message_history_cutoff', null, '2026-08-01T00:00:00.000Z'),
	change('verification_level', 1, 3),
	change('explicit_content_filter', 0, 2),
	change('nsfw', false, true),
	change('content_warning_level', 0, 1),
	change('content_warning_text', null, 'Spoilers ahead'),
	change('mfa_level', 0, 1),
	change('owner_id', TEST_ACTOR_ID, NEW_OWNER_ID),
	change('vanity_url_code', 'fluxer', 'fluxerhq'),
];

describe('presentGuildUpdate summaries', () => {
	it('names the new owner when ownership transfer is the only change', () => {
		expect(present([change('owner_id', TEST_ACTOR_ID, NEW_OWNER_ID)])).toEqual(
			summaryOnly('Hampus transferred community ownership to ender.'),
		);
	});

	it('keeps the Reason block on a summary that has no rows', () => {
		expect(present([change('owner_id', TEST_ACTOR_ID, NEW_OWNER_ID)], {reason: 'Stepping%20down'})).toEqual({
			summary: 'Hampus transferred community ownership to ender.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Stepping down'}],
			expandable: true,
		});
	});

	it('summarizes a lone vanity URL change in every direction', () => {
		expect(present([change('vanity_url_code', null, 'fluxer')])).toEqual(
			summaryOnly('Hampus set the vanity URL to fluxer.'),
		);
		expect(present([change('vanity_url_code', 'fluxer', 'fluxerhq')])).toEqual(
			summaryOnly('Hampus changed the vanity URL from fluxer to fluxerhq.'),
		);
		expect(present([change('vanity_url_code', 'fluxer', null)])).toEqual(
			summaryOnly('Hampus removed the vanity URL fluxer.'),
		);
	});

	it('summarizes pausing and resuming invites when that is the only change', () => {
		expect(present([featureChange([], ['INVITES_DISABLED'])])).toEqual(summaryOnly('Hampus paused invites.'));
		expect(present([featureChange(['INVITES_DISABLED'], [])])).toEqual(summaryOnly('Hampus resumed invites.'));
	});

	it('summarizes a lone rename', () => {
		expect(present([change('name', 'Old Name', 'New Name')])).toEqual(
			summaryOnly('Hampus renamed the community from Old Name to New Name.'),
		);
	});

	it('keeps a single-change summary when ignored keys change alongside it', () => {
		expect(
			present([
				change('guild_id', TEST_GUILD_ID, TEST_GUILD_ID),
				featureChange([], ['INVITES_DISABLED', 'DISCOVERABLE']),
				change('member_count', 10, 11),
			]),
		).toEqual(summaryOnly('Hampus paused invites.'));
	});

	it('names the system as the actor', () => {
		expect(present([featureChange([], ['INVITES_DISABLED'])], {user_id: '0'})).toEqual(
			summaryOnly('System paused invites.'),
		);
		expect(present([REQUIRE_2FA_CHANGE], {user_id: '0'})).toEqual({
			...settingsUpdate(REQUIRE_2FA_ROW),
			summary: 'System updated the community settings.',
		});
	});

	it('uses the settings summary with no rows when nothing changed', () => {
		expect(present(undefined)).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([])).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('adds the Reason block under the settings summary', () => {
		expect(present([REQUIRE_2FA_CHANGE], {reason: 'Raid protection'})).toEqual({
			...settingsUpdate(REQUIRE_2FA_ROW),
			blocks: [{kind: 'reason', text: 'Raid protection'}],
		});
	});
});

describe('presentGuildUpdate rows', () => {
	it.each(ROW_CASES)('$title', ({changes, rows}) => {
		expect(present(changes)).toEqual(settingsUpdate(...rows));
	});

	it('orders every row by the settings layout, not by the stored order', () => {
		expect(present([...EVERY_ROW_CHANGES].reverse())).toEqual(
			settingsUpdate(
				'~ Renamed the community from Old Name to New Name.',
				'+ Added a community icon.',
				'- Removed the community banner.',
				'+ Turned on the detached banner.',
				'~ Changed the invite background.',
				'~ Changed the invite background alignment from Centered to Left aligned.',
				'+ Added a chat embed background.',
				`+ Set the system channel to #${FIRST_CHANNEL_ID}.`,
				'~ Hid join messages in the system channel.',
				'~ Changed the default notification setting from All messages to Mentions only.',
				`~ Changed the AFK channel from #${FIRST_CHANNEL_ID} to #${SECOND_CHANNEL_ID}.`,
				'~ Changed the AFK timeout from 5 minutes to 15 minutes.',
				'+ Allowed flexible text channel names.',
				'~ Hid the community owner crown.',
				'+ Set the message history threshold to 2026-08-01T00:00:00.000Z.',
				'- Paused invites.',
				"+ Allowed others to clone this community's emojis.",
				"+ Allowed others to clone this community's stickers.",
				'~ Changed the verification level from Low to High.',
				'~ Changed the explicit content filter from Off to Filter everyone.',
				'+ Turned on mature content.',
				'+ Turned on the content warning.',
				'+ Set the custom warning text to Spoilers ahead.',
				REQUIRE_2FA_ROW,
				'~ Transferred community ownership to ender.',
				'~ Changed the vanity URL from fluxer to fluxerhq.',
			),
		);
	});

	it('gives every row a distinct id', () => {
		const result = presentGuildUpdate(
			makeEntry({action_type: AuditLogActionType.GUILD_UPDATE, changes: EVERY_ROW_CHANGES}),
		);
		expect(new Set(result.rows.map((row) => row.id)).size).toBe(26);
	});

	it('renders the overview save example in settings order', () => {
		expect(
			present([
				change('name', 'Old Name', 'New Name'),
				change('icon_hash', '1f2e', 'a_3c4d'),
				featureChange([], ['HIDE_OWNER_CROWN']),
				change('system_channel_flags', 0, 1),
				change('afk_timeout', 300, 900),
			]),
		).toEqual(
			settingsUpdate(
				'~ Renamed the community from Old Name to New Name.',
				'~ Changed the community icon.',
				'~ Hid join messages in the system channel.',
				'~ Changed the AFK timeout from 5 minutes to 15 minutes.',
				'~ Hid the community owner crown.',
			),
		);
	});

	it('renders the moderation save example in settings order', () => {
		expect(
			present([
				change('nsfw_level', 0, 3),
				change('nsfw', false, true),
				change('content_warning_level', 0, 1),
				change('content_warning_text', null, 'Spoilers ahead'),
			]),
		).toEqual(
			settingsUpdate(
				'+ Turned on mature content.',
				'+ Turned on the content warning.',
				'+ Set the custom warning text to Spoilers ahead.',
			),
		);
	});

	it('renders 60 seconds and 3600 seconds in their largest whole unit', () => {
		expect(present([change('afk_timeout', 60, 3600)])).toEqual(
			settingsUpdate('~ Changed the AFK timeout from 1 minute to 1 hour.'),
		);
	});

	it('reads values sent as strings', () => {
		expect(
			present([
				change('splash_card_alignment', '0', '2'),
				change('system_channel_flags', '0', '1'),
				change('afk_timeout', '300', '600'),
				change('nsfw', 'false', 'true'),
				change('mfa_level', '1', '0'),
			]),
		).toEqual(
			settingsUpdate(
				'~ Changed the invite background alignment from Centered to Right aligned.',
				'~ Hid join messages in the system channel.',
				'~ Changed the AFK timeout from 5 minutes to 10 minutes.',
				'+ Turned on mature content.',
				'- Stopped requiring 2FA for moderation actions.',
			),
		);
	});
});

describe('presentGuildUpdate legacy and ignored values', () => {
	it('reads legacy CLONE_*_DISABLED features with the inverse meaning', () => {
		expect(present([featureChange([], ['CLONE_EMOJI_DISABLED'])])).toEqual(
			settingsUpdate("- Stopped allowing others to clone this community's emojis."),
		);
		expect(present([featureChange(['CLONE_EMOJI_DISABLED'], [])])).toEqual(
			settingsUpdate("+ Allowed others to clone this community's emojis."),
		);
		expect(present([featureChange([], ['CLONE_STICKER_DISABLED'])])).toEqual(
			settingsUpdate("- Stopped allowing others to clone this community's stickers."),
		);
		expect(present([featureChange(['CLONE_STICKER_DISABLED'], [])])).toEqual(
			settingsUpdate("+ Allowed others to clone this community's stickers."),
		);
	});

	it('gives one cloning row decided by the ENABLED feature when both forms change', () => {
		expect(present([featureChange(['CLONE_EMOJI_DISABLED'], ['CLONE_EMOJI_ENABLED'])])).toEqual(
			settingsUpdate("+ Allowed others to clone this community's emojis."),
		);
		expect(present([featureChange(['CLONE_EMOJI_DISABLED', 'CLONE_EMOJI_ENABLED'], [])])).toEqual(
			settingsUpdate("- Stopped allowing others to clone this community's emojis."),
		);
		expect(present([featureChange(['CLONE_STICKER_DISABLED', 'CLONE_STICKER_ENABLED'], [])])).toEqual(
			settingsUpdate("- Stopped allowing others to clone this community's stickers."),
		);
	});

	it('ignores a change to untracked features only', () => {
		expect(present([featureChange([], ['DISCOVERABLE', 'UNLIMITED_EMOJI'])])).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('ignores a change to image dimensions only', () => {
		expect(
			present([
				change('banner_width', null, 960),
				change('banner_height', null, 540),
				change('splash_width', 1280, 1920),
				change('splash_height', 720, 1080),
				change('embed_splash_width', null, 640),
				change('embed_splash_height', null, 360),
			]),
		).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('ignores rules_channel_id, disabled_operations, region, guild_id, member_count and unlisted keys', () => {
		expect(
			present([
				change('rules_channel_id', FIRST_CHANNEL_ID, null),
				change('disabled_operations', 0, 1),
				change('region', 'us-east', 'eu-west'),
				change('guild_id', null, TEST_GUILD_ID),
				change('member_count', 10, 11),
				change('discovery_splash_hash', null, '1a2b'),
			]),
		).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('reads mature content from nsfw_level when nsfw is absent', () => {
		expect(present([change('nsfw_level', 0, 3)])).toEqual(settingsUpdate('+ Turned on mature content.'));
		expect(present([change('nsfw_level', 3, 0)])).toEqual(settingsUpdate('- Turned off mature content.'));
	});

	it('ignores nsfw_level values other than 0 and 3', () => {
		expect(present([change('nsfw_level', 0, 1)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('nsfw_level', 2, 3)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('ignores nsfw_level when nsfw is present', () => {
		expect(present([change('nsfw_level', 0, 3), change('nsfw', true, false)])).toEqual(
			settingsUpdate('- Turned off mature content.'),
		);
		expect(present([change('nsfw_level', 0, 3), change('nsfw', true, true)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('reads only the join messages bit of system_channel_flags', () => {
		expect(present([change('system_channel_flags', 1, 3)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('system_channel_flags', 2, 3)])).toEqual(
			settingsUpdate('~ Hid join messages in the system channel.'),
		);
	});

	it('omits a verification level outside the label table', () => {
		expect(present([change('verification_level', 3, 5)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('verification_level', 3, 5), REQUIRE_2FA_CHANGE])).toEqual(settingsUpdate(REQUIRE_2FA_ROW));
	});

	it('omits alignment, notification and filter values outside their label tables', () => {
		expect(
			present([
				change('splash_card_alignment', 0, 3),
				change('default_message_notifications', 1, 2),
				change('explicit_content_filter', 2, 3),
			]),
		).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('omits content warning and 2FA values other than 0 and 1', () => {
		expect(present([change('content_warning_level', 0, 2), change('mfa_level', 2, 1)])).toEqual(
			summaryOnly(SETTINGS_SUMMARY),
		);
	});

	it('omits an unparseable message history threshold', () => {
		expect(present([change('message_history_cutoff', null, 'not a date')])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('message_history_cutoff', 'not a date', '2026-08-01T00:00:00.000Z')])).toEqual(
			summaryOnly(SETTINGS_SUMMARY),
		);
	});

	it('gives the settings summary without rows when the new owner cannot be read', () => {
		expect(present([change('owner_id', TEST_ACTOR_ID, 'not-a-snowflake')])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('owner_id', TEST_ACTOR_ID, null)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('owner_id', TEST_ACTOR_ID, Number(NEW_OWNER_ID))])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('owner_id', TEST_ACTOR_ID, TEST_ACTOR_ID)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
	});

	it('treats blank text as empty and skips values that are equal after reading', () => {
		expect(
			present([
				change('name', 'Same Name', ' Same Name '),
				change('icon_hash', '', null),
				change('vanity_url_code', '   ', null),
				change('content_warning_text', 'Spoilers', 'Spoilers  '),
				change('afk_timeout', 300, 300.2),
				change('system_channel_id', FIRST_CHANNEL_ID, FIRST_CHANNEL_ID),
			]),
		).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('content_warning_text', 'Spoilers', '  ')])).toEqual(
			settingsUpdate('- Removed the custom warning text.'),
		);
	});

	it('omits every row whose values fail their readers without throwing', () => {
		expect(
			present([
				change('name', 5, 'New Name'),
				change('icon_hash', 7, 'a_1f2e'),
				change('banner_hash', null, {hash: '5b6a'}),
				change('features', 'DETACHED_BANNER', ['DETACHED_BANNER']),
				change('splash_hash', ['7c8d'], null),
				change('splash_card_alignment', 'left', 1),
				change('embed_splash_hash', true, null),
				change('system_channel_id', 'general', FIRST_CHANNEL_ID),
				change('system_channel_flags', -1, 1),
				change('default_message_notifications', null, 1),
				change('afk_channel_id', null, Number(FIRST_CHANNEL_ID)),
				change('afk_timeout', 'soon', 900),
				change('verification_level', 1.5, 2),
				change('explicit_content_filter', {}, 0),
				change('nsfw', 'yes', true),
				change('content_warning_level', null, 1),
				change('content_warning_text', 42, 'Spoilers ahead'),
				change('mfa_level', 'required', 1),
				change('vanity_url_code', null, 1234),
			]),
		).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([change('afk_timeout', -300, 900)])).toEqual(summaryOnly(SETTINGS_SUMMARY));
		expect(present([{key: 'name'}, {key: 'features', new_value: ['INVITES_DISABLED']}])).toEqual(
			summaryOnly(SETTINGS_SUMMARY),
		);
	});
});
