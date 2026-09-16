// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GUILD_AFK_CHANNEL_CHANGED_ROW,
	GUILD_AFK_CHANNEL_REMOVED_ROW,
	GUILD_AFK_CHANNEL_SET_ROW,
	GUILD_AFK_TIMEOUT_CHANGED_ROW,
	GUILD_BANNER_ADDED_ROW,
	GUILD_BANNER_CHANGED_ROW,
	GUILD_BANNER_REMOVED_ROW,
	GUILD_CHAT_EMBED_BACKGROUND_ADDED_ROW,
	GUILD_CHAT_EMBED_BACKGROUND_CHANGED_ROW,
	GUILD_CHAT_EMBED_BACKGROUND_REMOVED_ROW,
	GUILD_CONTENT_WARNING_TURNED_OFF_ROW,
	GUILD_CONTENT_WARNING_TURNED_ON_ROW,
	GUILD_CUSTOM_WARNING_TEXT_CHANGED_ROW,
	GUILD_CUSTOM_WARNING_TEXT_REMOVED_ROW,
	GUILD_CUSTOM_WARNING_TEXT_SET_ROW,
	GUILD_DEFAULT_NOTIFICATIONS_CHANGED_ROW,
	GUILD_DETACHED_BANNER_TURNED_OFF_ROW,
	GUILD_DETACHED_BANNER_TURNED_ON_ROW,
	GUILD_EMOJI_CLONING_ALLOWED_ROW,
	GUILD_EMOJI_CLONING_STOPPED_ROW,
	GUILD_EXPLICIT_CONTENT_FILTER_CHANGED_ROW,
	GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_ALLOWED_ROW,
	GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_DISALLOWED_ROW,
	GUILD_ICON_ADDED_ROW,
	GUILD_ICON_CHANGED_ROW,
	GUILD_ICON_REMOVED_ROW,
	GUILD_INVITE_BACKGROUND_ADDED_ROW,
	GUILD_INVITE_BACKGROUND_ALIGNMENT_CHANGED_ROW,
	GUILD_INVITE_BACKGROUND_CHANGED_ROW,
	GUILD_INVITE_BACKGROUND_REMOVED_ROW,
	GUILD_INVITES_PAUSED_ROW,
	GUILD_INVITES_PAUSED_SUMMARY,
	GUILD_INVITES_RESUMED_ROW,
	GUILD_INVITES_RESUMED_SUMMARY,
	GUILD_JOIN_MESSAGES_HIDDEN_ROW,
	GUILD_JOIN_MESSAGES_SHOWN_ROW,
	GUILD_MATURE_CONTENT_TURNED_OFF_ROW,
	GUILD_MATURE_CONTENT_TURNED_ON_ROW,
	GUILD_MESSAGE_HISTORY_THRESHOLD_CHANGED_ROW,
	GUILD_MESSAGE_HISTORY_THRESHOLD_REMOVED_ROW,
	GUILD_MESSAGE_HISTORY_THRESHOLD_SET_ROW,
	GUILD_MODERATION_2FA_NOT_REQUIRED_ROW,
	GUILD_MODERATION_2FA_REQUIRED_ROW,
	GUILD_OWNER_CROWN_HIDDEN_ROW,
	GUILD_OWNER_CROWN_SHOWN_ROW,
	GUILD_OWNERSHIP_TRANSFERRED_ROW,
	GUILD_OWNERSHIP_TRANSFERRED_SUMMARY,
	GUILD_RENAMED_ROW,
	GUILD_RENAMED_SUMMARY,
	GUILD_SETTINGS_UPDATED_SUMMARY,
	GUILD_STICKER_CLONING_ALLOWED_ROW,
	GUILD_STICKER_CLONING_STOPPED_ROW,
	GUILD_SYSTEM_CHANNEL_CHANGED_ROW,
	GUILD_SYSTEM_CHANNEL_REMOVED_ROW,
	GUILD_SYSTEM_CHANNEL_SET_ROW,
	GUILD_VANITY_URL_CHANGED_ROW,
	GUILD_VANITY_URL_CHANGED_SUMMARY,
	GUILD_VANITY_URL_REMOVED_ROW,
	GUILD_VANITY_URL_REMOVED_SUMMARY,
	GUILD_VANITY_URL_SET_ROW,
	GUILD_VANITY_URL_SET_SUMMARY,
	GUILD_VERIFICATION_LEVEL_CHANGED_ROW,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogGuildMessages';
import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogSentence,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	CENTERED_LABEL,
	DEFAULT_NOTIFICATIONS_ALL_MESSAGES_LABEL,
	DEFAULT_NOTIFICATIONS_MENTIONS_ONLY_LABEL,
	EXPLICIT_CONTENT_FILTER_EVERYONE_LABEL,
	EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_LABEL,
	EXPLICIT_CONTENT_FILTER_OFF_LABEL,
	LEFT_ALIGNED_LABEL,
	RIGHT_ALIGNED_LABEL,
	VERIFICATION_LEVEL_HIGH_LABEL,
	VERIFICATION_LEVEL_LOW_LABEL,
	VERIFICATION_LEVEL_MEDIUM_LABEL,
	VERIFICATION_LEVEL_NONE_LABEL,
	VERIFICATION_LEVEL_VERY_HIGH_LABEL,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {
	actorPlaceholder,
	featureDelta,
	readBitfield,
	readBoolean,
	readChange,
	readNumber,
	readSnowflake,
	readString,
	readStringArray,
	readTimestamp,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {
	ContentWarningLevel,
	GuildExplicitContentFilterTypes,
	GuildFeatures,
	GuildMFALevel,
	GuildNSFWLevel,
	GuildSplashCardAlignment,
	GuildVerificationLevel,
	SystemChannelFlags,
} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

interface GuildChange {
	row: AuditLogDetailRow;
	summary: AuditLogSentence | null;
}

interface FeatureLists {
	previous: Array<string>;
	next: Array<string>;
}

interface ToggleSentence {
	tone: AuditLogTone;
	descriptor: MessageDescriptor;
}

interface Toggle {
	on: ToggleSentence;
	off: ToggleSentence;
}

interface ImageSentences {
	added: MessageDescriptor;
	changed: MessageDescriptor;
	removed: MessageDescriptor;
}

interface ChannelSentences {
	set: MessageDescriptor;
	changed: MessageDescriptor;
	removed: MessageDescriptor;
}

type NullableChange<T> =
	| {kind: 'added'; value: T}
	| {kind: 'changed'; oldValue: T; newValue: T}
	| {kind: 'removed'; value: T};

type SentenceValues = AuditLogSentence['values'];

const SUPPRESS_JOIN_NOTIFICATIONS_FLAG = BigInt(SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS);

const SPLASH_CARD_ALIGNMENT_LABELS: ReadonlyMap<number, MessageDescriptor> = new Map([
	[GuildSplashCardAlignment.CENTER, CENTERED_LABEL],
	[GuildSplashCardAlignment.LEFT, LEFT_ALIGNED_LABEL],
	[GuildSplashCardAlignment.RIGHT, RIGHT_ALIGNED_LABEL],
]);
const DEFAULT_NOTIFICATIONS_LABELS: ReadonlyMap<number, MessageDescriptor> = new Map([
	[MessageNotifications.ALL_MESSAGES, DEFAULT_NOTIFICATIONS_ALL_MESSAGES_LABEL],
	[MessageNotifications.ONLY_MENTIONS, DEFAULT_NOTIFICATIONS_MENTIONS_ONLY_LABEL],
]);
const VERIFICATION_LEVEL_LABELS: ReadonlyMap<number, MessageDescriptor> = new Map([
	[GuildVerificationLevel.NONE, VERIFICATION_LEVEL_NONE_LABEL],
	[GuildVerificationLevel.LOW, VERIFICATION_LEVEL_LOW_LABEL],
	[GuildVerificationLevel.MEDIUM, VERIFICATION_LEVEL_MEDIUM_LABEL],
	[GuildVerificationLevel.HIGH, VERIFICATION_LEVEL_HIGH_LABEL],
	[GuildVerificationLevel.VERY_HIGH, VERIFICATION_LEVEL_VERY_HIGH_LABEL],
]);
const EXPLICIT_CONTENT_FILTER_LABELS: ReadonlyMap<number, MessageDescriptor> = new Map([
	[GuildExplicitContentFilterTypes.DISABLED, EXPLICIT_CONTENT_FILTER_OFF_LABEL],
	[GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES, EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_LABEL],
	[GuildExplicitContentFilterTypes.ALL_MEMBERS, EXPLICIT_CONTENT_FILTER_EVERYONE_LABEL],
]);

const ICON_SENTENCES: ImageSentences = {
	added: GUILD_ICON_ADDED_ROW,
	changed: GUILD_ICON_CHANGED_ROW,
	removed: GUILD_ICON_REMOVED_ROW,
};
const BANNER_SENTENCES: ImageSentences = {
	added: GUILD_BANNER_ADDED_ROW,
	changed: GUILD_BANNER_CHANGED_ROW,
	removed: GUILD_BANNER_REMOVED_ROW,
};
const INVITE_BACKGROUND_SENTENCES: ImageSentences = {
	added: GUILD_INVITE_BACKGROUND_ADDED_ROW,
	changed: GUILD_INVITE_BACKGROUND_CHANGED_ROW,
	removed: GUILD_INVITE_BACKGROUND_REMOVED_ROW,
};
const CHAT_EMBED_BACKGROUND_SENTENCES: ImageSentences = {
	added: GUILD_CHAT_EMBED_BACKGROUND_ADDED_ROW,
	changed: GUILD_CHAT_EMBED_BACKGROUND_CHANGED_ROW,
	removed: GUILD_CHAT_EMBED_BACKGROUND_REMOVED_ROW,
};
const SYSTEM_CHANNEL_SENTENCES: ChannelSentences = {
	set: GUILD_SYSTEM_CHANNEL_SET_ROW,
	changed: GUILD_SYSTEM_CHANNEL_CHANGED_ROW,
	removed: GUILD_SYSTEM_CHANNEL_REMOVED_ROW,
};
const AFK_CHANNEL_SENTENCES: ChannelSentences = {
	set: GUILD_AFK_CHANNEL_SET_ROW,
	changed: GUILD_AFK_CHANNEL_CHANGED_ROW,
	removed: GUILD_AFK_CHANNEL_REMOVED_ROW,
};

const DETACHED_BANNER_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_DETACHED_BANNER_TURNED_ON_ROW},
	off: {tone: 'remove', descriptor: GUILD_DETACHED_BANNER_TURNED_OFF_ROW},
};
const JOIN_MESSAGES_HIDDEN_TOGGLE: Toggle = {
	on: {tone: 'neutral', descriptor: GUILD_JOIN_MESSAGES_HIDDEN_ROW},
	off: {tone: 'neutral', descriptor: GUILD_JOIN_MESSAGES_SHOWN_ROW},
};
const FLEXIBLE_TEXT_CHANNEL_NAMES_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_ALLOWED_ROW},
	off: {tone: 'remove', descriptor: GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_DISALLOWED_ROW},
};
const OWNER_CROWN_HIDDEN_TOGGLE: Toggle = {
	on: {tone: 'neutral', descriptor: GUILD_OWNER_CROWN_HIDDEN_ROW},
	off: {tone: 'neutral', descriptor: GUILD_OWNER_CROWN_SHOWN_ROW},
};
const EMOJI_CLONING_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_EMOJI_CLONING_ALLOWED_ROW},
	off: {tone: 'remove', descriptor: GUILD_EMOJI_CLONING_STOPPED_ROW},
};
const STICKER_CLONING_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_STICKER_CLONING_ALLOWED_ROW},
	off: {tone: 'remove', descriptor: GUILD_STICKER_CLONING_STOPPED_ROW},
};
const MATURE_CONTENT_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_MATURE_CONTENT_TURNED_ON_ROW},
	off: {tone: 'remove', descriptor: GUILD_MATURE_CONTENT_TURNED_OFF_ROW},
};
const CONTENT_WARNING_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_CONTENT_WARNING_TURNED_ON_ROW},
	off: {tone: 'remove', descriptor: GUILD_CONTENT_WARNING_TURNED_OFF_ROW},
};
const MODERATION_2FA_TOGGLE: Toggle = {
	on: {tone: 'add', descriptor: GUILD_MODERATION_2FA_REQUIRED_ROW},
	off: {tone: 'remove', descriptor: GUILD_MODERATION_2FA_NOT_REQUIRED_ROW},
};

function settingChange(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: SentenceValues = {},
): GuildChange {
	return {row: {id, tone, sentence: {descriptor, values}}, summary: null};
}

function summarizedChange(
	id: string,
	tone: AuditLogTone,
	rowDescriptor: MessageDescriptor,
	summaryDescriptor: MessageDescriptor,
	actor: AuditLogPlaceholder,
	values: SentenceValues = {},
): GuildChange {
	return {
		row: {id, tone, sentence: {descriptor: rowDescriptor, values}},
		summary: {descriptor: summaryDescriptor, values: {actor, ...values}},
	};
}

function channelPlaceholder(id: string): AuditLogPlaceholder {
	return {kind: 'channel', id, recordedName: null, fallback: 'channel'};
}

function textPlaceholder(value: string): AuditLogPlaceholder {
	return {kind: 'text', value};
}

function readNullableText(value: unknown): string | null | undefined {
	if (value === null) return null;
	return typeof value === 'string' ? readString(value) : undefined;
}

function readNullableSnowflake(value: unknown): string | null | undefined {
	return value === null ? null : (readSnowflake(value) ?? undefined);
}

function readNullableTimestamp(value: unknown): number | null | undefined {
	return value === null ? null : (readTimestamp(value) ?? undefined);
}

function readSeconds(value: unknown): number | null {
	const seconds = readNumber(value);
	return seconds !== null && seconds >= 0 ? Math.round(seconds) : null;
}

function readSwitch(value: unknown, offValue: number, onValue: number): boolean | null {
	const setting = readNumber(value);
	if (setting === offValue) return false;
	if (setting === onValue) return true;
	return null;
}

function readLabel(value: unknown, labels: ReadonlyMap<number, MessageDescriptor>): MessageDescriptor | null {
	const setting = readNumber(value);
	return setting === null ? null : (labels.get(setting) ?? null);
}

function readJoinMessagesHidden(value: unknown): boolean | null {
	const flags = readBitfield(value);
	return flags === null ? null : (flags & SUPPRESS_JOIN_NOTIFICATIONS_FLAG) !== 0n;
}

function readDiff<T>(
	entry: GuildAuditLogEntryResponse,
	key: string,
	reader: (value: unknown) => T | null,
): {oldValue: T; newValue: T} | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const oldValue = reader(change.oldValue);
	const newValue = reader(change.newValue);
	if (oldValue === null || newValue === null || oldValue === newValue) return null;
	return {oldValue, newValue};
}

function readNullableChange<T>(
	entry: GuildAuditLogEntryResponse,
	key: string,
	reader: (value: unknown) => T | null | undefined,
): NullableChange<T> | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const oldValue = reader(change.oldValue);
	const newValue = reader(change.newValue);
	if (oldValue === undefined || newValue === undefined) return null;
	if (oldValue === null) return newValue === null ? null : {kind: 'added', value: newValue};
	if (newValue === null) return {kind: 'removed', value: oldValue};
	return oldValue === newValue ? null : {kind: 'changed', oldValue, newValue};
}

function readTurnedOn(
	entry: GuildAuditLogEntryResponse,
	key: string,
	reader: (value: unknown) => boolean | null,
): boolean | null {
	return readDiff(entry, key, reader)?.newValue ?? null;
}

function readFeatureLists(entry: GuildAuditLogEntryResponse): FeatureLists | null {
	const change = readChange(entry, 'features');
	if (change === null) return null;
	const previous = readStringArray(change.oldValue);
	const next = readStringArray(change.newValue);
	return previous === null || next === null ? null : {previous, next};
}

function readFeatureAdded(features: FeatureLists | null, feature: string): boolean | null {
	const delta = features === null ? null : featureDelta(features.previous, features.next, feature);
	return delta === null ? null : delta === 'added';
}

function readCloningAllowed(
	features: FeatureLists | null,
	enabledFeature: string,
	disabledFeature: string,
): boolean | null {
	const enabledAdded = readFeatureAdded(features, enabledFeature);
	if (enabledAdded !== null) return enabledAdded;
	const disabledAdded = readFeatureAdded(features, disabledFeature);
	return disabledAdded === null ? null : !disabledAdded;
}

function readMatureContentOn(entry: GuildAuditLogEntryResponse): boolean | null {
	if (readChange(entry, 'nsfw') !== null) return readTurnedOn(entry, 'nsfw', readBoolean);
	return readTurnedOn(entry, 'nsfw_level', (value) =>
		readSwitch(value, GuildNSFWLevel.SAFE, GuildNSFWLevel.AGE_RESTRICTED),
	);
}

function toggleChange(id: string, turnedOn: boolean | null, toggle: Toggle): GuildChange | null {
	if (turnedOn === null) return null;
	const {tone, descriptor} = turnedOn ? toggle.on : toggle.off;
	return settingChange(id, tone, descriptor);
}

function presentName(entry: GuildAuditLogEntryResponse, actor: AuditLogPlaceholder): GuildChange | null {
	const change = readDiff(entry, 'name', readString);
	if (change === null) return null;
	return summarizedChange('name', 'neutral', GUILD_RENAMED_ROW, GUILD_RENAMED_SUMMARY, actor, {
		oldName: {kind: 'name', value: change.oldValue},
		newName: {kind: 'name', value: change.newValue},
	});
}

function presentImage(entry: GuildAuditLogEntryResponse, key: string, sentences: ImageSentences): GuildChange | null {
	const change = readNullableChange(entry, key, readNullableText);
	if (change === null) return null;
	switch (change.kind) {
		case 'added':
			return settingChange(key, 'add', sentences.added);
		case 'changed':
			return settingChange(key, 'neutral', sentences.changed);
		case 'removed':
			return settingChange(key, 'remove', sentences.removed);
	}
}

function presentLabel(
	entry: GuildAuditLogEntryResponse,
	key: string,
	labels: ReadonlyMap<number, MessageDescriptor>,
	descriptor: MessageDescriptor,
	oldName: string,
	newName: string,
): GuildChange | null {
	const change = readDiff(entry, key, (value) => readLabel(value, labels));
	if (change === null) return null;
	return settingChange(key, 'neutral', descriptor, {
		[oldName]: {kind: 'label', descriptor: change.oldValue},
		[newName]: {kind: 'label', descriptor: change.newValue},
	});
}

function presentChannel(
	entry: GuildAuditLogEntryResponse,
	key: string,
	sentences: ChannelSentences,
): GuildChange | null {
	const change = readNullableChange(entry, key, readNullableSnowflake);
	if (change === null) return null;
	switch (change.kind) {
		case 'added':
			return settingChange(key, 'add', sentences.set, {channel: channelPlaceholder(change.value)});
		case 'changed':
			return settingChange(key, 'neutral', sentences.changed, {
				oldChannel: channelPlaceholder(change.oldValue),
				newChannel: channelPlaceholder(change.newValue),
			});
		case 'removed':
			return settingChange(key, 'remove', sentences.removed);
	}
}

function presentAfkTimeout(entry: GuildAuditLogEntryResponse): GuildChange | null {
	const change = readDiff(entry, 'afk_timeout', readSeconds);
	if (change === null) return null;
	return settingChange('afk_timeout', 'neutral', GUILD_AFK_TIMEOUT_CHANGED_ROW, {
		oldDuration: {kind: 'duration', seconds: change.oldValue},
		newDuration: {kind: 'duration', seconds: change.newValue},
	});
}

function presentMessageHistoryCutoff(entry: GuildAuditLogEntryResponse): GuildChange | null {
	const change = readNullableChange(entry, 'message_history_cutoff', readNullableTimestamp);
	if (change === null) return null;
	switch (change.kind) {
		case 'added':
			return settingChange('message_history_cutoff', 'add', GUILD_MESSAGE_HISTORY_THRESHOLD_SET_ROW, {
				date: {kind: 'date', timestamp: change.value},
			});
		case 'changed':
			return settingChange('message_history_cutoff', 'neutral', GUILD_MESSAGE_HISTORY_THRESHOLD_CHANGED_ROW, {
				oldDate: {kind: 'date', timestamp: change.oldValue},
				newDate: {kind: 'date', timestamp: change.newValue},
			});
		case 'removed':
			return settingChange('message_history_cutoff', 'remove', GUILD_MESSAGE_HISTORY_THRESHOLD_REMOVED_ROW);
	}
}

function presentInvitesPaused(features: FeatureLists | null, actor: AuditLogPlaceholder): GuildChange | null {
	const paused = readFeatureAdded(features, GuildFeatures.INVITES_DISABLED);
	if (paused === null) return null;
	return paused
		? summarizedChange('invites_disabled', 'remove', GUILD_INVITES_PAUSED_ROW, GUILD_INVITES_PAUSED_SUMMARY, actor)
		: summarizedChange('invites_disabled', 'add', GUILD_INVITES_RESUMED_ROW, GUILD_INVITES_RESUMED_SUMMARY, actor);
}

function presentContentWarningText(entry: GuildAuditLogEntryResponse): GuildChange | null {
	const change = readNullableChange(entry, 'content_warning_text', readNullableText);
	if (change === null) return null;
	switch (change.kind) {
		case 'added':
			return settingChange('content_warning_text', 'add', GUILD_CUSTOM_WARNING_TEXT_SET_ROW, {
				text: textPlaceholder(change.value),
			});
		case 'changed':
			return settingChange('content_warning_text', 'neutral', GUILD_CUSTOM_WARNING_TEXT_CHANGED_ROW, {
				oldText: textPlaceholder(change.oldValue),
				newText: textPlaceholder(change.newValue),
			});
		case 'removed':
			return settingChange('content_warning_text', 'remove', GUILD_CUSTOM_WARNING_TEXT_REMOVED_ROW);
	}
}

function presentOwnershipTransfer(entry: GuildAuditLogEntryResponse, actor: AuditLogPlaceholder): GuildChange | null {
	const change = readChange(entry, 'owner_id');
	if (change === null) return null;
	const newOwnerId = readSnowflake(change.newValue);
	if (newOwnerId === null || readSnowflake(change.oldValue) === newOwnerId) return null;
	return summarizedChange(
		'owner_id',
		'neutral',
		GUILD_OWNERSHIP_TRANSFERRED_ROW,
		GUILD_OWNERSHIP_TRANSFERRED_SUMMARY,
		actor,
		{user: {kind: 'user', id: newOwnerId}},
	);
}

function presentVanityUrl(entry: GuildAuditLogEntryResponse, actor: AuditLogPlaceholder): GuildChange | null {
	const change = readNullableChange(entry, 'vanity_url_code', readNullableText);
	if (change === null) return null;
	switch (change.kind) {
		case 'added':
			return summarizedChange('vanity_url_code', 'add', GUILD_VANITY_URL_SET_ROW, GUILD_VANITY_URL_SET_SUMMARY, actor, {
				code: textPlaceholder(change.value),
			});
		case 'changed':
			return summarizedChange(
				'vanity_url_code',
				'neutral',
				GUILD_VANITY_URL_CHANGED_ROW,
				GUILD_VANITY_URL_CHANGED_SUMMARY,
				actor,
				{oldCode: textPlaceholder(change.oldValue), newCode: textPlaceholder(change.newValue)},
			);
		case 'removed':
			return summarizedChange(
				'vanity_url_code',
				'remove',
				GUILD_VANITY_URL_REMOVED_ROW,
				GUILD_VANITY_URL_REMOVED_SUMMARY,
				actor,
				{code: textPlaceholder(change.value)},
			);
	}
}

export function presentGuildUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const features = readFeatureLists(entry);
	const changes = [
		presentName(entry, actor),
		presentImage(entry, 'icon_hash', ICON_SENTENCES),
		presentImage(entry, 'banner_hash', BANNER_SENTENCES),
		toggleChange('detached_banner', readFeatureAdded(features, GuildFeatures.DETACHED_BANNER), DETACHED_BANNER_TOGGLE),
		presentImage(entry, 'splash_hash', INVITE_BACKGROUND_SENTENCES),
		presentLabel(
			entry,
			'splash_card_alignment',
			SPLASH_CARD_ALIGNMENT_LABELS,
			GUILD_INVITE_BACKGROUND_ALIGNMENT_CHANGED_ROW,
			'oldAlignment',
			'newAlignment',
		),
		presentImage(entry, 'embed_splash_hash', CHAT_EMBED_BACKGROUND_SENTENCES),
		presentChannel(entry, 'system_channel_id', SYSTEM_CHANNEL_SENTENCES),
		toggleChange(
			'system_channel_flags',
			readTurnedOn(entry, 'system_channel_flags', readJoinMessagesHidden),
			JOIN_MESSAGES_HIDDEN_TOGGLE,
		),
		presentLabel(
			entry,
			'default_message_notifications',
			DEFAULT_NOTIFICATIONS_LABELS,
			GUILD_DEFAULT_NOTIFICATIONS_CHANGED_ROW,
			'oldSetting',
			'newSetting',
		),
		presentChannel(entry, 'afk_channel_id', AFK_CHANNEL_SENTENCES),
		presentAfkTimeout(entry),
		toggleChange(
			'text_channel_flexible_names',
			readFeatureAdded(features, GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES),
			FLEXIBLE_TEXT_CHANNEL_NAMES_TOGGLE,
		),
		toggleChange(
			'hide_owner_crown',
			readFeatureAdded(features, GuildFeatures.HIDE_OWNER_CROWN),
			OWNER_CROWN_HIDDEN_TOGGLE,
		),
		presentMessageHistoryCutoff(entry),
		presentInvitesPaused(features, actor),
		toggleChange(
			'emoji_cloning',
			readCloningAllowed(features, GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_EMOJI_DISABLED),
			EMOJI_CLONING_TOGGLE,
		),
		toggleChange(
			'sticker_cloning',
			readCloningAllowed(features, GuildFeatures.CLONE_STICKER_ENABLED, GuildFeatures.CLONE_STICKER_DISABLED),
			STICKER_CLONING_TOGGLE,
		),
		presentLabel(
			entry,
			'verification_level',
			VERIFICATION_LEVEL_LABELS,
			GUILD_VERIFICATION_LEVEL_CHANGED_ROW,
			'oldLevel',
			'newLevel',
		),
		presentLabel(
			entry,
			'explicit_content_filter',
			EXPLICIT_CONTENT_FILTER_LABELS,
			GUILD_EXPLICIT_CONTENT_FILTER_CHANGED_ROW,
			'oldFilter',
			'newFilter',
		),
		toggleChange('mature_content', readMatureContentOn(entry), MATURE_CONTENT_TOGGLE),
		toggleChange(
			'content_warning_level',
			readTurnedOn(entry, 'content_warning_level', (value) =>
				readSwitch(value, ContentWarningLevel.INHERIT, ContentWarningLevel.CONTENT_WARNING),
			),
			CONTENT_WARNING_TOGGLE,
		),
		presentContentWarningText(entry),
		toggleChange(
			'mfa_level',
			readTurnedOn(entry, 'mfa_level', (value) => readSwitch(value, GuildMFALevel.NONE, GuildMFALevel.ELEVATED)),
			MODERATION_2FA_TOGGLE,
		),
		presentOwnershipTransfer(entry, actor),
		presentVanityUrl(entry, actor),
	].filter((change): change is GuildChange => change !== null);
	const onlySummary = changes.length === 1 ? changes[0].summary : null;
	if (onlySummary !== null) return {summary: onlySummary, rows: [], blocks: []};
	return {
		summary: {descriptor: GUILD_SETTINGS_UPDATED_SUMMARY, values: {actor}},
		rows: changes.map((change) => change.row),
		blocks: [],
	};
}
