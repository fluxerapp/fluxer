// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import Channels from '@app/features/channel/state/Channels';
import {
	AuditLogActionKind,
	AuditLogTargetType,
} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabConstants';
import Guilds from '@app/features/guild/state/Guilds';
import {
	DIRECT_MESSAGE_DESCRIPTOR,
	PERSONAL_NOTES_DESCRIPTOR,
	REACTIONS_DESCRIPTOR,
	TEXT_CHANNEL_DESCRIPTOR,
	VOICE_CHANNEL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Users from '@app/features/user/state/Users';
import {getFormattedDateTime} from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	GuildFeatures,
	GuildOperations,
	GuildSplashCardAlignment,
	SystemChannelFlags,
} from '@fluxer/constants/src/GuildConstants';
import {SECONDS_PER_DAY, SECONDS_PER_HOUR, SECONDS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import type {AuditLogChange} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const MENTIONS_DESCRIPTOR = msg({
	message: '{everyoneMention} mentions',
	comment:
		'Activity log label for the permission or setting related to @everyone mentions. {everyoneMention} is the localized mention tag.',
});
const ON_DESCRIPTOR = msg({
	message: 'On',
	context: 'audit-log-boolean-value',
	comment: 'Audit log value for an enabled boolean setting.',
});
const OFF_DESCRIPTOR = msg({
	message: 'Off',
	context: 'audit-log-boolean-value',
	comment: 'Audit log value for a disabled boolean setting.',
});
const CATEGORY_DESCRIPTOR = msg({
	message: 'Category',
	context: 'audit-log-channel-type',
	comment: 'Audit log channel type label for channel categories.',
});
const LINK_CHANNEL_DESCRIPTOR = msg({
	message: 'Link channel',
	comment: 'Audit log channel type label for channels that point to an external URL.',
});
const GROUP_MESSAGE_DESCRIPTOR = msg({
	message: 'Group message',
	comment: 'Audit log channel type label for group DM channels.',
});
const AUTOMATIC_DESCRIPTOR = msg({
	message: 'Automatic',
	context: 'rtc-region-value',
	comment: 'Audit log voice region value meaning automatic region selection.',
});
const CENTERED_DESCRIPTOR = msg({
	message: 'Centered',
	comment: 'Audit log value for invite splash alignment: centered image placement.',
});
const LEFT_ALIGNED_DESCRIPTOR = msg({
	message: 'Left aligned',
	comment: 'Audit log value for invite splash alignment: left image placement.',
});
const RIGHT_ALIGNED_DESCRIPTOR = msg({
	message: 'Right aligned',
	comment: 'Audit log value for invite splash alignment: right image placement.',
});
const ANIMATED_ICON_DESCRIPTOR = msg({
	message: 'Animated icon',
	comment: 'Audit log community feature label for animated icon support.',
});
const ANIMATED_BANNER_DESCRIPTOR = msg({
	message: 'Animated banner',
	comment: 'Audit log community feature label for animated banner support.',
});
const BANNER_DESCRIPTOR = msg({
	message: 'Banner',
	comment: 'Audit log community feature label for banner image support.',
});
const EMOJI_CLONING_DISABLED_DESCRIPTOR = msg({
	message: 'Emoji cloning disabled',
	comment: 'Audit log community feature label indicating emoji cloning is disabled.',
});
const STICKER_CLONING_DISABLED_DESCRIPTOR = msg({
	message: 'Sticker cloning disabled',
	comment: 'Audit log community feature label indicating sticker cloning is disabled.',
});
const EMOJI_CLONING_ENABLED_DESCRIPTOR = msg({
	message: 'Emoji cloning enabled',
	comment: 'Audit log community feature label indicating emoji cloning is enabled.',
});
const STICKER_CLONING_ENABLED_DESCRIPTOR = msg({
	message: 'Sticker cloning enabled',
	comment: 'Audit log community feature label indicating sticker cloning is enabled.',
});
const DETACHED_BANNER_DESCRIPTOR = msg({
	message: 'Detached banner',
	comment: 'Audit log community feature label for detached banner layout support.',
});
const INVITE_SPLASH_DESCRIPTOR = msg({
	message: 'Invite splash',
	comment: 'Audit log community feature label for invite splash image support.',
});
const INVITES_DISABLED_DESCRIPTOR = msg({
	message: 'Invites disabled',
	comment: 'Audit log community feature label meaning invite creation is disabled.',
});
const FLEXIBLE_TEXT_CHANNEL_NAMES_DESCRIPTOR = msg({
	message: 'Flexible text channel names',
	comment: 'Audit log community feature label for customized text channel naming rules.',
});
const HIDE_COMMUNITY_OWNER_CROWN_DESCRIPTOR = msg({
	message: 'Hide community owner crown',
	comment: 'Audit log community feature label for hiding the owner crown badge.',
});
const MORE_EMOJI_SLOTS_DESCRIPTOR = msg({
	message: 'Legacy emoji slots',
	comment: 'Audit log community feature label for the legacy increased custom emoji capacity feature.',
});
const MORE_STICKER_SLOTS_DESCRIPTOR = msg({
	message: 'Legacy sticker slots',
	comment: 'Audit log community feature label for the legacy increased custom sticker capacity feature.',
});
const UNLIMITED_EMOJI_DESCRIPTOR = msg({
	message: 'Unlimited emoji',
	comment: 'Audit log community feature label for unlimited custom emoji capacity.',
});
const UNLIMITED_STICKERS_DESCRIPTOR = msg({
	message: 'Unlimited stickers',
	comment: 'Audit log community feature label for unlimited custom sticker capacity.',
});
const EXPRESSION_PURGE_DESCRIPTOR = msg({
	message: 'Expression purge',
	comment: 'Audit log community feature label for expression purge tooling.',
});
const VANITY_URL_DESCRIPTOR = msg({
	message: 'Vanity URL',
	comment: 'Audit log community feature label for a custom community invite URL.',
});
const VERIFIED_GUILD_DESCRIPTOR = msg({
	message: 'Verified community',
	comment: 'Audit log community feature label for verified communities.',
});
const AUDIO_BITRATE_128_KBPS_DESCRIPTOR = msg({
	message: '128 kbps voice quality',
	comment:
		'Audit log community feature label for a raised voice channel bitrate ceiling. kbps means kilobits per second.',
});
const AUDIO_BITRATE_256_KBPS_DESCRIPTOR = msg({
	message: '256 kbps voice quality',
	comment:
		'Audit log community feature label for a raised voice channel bitrate ceiling. kbps means kilobits per second.',
});
const AUDIO_BITRATE_384_KBPS_DESCRIPTOR = msg({
	message: '384 kbps voice quality',
	comment:
		'Audit log community feature label for a raised voice channel bitrate ceiling. kbps means kilobits per second.',
});
const VIP_VOICE_DESCRIPTOR = msg({
	message: 'VIP voice',
	comment: 'Audit log community feature label for elevated voice capacity/quality.',
});
const UNAVAILABLE_FOR_EVERYONE_DESCRIPTOR = msg({
	message: 'Unavailable for everyone',
	comment: 'Audit log community feature label meaning the community is unavailable to all users.',
});
const UNAVAILABLE_FOR_EVERYONE_BUT_STAFF_DESCRIPTOR = msg({
	message: 'Unavailable for everyone but staff',
	comment: 'Audit log community feature label meaning only staff can access the community.',
});
const VISIONARY_DESCRIPTOR = msg({
	message: 'Visionary',
	comment: 'Audit log community feature label for the Visionary premium feature.',
});
const PUSH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Push notifications',
	comment: 'Audit log operation flag label.',
});
const TYPING_EVENTS_DESCRIPTOR = msg({
	message: 'Typing events',
	comment: 'Audit log operation flag label.',
});
const INSTANT_INVITES_DESCRIPTOR = msg({
	message: 'Instant invites',
	comment: 'Audit log operation flag label for invite creation.',
});
const SEND_MESSAGES_DESCRIPTOR = msg({
	message: 'Send messages',
	comment: 'Audit log operation flag label for message sending.',
});
const MEMBER_LIST_UPDATES_DESCRIPTOR = msg({
	message: 'Member list updates',
	comment: 'Audit log operation flag label.',
});
const JOIN_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Join notifications',
	comment: 'Audit log system-channel flag label for member join notification posts.',
});

export interface BasicRecord {
	[key: string]: unknown;
}

export interface ChangeShapeWithUnknowns {
	key: string;
	oldValue: unknown;
	newValue: unknown;
}

export function isBasicRecord(value: unknown): value is BasicRecord {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function toChangeShape(raw: AuditLogChange): ChangeShapeWithUnknowns {
	const key = typeof raw.key === 'string' ? raw.key : '';
	const oldValue = 'oldValue' in raw ? raw.oldValue : raw.old_value;
	const newValue = 'newValue' in raw ? raw.newValue : raw.new_value;
	return {key, oldValue: oldValue ?? null, newValue: newValue ?? null};
}

export function safeScalarString(value: unknown, i18n: I18n): string | null {
	if (value === null || value === undefined) return null;
	if (typeof value === 'string') return value;
	if (typeof value === 'number') return String(value);
	if (typeof value === 'boolean') {
		return value ? i18n._(ON_DESCRIPTOR) : i18n._(OFF_DESCRIPTOR);
	}
	return null;
}

export function looksLikeSnowflake(s: string): boolean {
	return /^\d{16,22}$/.test(s);
}

export function isEmptyString(value: unknown): value is string {
	return typeof value === 'string' && value.trim().length === 0;
}

export function resolveIdToName(id: string, guildId: string): string | null {
	const user = Users.getUser(id);
	if (user) return NicknameUtils.getDisplayName(user);
	const channel = Channels.getChannel(id);
	if (channel?.name) return channel.name;
	const roles = Guilds.getGuildRoles(guildId, true);
	const role = roles.find((r) => r.id === id);
	if (role) return role.name;
	return null;
}

const targetTypeMap: Partial<Record<AuditLogActionType, AuditLogTargetType>> = {
	[AuditLogActionType.GUILD_UPDATE]: AuditLogTargetType.GUILD,
	[AuditLogActionType.CHANNEL_CREATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_UPDATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_DELETE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_CREATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_UPDATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_DELETE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.MEMBER_KICK]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_PRUNE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_BAN_ADD]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_BAN_REMOVE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_UPDATE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_ROLE_UPDATE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_MOVE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_DISCONNECT]: AuditLogTargetType.USER,
	[AuditLogActionType.BOT_ADD]: AuditLogTargetType.USER,
	[AuditLogActionType.ROLE_CREATE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.ROLE_UPDATE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.ROLE_DELETE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.INVITE_CREATE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.INVITE_UPDATE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.INVITE_DELETE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.WEBHOOK_CREATE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.WEBHOOK_UPDATE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.WEBHOOK_DELETE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.EMOJI_CREATE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.EMOJI_UPDATE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.EMOJI_DELETE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.STICKER_CREATE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.STICKER_UPDATE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.STICKER_DELETE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.MESSAGE_DELETE]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_BULK_DELETE]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_PIN]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_UNPIN]: AuditLogTargetType.MESSAGE,
};

export function getTargetType(actionType: AuditLogActionType): AuditLogTargetType {
	return targetTypeMap[actionType] ?? AuditLogTargetType.ALL;
}

const suppressedDetailActions = new Set<AuditLogActionType>([
	AuditLogActionType.MEMBER_KICK,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
	AuditLogActionType.MESSAGE_DELETE,
	AuditLogActionType.MESSAGE_BULK_DELETE,
	AuditLogActionType.MESSAGE_PIN,
	AuditLogActionType.MESSAGE_UNPIN,
	AuditLogActionType.CHANNEL_DELETE,
]);
const NotRenderedChangeKeys: Partial<Record<AuditLogTargetType, Record<string, true>>> = {
	[AuditLogTargetType.GUILD]: {
		guild_id: true,
		banner_width: true,
		banner_height: true,
		splash_width: true,
		splash_height: true,
		embed_splash_width: true,
		embed_splash_height: true,
		member_count: true,
	},
	[AuditLogTargetType.CHANNEL]: {type: true, id: true},
	[AuditLogTargetType.INVITE]: {guild_id: true, channel_id: true, inviter_id: true},
	[AuditLogTargetType.WEBHOOK]: {application_id: true, id: true, guild_id: true, creator_id: true},
	[AuditLogTargetType.USER]: {
		user_id: true,
	},
	[AuditLogTargetType.EMOJI]: {emoji_id: true, creator_id: true},
	[AuditLogTargetType.STICKER]: {sticker_id: true, creator_id: true},
	[AuditLogTargetType.ROLE]: {role_id: true},
};

export function shouldSuppressDetailsForAction(actionType: AuditLogActionType): boolean {
	return suppressedDetailActions.has(actionType);
}

export function shouldHideChangeKey(targetType: AuditLogTargetType, changeKey: string): boolean {
	const target = NotRenderedChangeKeys[targetType];
	return target != null && target[changeKey] === true;
}

const createActions = new Set<AuditLogActionType>([
	AuditLogActionType.CHANNEL_CREATE,
	AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
	AuditLogActionType.ROLE_CREATE,
	AuditLogActionType.INVITE_CREATE,
	AuditLogActionType.WEBHOOK_CREATE,
	AuditLogActionType.EMOJI_CREATE,
	AuditLogActionType.STICKER_CREATE,
	AuditLogActionType.BOT_ADD,
	AuditLogActionType.MEMBER_BAN_ADD,
	AuditLogActionType.MESSAGE_PIN,
]);
const updateActions = new Set<AuditLogActionType>([
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.INVITE_UPDATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.STICKER_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
]);

export function getActionKind(actionType: AuditLogActionType): AuditLogActionKind {
	if (createActions.has(actionType)) return AuditLogActionKind.CREATE;
	if (updateActions.has(actionType)) return AuditLogActionKind.UPDATE;
	return AuditLogActionKind.DELETE;
}

export function normalizeChanges(changes?: Array<AuditLogChange> | null): Array<AuditLogChange> {
	return changes ?? [];
}

export function getChannelTypeLabel(value: unknown, i18n: I18n): string | null {
	if (typeof value !== 'number') return null;
	switch (value) {
		case ChannelTypes.GUILD_TEXT:
			return i18n._(TEXT_CHANNEL_DESCRIPTOR);
		case ChannelTypes.GUILD_VOICE:
			return i18n._(VOICE_CHANNEL_DESCRIPTOR);
		case ChannelTypes.GUILD_CATEGORY:
			return i18n._(CATEGORY_DESCRIPTOR);
		case ChannelTypes.GUILD_LINK:
			return i18n._(LINK_CHANNEL_DESCRIPTOR);
		case ChannelTypes.DM:
			return i18n._(DIRECT_MESSAGE_DESCRIPTOR);
		case ChannelTypes.GROUP_DM:
			return i18n._(GROUP_MESSAGE_DESCRIPTOR);
		case ChannelTypes.DM_PERSONAL_NOTES:
			return i18n._(PERSONAL_NOTES_DESCRIPTOR);
		default:
			return null;
	}
}

export function getRtcRegionLabel(value: unknown, i18n: I18n): string | null {
	if (value === null || value === undefined) {
		return i18n._(AUTOMATIC_DESCRIPTOR);
	}
	if (isEmptyString(value)) {
		return i18n._(AUTOMATIC_DESCRIPTOR);
	}
	if (typeof value === 'string') return value;
	return null;
}

export function getSplashAlignmentLabel(value: unknown, i18n: I18n): string | null {
	if (typeof value !== 'number') return null;
	switch (value) {
		case GuildSplashCardAlignment.CENTER:
			return i18n._(CENTERED_DESCRIPTOR);
		case GuildSplashCardAlignment.LEFT:
			return i18n._(LEFT_ALIGNED_DESCRIPTOR);
		case GuildSplashCardAlignment.RIGHT:
			return i18n._(RIGHT_ALIGNED_DESCRIPTOR);
		default:
			return null;
	}
}

const featureLabelMap: Record<string, MessageDescriptor> = {
	[GuildFeatures.ANIMATED_ICON]: ANIMATED_ICON_DESCRIPTOR,
	[GuildFeatures.ANIMATED_BANNER]: ANIMATED_BANNER_DESCRIPTOR,
	[GuildFeatures.AUDIO_BITRATE_128_KBPS]: AUDIO_BITRATE_128_KBPS_DESCRIPTOR,
	[GuildFeatures.AUDIO_BITRATE_256_KBPS]: AUDIO_BITRATE_256_KBPS_DESCRIPTOR,
	[GuildFeatures.AUDIO_BITRATE_384_KBPS]: AUDIO_BITRATE_384_KBPS_DESCRIPTOR,
	[GuildFeatures.BANNER]: BANNER_DESCRIPTOR,
	[GuildFeatures.CLONE_EMOJI_DISABLED]: EMOJI_CLONING_DISABLED_DESCRIPTOR,
	[GuildFeatures.CLONE_STICKER_DISABLED]: STICKER_CLONING_DISABLED_DESCRIPTOR,
	[GuildFeatures.CLONE_EMOJI_ENABLED]: EMOJI_CLONING_ENABLED_DESCRIPTOR,
	[GuildFeatures.CLONE_STICKER_ENABLED]: STICKER_CLONING_ENABLED_DESCRIPTOR,
	[GuildFeatures.DETACHED_BANNER]: DETACHED_BANNER_DESCRIPTOR,
	[GuildFeatures.INVITE_SPLASH]: INVITE_SPLASH_DESCRIPTOR,
	[GuildFeatures.INVITES_DISABLED]: INVITES_DISABLED_DESCRIPTOR,
	[GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES]: FLEXIBLE_TEXT_CHANNEL_NAMES_DESCRIPTOR,
	[GuildFeatures.HIDE_OWNER_CROWN]: HIDE_COMMUNITY_OWNER_CROWN_DESCRIPTOR,
	[GuildFeatures.MORE_EMOJI]: MORE_EMOJI_SLOTS_DESCRIPTOR,
	[GuildFeatures.MORE_STICKERS]: MORE_STICKER_SLOTS_DESCRIPTOR,
	[GuildFeatures.UNLIMITED_EMOJI]: UNLIMITED_EMOJI_DESCRIPTOR,
	[GuildFeatures.UNLIMITED_STICKERS]: UNLIMITED_STICKERS_DESCRIPTOR,
	[GuildFeatures.EXPRESSION_PURGE_ALLOWED]: EXPRESSION_PURGE_DESCRIPTOR,
	[GuildFeatures.VANITY_URL]: VANITY_URL_DESCRIPTOR,
	[GuildFeatures.VERIFIED]: VERIFIED_GUILD_DESCRIPTOR,
	[GuildFeatures.VIP_VOICE]: VIP_VOICE_DESCRIPTOR,
	[GuildFeatures.UNAVAILABLE_FOR_EVERYONE]: UNAVAILABLE_FOR_EVERYONE_DESCRIPTOR,
	[GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF]: UNAVAILABLE_FOR_EVERYONE_BUT_STAFF_DESCRIPTOR,
	[GuildFeatures.VISIONARY]: VISIONARY_DESCRIPTOR,
};
const normalizeFeatureList = (value: unknown): Array<string> =>
	Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : [];

export function getFeatureDiff(
	oldValue: unknown,
	newValue: unknown,
): {
	added: Array<string>;
	removed: Array<string>;
} {
	const oldFeatures = new Set(normalizeFeatureList(oldValue));
	const newFeatures = normalizeFeatureList(newValue);
	const added = newFeatures.filter((feature) => !oldFeatures.has(feature));
	const removed = normalizeFeatureList(oldValue).filter((feature) => !newFeatures.includes(feature));
	return {added, removed};
}

export function getFeatureLabel(feature: string, i18n: I18n): string | null {
	const msg = featureLabelMap[feature];
	return msg ? i18n._(msg) : null;
}

interface FlagLabel {
	flag: number;
	label: MessageDescriptor;
}

const operationFlagLabels: Array<FlagLabel> = [
	{
		flag: GuildOperations.PUSH_NOTIFICATIONS,
		label: PUSH_NOTIFICATIONS_DESCRIPTOR,
	},
	{
		flag: GuildOperations.EVERYONE_MENTIONS,
		label: {...MENTIONS_DESCRIPTOR, values: {everyoneMention: EVERYONE_MENTION}},
	},
	{
		flag: GuildOperations.TYPING_EVENTS,
		label: TYPING_EVENTS_DESCRIPTOR,
	},
	{
		flag: GuildOperations.INSTANT_INVITES,
		label: INSTANT_INVITES_DESCRIPTOR,
	},
	{
		flag: GuildOperations.SEND_MESSAGE,
		label: SEND_MESSAGES_DESCRIPTOR,
	},
	{
		flag: GuildOperations.REACTIONS,
		label: REACTIONS_DESCRIPTOR,
	},
	{
		flag: GuildOperations.MEMBER_LIST_UPDATES,
		label: MEMBER_LIST_UPDATES_DESCRIPTOR,
	},
];
const systemChannelFlagLabels: Array<FlagLabel> = [
	{
		flag: SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS,
		label: JOIN_NOTIFICATIONS_DESCRIPTOR,
	},
];
const getFlagLabels = (value: unknown, labels: Array<FlagLabel>, i18n: I18n): Array<string> => {
	const mask = typeof value === 'number' ? value : Number(value ?? 0);
	if (Number.isNaN(mask)) return [];
	return labels.filter(({flag}) => (mask & flag) !== 0).map(({label}) => i18n._(label));
};

export function getOperationDiff(
	oldValue: unknown,
	newValue: unknown,
	i18n: I18n,
): {
	added: Array<string>;
	removed: Array<string>;
} {
	const oldLabels = new Set(getFlagLabels(oldValue, operationFlagLabels, i18n));
	const newLabels = getFlagLabels(newValue, operationFlagLabels, i18n);
	const added = newLabels.filter((label) => !oldLabels.has(label));
	const removed = getFlagLabels(oldValue, operationFlagLabels, i18n).filter((label) => !newLabels.includes(label));
	return {added, removed};
}

export function getSystemChannelFlagDiff(
	oldValue: unknown,
	newValue: unknown,
	i18n: I18n,
): {
	added: Array<string>;
	removed: Array<string>;
} {
	const oldLabels = new Set(getFlagLabels(oldValue, systemChannelFlagLabels, i18n));
	const newLabels = getFlagLabels(newValue, systemChannelFlagLabels, i18n);
	const added = newLabels.filter((label) => !oldLabels.has(label));
	const removed = getFlagLabels(oldValue, systemChannelFlagLabels, i18n).filter((label) => !newLabels.includes(label));
	return {added, removed};
}

const formatDateValue = (raw: unknown): string | null => {
	const timestamp =
		typeof raw === 'string' ? Date.parse(raw) : raw instanceof Date ? raw.getTime() : Number(raw ?? NaN);
	if (Number.isNaN(timestamp)) return null;
	return getFormattedDateTime(timestamp);
};

export function formatDateStringValue(value: unknown): string | null {
	return formatDateValue(value);
}

export function formatAccentColor(value: unknown): string | null {
	const numberValue = typeof value === 'number' ? value : Number(value ?? NaN);
	if (Number.isNaN(numberValue)) return null;
	const hex = numberValue.toString(16).padStart(6, '0').toUpperCase();
	return `#${hex}`;
}

const ALL_PERMISSION_FLAGS: Array<bigint> = Object.values(Permissions);
const toBigIntSafe = (value: unknown): bigint | null => {
	if (typeof value === 'bigint') return value;
	if (typeof value === 'number') return BigInt(value);
	if (typeof value === 'string') {
		try {
			return BigInt(value);
		} catch {
			return null;
		}
	}
	return null;
};

export interface PermissionDiff {
	added: Array<bigint>;
	removed: Array<bigint>;
}

export function getPermissionDiff(oldValue: unknown, newValue: unknown): PermissionDiff {
	const oldPerms = toBigIntSafe(oldValue) ?? 0n;
	const newPerms = toBigIntSafe(newValue) ?? 0n;
	const added: Array<bigint> = [];
	const removed: Array<bigint> = [];
	for (const flag of ALL_PERMISSION_FLAGS) {
		const wasSet = (oldPerms & flag) === flag;
		const isSet = (newPerms & flag) === flag;
		if (!wasSet && isSet) {
			added.push(flag);
		} else if (wasSet && !isSet) {
			removed.push(flag);
		}
	}
	return {added, removed};
}

export interface DurationParts {
	days: number;
	hours: number;
	minutes: number;
	seconds: number;
}

export function secondsToDurationParts(totalSeconds: number): DurationParts {
	const days = Math.floor(totalSeconds / SECONDS_PER_DAY);
	const hours = Math.floor((totalSeconds % SECONDS_PER_DAY) / SECONDS_PER_HOUR);
	const minutes = Math.floor((totalSeconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
	const seconds = totalSeconds % 60;
	return {days, hours, minutes, seconds};
}
