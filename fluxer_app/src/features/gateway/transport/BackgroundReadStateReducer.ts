// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BackgroundMentionCountMode,
	type BackgroundMessageNotification,
	type BackgroundSnapshotSink,
	type BackgroundSnapshotSinkConfig,
	type ObserveMentionCounts,
	type ObserveMessageNotification,
} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {resolveReadStateMention} from '@app/features/read_state/state/read_states/ReadStateMentionMachine';
import {compareMessageIds, snowflakeTimestamp} from '@app/features/read_state/state/read_states/shared';
import {ME} from '@fluxer/constants/src/AppConstants';
import {
	ChannelTypes,
	MessageFlags,
	MessageTypes,
	TEXT_BASED_CHANNEL_TYPES,
} from '@fluxer/constants/src/ChannelConstants';
import {LARGE_GUILD_THRESHOLD} from '@fluxer/constants/src/GatewayConstants';
import {ContentWarningLevel, GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

const MAX_BACKGROUND_READ_STATE_CHANNELS = 100_000;
const MAX_BACKGROUND_READ_STATE_GUILDS = 100_000;
const MAX_BACKGROUND_READ_STATE_USERS = 100_000;

interface MuteConfig {
	readonly muted: boolean;
	readonly mutedUntil: number | null;
}

interface ChannelNotificationOverride {
	readonly mute: MuteConfig;
	readonly messageNotifications: number;
	readonly unreadBadges: number | null;
}

interface GuildNotificationSettings {
	readonly mute: MuteConfig;
	readonly messageNotifications: number;
	readonly suppressEveryone: boolean;
	readonly suppressRoles: boolean;
	readonly unreadBadges: number | null;
	readonly channelOverrides: ReadonlyMap<string, ChannelNotificationOverride>;
}

interface GuildDetails {
	readonly name: string | null;
	readonly defaultMessageNotifications: number;
	readonly memberCount: number | null;
	readonly largeFeature: boolean;
	readonly joinedAt: number | null;
	readonly matureContent: boolean;
	readonly contentWarning: boolean;
}

interface ChannelDetails {
	readonly name: string | null;
	readonly parentId: string | null;
	readonly type: number | null;
	readonly matureContent: boolean | null;
	readonly contentWarning: boolean | null;
}

const UNMUTED: MuteConfig = {muted: false, mutedUntil: null};

const DEFAULT_GUILD_SETTINGS: GuildNotificationSettings = {
	mute: UNMUTED,
	messageNotifications: MessageNotifications.NULL,
	suppressEveryone: false,
	suppressRoles: false,
	unreadBadges: null,
	channelOverrides: new Map(),
};

const NOTIFIABLE_MESSAGE_TYPES: ReadonlySet<number> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);

const UNREAD_TRACKED_CHANNEL_TYPES: ReadonlySet<number> = new Set([
	...TEXT_BASED_CHANNEL_TYPES,
	ChannelTypes.ANNOUNCEMENT_THREAD,
	ChannelTypes.PUBLIC_THREAD,
	ChannelTypes.PRIVATE_THREAD,
]);

class ReadStateCapacityExceeded extends Error {
	constructor(kind: 'channels' | 'guilds' | 'users', capacity: number) {
		super(`Background read state exceeded its ${capacity} tracked ${kind} bound`);
		this.name = 'ReadStateCapacityExceeded';
	}
}

export interface BackgroundChannelActivity {
	readonly channelId: string;
	readonly guildId: string | null;
	readonly parentId: string | null;
	readonly type: number | null;
	readonly name: string | null;
	readonly mentionCount: number;
	readonly unread: boolean;
	readonly lastMessageId: string | null;
	readonly ackMessageId: string | null;
}

export interface BackgroundReadStateTotals {
	readonly mentionCount: number;
	readonly unreadChannelCount: number;
	readonly lastMessageId: string | null;
	readonly degraded: boolean;
}

function asRecord(value: unknown): Record<string, unknown> | null {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return null;
	}
	return value as Record<string, unknown>;
}

function asArray(value: unknown): ReadonlyArray<unknown> {
	return Array.isArray(value) ? value : [];
}

function asString(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 ? value : null;
}

function asCount(value: unknown): number {
	return typeof value === 'number' && Number.isFinite(value) && value > 0 ? Math.floor(value) : 0;
}

function asInteger(value: unknown): number | null {
	return typeof value === 'number' && Number.isInteger(value) ? value : null;
}

function isExplicitNotificationLevel(level: number | null | undefined): level is number {
	return (
		level === MessageNotifications.ALL_MESSAGES ||
		level === MessageNotifications.ONLY_MENTIONS ||
		level === MessageNotifications.NO_MESSAGES
	);
}

function asIdSet(value: unknown): ReadonlySet<string> {
	const ids = new Set<string>();
	for (const entry of asArray(value)) {
		const id = asString(entry);
		if (id !== null) {
			ids.add(id);
		}
	}
	return ids;
}

function readMuteConfig(source: Record<string, unknown>): MuteConfig {
	if (source.muted !== true) {
		return UNMUTED;
	}
	const endTime = asString(asRecord(source.mute_config)?.end_time);
	if (endTime === null) {
		return {muted: true, mutedUntil: null};
	}
	const parsed = Date.parse(endTime);
	if (Number.isNaN(parsed)) {
		return {muted: true, mutedUntil: null};
	}
	return {muted: true, mutedUntil: parsed};
}

function isMuted(config: MuteConfig, now: number): boolean {
	if (!config.muted) {
		return false;
	}
	return config.mutedUntil === null || config.mutedUntil > now;
}

function readChannelOverrides(value: unknown): ReadonlyMap<string, ChannelNotificationOverride> {
	const overrides = new Map<string, ChannelNotificationOverride>();
	const record = asRecord(value);
	const entries = record === null ? asArray(value) : Object.values(record);
	for (const entry of entries) {
		const override = asRecord(entry);
		if (override === null) {
			continue;
		}
		const channelId = asString(override.channel_id);
		if (channelId === null) {
			continue;
		}
		overrides.set(channelId, {
			mute: readMuteConfig(override),
			messageNotifications: asInteger(override.message_notifications) ?? MessageNotifications.NULL,
			unreadBadges: asInteger(override.unread_badges),
		});
	}
	return overrides;
}

function readGuildSettings(value: unknown): {guildId: string; settings: GuildNotificationSettings} | null {
	const source = asRecord(value);
	if (source === null) {
		return null;
	}
	const guildId = source.guild_id == null ? ME : asString(source.guild_id);
	if (guildId === null) {
		return null;
	}
	return {
		guildId,
		settings: {
			mute: readMuteConfig(source),
			messageNotifications: asInteger(source.message_notifications) ?? MessageNotifications.NULL,
			suppressEveryone: source.suppress_everyone === true,
			suppressRoles: source.suppress_roles === true,
			unreadBadges: asInteger(source.unread_badges),
			channelOverrides: readChannelOverrides(source.channel_overrides),
		},
	};
}

function readGuildDetails(source: Record<string, unknown>, prior: GuildDetails | undefined): GuildDetails {
	const properties = asRecord(source.properties) ?? source;
	const features = asIdSet(properties.features);
	const memberCount = asInteger(source.member_count) ?? asInteger(properties.member_count);
	const joinedAt = asString(source.joined_at) ?? asString(properties.joined_at);
	const parsedJoinedAt = joinedAt === null ? Number.NaN : Date.parse(joinedAt);
	return {
		name: asString(properties.name) ?? prior?.name ?? null,
		defaultMessageNotifications:
			asInteger(properties.default_message_notifications) ??
			prior?.defaultMessageNotifications ??
			MessageNotifications.ALL_MESSAGES,
		memberCount: memberCount ?? prior?.memberCount ?? null,
		largeFeature: features.has(GuildFeatures.LARGE_GUILD_OVERRIDE) || features.has(GuildFeatures.VERY_LARGE_GUILD),
		joinedAt: Number.isNaN(parsedJoinedAt) ? (prior?.joinedAt ?? null) : parsedJoinedAt,
		matureContent: typeof properties.nsfw === 'boolean' ? properties.nsfw : (prior?.matureContent ?? false),
		contentWarning:
			typeof properties.content_warning_level === 'number'
				? properties.content_warning_level === ContentWarningLevel.CONTENT_WARNING
				: (prior?.contentWarning ?? false),
	};
}

function effectiveGuildMessageNotifications(details: GuildDetails | undefined): number {
	if (details === undefined) {
		return MessageNotifications.ALL_MESSAGES;
	}
	if (details.memberCount === null || details.memberCount < 0) {
		return details.defaultMessageNotifications;
	}
	if (details.largeFeature || details.memberCount > LARGE_GUILD_THRESHOLD) {
		return MessageNotifications.ONLY_MENTIONS;
	}
	return details.defaultMessageNotifications;
}

function readContentWarning(value: unknown): boolean | null {
	const level = asInteger(value);
	return level === null || level === ContentWarningLevel.INHERIT ? null : level === ContentWarningLevel.CONTENT_WARNING;
}

function readChannelDetails(source: Record<string, unknown>): ChannelDetails {
	return {
		name: asString(source.name),
		parentId: asString(source.parent_id),
		type: asInteger(source.type),
		matureContent: typeof source.nsfw_override === 'boolean' ? source.nsfw_override : null,
		contentWarning: readContentWarning(source.content_warning_level),
	};
}

const THREAD_CHANNEL_TYPES: ReadonlySet<number> = new Set([
	ChannelTypes.ANNOUNCEMENT_THREAD,
	ChannelTypes.PUBLIC_THREAD,
	ChannelTypes.PRIVATE_THREAD,
]);

export class BackgroundReadStateReducer implements BackgroundSnapshotSink {
	private readonly userId: string;
	private readonly observeMentionCounts: ObserveMentionCounts;
	private readonly observeMessageNotification: ObserveMessageNotification;
	private readonly observeMention: ObserveMessageNotification | null;
	private readonly mentionCounts = new Map<string, number>();
	private readonly lastMessageIds = new Map<string, string>();
	private readonly ackMessageIds = new Map<string, string>();
	private readonly unreadChannels = new Set<string>();
	private newestMessageId: string | null = null;
	private degraded = false;
	private unreadBadgeCustomizationEnabled = false;
	private readonly channelGuilds = new Map<string, string>();
	private readonly guildChannels = new Map<string, Set<string>>();
	private readonly guildSettings = new Map<string, GuildNotificationSettings>();
	private readonly guildRoles = new Map<string, ReadonlySet<string>>();
	private readonly guildDetails = new Map<string, GuildDetails>();
	private readonly channelDetails = new Map<string, ChannelDetails>();
	private readonly blockedUserIds = new Set<string>();

	constructor({
		userId,
		observeMentionCounts,
		observeMessageNotification,
		observeMention,
	}: BackgroundSnapshotSinkConfig) {
		this.userId = userId;
		this.observeMentionCounts = observeMentionCounts;
		this.observeMessageNotification = observeMessageNotification;
		this.observeMention = observeMention ?? null;
	}

	get isDegraded(): boolean {
		return this.degraded;
	}

	totals(): BackgroundReadStateTotals {
		let mentionCount = 0;
		for (const [channelId, count] of this.mentionCounts) {
			if (this.channelDetails.has(channelId)) {
				mentionCount += count;
			}
		}
		return {
			mentionCount,
			unreadChannelCount: this.unreadChannels.size,
			lastMessageId: this.newestMessageId,
			degraded: this.degraded,
		};
	}

	mentionCountsSnapshot(): Map<string, number> {
		return new Map(this.mentionCounts);
	}

	*channelActivity(): IterableIterator<BackgroundChannelActivity> {
		const channelIds = new Set<string>(this.unreadChannels);
		for (const [channelId, count] of this.mentionCounts) {
			if (count > 0 && this.channelDetails.has(channelId)) {
				channelIds.add(channelId);
			}
		}
		for (const channelId of channelIds) {
			const details = this.channelDetails.get(channelId);
			yield {
				channelId,
				guildId: this.channelGuilds.get(channelId) ?? null,
				parentId: details?.parentId ?? null,
				type: details?.type ?? null,
				name: details?.name ?? null,
				mentionCount: this.mentionCounts.get(channelId) ?? 0,
				unread: this.unreadChannels.has(channelId),
				lastMessageId: this.lastMessageIds.get(channelId) ?? null,
				ackMessageId: this.ackMessageIds.get(channelId) ?? null,
			};
		}
	}

	guildName(guildId: string): string | null {
		return this.guildDetails.get(guildId)?.name ?? null;
	}

	refreshUnreadState(): void {
		this.refreshAllUnread();
	}

	setUnreadBadgeCustomizationEnabled(enabled: boolean): void {
		if (this.unreadBadgeCustomizationEnabled === enabled) {
			return;
		}
		this.unreadBadgeCustomizationEnabled = enabled;
		this.refreshAllUnread();
	}

	isChannelGated(channelId: string, guildId: string | null): boolean {
		let channel = this.channelDetails.get(channelId);
		if (channel?.type != null && THREAD_CHANNEL_TYPES.has(channel.type) && channel.parentId !== null) {
			channel = this.channelDetails.get(channel.parentId);
		}
		const parent = channel?.parentId == null ? undefined : this.channelDetails.get(channel.parentId);
		const resolvedGuildId = guildId ?? this.channelGuilds.get(channelId) ?? null;
		const guild = resolvedGuildId === null ? undefined : this.guildDetails.get(resolvedGuildId);
		const matureContent = channel?.matureContent ?? parent?.matureContent ?? guild?.matureContent ?? false;
		const contentWarning = channel?.contentWarning ?? parent?.contentWarning ?? guild?.contentWarning ?? false;
		return matureContent || contentWarning;
	}

	applyReady(data: unknown): void {
		this.degraded = false;
		try {
			this.applyReadyUnguarded(data);
		} catch (error) {
			this.degradeOnCapacity(error);
		}
	}

	applyDispatch(type: string, data: unknown): void {
		if (this.degraded) {
			return;
		}
		try {
			this.applyDispatchUnguarded(type, data);
		} catch (error) {
			this.degradeOnCapacity(error);
		}
	}

	private degradeOnCapacity(error: unknown): void {
		if (!(error instanceof ReadStateCapacityExceeded)) {
			throw error;
		}
		this.clearState();
		this.degraded = true;
		this.observeMentionCounts(new Map(), BackgroundMentionCountMode.REPLACE);
	}

	private applyReadyUnguarded(data: unknown): void {
		const ready = asRecord(data);
		this.clearState();
		if (ready === null) {
			this.observeMentionCounts(new Map(), BackgroundMentionCountMode.REPLACE);
			return;
		}
		for (const entry of asArray(ready.guilds)) {
			this.seedGuild(entry);
		}
		for (const entry of asArray(ready.private_channels)) {
			this.storeChannel(entry);
		}
		for (const entry of asArray(ready.relationships)) {
			this.storeRelationship(entry);
		}
		for (const entry of asArray(ready.user_guild_settings)) {
			this.storeGuildSettings(entry);
		}
		for (const entry of asArray(ready.read_states)) {
			const readState = asRecord(entry);
			const channelId = asString(readState?.id);
			if (channelId === null) {
				continue;
			}
			const mentionCount = asCount(readState?.mention_count);
			if (mentionCount > 0) {
				this.storeMentionCount(channelId, mentionCount);
			}
			const ackMessageId = asString(readState?.last_message_id);
			if (ackMessageId !== null) {
				this.assertChannelCapacity(this.ackMessageIds, channelId);
				this.ackMessageIds.set(channelId, ackMessageId);
			}
		}
		this.refreshAllUnread();
		const visible = new Map<string, number>();
		for (const [channelId, mentionCount] of this.mentionCounts) {
			if (this.channelDetails.has(channelId)) {
				visible.set(channelId, mentionCount);
			}
		}
		this.observeMentionCounts(visible, BackgroundMentionCountMode.REPLACE);
	}

	private applyDispatchUnguarded(type: string, data: unknown): void {
		const payload = asRecord(data);
		if (payload === null) {
			return;
		}
		switch (type) {
			case 'MESSAGE_CREATE':
				this.applyMessageCreate(payload);
				return;
			case 'MESSAGE_ACK':
				this.applyMessageAck(payload);
				return;
			case 'CHANNEL_CREATE':
			case 'CHANNEL_UPDATE':
			case 'THREAD_CREATE':
			case 'THREAD_UPDATE':
				this.storeChannel(payload);
				return;
			case 'CHANNEL_DELETE':
			case 'THREAD_DELETE':
				this.applyChannelDelete(payload);
				return;
			case 'THREAD_LIST_SYNC':
				for (const entry of asArray(payload.threads)) {
					this.storeChannel(entry);
				}
				return;
			case 'GUILD_CREATE':
				this.seedGuild(payload);
				return;
			case 'GUILD_UPDATE':
				this.storeGuildDetails(payload);
				return;
			case 'CHANNEL_UPDATE_BULK':
				for (const entry of asArray(payload.channels)) {
					this.storeChannel(entry);
				}
				return;
			case 'GUILD_DELETE':
				this.applyGuildDelete(payload);
				return;
			case 'USER_GUILD_SETTINGS_UPDATE':
				this.storeGuildSettings(payload);
				this.refreshAllUnread();
				return;
			case 'RELATIONSHIP_ADD':
			case 'RELATIONSHIP_UPDATE':
				this.storeRelationship(payload);
				return;
			case 'RELATIONSHIP_REMOVE': {
				const userId = asString(payload.id);
				if (userId !== null) {
					this.blockedUserIds.delete(userId);
				}
				return;
			}
			default:
				return;
		}
	}

	reset(): void {
		this.clearState();
		this.observeMentionCounts(new Map(), BackgroundMentionCountMode.REPLACE);
	}

	private clearState(): void {
		this.mentionCounts.clear();
		this.lastMessageIds.clear();
		this.ackMessageIds.clear();
		this.unreadChannels.clear();
		this.newestMessageId = null;
		this.channelGuilds.clear();
		this.guildChannels.clear();
		this.guildSettings.clear();
		this.guildRoles.clear();
		this.guildDetails.clear();
		this.channelDetails.clear();
		this.blockedUserIds.clear();
	}

	private seedGuild(value: unknown): void {
		const guild = asRecord(value);
		const guildId = asString(guild?.id);
		if (guild === null || guildId === null || guild.unavailable === true) {
			return;
		}
		this.storeGuildDetails(guild);
		const revealed = new Map<string, number>();
		for (const entry of [...asArray(guild.channels), ...asArray(guild.threads)]) {
			const channel = asRecord(entry);
			const channelId = asString(channel?.id);
			if (channel !== null && channelId !== null) {
				this.linkChannel(guildId, channelId);
				const mentionCount = this.storeChannelDetails(channelId, channel);
				if (mentionCount > 0) {
					revealed.set(channelId, mentionCount);
				}
			}
		}
		if (revealed.size > 0) {
			this.observeMentionCounts(revealed, BackgroundMentionCountMode.MERGE);
		}
		for (const entry of asArray(guild.members)) {
			const member = asRecord(entry);
			if (asString(asRecord(member?.user)?.id) === this.userId) {
				this.assertGuildCapacity(this.guildRoles, guildId);
				this.guildRoles.set(guildId, asIdSet(member?.roles));
			}
		}
	}

	private linkChannel(guildId: string, channelId: string): void {
		const previousGuildId = this.channelGuilds.get(channelId);
		if (previousGuildId === guildId) {
			return;
		}
		this.assertChannelCapacity(this.channelGuilds, channelId);
		if (previousGuildId !== undefined) {
			this.removeChannelFromGuild(previousGuildId, channelId);
		}
		this.channelGuilds.set(channelId, guildId);
		const channels = this.guildChannels.get(guildId);
		if (channels === undefined) {
			this.assertGuildCapacity(this.guildChannels, guildId);
			this.guildChannels.set(guildId, new Set([channelId]));
			return;
		}
		channels.add(channelId);
	}

	private storeGuildDetails(guild: Record<string, unknown>): void {
		const guildId = asString(guild.id);
		if (guildId === null) {
			return;
		}
		this.assertGuildCapacity(this.guildDetails, guildId);
		this.guildDetails.set(guildId, readGuildDetails(guild, this.guildDetails.get(guildId)));
	}

	private storeChannel(value: unknown): void {
		const channel = asRecord(value);
		const channelId = asString(channel?.id);
		if (channel === null || channelId === null) {
			return;
		}
		const guildId = asString(channel.guild_id);
		if (guildId !== null) {
			this.linkChannel(guildId, channelId);
		}
		this.revealChannel(channelId, channel);
	}

	private revealChannel(channelId: string, channel: Record<string, unknown>): void {
		const mentionCount = this.storeChannelDetails(channelId, channel);
		if (mentionCount > 0) {
			this.observeMentionCounts(new Map([[channelId, mentionCount]]), BackgroundMentionCountMode.MERGE);
		}
	}

	private storeChannelDetails(channelId: string, channel: Record<string, unknown>): number {
		this.assertChannelCapacity(this.channelDetails, channelId);
		const known = this.channelDetails.has(channelId);
		const prior = this.channelDetails.get(channelId);
		const details = readChannelDetails(channel);
		this.channelDetails.set(channelId, {
			name: 'name' in channel ? details.name : (prior?.name ?? null),
			parentId: 'parent_id' in channel ? details.parentId : (prior?.parentId ?? null),
			type: 'type' in channel ? details.type : (prior?.type ?? null),
			matureContent: 'nsfw_override' in channel ? details.matureContent : (prior?.matureContent ?? null),
			contentWarning: 'content_warning_level' in channel ? details.contentWarning : (prior?.contentWarning ?? null),
		});
		const lastMessageId = asString(channel.last_message_id);
		if (lastMessageId !== null) {
			this.recordLastMessage(channelId, lastMessageId);
		} else {
			this.refreshUnread(channelId);
		}
		return known ? 0 : (this.mentionCounts.get(channelId) ?? 0);
	}

	private recordLastMessage(channelId: string, messageId: string): void {
		const previous = this.lastMessageIds.get(channelId);
		if (previous === undefined || compareMessageIds(previous, messageId) < 0) {
			this.assertChannelCapacity(this.lastMessageIds, channelId);
			this.lastMessageIds.set(channelId, messageId);
		}
		if (this.newestMessageId === null || compareMessageIds(this.newestMessageId, messageId) < 0) {
			this.newestMessageId = messageId;
		}
		this.refreshUnread(channelId);
	}

	private recordAck(channelId: string, messageId: string): void {
		const previous = this.ackMessageIds.get(channelId);
		if (previous === undefined || compareMessageIds(previous, messageId) < 0) {
			this.assertChannelCapacity(this.ackMessageIds, channelId);
			this.ackMessageIds.set(channelId, messageId);
		}
		this.refreshUnread(channelId);
	}

	private refreshAllUnread(): void {
		this.unreadChannels.clear();
		for (const channelId of this.lastMessageIds.keys()) {
			this.refreshUnread(channelId);
		}
	}

	private refreshUnread(channelId: string): void {
		if (this.isChannelUnread(channelId)) {
			this.unreadChannels.add(channelId);
		} else {
			this.unreadChannels.delete(channelId);
		}
	}

	private isChannelUnread(channelId: string): boolean {
		const lastMessageId = this.lastMessageIds.get(channelId);
		const details = this.channelDetails.get(channelId);
		if (lastMessageId === undefined || details === undefined) {
			return false;
		}
		if (details.type !== null && !UNREAD_TRACKED_CHANNEL_TYPES.has(details.type)) {
			return false;
		}
		const guildId = this.channelGuilds.get(channelId) ?? null;
		if (this.isGuildOrChannelMuted(guildId, channelId)) {
			return false;
		}
		if (guildId !== null) {
			const unreadBadges = this.resolveUnreadBadges(guildId, channelId, details);
			if (unreadBadges === MessageNotifications.NO_MESSAGES || unreadBadges === MessageNotifications.ONLY_MENTIONS) {
				return false;
			}
		}
		const ackMessageId = this.ackMessageIds.get(channelId);
		if (ackMessageId !== undefined) {
			return compareMessageIds(ackMessageId, lastMessageId) < 0;
		}
		if (guildId === null) {
			return true;
		}
		const joinedAt = this.guildDetails.get(guildId)?.joinedAt ?? null;
		return joinedAt !== null && joinedAt < snowflakeTimestamp(lastMessageId);
	}

	private storeRelationship(value: unknown): void {
		const relationship = asRecord(value);
		const userId = asString(relationship?.id);
		if (relationship === null || userId === null) {
			return;
		}
		if (relationship.type !== RelationshipTypes.BLOCKED) {
			this.blockedUserIds.delete(userId);
			return;
		}
		if (!this.blockedUserIds.has(userId) && this.blockedUserIds.size >= MAX_BACKGROUND_READ_STATE_USERS) {
			throw new ReadStateCapacityExceeded('users', MAX_BACKGROUND_READ_STATE_USERS);
		}
		this.blockedUserIds.add(userId);
	}

	private storeGuildSettings(value: unknown): void {
		const parsed = readGuildSettings(value);
		if (parsed !== null) {
			this.assertGuildCapacity(this.guildSettings, parsed.guildId);
			this.guildSettings.set(parsed.guildId, parsed.settings);
		}
	}

	private applyMessageCreate(message: Record<string, unknown>): void {
		const channelId = asString(message.channel_id);
		if (channelId === null) {
			return;
		}
		const authorId = asString(asRecord(message.author)?.id);
		const messageId = asString(message.id);
		const guildId = asString(message.guild_id);
		if (guildId !== null) {
			this.linkChannel(guildId, channelId);
		}
		if (!this.channelDetails.has(channelId)) {
			this.revealChannel(channelId, {});
		}
		if (authorId === this.userId) {
			if (messageId !== null) {
				this.recordLastMessage(channelId, messageId);
				this.recordAck(channelId, messageId);
			}
			this.setMentionCount(channelId, 0);
			return;
		}
		if (messageId !== null) {
			this.recordLastMessage(channelId, messageId);
		}
		const muted = this.isGuildOrChannelMuted(guildId, channelId);
		const authorBlocked = authorId !== null && this.blockedUserIds.has(authorId);
		const mentioned = this.messageMentionsUser(message, guildId, muted, authorBlocked);
		const channel = this.channelDetails.get(channelId);
		const notification: BackgroundMessageNotification = {
			message: message as unknown as WireMessage,
			guildName: guildId === null ? null : (this.guildDetails.get(guildId)?.name ?? null),
			channelName: channel?.name ?? null,
			channelType: channel?.type ?? null,
			mentioned,
			gated: this.isChannelGated(channelId, guildId),
		};
		if (mentioned) {
			this.setMentionCount(channelId, (this.mentionCounts.get(channelId) ?? 0) + 1);
			if (messageId !== null) {
				this.observeMention?.(notification);
			}
		}
		if (authorId === null || authorBlocked || muted || !this.shouldNotify(message, guildId, channelId, mentioned)) {
			return;
		}
		this.observeMessageNotification(notification);
	}

	private isGuildOrChannelMuted(guildId: string | null, channelId: string): boolean {
		const settings = this.settingsFor(guildId ?? ME);
		const now = Date.now();
		if (isMuted(settings.mute, now)) {
			return true;
		}
		let currentId: string | null = channelId;
		for (let depth = 0; currentId !== null && depth < 3; depth += 1) {
			const override = settings.channelOverrides.get(currentId);
			if (override !== undefined && isMuted(override.mute, now)) {
				return true;
			}
			currentId = this.channelDetails.get(currentId)?.parentId ?? null;
		}
		return false;
	}

	private shouldNotify(
		message: Record<string, unknown>,
		guildId: string | null,
		channelId: string,
		mentioned: boolean,
	): boolean {
		if (asString(message.id) === null) {
			return false;
		}
		const flags = asInteger(message.flags) ?? 0;
		if ((flags & MessageFlags.SUPPRESS_NOTIFICATIONS) === MessageFlags.SUPPRESS_NOTIFICATIONS) {
			return false;
		}
		if (!NOTIFIABLE_MESSAGE_TYPES.has(asInteger(message.type) ?? MessageTypes.DEFAULT)) {
			return false;
		}
		const level = this.resolveMessageNotifications(guildId, channelId);
		if (level === MessageNotifications.NO_MESSAGES) {
			return false;
		}
		if (level === MessageNotifications.ALL_MESSAGES) {
			return true;
		}
		return mentioned || guildId === null;
	}

	private resolveUnreadBadges(guildId: string, channelId: string, details: ChannelDetails): number | null {
		const settings = this.settingsFor(guildId);
		const guildLevel = isExplicitNotificationLevel(settings.messageNotifications)
			? settings.messageNotifications
			: effectiveGuildMessageNotifications(this.guildDetails.get(guildId));
		if (guildLevel === MessageNotifications.ALL_MESSAGES || guildLevel === MessageNotifications.NO_MESSAGES) {
			return guildLevel;
		}
		if (!this.unreadBadgeCustomizationEnabled) {
			return null;
		}
		const targetId =
			details.type !== null && THREAD_CHANNEL_TYPES.has(details.type) && details.parentId !== null
				? details.parentId
				: channelId;
		const direct = settings.channelOverrides.get(targetId)?.unreadBadges;
		if (isExplicitNotificationLevel(direct)) {
			return direct;
		}
		const parentId = this.channelDetails.get(targetId)?.parentId ?? null;
		if (parentId !== null) {
			const parent = settings.channelOverrides.get(parentId)?.unreadBadges;
			if (isExplicitNotificationLevel(parent)) {
				return parent;
			}
		}
		return isExplicitNotificationLevel(settings.unreadBadges) ? settings.unreadBadges : null;
	}

	private resolveMessageNotifications(guildId: string | null, channelId: string): number {
		const settings = this.settingsFor(guildId ?? ME);
		const direct = settings.channelOverrides.get(channelId)?.messageNotifications;
		if (isExplicitNotificationLevel(direct)) {
			return direct;
		}
		const parentId = this.channelDetails.get(channelId)?.parentId ?? null;
		if (guildId !== null && parentId !== null) {
			const parent = settings.channelOverrides.get(parentId)?.messageNotifications;
			if (isExplicitNotificationLevel(parent)) {
				return parent;
			}
		}
		if (isExplicitNotificationLevel(settings.messageNotifications)) {
			return settings.messageNotifications;
		}
		if (guildId === null) {
			return MessageNotifications.ALL_MESSAGES;
		}
		return effectiveGuildMessageNotifications(this.guildDetails.get(guildId));
	}

	private messageMentionsUser(
		message: Record<string, unknown>,
		guildId: string | null,
		muted: boolean,
		authorBlocked: boolean,
	): boolean {
		const settings = this.settingsFor(guildId ?? ME);
		const mentionedIds = new Set<string>();
		for (const mention of asArray(message.mentions)) {
			const mentionedId = asString(asRecord(mention)?.id);
			if (mentionedId !== null) {
				mentionedIds.add(mentionedId);
			}
		}
		const mentionedRoles = asIdSet(message.mention_roles);
		const memberRoles = guildId === null ? null : (this.guildRoles.get(guildId) ?? null);
		let hasRoleMention = false;
		if (memberRoles !== null) {
			for (const roleId of mentionedRoles) {
				if (memberRoles.has(roleId)) {
					hasRoleMention = true;
					break;
				}
			}
		}
		return resolveReadStateMention({
			authorBlocked,
			hasUserMention: mentionedIds.has(this.userId),
			hasEveryoneMention: !settings.suppressEveryone && message.mention_everyone === true,
			hasRoleMention: !settings.suppressRoles && hasRoleMention,
			isPrivate: guildId === null,
			isMuted: muted,
		}).shouldMention;
	}

	private applyMessageAck(payload: Record<string, unknown>): void {
		const channelId = asString(payload.channel_id);
		if (channelId === null) {
			return;
		}
		const messageId = asString(payload.message_id);
		if (messageId !== null) {
			this.recordAck(channelId, messageId);
		}
		this.setMentionCount(channelId, asCount(payload.mention_count));
	}

	private applyChannelDelete(payload: Record<string, unknown>): void {
		const channelId = asString(payload.id);
		if (channelId === null) {
			return;
		}
		this.forgetChannel(channelId);
		this.setMentionCount(channelId, 0);
	}

	private applyGuildDelete(payload: Record<string, unknown>): void {
		const guildId = asString(payload.id);
		if (guildId === null || payload.unavailable === true) {
			return;
		}
		const channels = this.guildChannels.get(guildId) ?? new Set<string>();
		const cleared = new Map<string, number>();
		for (const channelId of channels) {
			this.channelGuilds.delete(channelId);
			this.channelDetails.delete(channelId);
			this.mentionCounts.delete(channelId);
			this.forgetChannelReadState(channelId);
			cleared.set(channelId, 0);
		}
		this.guildChannels.delete(guildId);
		this.guildSettings.delete(guildId);
		this.guildRoles.delete(guildId);
		this.guildDetails.delete(guildId);
		if (cleared.size > 0) {
			this.observeMentionCounts(cleared, BackgroundMentionCountMode.MERGE);
		}
	}

	private forgetChannelReadState(channelId: string): void {
		this.lastMessageIds.delete(channelId);
		this.ackMessageIds.delete(channelId);
		this.unreadChannels.delete(channelId);
	}

	private forgetChannel(channelId: string): void {
		const guildId = this.channelGuilds.get(channelId);
		this.forgetChannelReadState(channelId);
		this.channelGuilds.delete(channelId);
		this.channelDetails.delete(channelId);
		if (guildId !== undefined) {
			this.removeChannelFromGuild(guildId, channelId);
		}
	}

	private removeChannelFromGuild(guildId: string, channelId: string): void {
		const channels = this.guildChannels.get(guildId);
		if (channels === undefined) {
			return;
		}
		channels.delete(channelId);
		if (channels.size === 0) {
			this.guildChannels.delete(guildId);
		}
	}

	private settingsFor(guildId: string): GuildNotificationSettings {
		return this.guildSettings.get(guildId) ?? DEFAULT_GUILD_SETTINGS;
	}

	private setMentionCount(channelId: string, mentionCount: number): void {
		if ((this.mentionCounts.get(channelId) ?? 0) === mentionCount) {
			return;
		}
		if (mentionCount > 0) {
			this.storeMentionCount(channelId, mentionCount);
		} else {
			this.mentionCounts.delete(channelId);
		}
		if (mentionCount > 0 && !this.channelDetails.has(channelId)) {
			return;
		}
		this.observeMentionCounts(new Map([[channelId, mentionCount]]), BackgroundMentionCountMode.MERGE);
	}

	private storeMentionCount(channelId: string, mentionCount: number): void {
		this.assertChannelCapacity(this.mentionCounts, channelId);
		this.mentionCounts.set(channelId, mentionCount);
	}

	private assertChannelCapacity(map: ReadonlyMap<string, unknown>, channelId: string): void {
		if (!map.has(channelId) && map.size >= MAX_BACKGROUND_READ_STATE_CHANNELS) {
			throw new ReadStateCapacityExceeded('channels', MAX_BACKGROUND_READ_STATE_CHANNELS);
		}
	}

	private assertGuildCapacity(map: ReadonlyMap<string, unknown>, guildId: string): void {
		if (!map.has(guildId) && map.size >= MAX_BACKGROUND_READ_STATE_GUILDS) {
			throw new ReadStateCapacityExceeded('guilds', MAX_BACKGROUND_READ_STATE_GUILDS);
		}
	}
}
