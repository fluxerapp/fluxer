// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BackgroundMentionCountMode,
	type BackgroundSnapshotSink,
	type BackgroundSnapshotSinkConfig,
	type ObserveMentionCounts,
	type ObserveMessageNotification,
} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {resolveReadStateMention} from '@app/features/read_state/state/read_states/ReadStateMentionMachine';
import {ME} from '@fluxer/constants/src/AppConstants';
import {MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {LARGE_GUILD_THRESHOLD} from '@fluxer/constants/src/GatewayConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
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
}

interface GuildNotificationSettings {
	readonly mute: MuteConfig;
	readonly messageNotifications: number;
	readonly suppressEveryone: boolean;
	readonly suppressRoles: boolean;
	readonly channelOverrides: ReadonlyMap<string, ChannelNotificationOverride>;
}

interface GuildDetails {
	readonly name: string | null;
	readonly defaultMessageNotifications: number;
	readonly memberCount: number | null;
	readonly largeFeature: boolean;
}

interface ChannelDetails {
	readonly name: string | null;
	readonly parentId: string | null;
	readonly type: number | null;
}

const UNMUTED: MuteConfig = {muted: false, mutedUntil: null};

const DEFAULT_GUILD_SETTINGS: GuildNotificationSettings = {
	mute: UNMUTED,
	messageNotifications: MessageNotifications.NULL,
	suppressEveryone: false,
	suppressRoles: false,
	channelOverrides: new Map(),
};

const NOTIFIABLE_MESSAGE_TYPES: ReadonlySet<number> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);

export class BackgroundReadStateCapacityError extends Error {
	constructor(kind: 'channels' | 'guilds' | 'users', capacity: number) {
		super(`Background read state exceeded its ${capacity} tracked ${kind} bound`);
		this.name = 'BackgroundReadStateCapacityError';
	}
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

function isExplicitNotificationLevel(level: number | undefined): level is number {
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
			channelOverrides: readChannelOverrides(source.channel_overrides),
		},
	};
}

function readGuildDetails(source: Record<string, unknown>, prior: GuildDetails | undefined): GuildDetails {
	const properties = asRecord(source.properties) ?? source;
	const features = asIdSet(properties.features);
	const memberCount = asInteger(source.member_count) ?? asInteger(properties.member_count);
	return {
		name: asString(properties.name) ?? prior?.name ?? null,
		defaultMessageNotifications:
			asInteger(properties.default_message_notifications) ??
			prior?.defaultMessageNotifications ??
			MessageNotifications.ALL_MESSAGES,
		memberCount: memberCount ?? prior?.memberCount ?? null,
		largeFeature: features.has(GuildFeatures.LARGE_GUILD_OVERRIDE) || features.has(GuildFeatures.VERY_LARGE_GUILD),
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

function readChannelDetails(source: Record<string, unknown>): ChannelDetails {
	return {
		name: asString(source.name),
		parentId: asString(source.parent_id),
		type: asInteger(source.type),
	};
}

export class BackgroundReadStateReducer implements BackgroundSnapshotSink {
	private readonly userId: string;
	private readonly observeMentionCounts: ObserveMentionCounts;
	private readonly observeMessageNotification: ObserveMessageNotification;
	private readonly mentionCounts = new Map<string, number>();
	private readonly channelGuilds = new Map<string, string>();
	private readonly guildChannels = new Map<string, Set<string>>();
	private readonly guildSettings = new Map<string, GuildNotificationSettings>();
	private readonly guildRoles = new Map<string, ReadonlySet<string>>();
	private readonly guildDetails = new Map<string, GuildDetails>();
	private readonly channelDetails = new Map<string, ChannelDetails>();
	private readonly blockedUserIds = new Set<string>();

	constructor({userId, observeMentionCounts, observeMessageNotification}: BackgroundSnapshotSinkConfig) {
		this.userId = userId;
		this.observeMentionCounts = observeMentionCounts;
		this.observeMessageNotification = observeMessageNotification;
	}

	applyReady(data: unknown): void {
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
			const mentionCount = asCount(readState?.mention_count);
			if (channelId !== null && mentionCount > 0) {
				this.storeMentionCount(channelId, mentionCount);
			}
		}
		const visible = new Map<string, number>();
		for (const [channelId, mentionCount] of this.mentionCounts) {
			if (this.channelDetails.has(channelId)) {
				visible.set(channelId, mentionCount);
			}
		}
		this.observeMentionCounts(visible, BackgroundMentionCountMode.REPLACE);
	}

	applyDispatch(type: string, data: unknown): void {
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
			case 'GUILD_DELETE':
				this.applyGuildDelete(payload);
				return;
			case 'USER_GUILD_SETTINGS_UPDATE':
				this.storeGuildSettings(payload);
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
		this.channelDetails.set(channelId, readChannelDetails(channel));
		return known ? 0 : (this.mentionCounts.get(channelId) ?? 0);
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
			throw new BackgroundReadStateCapacityError('users', MAX_BACKGROUND_READ_STATE_USERS);
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
		if (authorId === this.userId) {
			this.setMentionCount(channelId, 0);
			return;
		}
		const guildId = asString(message.guild_id);
		if (guildId !== null) {
			this.linkChannel(guildId, channelId);
		}
		if (!this.channelDetails.has(channelId)) {
			this.revealChannel(channelId, {});
		}
		const muted = this.isGuildOrChannelMuted(guildId, channelId);
		const authorBlocked = authorId !== null && this.blockedUserIds.has(authorId);
		const mentioned = this.messageMentionsUser(message, guildId, muted, authorBlocked);
		if (mentioned) {
			this.setMentionCount(channelId, (this.mentionCounts.get(channelId) ?? 0) + 1);
		}
		if (authorId === null || authorBlocked || muted || !this.shouldNotify(message, guildId, channelId, mentioned)) {
			return;
		}
		const channel = this.channelDetails.get(channelId);
		this.observeMessageNotification({
			message: message as unknown as WireMessage,
			guildName: guildId === null ? null : (this.guildDetails.get(guildId)?.name ?? null),
			channelName: channel?.name ?? null,
			channelType: channel?.type ?? null,
		});
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
		if (channelId !== null) {
			this.setMentionCount(channelId, asCount(payload.mention_count));
		}
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

	private forgetChannel(channelId: string): void {
		const guildId = this.channelGuilds.get(channelId);
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
			throw new BackgroundReadStateCapacityError('channels', MAX_BACKGROUND_READ_STATE_CHANNELS);
		}
	}

	private assertGuildCapacity(map: ReadonlyMap<string, unknown>, guildId: string): void {
		if (!map.has(guildId) && map.size >= MAX_BACKGROUND_READ_STATE_GUILDS) {
			throw new BackgroundReadStateCapacityError('guilds', MAX_BACKGROUND_READ_STATE_GUILDS);
		}
	}
}
