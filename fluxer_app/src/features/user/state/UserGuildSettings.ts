// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {Logger} from '@app/features/platform/utils/AppLogger';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import {resolveUnreadInboxVisibility} from '@app/features/user/state/UnreadInboxVisibility';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import type {ChannelId, GuildId} from '@fluxer/schema/src/branded/WireIds';
import {action, computed, makeObservable, observable, reaction} from 'mobx';

const logger = new Logger('UserGuildSettings');
const PRIVATE_CHANNEL_SENTINEL: string = ME;
const isGuildContextId = (guildId: string): boolean =>
	guildId !== PRIVATE_CHANNEL_SENTINEL && guildId !== FAVORITES_GUILD_ID;

export interface ChannelOverride {
	channel_id: string;
	collapsed: boolean;
	message_notifications: number;
	muted: boolean;
	mute_config?: {
		selected_time_window?: number;
		end_time?: string;
	} | null;
	unread_badges?: number | null;
}

interface GuildSettings {
	guild_id: string;
	suppress_everyone: boolean;
	suppress_roles: boolean;
	mute_scheduled_events: boolean;
	mobile_push: boolean;
	muted: boolean;
	message_notifications: number;
	channel_overrides: Record<ChannelId, ChannelOverride>;
	mute_config?: {
		selected_time_window?: number;
		end_time?: string;
	} | null;
	version?: number;
	hide_muted_channels?: boolean;
	unread_badges?: number | null;
}

export interface GatewayGuildSettings {
	guild_id: string;
	suppress_everyone?: boolean;
	suppress_roles?: boolean;
	mute_scheduled_events?: boolean;
	mobile_push?: boolean;
	muted?: boolean;
	message_notifications?: number;
	channel_overrides?: Array<ChannelOverride> | Record<ChannelId, ChannelOverride> | null;
	mute_config?: {
		selected_time_window?: number;
		end_time?: string;
	} | null;
	version?: number;
	hide_muted_channels?: boolean;
	unread_badges?: number | null;
}

const DEFAULT_GUILD_SETTINGS: Omit<GuildSettings, 'guild_id'> = {
	suppress_everyone: false,
	suppress_roles: false,
	mute_scheduled_events: false,
	mobile_push: true,
	muted: false,
	message_notifications: MessageNotifications.ALL_MESSAGES,
	channel_overrides: {},
	mute_config: null,
	version: -1,
	hide_muted_channels: false,
	unread_badges: null,
};
const DEFAULT_CHANNEL_OVERRIDE = (channelId: string): ChannelOverride => ({
	channel_id: channelId,
	collapsed: false,
	message_notifications: MessageNotifications.INHERIT,
	muted: false,
	mute_config: null,
	unread_badges: null,
});

function isExplicitNotificationLevel(value: number | null | undefined): value is number {
	return value != null && value !== MessageNotifications.INHERIT && value !== MessageNotifications.NULL;
}

function parseEndTime(end_time?: string): number | null {
	if (end_time == null) return null;
	const ms = Date.parse(end_time);
	return Number.isFinite(ms) ? ms : null;
}

function isMuted(config: {
	muted?: boolean;
	mute_config?: {
		end_time?: string;
	} | null;
}): boolean {
	if (config.muted !== true) return false;
	const end = config.mute_config?.end_time;
	if (end == null) return true;
	const endMs = parseEndTime(end);
	return endMs == null ? true : endMs > Date.now();
}

interface UnreadInboxReadState {
	hasUnread: boolean;
	hasMentions: boolean;
}

class UserGuildSettings {
	private readonly guildSettings = observable.map<GuildId, GuildSettings>();
	private readonly mutedChannels = observable.map<GuildId, Set<ChannelId>>();
	updateCounter = 0;
	private readonly pendingGuildUpdates = new Set<GuildId | null>();
	private readonly guildMuteTimers = new Map<GuildId, NodeJS.Timeout>();
	private readonly channelMuteTimers = new Map<string, NodeJS.Timeout>();

	constructor() {
		makeObservable<this, 'notifyChange'>(this, {
			updateCounter: observable,
			version: computed,
			updateGuildSettings: action,
			updateChannelOverride: action,
			updateChannelOverrides: action,
			handleConnectionOpen: action,
			notifyChange: action,
		});
	}

	get version(): number {
		return this.updateCounter;
	}

	private storageKeyFor(guildId: string | null): string {
		return guildId ?? PRIVATE_CHANNEL_SENTINEL;
	}

	private notifyChange(): void {
		this.updateCounter++;
	}

	private markGuildUpdated(guildId: string | null): void {
		this.pendingGuildUpdates.add(guildId as GuildId | null);
	}

	private getDefaultSettings(guildId: string): GuildSettings {
		return {
			...DEFAULT_GUILD_SETTINGS,
			guild_id: guildId,
			message_notifications: isGuildContextId(guildId)
				? MessageNotifications.INHERIT
				: DEFAULT_GUILD_SETTINGS.message_notifications,
		};
	}

	private getStoredSettings(guildId: string | null): GuildSettings | undefined {
		const key = this.storageKeyFor(guildId);
		return this.guildSettings.get(key as GuildId);
	}

	private ensureGuildSettings(guildId: string | null): GuildSettings {
		const key = this.storageKeyFor(guildId);
		let settings = this.getStoredSettings(key);
		if (settings == null) {
			settings = this.getDefaultSettings(key);
			this.guildSettings.set(key as GuildId, settings);
		}
		return settings;
	}

	getGuildSettings(guildId: string | null): GuildSettings {
		const key = this.storageKeyFor(guildId);
		return this.getStoredSettings(key) ?? this.getDefaultSettings(key);
	}

	getSettings(guildId: string | null): GuildSettings {
		return this.getGuildSettings(guildId);
	}

	private normalizeOverride(channelId: string, override?: Partial<ChannelOverride> | null): ChannelOverride {
		return {
			...DEFAULT_CHANNEL_OVERRIDE(channelId),
			...(override ?? {}),
			channel_id: channelId,
		};
	}

	private normalizeChannelOverrides(
		overrides?: Array<ChannelOverride> | Record<ChannelId, ChannelOverride> | null,
	): Record<ChannelId, ChannelOverride> {
		if (overrides == null) return {};
		const result: Record<string, ChannelOverride> = {};
		if (Array.isArray(overrides)) {
			for (const o of overrides) {
				result[o.channel_id] = this.normalizeOverride(o.channel_id, o);
			}
			return result as Record<ChannelId, ChannelOverride>;
		}
		for (const [id, o] of Object.entries(overrides)) {
			result[id] = this.normalizeOverride(id, o);
		}
		return result as Record<ChannelId, ChannelOverride>;
	}

	private updateMutedChannelsCache(guildId: string, settings: GuildSettings): void {
		const set = new Set<ChannelId>();
		for (const [channelId, override] of Object.entries(settings.channel_overrides)) {
			if (isMuted(override)) {
				set.add(channelId as ChannelId);
			}
		}
		if (set.size > 0) {
			this.mutedChannels.set(guildId as GuildId, set);
		} else {
			this.mutedChannels.delete(guildId as GuildId);
		}
	}

	private channelTimerKey(guildId: string, channelId: string): string {
		return `${guildId}:${channelId}`;
	}

	private clearAllTimers(): void {
		for (const t of this.guildMuteTimers.values()) clearTimeout(t);
		for (const t of this.channelMuteTimers.values()) clearTimeout(t);
		this.guildMuteTimers.clear();
		this.channelMuteTimers.clear();
	}

	private clearGuildMuteTimer(guildId: string): void {
		const t = this.guildMuteTimers.get(guildId as GuildId);
		if (t) {
			clearTimeout(t);
			this.guildMuteTimers.delete(guildId as GuildId);
		}
	}

	private clearGuildChannelTimers(guildId: string): void {
		const prefix = `${guildId}:`;
		for (const key of Array.from(this.channelMuteTimers.keys())) {
			if (key.startsWith(prefix)) {
				clearTimeout(this.channelMuteTimers.get(key)!);
				this.channelMuteTimers.delete(key);
			}
		}
	}

	private setupMuteTimers(guildId: string, settings: GuildSettings): void {
		this.clearGuildMuteTimer(guildId);
		this.clearGuildChannelTimers(guildId);
		if (settings.muted && settings.mute_config?.end_time) {
			const endMs = parseEndTime(settings.mute_config.end_time);
			if (endMs != null) {
				const delay = endMs - Date.now();
				if (delay > 0) {
					this.guildMuteTimers.set(
						guildId as GuildId,
						setTimeout(() => {
							this.updateGuildSettings(guildId, {muted: false, mute_config: null});
							logger.debug(`Guild mute expired`, {guildId});
						}, delay),
					);
				}
			}
		}
		for (const [channelId, override] of Object.entries(settings.channel_overrides)) {
			if (override.muted && override.mute_config?.end_time) {
				const endMs = parseEndTime(override.mute_config.end_time);
				if (endMs == null) continue;
				const delay = endMs - Date.now();
				if (delay > 0) {
					this.channelMuteTimers.set(
						this.channelTimerKey(guildId, channelId),
						setTimeout(() => {
							this.updateChannelOverride(guildId, channelId, {muted: false, mute_config: null});
							logger.debug(`Channel mute expired`, {guildId, channelId});
						}, delay),
					);
				}
			}
		}
	}

	private sanitizeGuildMute(settings: GuildSettings): GuildSettings {
		if (!settings.muted) return settings;
		const endMs = parseEndTime(settings.mute_config?.end_time);
		if (endMs != null && endMs <= Date.now()) {
			return {...settings, muted: false, mute_config: null};
		}
		return settings;
	}

	private sanitizeChannelMutes(settings: GuildSettings): GuildSettings {
		let changed = false;
		const overrides: Record<string, ChannelOverride> = {...settings.channel_overrides};
		for (const [channelId, override] of Object.entries(overrides)) {
			if (!override.muted) continue;
			const endMs = parseEndTime(override.mute_config?.end_time);
			if (endMs != null && endMs <= Date.now()) {
				overrides[channelId] = {...override, muted: false, mute_config: null};
				changed = true;
			}
		}
		return changed ? {...settings, channel_overrides: overrides as Record<ChannelId, ChannelOverride>} : settings;
	}

	updateGuildSettings(guildId: string | null, updates: Partial<GatewayGuildSettings>): void {
		const key = this.storageKeyFor(guildId);
		const existing = this.ensureGuildSettings(key);
		const overridesInput = 'channel_overrides' in updates ? updates.channel_overrides : existing.channel_overrides;
		const merged: GuildSettings = {
			...existing,
			...updates,
			guild_id: key,
			channel_overrides: this.normalizeChannelOverrides(overridesInput),
		};
		const sanitized = this.sanitizeChannelMutes(this.sanitizeGuildMute(merged));
		this.guildSettings.set(key as GuildId, sanitized);
		this.updateMutedChannelsCache(key, sanitized);
		this.setupMuteTimers(key, sanitized);
		this.markGuildUpdated(guildId);
		this.notifyChange();
	}

	updateChannelOverride(guildId: string | null, channelId: string, updates: Partial<ChannelOverride>): void {
		const key = this.storageKeyFor(guildId);
		const settings = this.ensureGuildSettings(key);
		const existing = settings.channel_overrides[channelId as ChannelId] ?? DEFAULT_CHANNEL_OVERRIDE(channelId);
		const updated = {...existing, ...updates, channel_id: channelId};
		const next: GuildSettings = {
			...settings,
			channel_overrides: {...settings.channel_overrides, [channelId]: updated} as Record<ChannelId, ChannelOverride>,
		};
		const sanitized = this.sanitizeChannelMutes(this.sanitizeGuildMute(next));
		this.guildSettings.set(key as GuildId, sanitized);
		this.updateMutedChannelsCache(key, sanitized);
		this.setupMuteTimers(key, sanitized);
		this.markGuildUpdated(guildId);
		this.notifyChange();
	}

	updateChannelOverrides(guildId: string | null, overrides: Record<ChannelId, Partial<ChannelOverride>>): void {
		const key = this.storageKeyFor(guildId);
		const settings = this.ensureGuildSettings(key);
		const nextOverrides: Record<string, ChannelOverride> = {...settings.channel_overrides};
		for (const [channelId, updates] of Object.entries(overrides)) {
			const existing = nextOverrides[channelId] ?? DEFAULT_CHANNEL_OVERRIDE(channelId);
			nextOverrides[channelId] = {...existing, ...updates, channel_id: channelId};
		}
		const next: GuildSettings = {...settings, channel_overrides: nextOverrides as Record<ChannelId, ChannelOverride>};
		const sanitized = this.sanitizeChannelMutes(this.sanitizeGuildMute(next));
		this.guildSettings.set(key as GuildId, sanitized);
		this.updateMutedChannelsCache(key, sanitized);
		this.setupMuteTimers(key, sanitized);
		this.markGuildUpdated(guildId);
		this.notifyChange();
	}

	isSuppressEveryoneEnabled(guildId: string | null): boolean {
		return guildId != null && this.getGuildSettings(guildId).suppress_everyone;
	}

	isSuppressRolesEnabled(guildId: string | null): boolean {
		return guildId != null && this.getGuildSettings(guildId).suppress_roles;
	}

	isMuteScheduledEventsEnabled(guildId: string | null): boolean {
		return guildId != null && this.getGuildSettings(guildId).mute_scheduled_events;
	}

	isMobilePushEnabled(guildId: string | null): boolean {
		return guildId == null || this.getGuildSettings(guildId).mobile_push;
	}

	isMuted(guildId: string | null): boolean {
		return isMuted(this.getGuildSettings(guildId));
	}

	getGuildMessageNotifications(guildId: string | null): number {
		if (guildId == null) {
			const privateSettings = this.getGuildSettings(guildId);
			return privateSettings.message_notifications === MessageNotifications.INHERIT ||
				privateSettings.message_notifications === MessageNotifications.NULL
				? MessageNotifications.ALL_MESSAGES
				: privateSettings.message_notifications;
		}
		const settings = this.getGuildSettings(guildId);
		if (
			settings.message_notifications === MessageNotifications.INHERIT ||
			settings.message_notifications === MessageNotifications.NULL
		) {
			const guild = Guilds.getGuild(guildId);
			return guild?.effectiveMessageNotifications ?? MessageNotifications.ALL_MESSAGES;
		}
		return settings.message_notifications;
	}

	getGuildIds(): Array<GuildId> {
		return Array.from(this.guildSettings.keys());
	}

	getStoredGuildMessageNotifications(guildId: string): number {
		return this.getGuildSettings(guildId).message_notifications;
	}

	getChannelOverrides(guildId: string | null): Record<ChannelId, ChannelOverride> {
		return this.getGuildSettings(guildId).channel_overrides;
	}

	getChannelOverride(guildId: string | null, channelId: string): ChannelOverride | undefined {
		return this.getGuildSettings(guildId).channel_overrides[channelId as ChannelId];
	}

	getChannelMessageNotifications(guildId: string | null, channelId: string): number {
		return (
			this.getGuildSettings(guildId).channel_overrides[channelId as ChannelId]?.message_notifications ??
			MessageNotifications.NULL
		);
	}

	isChannelMuted(guildId: string | null, channelId: string): boolean {
		const override = this.getGuildSettings(guildId).channel_overrides[channelId as ChannelId];
		return override != null && isMuted(override);
	}

	isCategoryMuted(guildId: string | null, channelId: string): boolean {
		if (guildId == null) return false;
		const channel = Channels.getChannel(channelId);
		return channel?.parentId != null && this.isChannelMuted(guildId, channel.parentId);
	}

	isGuildOrCategoryOrChannelMuted(guildId: string | null, channelId: string): boolean {
		return this.isMuted(guildId) || this.isCategoryMuted(guildId, channelId) || this.isChannelMuted(guildId, channelId);
	}

	isGuildOrChannelMuted(guildId: string | null, channelId: string): boolean {
		return this.isMuted(guildId) || this.isChannelMuted(guildId, channelId);
	}

	isCategoryOrChannelMuted(guildId: string | null, channelId: string): boolean {
		return this.isCategoryMuted(guildId, channelId) || this.isChannelMuted(guildId, channelId);
	}

	getMutedChannels(guildId: string): Set<ChannelId> {
		return new Set(this.mutedChannels.get(guildId as GuildId) ?? []);
	}

	isChannelCollapsed(guildId: string | null, channelId: string): boolean {
		return this.getChannelOverride(guildId, channelId)?.collapsed ?? false;
	}

	resolvedMessageNotifications(channel: {id: string; guildId?: string; parentId?: string; type: number}): number {
		const guildId = channel.guildId ?? null;
		const direct = this.getChannelMessageNotifications(guildId, channel.id);
		if (direct !== MessageNotifications.NULL && direct !== MessageNotifications.INHERIT) return direct;
		if (channel.parentId != null && guildId != null) {
			const parent = this.getChannelMessageNotifications(guildId, channel.parentId);
			if (parent !== MessageNotifications.NULL && parent !== MessageNotifications.INHERIT) return parent;
		}
		return this.getGuildMessageNotifications(guildId);
	}

	getChannelUnreadBadgesLevel(guildId: string | null, channelId: string): number | null {
		if (!AdvancedSettings.unreadBadgeCustomizationEnabled) return null;
		const value = this.getGuildSettings(guildId).channel_overrides[channelId as ChannelId]?.unread_badges;
		return value == null ? null : value;
	}

	getGuildUnreadBadgesLevel(guildId: string | null): number | null {
		if (!AdvancedSettings.unreadBadgeCustomizationEnabled) return null;
		const value = this.getGuildSettings(guildId).unread_badges;
		return value == null ? null : value;
	}

	private getLockedCommunityUnreadBadgesLevel(guildId: string | null): number | null {
		if (guildId == null) return null;
		if (this.isMuted(guildId)) return MessageNotifications.NO_MESSAGES;
		const notificationLevel = this.getGuildMessageNotifications(guildId);
		if (notificationLevel === MessageNotifications.ALL_MESSAGES) return MessageNotifications.ALL_MESSAGES;
		if (notificationLevel === MessageNotifications.NO_MESSAGES) return MessageNotifications.NO_MESSAGES;
		return null;
	}

	getCommunityUnreadBadgesLevel(guildId: string | null): number | null {
		const lockedLevel = this.getLockedCommunityUnreadBadgesLevel(guildId);
		if (lockedLevel != null) return lockedLevel;
		const guildLevel = this.getGuildUnreadBadgesLevel(guildId);
		if (isExplicitNotificationLevel(guildLevel)) return guildLevel;
		return null;
	}

	resolvedUnreadBadgesLevel(channel: {id: string; guildId?: string; parentId?: string; type: number}): number | null {
		const guildId = channel.guildId ?? null;
		const direct = this.getChannelUnreadBadgesLevel(guildId, channel.id);
		if (isExplicitNotificationLevel(direct)) return direct;
		if (channel.parentId != null && guildId != null) {
			const parent = this.getChannelUnreadBadgesLevel(guildId, channel.parentId);
			if (isExplicitNotificationLevel(parent)) return parent;
		}
		const guildLevel = this.getGuildUnreadBadgesLevel(guildId);
		if (isExplicitNotificationLevel(guildLevel)) return guildLevel;
		return null;
	}

	resolvedGuildUnreadBadgesLevel(channel: {
		id: string;
		guildId?: string;
		parentId?: string;
		type: number;
	}): number | null {
		const guildId = channel.guildId ?? null;
		const lockedLevel = this.getLockedCommunityUnreadBadgesLevel(guildId);
		if (lockedLevel != null) return lockedLevel;
		return this.resolvedUnreadBadgesLevel(channel);
	}

	shouldShowChannelInUnreadInbox(
		channel: {id: string; guildId?: string; parentId?: string; type: number},
		readState: UnreadInboxReadState,
	): boolean {
		const guildId = channel.guildId ?? null;
		return resolveUnreadInboxVisibility({
			unreadBadgesLevel: this.resolvedUnreadBadgesLevel(channel),
			isMuted: this.isGuildOrCategoryOrChannelMuted(guildId, channel.id),
			hasUnread: readState.hasUnread,
			hasMentions: readState.hasMentions,
		});
	}

	resolveUnreadSetting(channel: {id: string; guildId?: string; parentId?: string; type: number}): string {
		const level = this.resolvedMessageNotifications(channel);
		return level === MessageNotifications.ALL_MESSAGES ? 'all_messages' : 'only_mentions';
	}

	allowNoMessages(channel: {id: string; guildId?: string; parentId?: string; type: number}): boolean {
		return (
			this.isGuildOrChannelMuted(channel.guildId ?? null, channel.id) ||
			this.resolvedMessageNotifications(channel) === MessageNotifications.NO_MESSAGES
		);
	}

	allowAllMessages(channel: {id: string; guildId?: string; parentId?: string; type: number}): boolean {
		return (
			!this.isGuildOrChannelMuted(channel.guildId ?? null, channel.id) &&
			this.resolvedMessageNotifications(channel) === MessageNotifications.ALL_MESSAGES
		);
	}

	handleConnectionOpen(userGuildSettings: Array<GatewayGuildSettings>): void {
		this.clearAllTimers();
		this.guildSettings.clear();
		this.mutedChannels.clear();
		for (const settings of userGuildSettings) {
			this.updateGuildSettings(settings.guild_id, settings);
		}
		this.notifyChange();
	}

	handleGuildSettingsUpdate(action: {guildId: string; settings: Partial<GatewayGuildSettings>}): void {
		this.updateGuildSettings(action.guildId, action.settings);
	}

	handleChannelSettingsUpdate(action: {guildId: string; channelId: string; settings: Partial<ChannelOverride>}): void {
		this.updateChannelOverride(action.guildId, action.channelId, action.settings);
	}

	handleBulkChannelSettingsUpdate(action: {
		guildId: string;
		overrides: Record<ChannelId, Partial<ChannelOverride>>;
	}): void {
		this.updateChannelOverrides(action.guildId, action.overrides);
	}

	handleUserGuildSettingsUpdate(data: GatewayGuildSettings): void {
		this.updateGuildSettings(data.guild_id, data);
	}

	handleGuildCreate(data: {id: string}): void {
		this.ensureGuildSettings(data.id);
		this.markGuildUpdated(data.id);
		this.notifyChange();
	}

	consumePendingGuildUpdates(): Array<GuildId | null> {
		if (this.pendingGuildUpdates.size === 0) return [];
		const ids = Array.from(this.pendingGuildUpdates);
		this.pendingGuildUpdates.clear();
		return ids;
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new UserGuildSettings();
