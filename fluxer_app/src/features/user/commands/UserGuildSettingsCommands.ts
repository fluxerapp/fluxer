// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {UserGuildSettingsPartial} from '@app/features/user/models/UserGuildSettings';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import type {ChannelOverride, GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {ME} from '@fluxer/constants/src/AppConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';

const logger = new Logger('UserGuildSettingsCommands');
const pendingUpdates: Map<string, NodeJS.Timeout> = new Map();
const pendingPayloads: Map<string, UserGuildSettingsPartial> = new Map();
const BULK_PERSIST_DELAY_MS = 350;

let bulkPersistenceQueue: Promise<void> = Promise.resolve();

interface PersistenceOptions {
	persistImmediately?: boolean;
}

export type UserGuildSettingsUpdate = UserGuildSettingsPartial;
export type ChannelOverrideUpdate = Partial<ChannelOverride> | null;

const mergePayloads = (a: UserGuildSettingsPartial, b: UserGuildSettingsPartial): UserGuildSettingsPartial => ({
	...a,
	...b,
});
const getUpdateKey = (guildId: string | null): string => guildId ?? ME;
const addPendingPayload = (key: string, updates: UserGuildSettingsPartial): void => {
	const currentPending = pendingPayloads.get(key) ?? {};
	const mergedUpdates = mergePayloads(currentPending, updates);
	pendingPayloads.set(key, mergedUpdates);
};
const clearPendingTimer = (key: string): void => {
	const pendingTimeout = pendingUpdates.get(key);
	if (pendingTimeout) {
		clearTimeout(pendingTimeout);
		pendingUpdates.delete(key);
	}
};
const settingsEndpoint = (guildId: string | null): string =>
	guildId == null ? Endpoints.USER_GUILD_SETTINGS_ME : Endpoints.USER_GUILD_SETTINGS(guildId);
const defaultChannelOverride = (
	channelId: string,
	currentOverride?: ChannelOverride,
	override?: Partial<ChannelOverride>,
): ChannelOverride => ({
	channel_id: channelId,
	collapsed: false,
	message_notifications: MessageNotifications.INHERIT,
	muted: false,
	mute_config: null,
	...currentOverride,
	...(override ?? {}),
});
const hasChannelOverrides = (overrides: Record<string, ChannelOverride>): boolean => Object.keys(overrides).length > 0;
const commitChannelOverrides = (
	guildId: string | null,
	overrides: Record<string, ChannelOverride>,
	options?: PersistenceOptions,
): void => {
	const hasOverrides = hasChannelOverrides(overrides);
	UserGuildSettings.updateGuildSettings(guildId, {
		channel_overrides: hasOverrides ? overrides : {},
	} as Partial<GatewayGuildSettings>);
	scheduleUpdate(
		guildId,
		{
			channel_overrides: hasOverrides ? overrides : null,
		},
		options,
	);
};
const flushPendingUpdate = async (guildId: string | null, key = getUpdateKey(guildId)): Promise<void> => {
	pendingUpdates.delete(key);
	const payload = pendingPayloads.get(key);
	pendingPayloads.delete(key);
	if (!payload) {
		return;
	}
	try {
		logger.debug(`Persisting settings update for guild ${key}`, payload);
		await http.patch(settingsEndpoint(guildId), {
			body: payload,
		});
		logger.debug(`Successfully updated settings for guild ${key}`);
	} catch (error) {
		logger.error(`Failed to update settings for guild ${key}:`, error);
	}
};
const enqueueBulkFlush = (guildId: string | null, key = getUpdateKey(guildId)): void => {
	clearPendingTimer(key);
	bulkPersistenceQueue = bulkPersistenceQueue
		.catch(() => undefined)
		.then(() => flushPendingUpdate(guildId, key))
		.then(() => new Promise<void>((resolve) => setTimeout(resolve, BULK_PERSIST_DELAY_MS)));
};
const scheduleUpdate = (guildId: string | null, updates: UserGuildSettingsPartial, options?: PersistenceOptions) => {
	const key = getUpdateKey(guildId);
	addPendingPayload(key, updates);
	if (options?.persistImmediately) {
		if (pendingUpdates.has(key)) {
			clearPendingTimer(key);
			logger.debug(`Cancelled coalesced update for guild ${key} to flush immediately`);
		}
		void flushPendingUpdate(guildId, key);
		return;
	}
	if (pendingUpdates.has(key)) {
		clearPendingTimer(key);
		logger.debug(`Cleared pending update for guild ${key} (coalescing with new update)`);
	}
	pendingUpdates.set(
		key,
		setTimeout(() => {
			void flushPendingUpdate(guildId, key);
		}, 3000),
	);
	logger.debug(`Scheduled coalesced settings update for guild ${key} in 3 seconds`);
};

export function updateGuildSettings(
	guildId: string | null,
	updates: UserGuildSettingsPartial,
	options?: PersistenceOptions,
): void {
	UserGuildSettings.getSettingsForScope(guildId);
	UserGuildSettings.updateGuildSettings(guildId, updates as Partial<GatewayGuildSettings>);
	scheduleUpdate(guildId, updates, options);
}

export function bulkUpdateGuildSettings(
	guildIds: ReadonlyArray<string | null>,
	updates: UserGuildSettingsPartial,
): void {
	const uniqueGuildIds = Array.from(new Set(guildIds));
	if (uniqueGuildIds.length === 0) return;
	for (const guildId of uniqueGuildIds) {
		UserGuildSettings.getSettingsForScope(guildId);
		UserGuildSettings.updateGuildSettings(guildId, updates as Partial<GatewayGuildSettings>);
		const key = getUpdateKey(guildId);
		addPendingPayload(key, updates);
		enqueueBulkFlush(guildId, key);
	}
}

export function toggleHideMutedChannels(guildId: string | null): void {
	const currentSettings = UserGuildSettings.getSettingsForScope(guildId);
	const newValue = !currentSettings.hide_muted_channels;
	updateGuildSettings(guildId, {hide_muted_channels: newValue});
}

export function updateChannelOverride(
	guildId: string | null,
	channelId: string,
	override: ChannelOverrideUpdate,
	options?: PersistenceOptions,
): void {
	const currentSettings = UserGuildSettings.getSettingsForScope(guildId);
	const currentOverride = UserGuildSettings.getChannelOverride(guildId, channelId);
	let newOverride: ChannelOverride | null = null;
	if (override != null) {
		newOverride = defaultChannelOverride(channelId, currentOverride, override);
	}
	const newChannelOverrides: Record<string, ChannelOverride> = {...(currentSettings.channel_overrides ?? {})};
	if (newOverride == null) {
		delete newChannelOverrides[channelId];
	} else {
		newChannelOverrides[channelId] = newOverride;
	}
	commitChannelOverrides(guildId, newChannelOverrides, options);
}

export function bulkUpdateChannelOverrides(
	guildId: string | null,
	channelIds: ReadonlyArray<string>,
	override: ChannelOverrideUpdate,
	options?: PersistenceOptions,
): void {
	const uniqueChannelIds = Array.from(new Set(channelIds));
	if (uniqueChannelIds.length === 0) return;
	const currentSettings = UserGuildSettings.getSettingsForScope(guildId);
	const newChannelOverrides: Record<string, ChannelOverride> = {...(currentSettings.channel_overrides ?? {})};
	for (const channelId of uniqueChannelIds) {
		const currentOverride = UserGuildSettings.getChannelOverride(guildId, channelId);
		if (override == null) {
			delete newChannelOverrides[channelId];
			continue;
		}
		newChannelOverrides[channelId] = defaultChannelOverride(channelId, currentOverride, override);
	}
	commitChannelOverrides(guildId, newChannelOverrides, options);
}

export function toggleChannelCollapsed(guildId: string | null, channelId: string): void {
	const isCollapsed = UserGuildSettings.isChannelSectionCollapsed(guildId, channelId);
	updateChannelOverride(guildId, channelId, {collapsed: !isCollapsed});
}

export function updateMessageNotifications(
	guildId: string | null,
	level: number,
	channelId?: string,
	options?: PersistenceOptions,
): void {
	if (channelId) {
		updateChannelOverride(guildId, channelId, {message_notifications: level}, options);
	} else {
		updateGuildSettings(guildId, {message_notifications: level}, options);
	}
}

export function updateUnreadBadgesLevel(
	guildId: string | null,
	level: number | null,
	channelId?: string,
	options?: PersistenceOptions,
): void {
	if (!AdvancedSettings.unreadBadgeCustomizationEnabled) return;
	if (channelId) {
		updateChannelOverride(guildId, channelId, {unread_badges: level}, options);
	} else {
		updateGuildSettings(guildId, {unread_badges: level}, options);
	}
}

export function toggleChannelMuted(guildId: string | null, channelId: string, options?: PersistenceOptions): void {
	const isMuted = UserGuildSettings.isChannelDirectlyMuted(guildId, channelId);
	updateChannelOverride(guildId, channelId, {muted: !isMuted}, options);
}

export function toggleAllCategoriesCollapsed(guildId: string | null, categoryIds: Array<string>): void {
	const uniqueCategoryIds = Array.from(new Set(categoryIds));
	if (uniqueCategoryIds.length === 0) return;
	const allCollapsed = uniqueCategoryIds.every((categoryId) =>
		UserGuildSettings.isChannelSectionCollapsed(guildId, categoryId),
	);
	const newCollapsedState = !allCollapsed;
	for (const categoryId of uniqueCategoryIds) {
		UserGuildSettings.updateChannelOverride(guildId, categoryId, {collapsed: newCollapsedState});
	}
	const currentSettings = UserGuildSettings.getSettingsForScope(guildId);
	const newChannelOverrides: Record<string, ChannelOverride> = {...(currentSettings.channel_overrides ?? {})};
	for (const categoryId of uniqueCategoryIds) {
		const currentOverride = newChannelOverrides[categoryId];
		newChannelOverrides[categoryId] = {
			channel_id: categoryId,
			collapsed: newCollapsedState,
			message_notifications: currentOverride?.message_notifications ?? MessageNotifications.INHERIT,
			muted: currentOverride?.muted ?? false,
			mute_config: currentOverride?.mute_config ?? null,
			unread_badges: currentOverride?.unread_badges ?? null,
		};
	}
	scheduleUpdate(guildId, {
		channel_overrides: newChannelOverrides,
	});
}
