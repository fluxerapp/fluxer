/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {ME, MessageNotifications} from '~/Constants';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {UserGuildSettingsPartial} from '~/records/UserGuildSettingsRecord';
import GuildStore from '~/stores/GuildStore';
import type {ChannelOverride, GatewayGuildSettings} from '~/stores/UserGuildSettingsStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';

const logger = new Logger('UserGuildSettingsActionCreators');

const pendingUpdates: Map<string, NodeJS.Timeout> = new Map();
const pendingPayloads: Map<string, UserGuildSettingsPartial> = new Map();

interface PersistenceOptions {
	persistImmediately?: boolean;
}

const mergePayloads = (a: UserGuildSettingsPartial, b: UserGuildSettingsPartial): UserGuildSettingsPartial => {
	let merged: UserGuildSettingsPartial = {...a, ...b};

	const aCO = a.channel_overrides;
	const bCO = b.channel_overrides;

	if (aCO != null && bCO != null) {
		if (aCO !== null && bCO !== null) {
			merged = {...merged, channel_overrides: {...aCO, ...bCO}};
		}
	}

	return merged;
};

const scheduleUpdate = (guildId: string | null, updates: UserGuildSettingsPartial, options?: PersistenceOptions) => {
	const key = guildId ?? ME;

	const currentPending = pendingPayloads.get(key) ?? {};
	const mergedUpdates = mergePayloads(currentPending, updates);
	pendingPayloads.set(key, mergedUpdates);

	const flush = async () => {
		pendingUpdates.delete(key);
		const payload = pendingPayloads.get(key);
		pendingPayloads.delete(key);

		if (!payload) {
			return;
		}

		try {
			logger.debug(`Persisting settings update for guild ${key}`, payload);
			const endpoint = guildId == null ? Endpoints.USER_GUILD_SETTINGS_ME : Endpoints.USER_GUILD_SETTINGS(guildId);

			await http.patch({
				url: endpoint,
				body: payload,
			});

			logger.debug(`Successfully updated settings for guild ${key}`);
		} catch (error) {
			logger.error(`Failed to update settings for guild ${key}:`, error);
		}
	};

	if (options?.persistImmediately) {
		const pendingTimeout = pendingUpdates.get(key);
		if (pendingTimeout) {
			clearTimeout(pendingTimeout);
			pendingUpdates.delete(key);
			logger.debug(`Cancelled coalesced update for guild ${key} to flush immediately`);
		}
		void flush();
		return;
	}

	const existing = pendingUpdates.get(key);
	if (existing) {
		clearTimeout(existing);
		logger.debug(`Cleared pending update for guild ${key} (coalescing with new update)`);
	}

	pendingUpdates.set(
		key,
		setTimeout(() => {
			void flush();
		}, 3000),
	);

	logger.debug(`Scheduled coalesced settings update for guild ${key} in 3 seconds`);
};

export const updateGuildSettings = (
	guildId: string | null,
	updates: UserGuildSettingsPartial,
	options?: PersistenceOptions,
): void => {
	UserGuildSettingsStore.getSettings(guildId);
	UserGuildSettingsStore.updateGuildSettings(guildId, updates as Partial<GatewayGuildSettings>);
	scheduleUpdate(guildId, updates, options);
};

export const toggleHideMutedChannels = (guildId: string | null): void => {
	const currentSettings = UserGuildSettingsStore.getSettings(guildId);
	const newValue = !currentSettings.hide_muted_channels;
	updateGuildSettings(guildId, {hide_muted_channels: newValue});
};

export const updateChannelOverride = (
	guildId: string | null,
	channelId: string,
	override: Partial<ChannelOverride> | null,
	options?: PersistenceOptions,
): void => {
	const currentSettings = UserGuildSettingsStore.getSettings(guildId);
	const currentOverride = UserGuildSettingsStore.getChannelOverride(guildId, channelId);

	let newOverride: ChannelOverride | null = null;
	if (override != null) {
		newOverride = {
			channel_id: channelId,
			collapsed: false,
			message_notifications: MessageNotifications.INHERIT,
			muted: false,
			mute_config: null,
			...currentOverride,
			...override,
		};
	}

	const newChannelOverrides = {...(currentSettings.channel_overrides ?? {})};

	if (newOverride == null) {
		delete newChannelOverrides[channelId];
	} else {
		newChannelOverrides[channelId] = newOverride;
	}

	const hasOverrides = Object.keys(newChannelOverrides).length > 0;

	UserGuildSettingsStore.updateGuildSettings(guildId, {
		channel_overrides: hasOverrides ? newChannelOverrides : {},
	} as Partial<GatewayGuildSettings>);

	scheduleUpdate(
		guildId,
		{
			channel_overrides: hasOverrides ? newChannelOverrides : null,
		},
		options,
	);
};

export const toggleChannelCollapsed = (guildId: string | null, channelId: string): void => {
	const isCollapsed = UserGuildSettingsStore.isChannelCollapsed(guildId, channelId);
	updateChannelOverride(guildId, channelId, {collapsed: !isCollapsed});
};

export const updateMessageNotifications = (
	guildId: string | null,
	level: number,
	channelId?: string,
	options?: PersistenceOptions,
): void => {
	if (channelId) {
		updateChannelOverride(guildId, channelId, {message_notifications: level}, options);
	} else {
		updateGuildSettings(guildId, {message_notifications: level}, options);
	}
};

export const toggleChannelMuted = (guildId: string | null, channelId: string, options?: PersistenceOptions): void => {
	const isMuted = UserGuildSettingsStore.isChannelMuted(guildId, channelId);
	updateChannelOverride(guildId, channelId, {muted: !isMuted}, options);
};

export const toggleAllCategoriesCollapsed = (guildId: string | null, categoryIds: Array<string>): void => {
	if (categoryIds.length === 0) return;

	const allCollapsed = categoryIds.every((categoryId) =>
		UserGuildSettingsStore.isChannelCollapsed(guildId, categoryId),
	);

	const newCollapsedState = !allCollapsed;

	if (guildId) {
		for (const categoryId of categoryIds) {
			UserGuildSettingsStore.updateChannelOverride(guildId, categoryId, {collapsed: newCollapsedState});
		}
	}

	scheduleUpdate(guildId, {
		channel_overrides: (() => {
			const currentSettings = UserGuildSettingsStore.getSettings(guildId);

			const newChannelOverrides = {...(currentSettings.channel_overrides ?? {})};
			for (const categoryId of categoryIds) {
				const currentOverride = UserGuildSettingsStore.getChannelOverride(guildId, categoryId);
				if (newCollapsedState) {
					newChannelOverrides[categoryId] = {
						channel_id: categoryId,
						collapsed: true,
						message_notifications: currentOverride?.message_notifications ?? MessageNotifications.INHERIT,
						muted: currentOverride?.muted ?? false,
						mute_config: currentOverride?.mute_config ?? null,
					};
				} else {
					delete newChannelOverrides[categoryId];
				}
			}

			return Object.keys(newChannelOverrides).length > 0 ? newChannelOverrides : null;
		})(),
	});
};

export const repairGuildNotificationInheritance = (): void => {
	const guildIds = UserGuildSettingsStore.getGuildIds();
	if (guildIds.length === 0) return;

	for (const guildId of guildIds) {
		const guild = GuildStore.getGuild(guildId);
		if (!guild) continue;

		const storedLevel = UserGuildSettingsStore.getStoredGuildMessageNotifications(guildId);
		if (storedLevel === MessageNotifications.INHERIT || storedLevel === MessageNotifications.NULL) continue;

		const guildDefault = guild.defaultMessageNotifications ?? MessageNotifications.ALL_MESSAGES;
		if (storedLevel !== guildDefault) continue;

		updateGuildSettings(guildId, {message_notifications: MessageNotifications.INHERIT});
	}
};
