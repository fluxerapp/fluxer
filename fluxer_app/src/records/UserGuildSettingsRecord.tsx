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

export type MuteConfig = Readonly<{
	end_time?: string | null;
	selected_time_window?: number;
}> | null;

export type ChannelOverride = Readonly<{
	collapsed: boolean;
	message_notifications: number;
	muted: boolean;
	mute_config?: MuteConfig;
}>;

export type UserGuildSettings = Readonly<{
	guild_id: string | null;
	message_notifications: number;
	muted: boolean;
	mute_config?: MuteConfig;
	mobile_push: boolean;
	suppress_everyone: boolean;
	suppress_roles: boolean;
	hide_muted_channels: boolean;
	channel_overrides?: Record<string, ChannelOverride> | null;
	version: number;
}>;

export type UserGuildSettingsPartial = Partial<Omit<UserGuildSettings, 'guild_id' | 'version'>>;
