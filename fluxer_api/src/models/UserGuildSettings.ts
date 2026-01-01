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

import type {ChannelOverride, UserGuildSettingsRow} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, UserID} from '../BrandedTypes';
import {GuildChannelOverride} from './GuildChannelOverride';
import {MuteConfiguration} from './MuteConfiguration';

export class UserGuildSettings {
	readonly userId: UserID;
	readonly guildId: GuildID;
	readonly messageNotifications: number | null;
	readonly muted: boolean;
	readonly muteConfig: MuteConfiguration | null;
	readonly mobilePush: boolean;
	readonly suppressEveryone: boolean;
	readonly suppressRoles: boolean;
	readonly hideMutedChannels: boolean;
	readonly channelOverrides: Map<ChannelID, GuildChannelOverride>;
	readonly version: number;

	constructor(row: UserGuildSettingsRow) {
		this.userId = row.user_id;
		this.guildId = row.guild_id;
		this.messageNotifications = row.message_notifications ?? null;
		this.muted = row.muted ?? false;
		this.muteConfig = row.mute_config ? new MuteConfiguration(row.mute_config) : null;
		this.mobilePush = row.mobile_push ?? false;
		this.suppressEveryone = row.suppress_everyone ?? false;
		this.suppressRoles = row.suppress_roles ?? false;
		this.hideMutedChannels = row.hide_muted_channels ?? false;
		this.channelOverrides = new Map();
		if (row.channel_overrides) {
			for (const [channelId, override] of row.channel_overrides) {
				this.channelOverrides.set(channelId, new GuildChannelOverride(override));
			}
		}
		this.version = row.version;
	}

	toRow(): UserGuildSettingsRow {
		const channelOverridesMap: Map<ChannelID, ChannelOverride> | null =
			this.channelOverrides.size > 0
				? new Map(
						Array.from(this.channelOverrides.entries()).map(([id, override]) => [id, override.toChannelOverride()]),
					)
				: null;

		return {
			user_id: this.userId,
			guild_id: this.guildId,
			message_notifications: this.messageNotifications,
			muted: this.muted,
			mute_config: this.muteConfig?.toMuteConfig() ?? null,
			mobile_push: this.mobilePush,
			suppress_everyone: this.suppressEveryone,
			suppress_roles: this.suppressRoles,
			hide_muted_channels: this.hideMutedChannels,
			channel_overrides: channelOverridesMap,
			version: this.version,
		};
	}
}
