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

import type {ChannelOverride} from '~/database/CassandraTypes';
import {MuteConfiguration} from './MuteConfiguration';

export class GuildChannelOverride {
	readonly collapsed: boolean;
	readonly messageNotifications: number | null;
	readonly muted: boolean;
	readonly muteConfig: MuteConfiguration | null;

	constructor(override: ChannelOverride) {
		this.collapsed = override.collapsed ?? false;
		this.messageNotifications = override.message_notifications ?? null;
		this.muted = override.muted ?? false;
		this.muteConfig = override.mute_config ? new MuteConfiguration(override.mute_config) : null;
	}

	toChannelOverride(): ChannelOverride {
		return {
			collapsed: this.collapsed,
			message_notifications: this.messageNotifications,
			muted: this.muted,
			mute_config: this.muteConfig?.toMuteConfig() ?? null,
		};
	}
}
