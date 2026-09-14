// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelOverride} from '@app/api/database/types/UserTypes';
import {MuteConfiguration} from '@app/api/models/MuteConfiguration';
import type {ChannelMessageNotifications} from '@fluxer/constants/src/NotificationConstants';

export class GuildChannelOverride {
	readonly collapsed: boolean;
	readonly messageNotifications: ChannelMessageNotifications | null;
	readonly muted: boolean;
	readonly muteConfig: MuteConfiguration | null;
	readonly unreadBadges: ChannelMessageNotifications | null;

	constructor(override: ChannelOverride) {
		this.collapsed = override.collapsed ?? false;
		this.messageNotifications = (override.message_notifications ?? null) as ChannelMessageNotifications | null;
		this.muted = override.muted ?? false;
		this.muteConfig = override.mute_config ? new MuteConfiguration(override.mute_config) : null;
		this.unreadBadges = (override.unread_badges ?? null) as ChannelMessageNotifications | null;
	}

	toChannelOverride(): ChannelOverride {
		return {
			collapsed: this.collapsed,
			message_notifications: this.messageNotifications,
			muted: this.muted,
			mute_config: this.muteConfig?.toMuteConfig() ?? null,
			unread_badges: this.unreadBadges,
		};
	}
}
