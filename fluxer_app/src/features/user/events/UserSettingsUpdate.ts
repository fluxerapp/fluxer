// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildList from '@app/features/guild/state/GuildList';
import UserSettings from '@app/features/user/state/UserSettings';

export interface UserSettingsPayload {
	flags: number;
	status: string;
	theme: string;
	time_format: number;
	guild_positions: Array<string>;
	locale: string;
	synced_preferences?: string;
}

export function handleUserSettingsUpdate(data: UserSettingsPayload, _context: GatewayHandlerContext): void {
	UserSettings.updateUserSettings(data);
	GuildList.sortGuilds();
}
