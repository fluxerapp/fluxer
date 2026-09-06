// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {GuildAuditLogController} from './GuildAuditLogController';
import {GuildBaseController} from './GuildBaseController';
import {GuildChannelController} from './GuildChannelController';
import {GuildDiscoveryController} from './GuildDiscoveryController';
import {GuildEmojiController} from './GuildEmojiController';
import {GuildMemberController} from './GuildMemberController';
import {GuildMemberSearchController} from './GuildMemberSearchController';
import {GuildRoleController} from './GuildRoleController';
import {GuildStickerController} from './GuildStickerController';

export function registerGuildControllers(app: HonoApp) {
	GuildBaseController(app);
	GuildMemberController(app);
	GuildMemberSearchController(app);
	GuildRoleController(app);
	GuildChannelController(app);
	GuildEmojiController(app);
	GuildStickerController(app);
	GuildAuditLogController(app);
	GuildDiscoveryController(app);
}
