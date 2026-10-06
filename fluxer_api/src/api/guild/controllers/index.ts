// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildAuditLogController} from '@app/api/guild/controllers/GuildAuditLogController';
import {GuildBaseController} from '@app/api/guild/controllers/GuildBaseController';
import {GuildChannelController} from '@app/api/guild/controllers/GuildChannelController';
import {GuildDiscoveryController} from '@app/api/guild/controllers/GuildDiscoveryController';
import {GuildEmojiController} from '@app/api/guild/controllers/GuildEmojiController';
import {GuildMemberController} from '@app/api/guild/controllers/GuildMemberController';
import {GuildMemberSearchController} from '@app/api/guild/controllers/GuildMemberSearchController';
import {GuildRoleController} from '@app/api/guild/controllers/GuildRoleController';
import {GuildStickerController} from '@app/api/guild/controllers/GuildStickerController';
import type {HonoApp} from '@app/api/types/HonoEnv';

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
