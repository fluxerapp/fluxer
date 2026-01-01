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

import type {HonoApp} from '~/App';
import {GuildAuditLogController} from './GuildAuditLogController';
import {GuildBaseController} from './GuildBaseController';
import {GuildChannelController} from './GuildChannelController';
import {GuildEmojiController} from './GuildEmojiController';
import {GuildMemberController} from './GuildMemberController';
import {GuildRoleController} from './GuildRoleController';
import {GuildStickerController} from './GuildStickerController';

export const registerGuildControllers = (app: HonoApp) => {
	GuildBaseController(app);
	GuildMemberController(app);
	GuildRoleController(app);
	GuildChannelController(app);
	GuildEmojiController(app);
	GuildStickerController(app);
	GuildAuditLogController(app);
};
