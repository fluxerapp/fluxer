// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerGuildControllers} from '@app/api/guild/controllers/index';
import type {HonoApp} from '@app/api/types/HonoEnv';

export function GuildController(app: HonoApp) {
	registerGuildControllers(app);
}
