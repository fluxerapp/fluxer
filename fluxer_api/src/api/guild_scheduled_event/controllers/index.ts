// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {GuildScheduledEventController} from './GuildScheduledEventController';

export function registerGuildScheduledEventControllers(app: HonoApp) {
	GuildScheduledEventController(app);
}
