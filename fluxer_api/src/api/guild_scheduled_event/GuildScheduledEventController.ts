// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../types/HonoEnv';
import {registerGuildScheduledEventControllers} from './controllers/index';

export function GuildScheduledEventController(app: HonoApp) {
	registerGuildScheduledEventControllers(app);
}
