// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {CallController} from './CallController';
import {ChannelController} from './ChannelController';
import {MessageController} from './MessageController';
import {MessageInteractionController} from './MessageInteractionController';
import {StreamController} from './StreamController';

export function registerChannelControllers(app: HonoApp) {
	ChannelController(app);
	MessageInteractionController(app);
	MessageController(app);
	CallController(app);
	StreamController(app);
}
