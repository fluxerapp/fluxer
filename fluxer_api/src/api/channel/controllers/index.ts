// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {CallController} from './CallController';
import {ChannelController} from './ChannelController';
import {MessageController} from './MessageController';
import {MessageInteractionController} from './MessageInteractionController';
import {ScheduledMessageController} from './ScheduledMessageController';
import {StreamController} from './StreamController';
import {ThreadController} from './ThreadController';
import {VoiceDiagnosticsController} from './VoiceDiagnosticsController';
import {VoicePresenceController} from './VoicePresenceController';

export function registerChannelControllers(app: HonoApp) {
	ChannelController(app);
	MessageInteractionController(app);
	MessageController(app);
	ScheduledMessageController(app);
	CallController(app);
	StreamController(app);
	ThreadController(app);
	VoiceDiagnosticsController(app);
	VoicePresenceController(app);
}
