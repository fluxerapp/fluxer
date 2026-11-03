// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

export const VOICE_ENGINE_V2_COALESCED_TRACKS_CAP = 64;

export type VoiceEngineV2FrameReceivedEvent = Extract<VoiceEngineV2Event, {type: 'inboundVideo.frameReceived'}>;

export function isVoiceEngineV2FrameReceivedEvent(event: VoiceEngineV2Event): event is VoiceEngineV2FrameReceivedEvent {
	assert.ok(event !== null, 'isVoiceEngineV2FrameReceivedEvent requires a non-null event');
	assert.equal(typeof event.type, 'string', 'isVoiceEngineV2FrameReceivedEvent requires an event with a string type');
	return event.type === 'inboundVideo.frameReceived';
}

export function canCoalesceVoiceEngineV2Events(tailEvent: VoiceEngineV2Event, nextEvent: VoiceEngineV2Event): boolean {
	if (!isVoiceEngineV2FrameReceivedEvent(nextEvent)) return false;
	if (!isVoiceEngineV2FrameReceivedEvent(tailEvent)) return false;
	assert.equal(typeof tailEvent.frame.trackSid, 'string', 'tail frame event must have a string trackSid');
	assert.equal(typeof nextEvent.frame.trackSid, 'string', 'next frame event must have a string trackSid');
	return tailEvent.frame.trackSid === nextEvent.frame.trackSid;
}
