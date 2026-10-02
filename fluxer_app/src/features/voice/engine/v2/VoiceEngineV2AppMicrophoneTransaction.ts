// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2MicrophoneOptions} from '@fluxer/voice_engine_v2';
import type {LocalAudioTrack, Room} from 'livekit-client';

export interface MicrophoneEnableContext {
	readonly room: Room;
	readonly channelId: string | null;
	readonly options: VoiceEngineV2MicrophoneOptions;
}

export interface MicrophoneEnableState {
	microphoneWasPublished: boolean;
	audioTrack: LocalAudioTrack | null;
}

export function createInitialMicrophoneEnableState(): MicrophoneEnableState {
	return {
		microphoneWasPublished: false,
		audioTrack: null,
	};
}
