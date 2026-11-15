// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind from '@app/features/input/state/InputKeybind';
import {selectVoiceLocalAudioEffectiveSelfMute} from '@app/features/voice/engine/VoiceLocalAudioReconcilePolicy';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {getActiveVoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {VoiceEngineV2AudioControls, VoiceEngineV2AudioMode} from '@fluxer/voice_engine_v2/src/protocol/types';

export function isPushToTalkActiveFromAppState(): boolean {
	if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
	return Keybind.isPushToTalkEffective();
}

export function isPushToMuteActiveFromAppState(): boolean {
	if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
	return Keybind.isPushToMuteEffective();
}

function getVoiceAudioModeFromAppState(): VoiceEngineV2AudioMode {
	if (isPushToTalkActiveFromAppState()) return 'pushToTalk';
	if (isPushToMuteActiveFromAppState()) return 'pushToMute';
	return 'voiceActivity';
}

export function getLatchedLocalSelfMute(microphoneFailureLatched: boolean): boolean {
	return selectVoiceLocalAudioEffectiveSelfMute({
		localSelfMute: LocalVoiceState.getSelfMute(),
		microphoneFailureLatched,
	});
}

export function getVoiceAudioControlsFromAppState(microphoneFailureLatched: boolean): VoiceEngineV2AudioControls {
	return {
		mode: getVoiceAudioModeFromAppState(),
		locallyMuted: getLatchedLocalSelfMute(microphoneFailureLatched),
		preferredLocallyMuted: LocalVoiceState.getSelfMute(),
		locallyDeafened: LocalVoiceState.getSelfDeaf(),
		mutedByPermission: LocalVoiceState.getMutedByPermission(),
		hasUserSetMute: LocalVoiceState.getHasUserSetMute(),
		hasUserSetDeaf: LocalVoiceState.getHasUserSetDeaf(),
		shouldUnmuteOnUndeafen: LocalVoiceState.shouldUnmuteOnUndeafen,
		pushToTalkActive: isPushToTalkActiveFromAppState() && Keybind.pushToTalkHeld,
		pushToMuteActive: isPushToMuteActiveFromAppState() && Keybind.pushToMuteHeld,
		inputVolume: VoiceSettings.inputVolume,
		outputVolume: VoiceSettings.outputVolume,
	};
}
