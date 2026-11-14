// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getVoiceConnectionContextFromMediaEngine,
	getVoiceStateByConnectionIdFromMediaEngine,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {isVoicePermissionMuteActive} from '@app/features/voice/utils/VoicePermissionUtils';

export interface EffectiveAudioState {
	selfMute: boolean;
	selfDeaf: boolean;
	serverMute: boolean;
	serverDeaf: boolean;
	effectiveMute: boolean;
	effectiveDeaf: boolean;
}

interface ComputeEffectiveAudioStateParams {
	selfMute: boolean;
	selfDeaf: boolean;
	serverMute?: boolean;
	serverDeaf?: boolean;
}

interface EffectiveAudioStateOptions {
	connectionId?: string | null;
	selfMute?: boolean;
	selfDeaf?: boolean;
	serverMute?: boolean;
	serverDeaf?: boolean;
}

function computeEffectiveAudioState(params: ComputeEffectiveAudioStateParams): EffectiveAudioState {
	const serverMute = params.serverMute ?? false;
	const serverDeaf = params.serverDeaf ?? false;
	return {
		selfMute: params.selfMute,
		selfDeaf: params.selfDeaf,
		serverMute,
		serverDeaf,
		effectiveMute: serverMute || serverDeaf || params.selfMute || params.selfDeaf,
		effectiveDeaf: serverDeaf || params.selfDeaf,
	};
}

export function getEffectiveAudioState(options?: EffectiveAudioStateOptions): EffectiveAudioState {
	const selfMute = options?.selfMute ?? LocalVoiceState.getSelfMute();
	const selfDeaf = options?.selfDeaf ?? LocalVoiceState.getSelfDeaf();
	const connectionContext = getVoiceConnectionContextFromMediaEngine();
	const connectionId = options?.connectionId ?? connectionContext?.connectionId;
	const serverVoiceState = getVoiceStateByConnectionIdFromMediaEngine(connectionId);
	const permissionMuted = isVoicePermissionMuteActive(
		serverVoiceState,
		connectionContext?.guildId,
		connectionContext?.channelId,
	);
	const serverMute = options?.serverMute ?? serverVoiceState?.mute ?? false;
	return computeEffectiveAudioState({
		selfMute,
		selfDeaf,
		serverMute: serverMute || permissionMuted,
		serverDeaf: options?.serverDeaf ?? serverVoiceState?.deaf ?? false,
	});
}
