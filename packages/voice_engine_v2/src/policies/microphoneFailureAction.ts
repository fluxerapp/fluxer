// SPDX-License-Identifier: AGPL-3.0-or-later

export type VoiceEngineV2MicrophoneOperationFailureAction = 'none' | 'defer' | 'mute' | 'crash';

interface VoiceEngineV2OperationFailureLike {
	ok: false;
	error: {
		code: string;
		message: string;
		capability?: string;
	};
}

export type VoiceEngineV2OperationResultLike = {ok: true} | VoiceEngineV2OperationFailureLike;

export interface VoiceEngineV2MicrophoneFailureContext {
	reconnecting: boolean;
}

const VOICE_ENGINE_V2_MICROPHONE_FAILURE_CONTEXT_DEFAULT: VoiceEngineV2MicrophoneFailureContext = {
	reconnecting: false,
};

function isUnsupportedMicrophoneCapture(result: VoiceEngineV2OperationFailureLike): boolean {
	return (
		(result.error.code === 'unsupported-capability' || result.error.code === 'unsupportedCapability') &&
		result.error.capability === 'microphoneCapture'
	);
}

function isInvalidArgumentFailure(result: VoiceEngineV2OperationFailureLike): boolean {
	return result.error.code === 'invalid-args' || result.error.code === 'invalidArgument';
}

function isNotConnectedFailure(result: VoiceEngineV2OperationFailureLike): boolean {
	if (result.error.code === 'not-connected' || result.error.code === 'notConnected') return true;
	return result.error.code === 'native-error' && result.error.message.trim().toLowerCase() === 'not connected';
}

function isTransientTransportFailure(result: VoiceEngineV2OperationFailureLike): boolean {
	if (result.error.code === 'timeout') return true;
	if (result.error.code === 'cancelled') return true;
	return result.error.code === 'liveKitError';
}

export function getVoiceEngineV2MicrophoneOperationFailureAction(
	result: VoiceEngineV2OperationResultLike,
	requestedEnabled: boolean,
	context: VoiceEngineV2MicrophoneFailureContext = VOICE_ENGINE_V2_MICROPHONE_FAILURE_CONTEXT_DEFAULT,
): VoiceEngineV2MicrophoneOperationFailureAction {
	if (result.ok) return 'none';
	if (isUnsupportedMicrophoneCapture(result) || isInvalidArgumentFailure(result)) return 'crash';
	if (!requestedEnabled) return 'none';
	if (isNotConnectedFailure(result)) return 'defer';
	if (isTransientTransportFailure(result)) return 'defer';
	if (context.reconnecting) return 'defer';
	return 'mute';
}
