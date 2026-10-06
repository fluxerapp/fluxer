// SPDX-License-Identifier: AGPL-3.0-or-later

export interface LocalSpeakingOverrideInput {
	pushToTalkActive: boolean;
	pushToMuteActive: boolean;
	pushToMuteHeld: boolean;
	selfDeaf: boolean;
	effectiveSelfMute: boolean;
	hasMicrophonePublication: boolean;
	microphonePublicationMuted: boolean;
}

export function resolveLocalSpeakingOverrideState(input: LocalSpeakingOverrideInput): boolean | null {
	const effectivelyMuted =
		input.selfDeaf || input.effectiveSelfMute || !input.hasMicrophonePublication || input.microphonePublicationMuted;
	if (input.pushToTalkActive) return !effectivelyMuted;
	if (input.pushToMuteActive && input.pushToMuteHeld) return false;
	if (effectivelyMuted) return false;
	return null;
}
