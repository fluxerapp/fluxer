// SPDX-License-Identifier: AGPL-3.0-or-later

export interface LocalSpeakingSuppressionInput {
	selfDeaf: boolean;
	effectiveSelfMute: boolean;
	hasMicrophonePublication: boolean;
	microphonePublicationMuted: boolean;
}

export function isLocalSpeakingSuppressed(input: LocalSpeakingSuppressionInput): boolean {
	return (
		input.selfDeaf || input.effectiveSelfMute || !input.hasMicrophonePublication || input.microphonePublicationMuted
	);
}
