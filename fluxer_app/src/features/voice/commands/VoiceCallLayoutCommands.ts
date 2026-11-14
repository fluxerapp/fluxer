// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LayoutMode, PinnedParticipantSource} from '@app/features/voice/state/VoiceCallLayout';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';

type VoiceLayoutCommand =
	| {kind: 'layout-mode'; mode: LayoutMode}
	| {kind: 'pin-participant'; identity: string | null; source?: PinnedParticipantSource};

function dispatchVoiceLayoutCommand(command: VoiceLayoutCommand): void {
	switch (command.kind) {
		case 'layout-mode':
			VoiceCallLayout.setLayoutMode(command.mode);
			return;
		case 'pin-participant':
			VoiceCallLayout.setPinnedParticipant(command.identity, command.source);
			return;
	}
}

export function setLayoutMode(mode: LayoutMode): void {
	dispatchVoiceLayoutCommand({kind: 'layout-mode', mode});
}

export function setPinnedParticipant(identity: string | null, source?: PinnedParticipantSource): void {
	dispatchVoiceLayoutCommand({kind: 'pin-participant', identity, source});
}
