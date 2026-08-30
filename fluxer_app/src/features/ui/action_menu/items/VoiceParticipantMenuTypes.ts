// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('VoiceParticipantMenuTypes');

export type VoiceParticipantMenuSurface =
	| 'participant-list'
	| 'call-tile'
	| 'call-avatar'
	| 'participant-avatar-list'
	| 'stream-spectator-list';

export type VoiceParticipantVisualSource = 'camera' | 'screen-share';

export interface VoiceParticipantMenuParticipantSource {
	kind: 'participant';
	focusSource?: VoiceParticipantVisualSource;
}

export interface VoiceParticipantMenuCameraSource {
	kind: 'camera';
}

export type VoiceParticipantStreamState =
	| {
			kind: 'own';
	  }
	| {
			kind: 'remote-unwatched';
			onWatch: () => void;
	  }
	| {
			kind: 'remote-watched';
			hasAudio: boolean;
			onStopWatching: () => void;
	  };

export interface VoiceParticipantMenuScreenShareSource {
	kind: 'screen-share';
	streamKey: string;
	state: VoiceParticipantStreamState;
}

export type VoiceParticipantMenuSource =
	| VoiceParticipantMenuParticipantSource
	| VoiceParticipantMenuCameraSource
	| VoiceParticipantMenuScreenShareSource;

export interface VoiceParticipantMenuRequest {
	surface: VoiceParticipantMenuSurface;
	source: VoiceParticipantMenuSource;
}

export function normalizeVoiceParticipantMenuRequest(request: VoiceParticipantMenuRequest): VoiceParticipantMenuSource {
	const {surface, source} = request;
	if (source.kind === 'participant') return source;
	if (source.kind === 'camera') {
		if (surface !== 'call-tile') {
			logger.error('Camera participant menus require the call-tile surface', {surface});
			return {kind: 'participant', focusSource: 'camera'};
		}
		return source;
	}
	if (surface !== 'call-tile') {
		logger.error('Screen-share participant menus require the call-tile surface', {surface});
		return {kind: 'participant', focusSource: 'screen-share'};
	}
	if (source.streamKey.length === 0) {
		logger.error('Screen-share participant menus require a non-empty stream key', {surface});
		return {kind: 'participant', focusSource: 'screen-share'};
	}
	return source;
}
