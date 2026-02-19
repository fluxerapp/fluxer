/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import VoiceSettingsStore from '@app/stores/VoiceSettingsStore';
import MediaEngineStore from '@app/stores/voice/MediaEngineFacade';

export function update(
	settings: Partial<{
		inputDeviceId: string;
		outputDeviceId: string;
		videoDeviceId: string;
		inputVolume: number;
		outputVolume: number;
		echoCancellation: boolean;
		noiseSuppression: boolean;
		autoGainControl: boolean;
		cameraResolution: 'low' | 'medium' | 'high';
		screenshareResolution: 'low' | 'medium' | 'high' | 'ultra' | '4k';
		videoFrameRate: number;
		backgroundImageId: string;
		backgroundImages: Array<{id: string; createdAt: number}>;
		showGridView: boolean;
		showMyOwnCamera: boolean;
		showMyOwnScreenShare: boolean;
		showNonVideoParticipants: boolean;
		showParticipantsCarousel: boolean;
		showVoiceConnectionAvatarStack: boolean;
		showVoiceConnectionId: boolean;
		pauseOwnScreenSharePreviewOnUnfocus: boolean;
		disablePictureInPicturePopout: boolean;
		screenShareHardwareAcceleration: boolean;
	}>,
): void {
	VoiceSettingsStore.updateSettings(settings);
	if (settings.outputVolume !== undefined) {
		MediaEngineStore.applyAllLocalAudioPreferences();
	}
	if (settings.inputVolume !== undefined) {
		MediaEngineStore.applyLocalInputVolume();
	}
}
