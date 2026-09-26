// SPDX-License-Identifier: AGPL-3.0-or-later

import {webCameraSegmentationRuntimeAvailable} from '@app/features/voice/utils/camera-effects/WebCameraBackgroundSupport';

export function areVoiceBackgroundsAvailable(): boolean {
	return webCameraSegmentationRuntimeAvailable();
}
