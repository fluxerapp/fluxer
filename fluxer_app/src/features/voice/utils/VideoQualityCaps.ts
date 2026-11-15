// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MAX_CAMERA_RESOLUTION,
	MAX_CAMERA_RESOLUTION_CEILING,
	MAX_SCREEN_SHARE_FRAME_RATE,
	MAX_SCREEN_SHARE_FRAME_RATE_CEILING,
	MAX_SCREEN_SHARE_RESOLUTION,
	MAX_SCREEN_SHARE_RESOLUTION_CEILING,
} from '@fluxer/constants/src/LimitConstants';

export interface VideoQualityCaps {
	screenShareMaxHeight: number;
	screenShareMaxFrameRate: number;
	cameraMaxHeight: number;
}

export const BASE_VIDEO_QUALITY_CAPS: VideoQualityCaps = {
	screenShareMaxHeight: MAX_SCREEN_SHARE_RESOLUTION,
	screenShareMaxFrameRate: MAX_SCREEN_SHARE_FRAME_RATE,
	cameraMaxHeight: MAX_CAMERA_RESOLUTION,
};

export const FULL_VIDEO_QUALITY_CAPS: VideoQualityCaps = {
	screenShareMaxHeight: MAX_SCREEN_SHARE_RESOLUTION_CEILING,
	screenShareMaxFrameRate: MAX_SCREEN_SHARE_FRAME_RATE_CEILING,
	cameraMaxHeight: MAX_CAMERA_RESOLUTION_CEILING,
};

export function isFullVideoQuality(caps: VideoQualityCaps): boolean {
	return (
		caps.screenShareMaxHeight >= FULL_VIDEO_QUALITY_CAPS.screenShareMaxHeight &&
		caps.screenShareMaxFrameRate >= FULL_VIDEO_QUALITY_CAPS.screenShareMaxFrameRate &&
		caps.cameraMaxHeight >= FULL_VIDEO_QUALITY_CAPS.cameraMaxHeight
	);
}
