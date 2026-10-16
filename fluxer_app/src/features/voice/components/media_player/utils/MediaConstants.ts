// SPDX-License-Identifier: AGPL-3.0-or-later

export const AUDIO_PLAYBACK_RATES = [1, 1.5, 2, 0.75] as const;
export const DEFAULT_SEEK_AMOUNT = 10;
export const DEFAULT_VOLUME = 1;
export const VIDEO_PLAYBACK_RATES = [1, 1.5, 2, 0.75] as const;
export const VIDEO_BREAKPOINTS = {
	SMALL: 240,
	MEDIUM: 320,
	LARGE: 400,
} as const;
export const VOLUME_STEP = 0.1;
export const SEEK_STEP = 10;
export const VOLUME_STORAGE_KEY = 'fluxer:media_player:volume';
export const MUTE_STORAGE_KEY = 'fluxer:media_player:muted';
