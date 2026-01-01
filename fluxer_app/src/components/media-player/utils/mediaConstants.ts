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

export const AUDIO_PLAYBACK_RATES = [0.5, 1, 1.25, 1.5, 2] as const;

export const VIDEO_PLAYBACK_RATES = [0.5, 1, 1.5, 2] as const;

export const DEFAULT_SEEK_AMOUNT = 10;

export const DEFAULT_VOLUME = 1;

export const VIDEO_BREAKPOINTS = {
	SMALL: 240,
	MEDIUM: 320,
	LARGE: 400,
} as const;

export const VOLUME_STEP = 0.1;

export const SEEK_STEP = 10;

export const VOLUME_STORAGE_KEY = 'fluxer:media-player:volume';

export const MUTE_STORAGE_KEY = 'fluxer:media-player:muted';

export const PLAYBACK_RATE_STORAGE_KEY = 'fluxer:media-player:playback-rate';
