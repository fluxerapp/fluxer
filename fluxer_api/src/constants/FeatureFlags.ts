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

export const FeatureFlags = {
	MESSAGE_SCHEDULING: 'message_scheduling',
	EXPRESSION_PACKS: 'expression_packs',
} as const;

export type FeatureFlag = (typeof FeatureFlags)[keyof typeof FeatureFlags];

export const ALL_FEATURE_FLAGS: Array<FeatureFlag> = Object.values(FeatureFlags);

export const FEATURE_FLAG_KEY_PREFIX = 'feature_flag:';
export const FEATURE_FLAG_REDIS_KEY = 'feature_flags:config';
export const FEATURE_FLAG_POLL_INTERVAL_MS = 30000;
export const FEATURE_FLAG_POLL_JITTER_MS = 5000;
export const FEATURE_FLAG_USER_CACHE_PREFIX = 'feature_flag:user';
export const FEATURE_FLAG_USER_CACHE_TTL_SECONDS = 30;
