// SPDX-License-Identifier: AGPL-3.0-or-later

import tags from '@app/api/user/profile_badges/VisionaryNumberTags.json' with {type: 'json'};

export interface VisionaryNumberTagFile {
	asset: string;
	width: number;
	height: number;
}

export const VISIONARY_NUMBER_TAGS: Readonly<Record<string, VisionaryNumberTagFile>> = tags;

export function getVisionaryNumberTag(sequence: number): VisionaryNumberTagFile | null {
	const key = String(sequence);
	return Object.hasOwn(VISIONARY_NUMBER_TAGS, key) ? VISIONARY_NUMBER_TAGS[key] : null;
}
