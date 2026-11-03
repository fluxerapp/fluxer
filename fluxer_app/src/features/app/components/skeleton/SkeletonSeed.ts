// SPDX-License-Identifier: AGPL-3.0-or-later

import {hashString} from '@app/lib/placeholder-gradient';

function createSkeletonRandom(seed: number): () => number {
	let state = seed >>> 0;
	if (seed === 0) {
		state = 0x9e3779b9;
	}
	return () => {
		state ^= state << 13;
		state ^= state >>> 17;
		state ^= state << 5;
		return (state >>> 0) / 4294967296;
	};
}

export function createSkeletonRandomFromKey(key: string): () => number {
	return createSkeletonRandom(hashString(key));
}
