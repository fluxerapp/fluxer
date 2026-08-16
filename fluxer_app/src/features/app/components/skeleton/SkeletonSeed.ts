// SPDX-License-Identifier: AGPL-3.0-or-later

function hashSkeletonSeed(input: string): number {
	let hash = 2166136261;
	for (let index = 0; index < input.length; index++) {
		hash ^= input.charCodeAt(index);
		hash = Math.imul(hash, 16777619);
	}
	return hash >>> 0;
}

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
	return createSkeletonRandom(hashSkeletonSeed(key));
}
