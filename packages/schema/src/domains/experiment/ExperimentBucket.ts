// SPDX-License-Identifier: AGPL-3.0-or-later

const FNV_OFFSET_BASIS_32 = 0x811c9dc5;
const FNV_PRIME_32 = 0x01000193;

export const EXPERIMENT_BUCKET_RESOLUTION = 10000;

export function experimentBucket(userId: string, salt: string): number {
	let hash = FNV_OFFSET_BASIS_32;
	const input = `${salt}:${userId}`;
	for (let index = 0; index < input.length; index++) {
		hash ^= input.charCodeAt(index) & 0xff;
		hash = Math.imul(hash, FNV_PRIME_32) >>> 0;
	}
	return hash % EXPERIMENT_BUCKET_RESOLUTION;
}
