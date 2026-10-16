// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes, timingSafeEqual} from 'node:crypto';

export function sha256Hex(value: string): string {
	return createHash('sha256').update(value).digest('hex');
}

export function secretMatchesHash(
	presented: string | null | undefined,
	storedHash: string | null | undefined,
): boolean {
	if (presented == null || !storedHash) return false;
	const presentedBuffer = Buffer.from(sha256Hex(presented), 'hex');
	const storedBuffer = Buffer.from(storedHash, 'hex');
	return presentedBuffer.length === storedBuffer.length && timingSafeEqual(presentedBuffer, storedBuffer);
}

export function constantTimeEquals(a: string, b: string): boolean {
	const bufferA = Buffer.from(a);
	const bufferB = Buffer.from(b);
	return bufferA.length === bufferB.length && timingSafeEqual(bufferA, bufferB);
}

export function randomBase64Url(bytes: number): string {
	return randomBytes(bytes).toString('base64url');
}
