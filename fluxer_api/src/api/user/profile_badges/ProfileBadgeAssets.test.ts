// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';
import {PROFILE_BADGE_ASSETS} from '@app/api/user/profile_badges/ProfileBadgeAssets';
import {VISIONARY_NUMBER_TAGS} from '@app/api/user/profile_badges/VisionaryNumberTags';
import {describe, expect, test} from 'vitest';

const BADGES_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../../../../fluxer_static/badges');
const PNG_SIGNATURE = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);

function pngSize(bytes: Buffer): {width: number; height: number} {
	expect(bytes.subarray(0, 8).equals(PNG_SIGNATURE)).toBe(true);
	expect(bytes.subarray(12, 16).toString('ascii')).toBe('IHDR');
	return {width: bytes.readUInt32BE(16), height: bytes.readUInt32BE(20)};
}

function expectWebp(bytes: Buffer): void {
	expect(bytes.subarray(0, 4).toString('ascii')).toBe('RIFF');
	expect(bytes.subarray(8, 12).toString('ascii')).toBe('WEBP');
}

function webpCanvasSize(bytes: Buffer): {width: number; height: number} {
	expectWebp(bytes);
	expect(bytes.subarray(12, 16).toString('ascii')).toBe('VP8X');
	return {width: bytes.readUIntLE(24, 3) + 1, height: bytes.readUIntLE(27, 3) + 1};
}

const hashPrefix = (bytes: Buffer) => createHash('sha256').update(bytes).digest('hex').slice(0, 16);

describe('profile badge assets', () => {
	const hashes = Object.values(PROFILE_BADGE_ASSETS);

	test.each(Object.entries(PROFILE_BADGE_ASSETS))('%s ships a 128px PNG and a WebP named by its hash', (_, hash) => {
		const png = fs.readFileSync(path.join(BADGES_DIR, `${hash}.png`));
		expect(pngSize(png)).toEqual({width: 128, height: 128});
		expect(hashPrefix(png)).toBe(hash);
		expectWebp(fs.readFileSync(path.join(BADGES_DIR, `${hash}.webp`)));
	});

	test('every Visionary number tag ships a WebP named by its own hash at the listed size', () => {
		const tagHashes = Object.values(VISIONARY_NUMBER_TAGS).map((tag) => tag.asset);
		expect(new Set([...tagHashes, ...hashes]).size).toBe(tagHashes.length + hashes.length);
		for (const [number, tag] of Object.entries(VISIONARY_NUMBER_TAGS)) {
			const webp = fs.readFileSync(path.join(BADGES_DIR, `${tag.asset}.webp`));
			expect(hashPrefix(webp), number).toBe(tag.asset);
			expect(webpCanvasSize(webp), number).toEqual({width: tag.width, height: tag.height});
		}
	});
});
