// SPDX-License-Identifier: AGPL-3.0-or-later

import {deriveDominantAvatarColor} from '@app/api/utils/AvatarColorUtils';
import sharp from 'sharp';
import {describe, expect, it} from 'vitest';

type Rgba = readonly [number, number, number, number];

interface ColorCase {
	name: string;
	first: Rgba;
	firstCount: number;
	other: Rgba;
	expected: number;
}

const IMAGE_WIDTH = 64;
const CHANNELS = 4;
const SAMPLED_PIXELS = 32;
const PIXELS_PER_SAMPLE = 2;

async function sampledImage(row: ColorCase): Promise<Buffer> {
	const pixels = Buffer.alloc(IMAGE_WIDTH * CHANNELS);
	for (let sample = 0; sample < SAMPLED_PIXELS; sample++) {
		const rgba = sample < row.firstCount ? row.first : row.other;
		const offset = sample * PIXELS_PER_SAMPLE * CHANNELS;
		pixels.set(rgba, offset);
		pixels.set(rgba, offset + CHANNELS);
	}
	return sharp(pixels, {raw: {width: IMAGE_WIDTH, height: 1, channels: CHANNELS}})
		.png()
		.toBuffer();
}

const ELIGIBILITY_CASES: ReadonlyArray<ColorCase> = [
	{
		name: 'alpha 179 uses the fallback',
		first: [100, 100, 100, 179],
		firstCount: 32,
		other: [100, 100, 100, 179],
		expected: 0x4641d9,
	},
	{
		name: 'alpha 180 retains the gray',
		first: [100, 100, 100, 180],
		firstCount: 32,
		other: [100, 100, 100, 180],
		expected: 0x484848,
	},
	{
		name: 'brightness 11 uses the fallback',
		first: [11, 11, 11, 255],
		firstCount: 32,
		other: [11, 11, 11, 255],
		expected: 0x4641d9,
	},
	{
		name: 'brightness 12 is retained and lightened',
		first: [12, 12, 12, 255],
		firstCount: 32,
		other: [12, 12, 12, 255],
		expected: 0x474747,
	},
	{
		name: 'brightness 245 is retained and darkened',
		first: [245, 245, 245, 255],
		firstCount: 32,
		other: [245, 245, 245, 255],
		expected: 0x9e9e9e,
	},
	{
		name: 'brightness 246 uses the fallback',
		first: [246, 246, 246, 255],
		firstCount: 32,
		other: [246, 246, 246, 255],
		expected: 0x4641d9,
	},
];

describe('deriveDominantAvatarColor with real decoded images', () => {
	it.each(ELIGIBILITY_CASES)('$name', async (row) => {
		const image = await sampledImage(row);
		expect(await deriveDominantAvatarColor(image)).toBe(row.expected);
	});
});
