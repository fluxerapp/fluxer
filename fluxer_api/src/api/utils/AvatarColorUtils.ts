// SPDX-License-Identifier: AGPL-3.0-or-later

import sharp from 'sharp';

const FALLBACK_AVATAR_COLOR = 0x4641d9;
const LOW_CHROMA_THRESHOLD = 18;
const MEDIUM_CHROMA_THRESHOLD = 40;
const LOW_CHROMA_WEIGHT = 0.25;
const MEDIUM_CHROMA_WEIGHT = 0.6;
const HIGH_CHROMA_WEIGHT = 1;

function clamp(value: number, min: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

function scorePixel(r: number, g: number, b: number) {
	const max = Math.max(r, g, b);
	const min = Math.min(r, g, b);
	return {
		chroma: max - min,
		brightness: (r + g + b) / 3,
	};
}

function rgbToHsl(r: number, g: number, b: number): [number, number, number] {
	const rn = r / 255;
	const gn = g / 255;
	const bn = b / 255;
	const max = Math.max(rn, gn, bn);
	const min = Math.min(rn, gn, bn);
	const delta = max - min;
	let hue = 0;
	if (delta !== 0) {
		if (max === rn) {
			hue = ((gn - bn) / delta) % 6;
		} else if (max === gn) {
			hue = (bn - rn) / delta + 2;
		} else {
			hue = (rn - gn) / delta + 4;
		}
		hue *= 60;
		if (hue < 0) {
			hue += 360;
		}
	}
	const lightness = (max + min) / 2;
	const saturation = delta === 0 ? 0 : delta / (1 - Math.abs(2 * lightness - 1));
	return [hue, saturation, lightness];
}

function hslToRgb(h: number, s: number, l: number): [number, number, number] {
	const c = (1 - Math.abs(2 * l - 1)) * s;
	const hPrime = h / 60;
	const x = c * (1 - Math.abs((hPrime % 2) - 1));
	let r1 = 0;
	let g1 = 0;
	let b1 = 0;
	if (hPrime >= 0 && hPrime < 1) {
		r1 = c;
		g1 = x;
	} else if (hPrime >= 1 && hPrime < 2) {
		r1 = x;
		g1 = c;
	} else if (hPrime >= 2 && hPrime < 3) {
		g1 = c;
		b1 = x;
	} else if (hPrime >= 3 && hPrime < 4) {
		g1 = x;
		b1 = c;
	} else if (hPrime >= 4 && hPrime < 5) {
		r1 = x;
		b1 = c;
	} else if (hPrime >= 5 && hPrime < 6) {
		r1 = c;
		b1 = x;
	}
	const m = l - c / 2;
	return [Math.round((r1 + m) * 255), Math.round((g1 + m) * 255), Math.round((b1 + m) * 255)];
}

export async function deriveDominantAvatarColor(imageBuffer: Uint8Array): Promise<number> {
	try {
		const {data, info} = await sharp(Buffer.from(imageBuffer))
			.resize(64, 64, {fit: 'inside'})
			.ensureAlpha()
			.raw()
			.toBuffer({resolveWithObject: true});
		const channels = info.channels ?? 4;
		const stride = Math.max(channels, 1) * 2;
		const buckets = new Map<string, number>();
		for (let i = 0; i + channels <= data.length; i += stride) {
			const r = data[i];
			const g = data[i + 1];
			const b = data[i + 2];
			const a = channels >= 4 ? data[i + 3] : 255;
			if (a < 180) continue;
			const {chroma, brightness} = scorePixel(r, g, b);
			if (brightness > 245 || brightness < 12) continue;
			const qr = Math.floor(r / 12);
			const qg = Math.floor(g / 12);
			const qb = Math.floor(b / 12);
			const key = `${qr},${qg},${qb}`;
			let weight: number;
			if (chroma < LOW_CHROMA_THRESHOLD) {
				weight = LOW_CHROMA_WEIGHT;
			} else if (chroma < MEDIUM_CHROMA_THRESHOLD) {
				weight = MEDIUM_CHROMA_WEIGHT;
			} else {
				weight = HIGH_CHROMA_WEIGHT;
			}
			buckets.set(key, (buckets.get(key) ?? 0) + weight);
		}
		if (buckets.size === 0) {
			return FALLBACK_AVATAR_COLOR;
		}
		let bestScore = -Infinity;
		let bestKey: string | null = null;
		for (const [key, score] of buckets.entries()) {
			if (score > bestScore) {
				bestScore = score;
				bestKey = key;
			}
		}
		if (!bestKey) {
			return FALLBACK_AVATAR_COLOR;
		}
		const [qr, qg, qb] = bestKey.split(',').map((value) => Number(value));
		const baseR = Math.min(255, Math.max(0, qr * 12));
		const baseG = Math.min(255, Math.max(0, qg * 12));
		const baseB = Math.min(255, Math.max(0, qb * 12));
		const [h, s, l] = rgbToHsl(baseR, baseG, baseB);
		const nextL = clamp(l * 0.75, 0.28, 0.62);
		const [rr, gg, bb] = hslToRgb(h, s, nextL);
		const finalR = Math.max(Math.floor(rr), 30);
		const finalG = Math.max(Math.floor(gg), 30);
		const finalB = Math.max(Math.floor(bb), 30);
		return (finalR << 16) | (finalG << 8) | finalB;
	} catch {
		return FALLBACK_AVATAR_COLOR;
	}
}
