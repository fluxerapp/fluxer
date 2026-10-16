// SPDX-License-Identifier: AGPL-3.0-or-later

const hueAxis = [220, 260, 300, 340, 20, 60, 110, 150, 190] as const;

export function hashString(input: string): number {
	let hash = 2166136261;
	for (let i = 0; i < input.length; i++) {
		hash ^= input.charCodeAt(i);
		hash = Math.imul(hash, 16777619);
	}
	return hash >>> 0;
}

function pickHue(seed: number, offset: number): number {
	return hueAxis[(seed + offset) % hueAxis.length];
}

export interface DeterministicPlaceholderGradientStyle {
	background: string;
}

export function getDeterministicPlaceholderGradient(seed: string): DeterministicPlaceholderGradientStyle {
	const hash = hashString(seed || 'device');
	const h1 = pickHue(hash >> 0, 0);
	const h2 = pickHue(hash >> 3, 4);
	const h3 = pickHue(hash >> 6, 7);
	const s1 = 40 + ((hash >> 9) & 0x1f);
	const s2 = 36 + ((hash >> 12) & 0x1f);
	const l1 = 22 + ((hash >> 15) & 0x0f);
	const l2 = 18 + ((hash >> 18) & 0x0f);
	const l3 = 12 + ((hash >> 21) & 0x0b);
	const x1 = 18 + ((hash >> 10) & 0x3f);
	const y1 = 22 + ((hash >> 14) & 0x3f);
	const x2 = 60 + ((hash >> 18) & 0x1f);
	const y2 = 48 + ((hash >> 22) & 0x1f);
	return {
		background: [
			`radial-gradient(circle at ${x1}% ${y1}%, hsl(${h1} ${s1}% ${l1}%) 0%, transparent 58%)`,
			`radial-gradient(circle at ${x2}% ${y2}%, hsl(${h2} ${s2}% ${l2}%) 0%, transparent 62%)`,
			`linear-gradient(140deg, hsl(${h3} 30% ${l3}%), hsl(${h1} 26% ${Math.max(8, l3 - 4)}%))`,
		].join(','),
	};
}
