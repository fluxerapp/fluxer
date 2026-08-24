// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {join} from 'node:path';
import sharp from 'sharp';
import {convertToCodePoints} from '../src/features/expressions/utils/EmojiCodepointUtils';

const EMOJI_SPRITES = {
	basePerRow: 42,
	skinTonePerRow: 10,
	pickerPerRow: 11,
	pickerCount: 50,
} as const;
const EMOJI_SIZE = 32;
const SPRITE_SCALES = [1, 2] as const;
const TWEMOJI_LOCAL_DIR = join(import.meta.dirname, '..', '..', 'fluxer_static', 'emoji');

interface EmojiObject {
	surrogates: string;
	skins?: Array<{
		surrogates: string;
	}>;
}

interface EmojiEntry {
	surrogates: string;
}

const svgCache = new Map<string, string | null>();

function loadLocalTwemojiSVG(codepoint: string): string | null {
	if (svgCache.has(codepoint)) {
		return svgCache.get(codepoint) ?? null;
	}
	const path = join(TWEMOJI_LOCAL_DIR, `${codepoint}.svg`);
	try {
		const body = readFileSync(path, 'utf-8');
		svgCache.set(codepoint, body);
		return body;
	} catch {
		svgCache.set(codepoint, null);
		return null;
	}
}

function fixSVGSize(svg: string, size: number): string {
	return svg.replace(/<svg([^>]*)>/i, `<svg$1 width="${size}" height="${size}">`);
}

async function renderSVGToBuffer(svgContent: string, size: number): Promise<Buffer> {
	const fixed = fixSVGSize(svgContent, size);
	return sharp(Buffer.from(fixed)).resize(size, size).png().toBuffer();
}

function hslToRgb(h: number, s: number, l: number): [number, number, number] {
	h = ((h % 360) + 360) % 360;
	h /= 360;
	let r: number, g: number, b: number;
	if (s === 0) {
		r = g = b = l;
	} else {
		const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
		const p = 2 * l - q;
		const hueToRgb = (p: number, q: number, t: number): number => {
			if (t < 0) t += 1;
			if (t > 1) t -= 1;
			if (t < 1 / 6) return p + (q - p) * 6 * t;
			if (t < 1 / 2) return q;
			if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;
			return p;
		};
		r = hueToRgb(p, q, h + 1 / 3);
		g = hueToRgb(p, q, h);
		b = hueToRgb(p, q, h - 1 / 3);
	}
	return [
		Math.round(Math.min(1, Math.max(0, r)) * 255),
		Math.round(Math.min(1, Math.max(0, g)) * 255),
		Math.round(Math.min(1, Math.max(0, b)) * 255),
	];
}

async function createPlaceholder(size: number): Promise<Buffer> {
	const h = Math.random() * 360;
	const [r, g, b] = hslToRgb(h, 0.7, 0.6);
	const radius = Math.floor(size * 0.4);
	const cx = Math.floor(size / 2);
	const cy = Math.floor(size / 2);
	const svg = `<svg width="${size}" height="${size}" xmlns="http://www.w3.org/2000/svg">
    <circle cx="${cx}" cy="${cy}" r="${radius}" fill="rgb(${r},${g},${b})"/>
  </svg>`;
	return sharp(Buffer.from(svg)).png().toBuffer();
}

async function loadEmojiImage(surrogate: string, size: number): Promise<Buffer> {
	const codepoint = convertToCodePoints(surrogate);
	const svg = loadLocalTwemojiSVG(codepoint);
	if (svg) {
		try {
			return await renderSVGToBuffer(svg, size);
		} catch (error) {
			console.error(`Failed to render SVG for ${codepoint}:`, error);
		}
	}
	if (codepoint.includes('-200d-')) {
		const basePart = codepoint.split('-200d-')[0];
		const baseSvg = loadLocalTwemojiSVG(basePart);
		if (baseSvg) {
			try {
				return await renderSVGToBuffer(baseSvg, size);
			} catch (error) {
				console.error(`Failed to render base SVG for ${basePart}:`, error);
			}
		}
	}
	console.error(`Missing SVG for ${codepoint} (${surrogate}), using placeholder`);
	return createPlaceholder(size);
}

async function renderSpriteSheet(
	emojiEntries: Array<EmojiEntry>,
	perRow: number,
	fileNameBase: string,
	outputDir: string,
): Promise<void> {
	if (perRow <= 0) {
		throw new Error('perRow must be > 0');
	}
	const rows = Math.ceil(emojiEntries.length / perRow);
	for (const scale of SPRITE_SCALES) {
		const size = EMOJI_SIZE * scale;
		const dstW = perRow * size;
		const dstH = rows * size;
		const compositeOps: Array<sharp.OverlayOptions> = [];
		for (let i = 0; i < emojiEntries.length; i++) {
			const item = emojiEntries[i];
			const emojiBuffer = await loadEmojiImage(item.surrogates, size);
			const row = Math.floor(i / perRow);
			const col = i % perRow;
			const x = col * size;
			const y = row * size;
			compositeOps.push({
				input: emojiBuffer,
				left: x,
				top: y,
			});
		}
		const sheet = await sharp({
			create: {
				width: dstW,
				height: dstH,
				channels: 4,
				background: {r: 0, g: 0, b: 0, alpha: 0},
			},
		})
			.composite(compositeOps)
			.png()
			.toBuffer();
		const suffix = scale !== 1 ? `@${scale}x` : '';
		const outPath = join(outputDir, `${fileNameBase}${suffix}.png`);
		writeFileSync(outPath, sheet);
		console.log(`Wrote ${outPath}`);
	}
}

async function generateMainSpriteSheet(
	emojiData: Record<string, Array<EmojiObject>>,
	outputDir: string,
): Promise<void> {
	const base: Array<EmojiEntry> = [];
	for (const objs of Object.values(emojiData)) {
		for (const obj of objs) {
			base.push({surrogates: obj.surrogates});
		}
	}
	await renderSpriteSheet(base, EMOJI_SPRITES.basePerRow, 'spritesheet-emoji', outputDir);
}

async function generateSkinToneSpriteSheets(
	emojiData: Record<string, Array<EmojiObject>>,
	outputDir: string,
): Promise<void> {
	const skinTones = ['\u{1F3FB}', '\u{1F3FC}', '\u{1F3FD}', '\u{1F3FE}', '\u{1F3FF}'];
	for (let skinIndex = 0; skinIndex < skinTones.length; skinIndex++) {
		const skinTone = skinTones[skinIndex];
		const skinCodepoint = convertToCodePoints(skinTone);
		const skinEntries: Array<EmojiEntry> = [];
		for (const objs of Object.values(emojiData)) {
			for (const obj of objs) {
				if (obj.skins && obj.skins.length > skinIndex && obj.skins[skinIndex].surrogates) {
					skinEntries.push({surrogates: obj.skins[skinIndex].surrogates});
				}
			}
		}
		if (skinEntries.length === 0) {
			continue;
		}
		await renderSpriteSheet(skinEntries, EMOJI_SPRITES.skinTonePerRow, `spritesheet-${skinCodepoint}`, outputDir);
	}
}

async function generatePickerSpriteSheet(outputDir: string): Promise<void> {
	const basicEmojis = [
		'\u{1F600}',
		'\u{1F603}',
		'\u{1F604}',
		'\u{1F601}',
		'\u{1F606}',
		'\u{1F605}',
		'\u{1F602}',
		'\u{1F923}',
		'\u{1F60A}',
		'\u{1F607}',
		'\u{1F642}',
		'\u{1F609}',
		'\u{1F60C}',
		'\u{1F60D}',
		'\u{1F970}',
		'\u{1F618}',
		'\u{1F617}',
		'\u{1F619}',
		'\u{1F61A}',
		'\u{1F60B}',
		'\u{1F61B}',
		'\u{1F61D}',
		'\u{1F61C}',
		'\u{1F92A}',
		'\u{1F928}',
		'\u{1F9D0}',
		'\u{1F913}',
		'\u{1F60E}',
		'\u{1F973}',
		'\u{1F60F}',
	];
	const entries: Array<EmojiEntry> = basicEmojis.map((e) => ({surrogates: e}));
	await renderSpriteSheet(entries, EMOJI_SPRITES.pickerPerRow, 'spritesheet-picker', outputDir);
}

async function main(): Promise<void> {
	const scriptDir = import.meta.dirname;
	const appDir = join(scriptDir, '..');
	const outputDir = join(appDir, 'src', 'media', 'images', 'emoji-sprites');
	mkdirSync(outputDir, {recursive: true});
	const emojiDataPath = join(appDir, 'src', 'media', 'data', 'emojis.json');
	const emojiData: Record<string, Array<EmojiObject>> = JSON.parse(readFileSync(emojiDataPath, 'utf-8'));
	console.log('Generating main sprite sheet...');
	await generateMainSpriteSheet(emojiData, outputDir);
	console.log('Generating skin tone sprite sheets...');
	await generateSkinToneSpriteSheets(emojiData, outputDir);
	console.log('Generating picker sprite sheet...');
	await generatePickerSpriteSheet(outputDir);
	console.log('Emoji sprites generated successfully.');
}

main().catch((err) => {
	console.error('Error:', err);
	process.exit(1);
});
