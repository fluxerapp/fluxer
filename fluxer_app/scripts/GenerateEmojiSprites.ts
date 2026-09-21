// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {join} from 'node:path';
import {convertToCodePoints} from '@app/features/expressions/utils/EmojiCodepointUtils';
import sharp, {type OverlayOptions} from 'sharp';

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

async function loadEmojiImage(surrogate: string, size: number): Promise<Buffer> {
	const codepoint = convertToCodePoints(surrogate);
	const svg = loadLocalTwemojiSVG(codepoint);
	if (svg == null) {
		throw new Error(`Missing SVG for ${codepoint} (${surrogate})`);
	}
	return renderSVGToBuffer(svg, size);
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
		const compositeOps: Array<OverlayOptions> = [];
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
	categories: Record<string, Array<EmojiObject>>,
	outputDir: string,
): Promise<void> {
	const base: Array<EmojiEntry> = [];
	for (const objs of Object.values(categories)) {
		for (const obj of objs) {
			base.push({surrogates: obj.surrogates});
		}
	}
	await renderSpriteSheet(base, EMOJI_SPRITES.basePerRow, 'spritesheet-emoji', outputDir);
}

async function generateSkinToneSpriteSheets(
	categories: Record<string, Array<EmojiObject>>,
	outputDir: string,
): Promise<void> {
	const skinTones = ['\u{1F3FB}', '\u{1F3FC}', '\u{1F3FD}', '\u{1F3FE}', '\u{1F3FF}'];
	for (let skinIndex = 0; skinIndex < skinTones.length; skinIndex++) {
		const skinTone = skinTones[skinIndex];
		const skinCodepoint = convertToCodePoints(skinTone);
		const skinEntries: Array<EmojiEntry> = [];
		for (const objs of Object.values(categories)) {
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
	const emojiData: {categories: Record<string, Array<EmojiObject>>} = JSON.parse(readFileSync(emojiDataPath, 'utf-8'));
	console.log('Generating main sprite sheet...');
	await generateMainSpriteSheet(emojiData.categories, outputDir);
	console.log('Generating skin tone sprite sheets...');
	await generateSkinToneSpriteSheets(emojiData.categories, outputDir);
	console.log('Generating picker sprite sheet...');
	await generatePickerSpriteSheet(outputDir);
	console.log('Emoji sprites generated successfully.');
}

main().catch((err) => {
	console.error('Error:', err);
	process.exit(1);
});
