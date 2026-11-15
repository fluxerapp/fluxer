// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFile} from 'node:fs/promises';
import bidiFactory from 'bidi-js';
import type {Blob as HarfBuzzBlob, Font as HarfBuzzFont} from 'harfbuzzjs';
import {
	appendBezierCurve,
	beginText,
	type Color,
	closePath,
	endText,
	fill,
	fillAndStroke,
	lineTo,
	moveTo,
	type PDFDocument,
	PDFHexString,
	PDFName,
	type PDFOperator,
	type PDFPage,
	PDFString,
	popGraphicsState,
	pushGraphicsState,
	setCharacterSqueeze,
	setFillingColor,
	setFontAndSize,
	setLineWidth,
	setStrokingColor,
	setTextMatrix,
	setTextRenderingMode,
	showText,
	TextRenderingMode,
} from 'pdf-lib';

declare const FLUXER_BUNDLED_FONTS_DIR: string | undefined;

const CJK_FONT = 'NotoSansCJK-Bold.ttc';
const CJK_FACE_JAPANESE = 0;
const CJK_FACE_KOREAN = 1;
const CJK_FACE_CHINESE = 2;
const DEFAULT_FONT = 'NotoSans-Bold.ttf';
const SYNTHETIC_BOLD_STROKE = 0.028;
const TEXT_LAYER_FONT = 'FxName';
const TEXT_LAYER_ADVANCE = 0.5;

const SCRIPT_FONTS: Readonly<Record<string, string>> = {
	Latin: DEFAULT_FONT,
	Greek: DEFAULT_FONT,
	Cyrillic: DEFAULT_FONT,
	Han: CJK_FONT,
	Hiragana: CJK_FONT,
	Katakana: CJK_FONT,
	Hangul: CJK_FONT,
	Bopomofo: CJK_FONT,
	Adlam: 'NotoSansAdlam-Bold.ttf',
	Arabic: 'NotoSansArabic-Bold.ttf',
	Armenian: 'NotoSansArmenian-Bold.ttf',
	Balinese: 'NotoSansBalinese-Bold.ttf',
	Bamum: 'NotoSansBamum-Bold.ttf',
	Bassa_Vah: 'NotoSansBassaVah-Bold.ttf',
	Batak: 'NotoSansBatak-Regular.ttf',
	Bengali: 'NotoSansBengali-Bold.ttf',
	Buginese: 'NotoSansBuginese-Regular.ttf',
	Canadian_Aboriginal: 'NotoSansCanadianAboriginal-Bold.ttf',
	Chakma: 'NotoSansChakma-Regular.ttf',
	Cham: 'NotoSansCham-Bold.ttf',
	Cherokee: 'NotoSansCherokee-Bold.ttf',
	Devanagari: 'NotoSansDevanagari-Bold.ttf',
	Ethiopic: 'NotoSansEthiopic-Bold.ttf',
	Georgian: 'NotoSansGeorgian-Bold.ttf',
	Gujarati: 'NotoSansGujarati-Bold.ttf',
	Gunjala_Gondi: 'NotoSansGunjalaGondi-Bold.ttf',
	Gurmukhi: 'NotoSansGurmukhi-Bold.ttf',
	Hanifi_Rohingya: 'NotoSansHanifiRohingya-Bold.ttf',
	Hebrew: 'NotoSansHebrew-Bold.ttf',
	Javanese: 'NotoSansJavanese-Bold.ttf',
	Kannada: 'NotoSansKannada-Bold.ttf',
	Kayah_Li: 'NotoSansKayahLi-Bold.ttf',
	Khmer: 'NotoSansKhmer-Bold.ttf',
	Lao: 'NotoSansLao-Bold.ttf',
	Lepcha: 'NotoSansLepcha-Regular.ttf',
	Limbu: 'NotoSansLimbu-Regular.ttf',
	Lisu: 'NotoSansLisu-Bold.ttf',
	Malayalam: 'NotoSansMalayalam-Bold.ttf',
	Mandaic: 'NotoSansMandaic-Regular.ttf',
	Masaram_Gondi: 'NotoSansMasaramGondi-Regular.ttf',
	Medefaidrin: 'NotoSansMedefaidrin-Bold.ttf',
	Meetei_Mayek: 'NotoSansMeeteiMayek-Bold.ttf',
	Mende_Kikakui: 'NotoSansMendeKikakui-Regular.ttf',
	Miao: 'NotoSansMiao-Regular.ttf',
	Mongolian: 'NotoSansMongolian-Regular.ttf',
	Myanmar: 'NotoSansMyanmar-Bold.ttf',
	Nag_Mundari: 'NotoSansNagMundari-Bold.ttf',
	New_Tai_Lue: 'NotoSansNewTaiLue-Bold.ttf',
	Newa: 'NotoSansNewa-Regular.ttf',
	Nko: 'NotoSansNKo-Regular.ttf',
	Nyiakeng_Puachue_Hmong: 'NotoSerifNPHmong-Bold.ttf',
	Ol_Chiki: 'NotoSansOlChiki-Bold.ttf',
	Oriya: 'NotoSansOriya-Bold.ttf',
	Osage: 'NotoSansOsage-Regular.ttf',
	Pahawh_Hmong: 'NotoSansPahawhHmong-Regular.ttf',
	Saurashtra: 'NotoSansSaurashtra-Regular.ttf',
	Sinhala: 'NotoSansSinhala-Bold.ttf',
	Sora_Sompeng: 'NotoSansSoraSompeng-Bold.ttf',
	Sundanese: 'NotoSansSundanese-Bold.ttf',
	Sunuwar: 'NotoSansSunuwar-Regular.ttf',
	Syloti_Nagri: 'NotoSansSylotiNagri-Regular.ttf',
	Syriac: 'NotoSansSyriac-Regular.ttf',
	Tai_Le: 'NotoSansTaiLe-Regular.ttf',
	Tai_Tham: 'NotoSansTaiTham-Bold.ttf',
	Tai_Viet: 'NotoSansTaiViet-Regular.ttf',
	Tamil: 'NotoSansTamil-Bold.ttf',
	Tangsa: 'NotoSansTangsa-Bold.ttf',
	Telugu: 'NotoSansTelugu-Bold.ttf',
	Thaana: 'NotoSansThaana-Bold.ttf',
	Thai: 'NotoSansThai-Bold.ttf',
	Tibetan: 'NotoSerifTibetan-Bold.ttf',
	Tifinagh: 'NotoSansTifinagh-Regular.ttf',
	Toto: 'NotoSerifToto-Bold.ttf',
	Vai: 'NotoSansVai-Regular.ttf',
	Wancho: 'NotoSansWancho-Regular.ttf',
	Warang_Citi: 'NotoSansWarangCiti-Regular.ttf',
	Yi: 'NotoSansYi-Regular.ttf',
};

export const OPERATOR_PASS_CERTIFICATE_NAME_SCRIPTS: ReadonlyArray<string> = Object.keys(SCRIPT_FONTS);

const SCRIPT_PATTERNS = OPERATOR_PASS_CERTIFICATE_NAME_SCRIPTS.map(
	(script) => [script, new RegExp(`^\\p{Script=${script}}$`, 'u')] as const,
);
const NEUTRAL = /^[\p{Script=Common}\p{Script=Inherited}]$/u;
const DEFAULT_IGNORABLE = /^\p{Default_Ignorable_Code_Point}$/u;
const KANA = /[\p{Script=Hiragana}\p{Script=Katakana}]/u;
const HANGUL = /\p{Script=Hangul}/u;

interface NameFont {
	font: HarfBuzzFont;
	unitsPerEm: number;
	codePoints: ReadonlySet<number>;
	syntheticBold: boolean;
}

interface PlacedGlyph {
	font: NameFont;
	path: string;
	x: number;
	y: number;
}

export interface ShapedOperatorPassCertificateName {
	text: string;
	glyphs: ReadonlyArray<PlacedGlyph>;
	width: number;
	ascent: number;
	descent: number;
}

type HarfBuzz = typeof import('harfbuzzjs');

let harfBuzzPromise: Promise<HarfBuzz> | null = null;
const blobPromises = new Map<string, Promise<HarfBuzzBlob>>();
const fontPromises = new Map<string, Promise<NameFont>>();
let bidiInstance: ReturnType<typeof bidiFactory> | null = null;

function bidi(): ReturnType<typeof bidiFactory> {
	bidiInstance ??= bidiFactory();
	return bidiInstance;
}

export function operatorPassCertificateFontsDirectory(): URL {
	if (typeof FLUXER_BUNDLED_FONTS_DIR === 'string') {
		return new URL(FLUXER_BUNDLED_FONTS_DIR, import.meta.url);
	}
	return new URL('./fonts/', import.meta.url);
}

function loadHarfBuzz(): Promise<HarfBuzz> {
	harfBuzzPromise ??= import('harfbuzzjs');
	return harfBuzzPromise;
}

function loadBlob(file: string): Promise<HarfBuzzBlob> {
	let promise = blobPromises.get(file);
	if (!promise) {
		promise = (async () => {
			const harfBuzz = await loadHarfBuzz();
			const bytes = await readFile(new URL(file, operatorPassCertificateFontsDirectory()));
			return new harfBuzz.Blob(new Uint8Array(bytes));
		})();
		blobPromises.set(file, promise);
	}
	return promise;
}

function loadFont(file: string, faceIndex: number): Promise<NameFont> {
	const key = `${file}#${faceIndex}`;
	let promise = fontPromises.get(key);
	if (!promise) {
		promise = (async () => {
			const harfBuzz = await loadHarfBuzz();
			const face = new harfBuzz.Face(await loadBlob(file), faceIndex);
			return {
				font: new harfBuzz.Font(face),
				unitsPerEm: face.upem,
				codePoints: new Set(face.collectUnicodes()),
				syntheticBold: file.endsWith('-Regular.ttf'),
			};
		})();
		fontPromises.set(key, promise);
	}
	return promise;
}

function scriptOf(character: string): string | null {
	for (const [script, pattern] of SCRIPT_PATTERNS) {
		if (pattern.test(character)) {
			return script;
		}
	}
	return null;
}

function cjkFaceFor(text: string): number {
	if (KANA.test(text)) {
		return CJK_FACE_JAPANESE;
	}
	return HANGUL.test(text) ? CJK_FACE_KOREAN : CJK_FACE_CHINESE;
}

function covers(font: NameFont, character: string): boolean {
	return font.codePoints.has(character.codePointAt(0) ?? 0) || DEFAULT_IGNORABLE.test(character);
}

async function assignFonts(characters: ReadonlyArray<string>, text: string): Promise<Array<NameFont | null>> {
	const cjkFace = cjkFaceFor(text);
	const fonts: Array<NameFont | null> = [];
	const neutral: Array<boolean> = [];
	for (const character of characters) {
		const script = NEUTRAL.test(character) ? null : scriptOf(character);
		const file = script ? SCRIPT_FONTS[script] : undefined;
		neutral.push(!script && NEUTRAL.test(character));
		fonts.push(file ? await loadFont(file, file === CJK_FONT ? cjkFace : 0) : null);
	}
	const fallback = await loadFont(DEFAULT_FONT, 0);
	for (let index = 0; index < characters.length; index++) {
		const character = characters[index];
		if (!neutral[index] || character === undefined) {
			continue;
		}
		const previous = index > 0 ? fonts[index - 1] : null;
		const next = fonts.slice(index + 1).find((font) => font != null) ?? null;
		const candidates = [previous, next, fallback];
		fonts[index] = candidates.find((font) => font != null && covers(font, character)) ?? null;
	}
	return fonts.map((font, index) => {
		const character = characters[index];
		return font && character !== undefined && covers(font, character) ? font : null;
	});
}

export async function canPrintOperatorPassCertificateName(text: string): Promise<boolean> {
	const fonts = await assignFonts(Array.from(text), text);
	return fonts.every((font) => font != null);
}

interface ShapedRun {
	level: number;
	glyphs: Array<{font: NameFont; glyph: number; advance: number; offsetX: number; offsetY: number}>;
}

function reorderRuns(runs: ReadonlyArray<ShapedRun>): Array<ShapedRun> {
	const ordered = [...runs];
	const highest = Math.max(0, ...ordered.map((run) => run.level));
	const lowestOdd = Math.min(...ordered.map((run) => run.level).filter((level) => level % 2 === 1), highest + 1);
	for (let level = highest; level >= lowestOdd; level--) {
		let start = 0;
		while (start < ordered.length) {
			if ((ordered[start]?.level ?? 0) < level) {
				start++;
				continue;
			}
			let end = start;
			while (end < ordered.length && (ordered[end]?.level ?? 0) >= level) {
				end++;
			}
			ordered.splice(start, end - start, ...ordered.slice(start, end).reverse());
			start = end;
		}
	}
	return ordered;
}

export async function shapeOperatorPassCertificateName(text: string): Promise<ShapedOperatorPassCertificateName> {
	const harfBuzz = await loadHarfBuzz();
	const characters = Array.from(text);
	const fonts = await assignFonts(characters, text);
	const {levels} = bidi().getEmbeddingLevels(text, 'auto');
	const runs: Array<ShapedRun> = [];
	let offset = 0;
	let runStart = 0;
	let runOffset = 0;
	for (let index = 0; index <= characters.length; index++) {
		const character = characters[index];
		const runFont = fonts[runStart];
		const runLevel = levels[runOffset] ?? 0;
		if (index > runStart && (character === undefined || fonts[index] !== runFont || levels[offset] !== runLevel)) {
			if (!runFont) {
				throw new Error('The certificate name has a character no certificate font can print');
			}
			const buffer = new harfBuzz.Buffer();
			buffer.addText(text, runOffset, offset - runOffset);
			buffer.setDirection(runLevel % 2 === 1 ? harfBuzz.Direction.RTL : harfBuzz.Direction.LTR);
			buffer.guessSegmentProperties();
			harfBuzz.shape(runFont.font, buffer);
			runs.push({
				level: runLevel,
				glyphs: buffer.getGlyphInfosAndPositions().map((glyph) => ({
					font: runFont,
					glyph: glyph.codepoint,
					advance: (glyph.xAdvance ?? 0) / runFont.unitsPerEm,
					offsetX: (glyph.xOffset ?? 0) / runFont.unitsPerEm,
					offsetY: (glyph.yOffset ?? 0) / runFont.unitsPerEm,
				})),
			});
			runStart = index;
			runOffset = offset;
		}
		offset += character?.length ?? 0;
	}
	const glyphs: Array<PlacedGlyph> = [];
	let x = 0;
	let ascent = 0;
	let descent = 0;
	for (const run of reorderRuns(runs)) {
		for (const glyph of run.glyphs) {
			const path = glyph.font.font.glyphToPath(glyph.glyph);
			for (const [, y] of path.matchAll(/-?[\d.]+,(-?[\d.]+)/g)) {
				const height = Number(y) / glyph.font.unitsPerEm + glyph.offsetY;
				ascent = Math.max(ascent, height);
				descent = Math.max(descent, -height);
			}
			glyphs.push({font: glyph.font, path, x: x + glyph.offsetX, y: glyph.offsetY});
			x += glyph.advance;
		}
	}
	return {text, glyphs, width: x, ascent, descent};
}

function round(value: number): number {
	return Math.round(value * 1000) / 1000;
}

function glyphOperators(path: string, place: (x: number, y: number) => [number, number]): Array<PDFOperator> {
	const operators: Array<PDFOperator> = [];
	let current: [number, number] = [0, 0];
	for (const [, command, values] of path.matchAll(/([MLQCZ])([^MLQCZ]*)/g)) {
		const numbers = (values ?? '')
			.split(/[\s,]+/)
			.filter((value) => value.length > 0)
			.map(Number);
		switch (command) {
			case 'M':
				current = [numbers[0] ?? 0, numbers[1] ?? 0];
				operators.push(moveTo(...place(...current)));
				break;
			case 'L':
				current = [numbers[0] ?? 0, numbers[1] ?? 0];
				operators.push(lineTo(...place(...current)));
				break;
			case 'Q': {
				const control: [number, number] = [numbers[0] ?? 0, numbers[1] ?? 0];
				const end: [number, number] = [numbers[2] ?? 0, numbers[3] ?? 0];
				operators.push(
					appendBezierCurve(
						...place(
							current[0] + (2 / 3) * (control[0] - current[0]),
							current[1] + (2 / 3) * (control[1] - current[1]),
						),
						...place(end[0] + (2 / 3) * (control[0] - end[0]), end[1] + (2 / 3) * (control[1] - end[1])),
						...place(...end),
					),
				);
				current = end;
				break;
			}
			case 'C':
				operators.push(
					appendBezierCurve(
						...place(numbers[0] ?? 0, numbers[1] ?? 0),
						...place(numbers[2] ?? 0, numbers[3] ?? 0),
						...place(numbers[4] ?? 0, numbers[5] ?? 0),
					),
				);
				current = [numbers[4] ?? 0, numbers[5] ?? 0];
				break;
			case 'Z':
				operators.push(closePath());
				break;
		}
	}
	return operators;
}

function toUnicodeCMap(codePoints: ReadonlyArray<number>): string {
	const entries = codePoints.map((codePoint, index) => {
		const encoded = String.fromCodePoint(codePoint);
		const units = Array.from({length: encoded.length}, (_, position) =>
			encoded.charCodeAt(position).toString(16).padStart(4, '0'),
		);
		return `<${(index + 1).toString(16).padStart(4, '0')}> <${units.join('')}>`;
	});
	const blocks: Array<string> = [];
	for (let start = 0; start < entries.length; start += 100) {
		const block = entries.slice(start, start + 100);
		blocks.push(`${block.length} beginbfchar\n${block.join('\n')}\nendbfchar`);
	}
	return [
		'/CIDInit /ProcSet findresource begin',
		'12 dict begin',
		'begincmap',
		'/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def',
		'/CMapName /Adobe-Identity-UCS def',
		'/CMapType 2 def',
		'1 begincodespacerange',
		'<0000> <FFFF>',
		'endcodespacerange',
		...blocks,
		'endcmap',
		'CMapName currentdict /CMap defineresource pop',
		'end',
		'end',
	].join('\n');
}

function addTextLayerFont(document: PDFDocument, codePoints: ReadonlyArray<number>) {
	const context = document.context;
	const descriptor = context.register(
		context.obj({
			Type: 'FontDescriptor',
			FontName: TEXT_LAYER_FONT,
			Flags: 4,
			FontBBox: [0, 0, 500, 1000],
			ItalicAngle: 0,
			Ascent: 1000,
			Descent: 0,
			CapHeight: 1000,
			StemV: 80,
		}),
	);
	const descendant = context.register(
		context.obj({
			Type: 'Font',
			Subtype: 'CIDFontType2',
			BaseFont: TEXT_LAYER_FONT,
			CIDSystemInfo: {Registry: PDFString.of('Adobe'), Ordering: PDFString.of('Identity'), Supplement: 0},
			FontDescriptor: descriptor,
			DW: TEXT_LAYER_ADVANCE * 1000,
			CIDToGIDMap: 'Identity',
		}),
	);
	const toUnicode = context.register(context.flateStream(toUnicodeCMap(codePoints)));
	return context.register(
		context.obj({
			Type: 'Font',
			Subtype: 'Type0',
			BaseFont: TEXT_LAYER_FONT,
			Encoding: 'Identity-H',
			DescendantFonts: [descendant],
			ToUnicode: toUnicode,
		}),
	);
}

export function drawOperatorPassCertificateName(
	document: PDFDocument,
	page: PDFPage,
	name: ShapedOperatorPassCertificateName,
	options: {centerX: number; baseline: number; size: number; color: Color},
): void {
	const {centerX, baseline, size, color} = options;
	const left = centerX - (name.width * size) / 2;
	const operators: Array<PDFOperator> = [
		pushGraphicsState(),
		setFillingColor(color),
		setStrokingColor(color),
		setLineWidth(round(size * SYNTHETIC_BOLD_STROKE)),
	];
	for (const glyph of name.glyphs) {
		const scale = size / glyph.font.unitsPerEm;
		const place = (x: number, y: number): [number, number] => [
			round(left + glyph.x * size + x * scale),
			round(baseline + glyph.y * size + y * scale),
		];
		const path = glyphOperators(glyph.path, place);
		if (path.length > 0) {
			operators.push(...path, glyph.font.syntheticBold ? fillAndStroke() : fill());
		}
	}

	const visual = Array.from(bidi().getReorderedString(name.text, bidi().getEmbeddingLevels(name.text, 'auto')));
	const codePoints = [...new Set(visual.map((character) => character.codePointAt(0) ?? 0))];
	const fontKey = PDFName.of(TEXT_LAYER_FONT);
	page.node.setFontDictionary(fontKey, addTextLayerFont(document, codePoints));
	const encoded = visual
		.map((character) => (codePoints.indexOf(character.codePointAt(0) ?? 0) + 1).toString(16).padStart(4, '0'))
		.join('');
	const naturalWidth = visual.length * TEXT_LAYER_ADVANCE * size;
	operators.push(
		beginText(),
		setFontAndSize(fontKey, size),
		setTextRenderingMode(TextRenderingMode.Invisible),
		setCharacterSqueeze(round(naturalWidth > 0 ? (name.width * size * 100) / naturalWidth : 100)),
		setTextMatrix(1, 0, 0, 1, round(left), round(baseline)),
		showText(PDFHexString.of(encoded)),
		endText(),
		popGraphicsState(),
	);
	page.pushOperators(...operators);
}
