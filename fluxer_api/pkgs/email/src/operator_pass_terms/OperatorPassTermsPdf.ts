// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getOperatorPassTermsBlocks,
	getOperatorPassTermsTitle,
	getOperatorPassTermsUrl,
	getOperatorPassTermsVersionLabel,
	getOperatorPassTraderLines,
	type OperatorPassTermsLanguage,
	toPlainTermsText,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import type {OperatorPassTermsBlock} from '@pkgs/email/src/operator_pass_terms/OperatorPassTermsText';
import {PDFDocument, type PDFFont, type PDFPage, rgb, StandardFonts} from 'pdf-lib';

const PAGE_WIDTH = 595.28;
const PAGE_HEIGHT = 841.89;
const MARGIN = 56;
const CONTENT_WIDTH = PAGE_WIDTH - MARGIN * 2;
const BODY_SIZE = 12;
const LINE_GAP = 4;
const TEXT_COLOR = rgb(0.1, 0.1, 0.12);
const MUTED_COLOR = rgb(0.35, 0.35, 0.4);
const BOLD_MARK = '**';

interface Fonts {
	regular: PDFFont;
	bold: PDFFont;
	italic: PDFFont;
}

interface TextStyle {
	font: PDFFont;
	size: number;
	color: ReturnType<typeof rgb>;
	indent: number;
	spaceBefore: number;
}

interface Segment {
	text: string;
	font: PDFFont;
}

type Word = ReadonlyArray<Segment>;

function toWords(text: string, regular: PDFFont, bold: PDFFont): Array<Word> {
	const words: Array<Word> = [];
	let current: Array<Segment> = [];
	text.split(BOLD_MARK).forEach((run, index) => {
		const font = index % 2 === 1 ? bold : regular;
		for (const part of run.split(/(\s+)/u)) {
			if (/^\s+$/u.test(part)) {
				if (current.length > 0) words.push(current);
				current = [];
			} else if (part) {
				current.push({text: part, font});
			}
		}
	});
	if (current.length > 0) words.push(current);
	return words;
}

function wordWidth(word: Word, size: number): number {
	return word.reduce((width, segment) => width + segment.font.widthOfTextAtSize(segment.text, size), 0);
}

function wrapWords(words: ReadonlyArray<Word>, size: number, space: number, width: number): Array<Array<Word>> {
	const lines: Array<Array<Word>> = [];
	let current: Array<Word> = [];
	let currentWidth = 0;
	for (const word of words) {
		const added = wordWidth(word, size);
		if (current.length === 0 || currentWidth + space + added <= width) {
			currentWidth += (current.length > 0 ? space : 0) + added;
			current.push(word);
			continue;
		}
		lines.push(current);
		current = [word];
		currentWidth = added;
	}
	if (current.length > 0) {
		lines.push(current);
	}
	return lines;
}

class PdfWriter {
	private page: PDFPage;
	private cursor: number;

	constructor(
		private readonly document: PDFDocument,
		private readonly bold: PDFFont,
	) {
		this.page = document.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
		this.cursor = PAGE_HEIGHT - MARGIN;
	}

	write(text: string, style: TextStyle, firstLinePrefix = ''): void {
		const lineHeight = style.size + LINE_GAP;
		const space = style.font.widthOfTextAtSize(' ', style.size);
		const prefixWidth = firstLinePrefix ? style.font.widthOfTextAtSize(firstLinePrefix, style.size) : 0;
		const lines = wrapWords(
			toWords(text, style.font, this.bold),
			style.size,
			space,
			CONTENT_WIDTH - style.indent - prefixWidth,
		);
		this.cursor -= style.spaceBefore;
		lines.forEach((line, index) => {
			if (this.cursor - lineHeight < MARGIN) {
				this.page = this.document.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
				this.cursor = PAGE_HEIGHT - MARGIN;
			}
			this.cursor -= lineHeight;
			let x = MARGIN + style.indent;
			if (index === 0 && firstLinePrefix) {
				this.page.drawText(firstLinePrefix, {
					x,
					y: this.cursor,
					size: style.size,
					font: style.font,
					color: style.color,
				});
			}
			x += prefixWidth;
			const runs: Array<{text: string; font: PDFFont; x: number}> = [];
			line.forEach((word, wordIndex) => {
				word.forEach((segment, segmentIndex) => {
					const previous = runs.at(-1);
					if (previous && previous.font === segment.font) {
						previous.text += (wordIndex > 0 && segmentIndex === 0 ? ' ' : '') + segment.text;
					} else {
						runs.push({text: segment.text, font: segment.font, x});
					}
					x += segment.font.widthOfTextAtSize(segment.text, style.size);
				});
				x += space;
			});
			for (const run of runs) {
				this.page.drawText(run.text, {
					x: run.x,
					y: this.cursor,
					size: style.size,
					font: run.font,
					color: style.color,
				});
			}
		});
	}
}

function styleFor(block: OperatorPassTermsBlock, fonts: Fonts): TextStyle {
	switch (block.kind) {
		case 'heading':
			return {font: fonts.bold, size: 15, color: TEXT_COLOR, indent: 0, spaceBefore: 14};
		case 'subheading':
			return {font: fonts.bold, size: 13, color: TEXT_COLOR, indent: 0, spaceBefore: 10};
		case 'bullet':
			return {font: fonts.regular, size: BODY_SIZE, color: TEXT_COLOR, indent: 10, spaceBefore: 4};
		case 'quote':
			return {font: fonts.italic, size: BODY_SIZE, color: MUTED_COLOR, indent: 14, spaceBefore: 8};
		default:
			return {font: fonts.regular, size: BODY_SIZE, color: TEXT_COLOR, indent: 0, spaceBefore: 8};
	}
}

export function getOperatorPassTermsPdfFilename(language: OperatorPassTermsLanguage, version: string): string {
	return `fluxer-operator-pass-terms-${version}-${language}.pdf`;
}

export async function renderOperatorPassTermsPdf(
	language: OperatorPassTermsLanguage,
	version: string,
	marketingBaseUrl: string,
): Promise<Uint8Array> {
	const blocks = getOperatorPassTermsBlocks(language, version);
	const document = await PDFDocument.create();
	const title = getOperatorPassTermsTitle(language);
	const issuedAt = new Date(`${version}T00:00:00Z`);
	document.setTitle(`${title} (${version})`);
	document.setAuthor('Fluxer Platform AB');
	document.setLanguage(language);
	document.setCreator('Fluxer');
	document.setProducer('Fluxer');
	document.setCreationDate(issuedAt);
	document.setModificationDate(issuedAt);
	const fonts: Fonts = {
		regular: await document.embedFont(StandardFonts.Helvetica),
		bold: await document.embedFont(StandardFonts.HelveticaBold),
		italic: await document.embedFont(StandardFonts.HelveticaOblique),
	};
	const writer = new PdfWriter(document, fonts.bold);
	writer.write(title, {font: fonts.bold, size: 20, color: TEXT_COLOR, indent: 0, spaceBefore: 0});
	writer.write(getOperatorPassTermsVersionLabel(language, version), {
		font: fonts.regular,
		size: BODY_SIZE,
		color: MUTED_COLOR,
		indent: 0,
		spaceBefore: 6,
	});
	writer.write(getOperatorPassTermsUrl(marketingBaseUrl), {
		font: fonts.regular,
		size: BODY_SIZE,
		color: MUTED_COLOR,
		indent: 0,
		spaceBefore: 2,
	});
	for (const block of blocks) {
		const style = styleFor(block, fonts);
		writer.write(toPlainTermsText(block.text, marketingBaseUrl), style, block.kind === 'bullet' ? '• ' : '');
	}
	const traderStyle = {font: fonts.regular, size: BODY_SIZE, color: MUTED_COLOR, indent: 0, spaceBefore: 2};
	getOperatorPassTraderLines(language).forEach((line, index) => {
		writer.write(line, {...traderStyle, spaceBefore: index === 0 ? 18 : 2});
	});
	return document.save({useObjectStreams: false});
}
