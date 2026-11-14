// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PDFDocument, PDFFont, PDFPage, RGB} from 'pdf-lib';
import QRCode from 'qrcode';

interface RecoveryKitSheetDetail {
	label: string;
	value: string;
}

export interface RecoveryKitSheet {
	productName: string;
	title: string;
	intro: string;
	details: ReadonlyArray<RecoveryKitSheetDetail>;
	keyLabel: string;
	recoveryKey: string;
	recoverUrl: string;
	qrCaption: string;
	stepsTitle: string;
	steps: ReadonlyArray<string>;
	backupCodes: {
		title: string;
		note: string;
		codes: ReadonlyArray<string>;
	} | null;
}

export function createRecoveryKitQrDataUrl(url: string): Promise<string> {
	return QRCode.toDataURL(url, {
		errorCorrectionLevel: 'M',
		margin: 0,
		width: 480,
		color: {dark: '#000000', light: '#ffffff'},
	});
}

const PAGE_WIDTH = 595.28;
const PAGE_HEIGHT = 841.89;
const MARGIN = 56;
const CONTENT_WIDTH = PAGE_WIDTH - MARGIN * 2;
const QR_SIZE = 112;
const QR_GAP = 24;
const KEY_SIZE = 17;
const KEY_BOX_HEIGHT = 44;
const LINE_HEIGHT_RATIO = 1.35;
const RASTER_SCALE = 4;

type FontKind = 'regular' | 'bold' | 'mono';

interface TextOptions {
	size: number;
	font: FontKind;
	color: RGB;
	x?: number;
	width?: number;
	after?: number;
}

function canEncode(font: PDFFont, text: string): boolean {
	try {
		font.encodeText(text);
		return true;
	} catch {
		return false;
	}
}

function wrapText(text: string, maxWidth: number, measure: (value: string) => number): Array<string> {
	const lines: Array<string> = [];
	let current = '';
	const pushWord = (word: string) => {
		if (measure(word) <= maxWidth) {
			current = word;
			return;
		}
		let chunk = '';
		for (const char of Array.from(word)) {
			if (chunk && measure(chunk + char) > maxWidth) {
				lines.push(chunk);
				chunk = char;
			} else {
				chunk += char;
			}
		}
		current = chunk;
	};
	for (const word of text.split(/\s+/).filter(Boolean)) {
		if (!current) {
			pushWord(word);
			continue;
		}
		const candidate = `${current} ${word}`;
		if (measure(candidate) <= maxWidth) {
			current = candidate;
		} else {
			lines.push(current);
			pushWord(word);
		}
	}
	if (current) {
		lines.push(current);
	}
	return lines;
}

function rgbToCss(color: RGB): string {
	return `rgb(${Math.round(color.red * 255)}, ${Math.round(color.green * 255)}, ${Math.round(color.blue * 255)})`;
}

function rasteriseLines(text: string, options: Required<Pick<TextOptions, 'size' | 'font' | 'color' | 'width'>>) {
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d');
	if (!context) {
		return [];
	}
	const family =
		options.font === 'mono'
			? 'ui-monospace, monospace'
			: window.getComputedStyle(document.body).fontFamily || 'sans-serif';
	const weight = options.font === 'regular' ? 400 : 700;
	const fontSpec = `${weight} ${options.size * RASTER_SCALE}px ${family}`;
	context.font = fontSpec;
	const maxWidth = options.width * RASTER_SCALE;
	const lines = wrapText(text, maxWidth, (value) => context.measureText(value).width);
	const lineHeight = Math.ceil(options.size * LINE_HEIGHT_RATIO * RASTER_SCALE);
	const rtl = document.documentElement.dir === 'rtl';
	return lines.map((line) => {
		const lineCanvas = document.createElement('canvas');
		const lineContext = lineCanvas.getContext('2d');
		lineCanvas.width = Math.max(1, Math.ceil(maxWidth));
		lineCanvas.height = lineHeight;
		if (lineContext) {
			lineContext.font = fontSpec;
			lineContext.fillStyle = rgbToCss(options.color);
			lineContext.textBaseline = 'middle';
			lineContext.direction = rtl ? 'rtl' : 'ltr';
			lineContext.textAlign = rtl ? 'right' : 'left';
			lineContext.fillText(line, rtl ? lineCanvas.width : 0, lineHeight / 2);
		}
		return {dataUrl: lineCanvas.toDataURL('image/png'), height: lineHeight / RASTER_SCALE};
	});
}

class SheetWriter {
	private page: PDFPage;
	y = PAGE_HEIGHT - MARGIN;

	constructor(
		private readonly pdf: PDFDocument,
		private readonly fonts: Record<FontKind, PDFFont>,
	) {
		this.page = pdf.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
	}

	get currentPage(): PDFPage {
		return this.page;
	}

	ensureSpace(height: number): void {
		if (this.y - height < MARGIN) {
			this.page = this.pdf.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
			this.y = PAGE_HEIGHT - MARGIN;
		}
	}

	async text(text: string, {size, font, color, x = MARGIN, width = CONTENT_WIDTH, after = 0}: TextOptions) {
		const pdfFont = this.fonts[font];
		const lineHeight = size * LINE_HEIGHT_RATIO;
		if (canEncode(pdfFont, text)) {
			for (const line of wrapText(text, width, (value) => pdfFont.widthOfTextAtSize(value, size))) {
				this.ensureSpace(lineHeight);
				this.page.drawText(line, {x, y: this.y - size, size, font: pdfFont, color});
				this.y -= lineHeight;
			}
		} else {
			for (const line of rasteriseLines(text, {size, font, color, width})) {
				this.ensureSpace(line.height);
				const image = await this.pdf.embedPng(line.dataUrl);
				this.page.drawImage(image, {x, y: this.y - line.height, width, height: line.height});
				this.y -= line.height;
			}
		}
		this.y -= after;
	}
}

export async function buildRecoveryKitPdf(sheet: RecoveryKitSheet): Promise<Uint8Array> {
	const {PDFDocument, StandardFonts, rgb} = await import('pdf-lib');
	const pdf = await PDFDocument.create();
	pdf.setTitle(sheet.title);
	pdf.setCreator(sheet.productName);
	pdf.setProducer(sheet.productName);
	const fonts: Record<FontKind, PDFFont> = {
		regular: await pdf.embedFont(StandardFonts.Helvetica),
		bold: await pdf.embedFont(StandardFonts.HelveticaBold),
		mono: await pdf.embedFont(StandardFonts.CourierBold),
	};
	const colours = {
		text: rgb(0.07, 0.09, 0.15),
		muted: rgb(0.36, 0.39, 0.45),
		rule: rgb(0.82, 0.84, 0.87),
		panel: rgb(0.95, 0.96, 0.97),
	};
	const writer = new SheetWriter(pdf, fonts);

	await writer.text(sheet.title, {size: 22, font: 'bold', color: colours.text, after: 8});
	await writer.text(sheet.intro, {size: 11, font: 'regular', color: colours.muted, after: 20});

	const top = writer.y;
	const qrX = PAGE_WIDTH - MARGIN - QR_SIZE;
	const qrImage = await pdf.embedPng(await createRecoveryKitQrDataUrl(sheet.recoverUrl));
	writer.currentPage.drawImage(qrImage, {x: qrX, y: top - QR_SIZE, width: QR_SIZE, height: QR_SIZE});
	writer.y = top - QR_SIZE - 6;
	await writer.text(sheet.qrCaption, {size: 8, font: 'regular', color: colours.muted, x: qrX, width: QR_SIZE});
	const qrBottom = writer.y;
	writer.y = top;
	const detailsWidth = CONTENT_WIDTH - QR_SIZE - QR_GAP;
	for (const detail of sheet.details) {
		await writer.text(detail.label, {size: 9, font: 'bold', color: colours.muted, width: detailsWidth, after: 2});
		await writer.text(detail.value, {size: 12, font: 'regular', color: colours.text, width: detailsWidth, after: 10});
	}
	writer.y = Math.min(writer.y, qrBottom) - 14;

	await writer.text(sheet.keyLabel, {size: 11, font: 'bold', color: colours.text, after: 6});
	writer.ensureSpace(KEY_BOX_HEIGHT);
	writer.currentPage.drawRectangle({
		x: MARGIN,
		y: writer.y - KEY_BOX_HEIGHT,
		width: CONTENT_WIDTH,
		height: KEY_BOX_HEIGHT,
		color: colours.panel,
		borderColor: colours.rule,
		borderWidth: 1,
	});
	const keyWidth = fonts.mono.widthOfTextAtSize(sheet.recoveryKey, KEY_SIZE);
	writer.currentPage.drawText(sheet.recoveryKey, {
		x: MARGIN + (CONTENT_WIDTH - keyWidth) / 2,
		y: writer.y - KEY_BOX_HEIGHT / 2 - KEY_SIZE * 0.3,
		size: KEY_SIZE,
		font: fonts.mono,
		color: colours.text,
	});
	writer.y -= KEY_BOX_HEIGHT + 24;

	await writer.text(sheet.stepsTitle, {size: 13, font: 'bold', color: colours.text, after: 6});
	for (const [index, step] of sheet.steps.entries()) {
		writer.ensureSpace(11 * LINE_HEIGHT_RATIO);
		writer.currentPage.drawText(`${index + 1}.`, {
			x: MARGIN,
			y: writer.y - 11,
			size: 11,
			font: fonts.bold,
			color: colours.text,
		});
		await writer.text(step, {
			size: 11,
			font: 'regular',
			color: colours.text,
			x: MARGIN + 18,
			width: CONTENT_WIDTH - 18,
			after: 4,
		});
	}

	if (sheet.backupCodes && sheet.backupCodes.codes.length > 0) {
		writer.y -= 16;
		await writer.text(sheet.backupCodes.title, {size: 13, font: 'bold', color: colours.text, after: 4});
		await writer.text(sheet.backupCodes.note, {size: 10, font: 'regular', color: colours.muted, after: 8});
		const codes = sheet.backupCodes.codes;
		const columnWidth = CONTENT_WIDTH / 2;
		for (let index = 0; index < codes.length; index += 2) {
			writer.ensureSpace(18);
			for (const [column, code] of codes.slice(index, index + 2).entries()) {
				writer.currentPage.drawText(code, {
					x: MARGIN + column * columnWidth,
					y: writer.y - 12,
					size: 12,
					font: fonts.mono,
					color: colours.text,
				});
			}
			writer.y -= 18;
		}
	}

	return pdf.save();
}
