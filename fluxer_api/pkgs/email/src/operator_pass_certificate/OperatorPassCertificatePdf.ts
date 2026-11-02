// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFile} from 'node:fs/promises';
import fontkit from '@pdf-lib/fontkit';
import {
	drawOperatorPassCertificateName,
	operatorPassCertificateFontsDirectory,
	type ShapedOperatorPassCertificateName,
	shapeOperatorPassCertificateName,
} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificateNameText';
import {PDFDocument, type PDFFont, type PDFPage, rgb} from 'pdf-lib';

export interface OperatorPassCertificatePage {
	number: number;
	holderName: string;
	paidAt: Date;
	earlyBird: boolean;
	termsVersion: string;
	issuedAt: Date;
	copy: 'original' | 'same_name' | 'new_name';
	source: 'purchase' | 'visionary';
	verificationUrl: string;
}

const PAGE_WIDTH = 841.89;
const PAGE_HEIGHT = 595.28;
const NAME_MAX_WIDTH = 680;
const INNER_FRAME_WIDTH = PAGE_WIDTH - 76;
const VERIFICATION_SIZE = 8;
const TEXT_COLOR = rgb(0.1, 0.1, 0.12);
const MUTED_COLOR = rgb(0.38, 0.38, 0.44);
const BRAND_COLOR = rgb(0x46 / 255, 0x41 / 255, 0xd9 / 255);
const ISSUER = 'Fluxer Platform AB';
const SIGNATURE_WIDTH = 170;
const SIGNATURE_TOP = 202;
const BADGE_SIZE = 52;
const BADGE_TOP = 534;
const BADGE_BACK =
	'M138.41,23.71L168.14,83.95L234.63,93.61A11.61,11.61,0,0,1,241.06,113.42L192.95,160.31L204.31,226.52A11.61,11.61,0,0,1,187.46,238.76L128,207.5L68.54,238.76A11.61,11.61,0,0,1,51.69,226.52L63.05,160.31L14.94,113.42A11.61,11.61,0,0,1,21.37,93.61L87.86,83.95L117.59,23.71A11.61,11.61,0,0,1,138.41,23.71Z';
const BADGE_FRONT =
	'M128,139.21L87.86,83.95L117.59,23.71A11.61,11.61,0,0,1,128,17.24ZM128,139.21L168.14,83.95L234.63,93.61A11.61,11.61,0,0,1,244,101.51ZM128,139.21L199.69,237.88A11.61,11.61,0,0,1,187.46,238.76L128,207.5ZM128,139.21L56.31,237.88A11.61,11.61,0,0,1,51.69,226.52L63.05,160.31ZM128,139.21L12,101.51A11.61,11.61,0,0,1,21.37,93.61L87.86,83.95Z';
const MONTHS = [
	'January',
	'February',
	'March',
	'April',
	'May',
	'June',
	'July',
	'August',
	'September',
	'October',
	'November',
	'December',
] as const;

export interface NotoSansFonts {
	regular: Uint8Array;
	bold: Uint8Array;
}

let fontsPromise: Promise<NotoSansFonts> | null = null;

export function loadNotoSansFonts(): Promise<NotoSansFonts> {
	fontsPromise ??= (async () => {
		const directory = operatorPassCertificateFontsDirectory();
		const [regular, bold] = await Promise.all([
			readFile(new URL('NotoSans-Regular.ttf', directory)),
			readFile(new URL('NotoSans-Bold.ttf', directory)),
		]);
		return {regular: new Uint8Array(regular), bold: new Uint8Array(bold)};
	})();
	return fontsPromise;
}

export function formatOperatorPassCertificateNumber(value: number): string {
	return String(value).padStart(6, '0');
}

export function getOperatorPassCertificateFilename(firstNumber: number): string {
	return `fluxer-operator-certificate-${formatOperatorPassCertificateNumber(firstNumber)}.pdf`;
}

function subsetTag(value: number): string {
	let remaining = value;
	let tag = '';
	for (let index = 0; index < 6; index++) {
		tag = String.fromCharCode(65 + (remaining % 26)) + tag;
		remaining = Math.floor(remaining / 26);
	}
	return tag;
}

function formatPaidAt(paidAt: Date): string {
	const hours = String(paidAt.getUTCHours()).padStart(2, '0');
	const minutes = String(paidAt.getUTCMinutes()).padStart(2, '0');
	return `${formatDate(paidAt)} at ${hours}:${minutes} UTC`;
}

function formatDate(at: Date): string {
	return `${at.getUTCDate()} ${MONTHS[at.getUTCMonth()]} ${at.getUTCFullYear()}`;
}

export function getOperatorPassCertificateNameNote(
	page: Pick<OperatorPassCertificatePage, 'issuedAt' | 'copy' | 'source'>,
): string {
	switch (page.copy) {
		case 'original':
			return page.source === 'visionary'
				? 'This name was chosen when the pass was turned on.'
				: 'This name was chosen at checkout.';
		case 'same_name':
			return `This copy was issued on ${formatDate(page.issuedAt)}.`;
		case 'new_name':
			return `This copy was issued on ${formatDate(page.issuedAt)} with an updated name.`;
	}
}

function drawCentered(page: PDFPage, text: string, font: PDFFont, size: number, y: number, color = TEXT_COLOR): void {
	const width = font.widthOfTextAtSize(text, size);
	page.drawText(text, {x: (PAGE_WIDTH - width) / 2, y, size, font, color});
}

const NAME_MAX_SIZE = 40;
const NAME_BASELINE = 340;
const NAME_LOWEST_INK = 324;
const NAME_HIGHEST_INK = 412;

function fitName(name: ShapedOperatorPassCertificateName): {size: number; baseline: number} {
	const height = name.ascent + name.descent;
	const size = Math.min(
		NAME_MAX_SIZE,
		name.width > 0 ? NAME_MAX_WIDTH / name.width : NAME_MAX_SIZE,
		height > 0 ? (NAME_HIGHEST_INK - NAME_LOWEST_INK) / height : NAME_MAX_SIZE,
	);
	const raised = Math.max(NAME_BASELINE, NAME_LOWEST_INK + name.descent * size);
	return {size, baseline: Math.min(raised, NAME_HIGHEST_INK - name.ascent * size)};
}

export async function measureOperatorPassCertificateName(
	holderName: string,
): Promise<{size: number; width: number; maxWidth: number}> {
	const name = await shapeOperatorPassCertificateName(holderName);
	const {size} = fitName(name);
	return {size, width: name.width * size, maxWidth: NAME_MAX_WIDTH};
}

function getVerificationLine(verificationUrl: string): string {
	return `Check this certificate at ${verificationUrl}`;
}

export async function measureOperatorPassCertificateVerificationLine(
	verificationUrl: string,
): Promise<{width: number; maxWidth: number}> {
	const document = await PDFDocument.create();
	document.registerFontkit(fontkit);
	const regular = await document.embedFont((await loadNotoSansFonts()).regular, {subset: true});
	return {
		width: regular.widthOfTextAtSize(getVerificationLine(verificationUrl), VERIFICATION_SIZE),
		maxWidth: INNER_FRAME_WIDTH,
	};
}

function applyMetadata(document: PDFDocument, title: string, at: Date): void {
	document.setTitle(title, {showInWindowTitleBar: true});
	document.setAuthor(ISSUER);
	document.setCreator(ISSUER);
	document.setProducer(ISSUER);
	document.setCreationDate(at);
	document.setModificationDate(at);
}

export async function renderOperatorPassCertificatePage(page: OperatorPassCertificatePage): Promise<Uint8Array> {
	const fonts = await loadNotoSansFonts();
	const document = await PDFDocument.create({updateMetadata: false});
	document.registerFontkit(fontkit);
	const tag = subsetTag(page.number);
	const regular = await document.embedFont(fonts.regular, {subset: true, customName: `${tag}+NotoSans-Regular`});
	const bold = await document.embedFont(fonts.bold, {subset: true, customName: `${tag}+NotoSans-Bold`});
	const number = formatOperatorPassCertificateNumber(page.number);
	applyMetadata(document, `Operator certificate No. ${number}`, page.issuedAt);

	const sheet = document.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
	sheet.drawRectangle({
		x: 28,
		y: 28,
		width: PAGE_WIDTH - 56,
		height: PAGE_HEIGHT - 56,
		borderColor: BRAND_COLOR,
		borderWidth: 2,
	});
	sheet.drawRectangle({
		x: 38,
		y: 38,
		width: INNER_FRAME_WIDTH,
		height: PAGE_HEIGHT - 76,
		borderColor: BRAND_COLOR,
		borderWidth: 0.5,
	});

	const badge = {x: (PAGE_WIDTH - BADGE_SIZE) / 2, y: BADGE_TOP, scale: BADGE_SIZE / 256, color: BRAND_COLOR};
	sheet.drawSvgPath(BADGE_BACK, {...badge, opacity: 0.55});
	sheet.drawSvgPath(BADGE_FRONT, badge);

	drawCentered(sheet, 'Operator certificate', bold, 30, 446, BRAND_COLOR);
	drawCentered(sheet, `No. ${number}`, regular, 14, 420, MUTED_COLOR);
	const name = await shapeOperatorPassCertificateName(page.holderName);
	drawOperatorPassCertificateName(document, sheet, name, {
		...fitName(name),
		centerX: PAGE_WIDTH / 2,
		color: TEXT_COLOR,
	});
	const visionary = page.source === 'visionary';
	drawCentered(
		sheet,
		visionary
			? `Operator Pass included with Visionary since ${formatDate(page.paidAt)}`
			: `Operator Pass bought on ${formatPaidAt(page.paidAt)}`,
		regular,
		15,
		306,
	);
	if (page.earlyBird && !visionary) {
		drawCentered(sheet, 'Early bird price', bold, 13, 280, BRAND_COLOR);
	}
	drawCentered(
		sheet,
		visionary
			? 'This certifies that an Operator Pass came with Visionary, in support of Fluxer, in the name above.'
			: 'This certifies that an Operator Pass was bought in support of Fluxer in the name above.',
		regular,
		12,
		236,
	);
	drawCentered(sheet, `Issued by ${ISSUER}, Haninge, Sweden`, regular, 12, 218);
	const {OPERATOR_PASS_CERTIFICATE_SIGNATURE: signature} = await import(
		'@pkgs/email/src/operator_pass_certificate/OperatorPassCertificateSignature'
	);
	sheet.drawSvgPath(signature.path, {
		x: (PAGE_WIDTH - SIGNATURE_WIDTH) / 2,
		y: SIGNATURE_TOP,
		scale: SIGNATURE_WIDTH / signature.width,
		color: TEXT_COLOR,
	});
	drawCentered(sheet, 'Hampus', regular, 11, 146);
	drawCentered(sheet, 'Creator of Fluxer', regular, 9, 133, MUTED_COLOR);
	drawCentered(sheet, getOperatorPassCertificateNameNote(page), regular, 9, 100, MUTED_COLOR);
	drawCentered(sheet, getVerificationLine(page.verificationUrl), regular, VERIFICATION_SIZE, 70, MUTED_COLOR);
	drawCentered(sheet, `Certificate No. ${number}, terms version ${page.termsVersion}`, regular, 8, 56, MUTED_COLOR);

	return document.save({useObjectStreams: false});
}

export async function combineOperatorPassCertificatePages(
	pages: ReadonlyArray<Uint8Array>,
	firstNumber: number,
	issuedAt: Date,
): Promise<Uint8Array> {
	if (pages.length === 1 && pages[0]) {
		return pages[0];
	}
	const combined = await PDFDocument.create({updateMetadata: false});
	applyMetadata(combined, `Operator certificate No. ${formatOperatorPassCertificateNumber(firstNumber)}`, issuedAt);
	for (const bytes of pages) {
		const source = await PDFDocument.load(bytes, {updateMetadata: false});
		const copied = await combined.copyPages(source, source.getPageIndices());
		for (const copiedPage of copied) {
			combined.addPage(copiedPage);
		}
	}
	return combined.save({useObjectStreams: false});
}
