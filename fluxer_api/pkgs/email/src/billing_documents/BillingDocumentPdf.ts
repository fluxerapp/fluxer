// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatMinorUnitPriceWithFraction} from '@fluxer/number_utils/src/PriceFormatting';
import fontkit from '@pdf-lib/fontkit';
import {
	type BillingDocumentBuyer,
	type BillingDocumentContent,
	type BillingDocumentCreditReason,
	type BillingDocumentLanguage,
	type BillingDocumentLine,
	type BillingDocumentTaxLine,
	isCreditDocument,
	stockholmDate,
} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import {loadNotoSansFonts} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificatePdf';
import {PDFDocument, type PDFFont, type PDFPage, rgb} from 'pdf-lib';

const PAGE_WIDTH = 595.28;
const PAGE_HEIGHT = 841.89;
const MARGIN = 48;
const CONTENT_WIDTH = PAGE_WIDTH - MARGIN * 2;
const FOOTER_SPACE = 40;
const TEXT_COLOR = rgb(0.1, 0.1, 0.12);
const MUTED_COLOR = rgb(0.38, 0.38, 0.44);
const BRAND_COLOR = rgb(0x46 / 255, 0x41 / 255, 0xd9 / 255);
const RULE_COLOR = rgb(0.82, 0.82, 0.86);
const BODY = 9.5;
const SMALL = 8;
const LINE_GAP = 3;

interface Labels {
	title: (content: BillingDocumentContent) => string;
	numberLabel: (content: BillingDocumentContent) => string;
	issueDate: string;
	supplyDate: string;
	period: string;
	currency: string;
	seller: (content: BillingDocumentContent) => string;
	buyer: (content: BillingDocumentContent) => string;
	organisationNumber: string;
	registeredOffice: string;
	vatNumber: string;
	buyerVatNumber: string;
	description: string;
	quantity: string;
	unitPrice: string;
	vatRate: string;
	amount: string;
	subtotal: string;
	vatLine: (rate: string, country: string | null, taxable: string) => string;
	reverseChargeLine: (country: string | null, taxable: string) => string;
	total: string;
	paid: (date: string, method: string | null) => string;
	amountDue: string;
	original: (number: string, date: string) => string;
	reason: string;
	reasons: Record<BillingDocumentCreditReason, string>;
	refundDate: string;
	disputeDate: string;
	reverseCharge: string;
	outsideScope: string;
	notCharged: string;
	voucher: string;
	voucherUk: string;
	donation: string;
	sekVat: (amount: string, rate: string, date: string) => string;
	references: string;
	page: (page: number, pages: number) => string;
}

const LABELS: Record<BillingDocumentLanguage, Labels> = {
	en: {
		title: (content) => {
			switch (content.kind) {
				case 'invoice':
					return content.buyer.business ? 'Invoice' : 'Receipt';
				case 'receipt':
					return 'Receipt';
				case 'donation_receipt':
					return 'Donation receipt';
				case 'credit_note':
					return 'Credit note';
				case 'refund_note':
					return 'Refund note';
			}
		},
		numberLabel: (content) => {
			switch (content.kind) {
				case 'invoice':
					return 'Invoice no.';
				case 'credit_note':
					return 'Credit note no.';
				case 'refund_note':
					return 'Refund note no.';
				default:
					return 'Receipt no.';
			}
		},
		issueDate: 'Issue date',
		supplyDate: 'Date of supply',
		period: 'Service period',
		currency: 'Currency',
		seller: (content) => (content.flow === 'donation' ? 'Received by' : 'Seller'),
		buyer: (content) => (content.flow === 'donation' ? 'Donor' : 'Buyer'),
		organisationNumber: 'Org. no.',
		registeredOffice: 'Registered office',
		vatNumber: 'VAT no.',
		buyerVatNumber: 'Buyer VAT no.',
		description: 'Description',
		quantity: 'Qty',
		unitPrice: 'Unit price excl. VAT',
		vatRate: 'VAT',
		amount: 'Amount excl. VAT',
		subtotal: 'Total excl. VAT',
		vatLine: (rate, country, taxable) => `VAT ${rate}${country ? ` (${country})` : ''} on ${taxable}`,
		reverseChargeLine: (country, taxable) => `Reverse charge${country ? ` (${country})` : ''} on ${taxable}`,
		total: 'Total',
		paid: (date, method) => (method ? `Paid ${date} by ${method}` : `Paid ${date}`),
		amountDue: 'Amount due',
		original: (number, date) => `Credit for ${number}, issued ${date}`,
		reason: 'Reason',
		reasons: {
			withdrawal: 'Withdrawal from the purchase',
			refund: 'Refund',
			duplicate: 'Duplicate payment',
			fraud: 'Fraudulent payment',
			dispute: 'Payment dispute we accepted',
		},
		refundDate: 'Refund date',
		disputeDate: 'Date we accepted the dispute',
		reverseCharge:
			'Omvänd betalningsskyldighet / Reverse charge. VAT is accounted for by the recipient (Article 196 Council Directive 2006/112/EC).',
		outsideScope:
			'Supplied outside Sweden (ML 2023:200 6 kap. 33 §, art. 44 Directive 2006/112/EC). VAT, if any, is accounted for by the recipient.',
		notCharged: 'No VAT charged.',
		voucher:
			'Gift code (multi-purpose voucher). VAT is charged when the code is redeemed, in the country where it is redeemed (ML 2023:200 2 kap. 27 §, Directive 2006/112/EC art. 30b).',
		voucherUk: 'In the United Kingdom, VATA 1994 Sch 10B applies.',
		donation: 'A gift to Fluxer Platform AB. Nothing was supplied in return, so no VAT is charged.',
		sekVat: (amount, rate, date) => `VAT in SEK: ${amount} (ECB reference rate ${rate} of ${date})`,
		references: 'References',
		page: (page, pages) => `Page ${page} of ${pages}`,
	},
	'sv-SE': {
		title: (content) => {
			switch (content.kind) {
				case 'invoice':
					return content.buyer.business ? 'Faktura' : 'Kvitto';
				case 'receipt':
					return 'Kvitto';
				case 'donation_receipt':
					return 'Gåvokvitto';
				case 'credit_note':
					return 'Kreditnota';
				case 'refund_note':
					return 'Återbetalningskvitto';
			}
		},
		numberLabel: (content) => {
			switch (content.kind) {
				case 'invoice':
					return 'Fakturanr';
				case 'credit_note':
					return 'Kreditnotanr';
				case 'refund_note':
					return 'Återbetalningsnr';
				default:
					return 'Kvittonr';
			}
		},
		issueDate: 'Utfärdad',
		supplyDate: 'Leveransdatum',
		period: 'Tjänsteperiod',
		currency: 'Valuta',
		seller: (content) => (content.flow === 'donation' ? 'Mottagare' : 'Säljare'),
		buyer: (content) => (content.flow === 'donation' ? 'Givare' : 'Köpare'),
		organisationNumber: 'Org.nr',
		registeredOffice: 'Säte',
		vatNumber: 'Momsreg.nr',
		buyerVatNumber: 'Köparens momsreg.nr',
		description: 'Beskrivning',
		quantity: 'Antal',
		unitPrice: 'À-pris exkl. moms',
		vatRate: 'Moms',
		amount: 'Belopp exkl. moms',
		subtotal: 'Summa exkl. moms',
		vatLine: (rate, country, taxable) => `Moms ${rate}${country ? ` (${country})` : ''} på ${taxable}`,
		reverseChargeLine: (country, taxable) =>
			`Omvänd betalningsskyldighet${country ? ` (${country})` : ''} på ${taxable}`,
		total: 'Totalt',
		paid: (date, method) => (method ? `Betald ${date} med ${method}` : `Betald ${date}`),
		amountDue: 'Att betala',
		original: (number, date) => `Kreditering av ${number}, utfärdad ${date}`,
		reason: 'Orsak',
		reasons: {
			withdrawal: 'Ångrat köp',
			refund: 'Återbetalning',
			duplicate: 'Dubbel betalning',
			fraud: 'Bedräglig betalning',
			dispute: 'Betalningstvist som vi har godtagit',
		},
		refundDate: 'Återbetalningsdatum',
		disputeDate: 'Datum då vi godtog tvisten',
		reverseCharge: 'Omvänd betalningsskyldighet. Köparen redovisar momsen (artikel 196 i rådets direktiv 2006/112/EG).',
		outsideScope:
			'Tjänsten är omsatt utanför Sverige (6 kap. 33 § mervärdesskattelagen (2023:200), artikel 44 i direktiv 2006/112/EG). Eventuell moms redovisas av köparen.',
		notCharged: 'Ingen moms har tagits ut.',
		voucher:
			'Presentkod (flerfunktionsvoucher). Moms tas ut när koden löses in, i det land där den löses in (2 kap. 27 § mervärdesskattelagen (2023:200), artikel 30b i direktiv 2006/112/EG).',
		voucherUk: 'I Storbritannien gäller VATA 1994 Sch 10B.',
		donation: 'En gåva till Fluxer Platform AB. Ingenting levererades i utbyte, så ingen moms tas ut.',
		sekVat: (amount, rate, date) => `Moms i SEK: ${amount} (ECB:s referenskurs ${rate} den ${date})`,
		references: 'Referenser',
		page: (page, pages) => `Sida ${page} av ${pages}`,
	},
	'pt-BR': {
		title: (content) => {
			switch (content.kind) {
				case 'invoice':
					return content.buyer.business ? 'Fatura' : 'Recibo';
				case 'receipt':
					return 'Recibo';
				case 'donation_receipt':
					return 'Recibo de doação';
				case 'credit_note':
					return 'Nota de crédito';
				case 'refund_note':
					return 'Comprovante de reembolso';
			}
		},
		numberLabel: (content) => {
			switch (content.kind) {
				case 'invoice':
					return 'Fatura nº';
				case 'credit_note':
					return 'Nota de crédito nº';
				case 'refund_note':
					return 'Comprovante nº';
				default:
					return 'Recibo nº';
			}
		},
		issueDate: 'Data de emissão',
		supplyDate: 'Data do fornecimento',
		period: 'Período do serviço',
		currency: 'Moeda',
		seller: (content) => (content.flow === 'donation' ? 'Recebido por' : 'Vendedor'),
		buyer: (content) => (content.flow === 'donation' ? 'Doador' : 'Comprador'),
		organisationNumber: 'Nº de registro sueco',
		registeredOffice: 'Sede',
		vatNumber: 'Nº de IVA',
		buyerVatNumber: 'Nº de IVA do comprador',
		description: 'Descrição',
		quantity: 'Qtd.',
		unitPrice: 'Preço unitário sem IVA',
		vatRate: 'IVA',
		amount: 'Valor sem IVA',
		subtotal: 'Total sem IVA',
		vatLine: (rate, country, taxable) => `IVA ${rate}${country ? ` (${country})` : ''} sobre ${taxable}`,
		reverseChargeLine: (country, taxable) =>
			`Inversão do sujeito passivo${country ? ` (${country})` : ''} sobre ${taxable}`,
		total: 'Total',
		paid: (date, method) => (method ? `Pago em ${date} com ${method}` : `Pago em ${date}`),
		amountDue: 'Valor a pagar',
		original: (number, date) => `Crédito referente a ${number}, emitido em ${date}`,
		reason: 'Motivo',
		reasons: {
			withdrawal: 'Arrependimento da compra',
			refund: 'Reembolso',
			duplicate: 'Pagamento duplicado',
			fraud: 'Pagamento fraudulento',
			dispute: 'Contestação de pagamento aceita por nós',
		},
		refundDate: 'Data do reembolso',
		disputeDate: 'Data em que aceitamos a contestação',
		reverseCharge:
			'Omvänd betalningsskyldighet / Autoliquidação. O IVA é declarado pelo destinatário (artigo 196.º da Diretiva 2006/112/CE do Conselho).',
		outsideScope:
			'Serviço prestado fora da Suécia (ML 2023:200, cap. 6, § 33; art. 44.º da Diretiva 2006/112/CE). O IVA eventualmente devido é declarado pelo destinatário.',
		notCharged: 'Nenhum IVA foi cobrado.',
		voucher:
			'Código de presente (voucher multifuncional). O IVA é cobrado quando o código é resgatado, no país em que é resgatado (ML 2023:200, cap. 2, § 27; art. 30.º-B da Diretiva 2006/112/CE).',
		voucherUk: 'No Reino Unido, aplica-se o VATA 1994 Sch 10B.',
		donation: 'Uma doação à Fluxer Platform AB. Nada foi fornecido em troca, portanto nenhum IVA é cobrado.',
		sekVat: (amount, rate, date) => `IVA em SEK: ${amount} (taxa de referência do BCE ${rate} de ${date})`,
		references: 'Referências',
		page: (page, pages) => `Página ${page} de ${pages}`,
	},
};

interface Fonts {
	regular: PDFFont;
	bold: PDFFont;
}

interface Column {
	x: number;
	width: number;
	align: 'left' | 'right';
}

function wrap(text: string, font: PDFFont, size: number, width: number): Array<string> {
	const lines: Array<string> = [];
	for (const paragraph of text.split('\n')) {
		let current = '';
		for (const word of paragraph.split(/\s+/u).filter(Boolean)) {
			const candidate = current ? `${current} ${word}` : word;
			if (current && font.widthOfTextAtSize(candidate, size) > width) {
				lines.push(current);
				current = word;
			} else {
				current = candidate;
			}
		}
		lines.push(current);
	}
	return lines;
}

class DocumentWriter {
	private page: PDFPage;
	private cursor: number;
	readonly pages: Array<PDFPage> = [];

	constructor(
		private readonly document: PDFDocument,
		readonly fonts: Fonts,
	) {
		this.page = this.addPage();
		this.cursor = PAGE_HEIGHT - MARGIN;
	}

	private addPage(): PDFPage {
		const page = this.document.addPage([PAGE_WIDTH, PAGE_HEIGHT]);
		this.pages.push(page);
		return page;
	}

	ensure(height: number): void {
		if (this.cursor - height < MARGIN + FOOTER_SPACE) {
			this.page = this.addPage();
			this.cursor = PAGE_HEIGHT - MARGIN;
		}
	}

	space(height: number): void {
		this.cursor -= height;
	}

	text(
		value: string,
		options: {font?: PDFFont; size?: number; color?: ReturnType<typeof rgb>; x?: number; width?: number} = {},
	): void {
		const font = options.font ?? this.fonts.regular;
		const size = options.size ?? BODY;
		const x = options.x ?? MARGIN;
		for (const line of wrap(value, font, size, options.width ?? CONTENT_WIDTH)) {
			this.ensure(size + LINE_GAP);
			this.cursor -= size + LINE_GAP;
			this.page.drawText(line, {x, y: this.cursor, size, font, color: options.color ?? TEXT_COLOR});
		}
	}

	columns(
		blocks: ReadonlyArray<{x: number; width: number; lines: ReadonlyArray<{text: string; bold?: boolean}>}>,
	): void {
		const wrapped = blocks.map((block) =>
			block.lines.flatMap((line) =>
				wrap(line.text, line.bold ? this.fonts.bold : this.fonts.regular, BODY, block.width).map((text) => ({
					text,
					bold: line.bold === true,
				})),
			),
		);
		const rows = Math.max(...wrapped.map((lines) => lines.length));
		this.ensure(rows * (BODY + LINE_GAP));
		const top = this.cursor;
		wrapped.forEach((lines, index) => {
			let y = top;
			for (const line of lines) {
				y -= BODY + LINE_GAP;
				this.page.drawText(line.text, {
					x: blocks[index]!.x,
					y,
					size: BODY,
					font: line.bold ? this.fonts.bold : this.fonts.regular,
					color: TEXT_COLOR,
				});
			}
		});
		this.cursor = top - rows * (BODY + LINE_GAP);
	}

	row(cells: ReadonlyArray<string>, columns: ReadonlyArray<Column>, bold = false): void {
		const font = bold ? this.fonts.bold : this.fonts.regular;
		const wrapped = cells.map((cell, index) => wrap(cell, font, BODY, columns[index]!.width));
		const rows = Math.max(...wrapped.map((lines) => lines.length));
		this.ensure(rows * (BODY + LINE_GAP) + 4);
		const top = this.cursor - 2;
		wrapped.forEach((lines, index) => {
			const column = columns[index]!;
			lines.forEach((line, lineIndex) => {
				const width = font.widthOfTextAtSize(line, BODY);
				this.page.drawText(line, {
					x: column.align === 'right' ? column.x + column.width - width : column.x,
					y: top - (lineIndex + 1) * (BODY + LINE_GAP),
					size: BODY,
					font,
					color: TEXT_COLOR,
				});
			});
		});
		this.cursor = top - rows * (BODY + LINE_GAP) - 2;
	}

	rule(): void {
		this.ensure(6);
		this.cursor -= 3;
		this.page.drawLine({
			start: {x: MARGIN, y: this.cursor},
			end: {x: PAGE_WIDTH - MARGIN, y: this.cursor},
			thickness: 0.5,
			color: RULE_COLOR,
		});
		this.cursor -= 3;
	}

	pair(label: string, value: string, x: number, width: number): void {
		this.columns([
			{x, width: 120, lines: [{text: label}]},
			{x: x + 124, width: width - 124, lines: [{text: value, bold: true}]},
		]);
	}
}

function intlLocale(language: BillingDocumentLanguage): string {
	return language === 'en' ? 'en-GB' : language;
}

function formatDate(iso: string, language: BillingDocumentLanguage): string {
	const day = stockholmDate(iso);
	const long = new Intl.DateTimeFormat(intlLocale(language), {dateStyle: 'long', timeZone: 'UTC'}).format(
		new Date(`${day}T12:00:00Z`),
	);
	return `${day} (${long})`;
}

function formatPeriod(start: string, end: string): string {
	return `${stockholmDate(start)} – ${stockholmDate(end)}`;
}

export function money(amountMinor: number, currency: string, language: BillingDocumentLanguage): string {
	const formatted = formatMinorUnitPriceWithFraction(amountMinor, currency, intlLocale(language));
	const code = currency.toUpperCase();
	return formatted.includes(code) ? formatted : `${formatted} ${code}`;
}

function formatRate(rate: number | null, language: BillingDocumentLanguage): string {
	if (rate == null) {
		return '–';
	}
	const number = new Intl.NumberFormat(intlLocale(language), {maximumFractionDigits: 4}).format(rate);
	return language === 'pt-BR' ? `${number}%` : `${number} %`;
}

function addressLines(content: BillingDocumentContent): Array<string> {
	const address = content.buyer.address;
	if (!address) {
		return content.buyer.country ? [content.buyer.country] : [];
	}
	const city = [address.postalCode, address.city].filter(Boolean).join(' ');
	return [address.line1, address.line2, [city, address.state].filter(Boolean).join(', '), address.country].filter(
		(line): line is string => Boolean(line),
	);
}

function statutoryLines(content: BillingDocumentContent, labels: Labels): Array<string> {
	switch (content.vatTreatment) {
		case 'reverse_charge_eu':
			return [labels.reverseCharge];
		case 'outside_scope':
			return [labels.outsideScope];
		case 'not_charged':
			return [labels.notCharged];
		case 'voucher':
			return content.buyer.country === 'GB' ? [labels.voucher, labels.voucherUk] : [labels.voucher];
		case 'donation':
			return [labels.donation];
		case 'charged':
			return [];
	}
}

function showsVat(content: BillingDocumentContent): boolean {
	return content.vatTreatment !== 'voucher' && content.vatTreatment !== 'donation';
}

function referenceLine(content: BillingDocumentContent): string {
	const references = content.references;
	return [
		references.checkoutSessionId,
		references.stripeInvoiceId,
		references.paymentIntentId,
		references.chargeId,
		references.refundId,
		references.disputeId,
		`doc ${content.documentId}`,
	]
		.filter(Boolean)
		.join(' · ');
}

function lineColumns(vat: boolean): Array<Column> {
	if (!vat) {
		return [
			{x: MARGIN, width: 330, align: 'left'},
			{x: MARGIN + 334, width: 40, align: 'right'},
			{x: MARGIN + 378, width: CONTENT_WIDTH - 378, align: 'right'},
		];
	}
	return [
		{x: MARGIN, width: 210, align: 'left'},
		{x: MARGIN + 214, width: 34, align: 'right'},
		{x: MARGIN + 252, width: 92, align: 'right'},
		{x: MARGIN + 348, width: 46, align: 'right'},
		{x: MARGIN + 398, width: CONTENT_WIDTH - 398, align: 'right'},
	];
}

function lineCells(
	line: BillingDocumentLine,
	content: BillingDocumentContent,
	vat: boolean,
	period: (line: BillingDocumentLine) => string | null,
): Array<string> {
	const language = content.language;
	const description = [line.description, period(line)].filter(Boolean).join('\n');
	if (!vat) {
		return [
			description,
			String(line.quantity),
			money(line.amountExclMinor + line.taxMinor, content.currency, language),
		];
	}
	return [
		description,
		String(line.quantity),
		money(line.unitAmountExclMinor, content.currency, language),
		formatRate(line.ratePercent, language),
		money(line.amountExclMinor, content.currency, language),
	];
}

function taxLineText(tax: BillingDocumentTaxLine, content: BillingDocumentContent, labels: Labels): string {
	if (tax.taxMinor === 0 && tax.taxabilityReason === 'reverse_charge') {
		return labels.reverseChargeLine(tax.country, money(tax.taxableMinor, content.currency, content.language));
	}
	return labels.vatLine(
		formatRate(tax.ratePercent, content.language),
		tax.country,
		money(tax.taxableMinor, content.currency, content.language),
	);
}

function subsetTag(seed: string): string {
	let hash = 0;
	for (const character of seed) {
		hash = (hash * 31 + (character.codePointAt(0) ?? 0)) % 308915776;
	}
	let tag = '';
	for (let index = 0; index < 6; index++) {
		tag = String.fromCharCode(65 + (hash % 26)) + tag;
		hash = Math.floor(hash / 26);
	}
	return tag;
}

export function buyerNameLines(buyer: BillingDocumentBuyer): Array<string> {
	if (!buyer.businessName) {
		return buyer.name ? [buyer.name] : [];
	}
	return buyer.name && buyer.name !== buyer.businessName ? [buyer.businessName, buyer.name] : [buyer.businessName];
}

export async function renderBillingDocumentPdf(content: BillingDocumentContent): Promise<Uint8Array> {
	const labels = LABELS[content.language];
	const fontData = await loadNotoSansFonts();
	const document = await PDFDocument.create({updateMetadata: false});
	document.registerFontkit(fontkit);
	const tag = subsetTag(content.number);
	const fonts: Fonts = {
		regular: await document.embedFont(fontData.regular, {subset: true, customName: `${tag}+NotoSans-Regular`}),
		bold: await document.embedFont(fontData.bold, {subset: true, customName: `${tag}+NotoSans-Bold`}),
	};
	const title = labels.title(content);
	const issuedAt = new Date(content.issuedAt);
	document.setTitle(`${title} ${content.number}`, {showInWindowTitleBar: true});
	document.setAuthor(content.seller.name);
	document.setCreator(content.seller.name);
	document.setProducer(content.seller.name);
	document.setLanguage(content.language);
	document.setCreationDate(issuedAt);
	document.setModificationDate(issuedAt);

	const writer = new DocumentWriter(document, fonts);
	const half = CONTENT_WIDTH / 2;
	writer.text(title, {font: fonts.bold, size: 22, color: BRAND_COLOR});
	writer.space(6);
	writer.pair(labels.numberLabel(content), content.number, MARGIN, half);
	writer.pair(labels.issueDate, formatDate(content.issuedAt, content.language), MARGIN, half);
	if (content.periodStart && content.periodEnd) {
		writer.pair(labels.period, formatPeriod(content.periodStart, content.periodEnd), MARGIN, CONTENT_WIDTH);
	} else if (content.supplyDate && stockholmDate(content.supplyDate) !== stockholmDate(content.issuedAt)) {
		writer.pair(labels.supplyDate, formatDate(content.supplyDate, content.language), MARGIN, half);
	}
	writer.pair(labels.currency, content.currency.toUpperCase(), MARGIN, half);
	if (content.original) {
		writer.space(4);
		writer.text(labels.original(content.original.number, formatDate(content.original.issuedAt, content.language)), {
			font: fonts.bold,
		});
		if (content.reason) {
			writer.text(`${labels.reason}: ${labels.reasons[content.reason]}`);
		}
	}
	writer.space(12);

	const seller = content.seller;
	const sellerLines = [
		{text: labels.seller(content), bold: true},
		{text: seller.name},
		{text: seller.address},
		{text: `${labels.organisationNumber} ${seller.organisationNumber}`},
		{text: `${labels.registeredOffice}: ${seller.registeredOffice}`},
		...(content.flow === 'donation'
			? []
			: seller.vatNumbers.map((number) => ({text: `${labels.vatNumber} ${number}`}))),
		{text: seller.email},
	];
	const buyer = content.buyer;
	const buyerLines = [
		{text: labels.buyer(content), bold: true},
		...buyerNameLines(buyer).map((text) => ({text})),
		...addressLines(content).map((text) => ({text})),
		...(buyer.vatId ? [{text: `${labels.buyerVatNumber} ${buyer.vatId}`}] : []),
		...(buyer.email ? [{text: buyer.email}] : []),
	];
	writer.columns([
		{x: MARGIN, width: half - 12, lines: sellerLines},
		{x: MARGIN + half, width: half, lines: buyerLines},
	]);
	writer.space(14);

	const vat = showsVat(content);
	const columns = lineColumns(vat);
	writer.row(
		vat
			? [labels.description, labels.quantity, labels.unitPrice, labels.vatRate, labels.amount]
			: [labels.description, labels.quantity, labels.total],
		columns,
		true,
	);
	writer.rule();
	const period = (line: BillingDocumentLine) =>
		line.periodStart && line.periodEnd ? formatPeriod(line.periodStart, line.periodEnd) : null;
	for (const line of content.lines) {
		writer.row(lineCells(line, content, vat, period), columns);
	}
	writer.rule();
	const totalsColumns: Array<Column> = [
		{x: MARGIN + 200, width: 210, align: 'left'},
		{x: MARGIN + 414, width: CONTENT_WIDTH - 414, align: 'right'},
	];
	if (vat) {
		writer.row([labels.subtotal, money(content.subtotalExclMinor, content.currency, content.language)], totalsColumns);
		for (const tax of content.taxLines) {
			writer.row(
				[taxLineText(tax, content, labels), money(tax.taxMinor, content.currency, content.language)],
				totalsColumns,
			);
		}
	}
	writer.row([labels.total, money(content.totalMinor, content.currency, content.language)], totalsColumns, true);
	if (content.sekTax) {
		const sek = content.sekTax;
		writer.row(
			[labels.sekVat(money(sek.amountMinor, 'SEK', content.language), sek.rate, sek.rateDate), ''],
			[{x: MARGIN + 200, width: CONTENT_WIDTH - 200, align: 'left'}, totalsColumns[1]!],
		);
	}
	if (content.payment) {
		writer.space(6);
		const paidDate = formatDate(content.payment.at, content.language);
		if (isCreditDocument(content.kind)) {
			writer.text(`${content.reason === 'dispute' ? labels.disputeDate : labels.refundDate}: ${paidDate}`);
		} else {
			writer.text(labels.paid(paidDate, content.payment.method));
			writer.text(`${labels.amountDue}: ${money(0, content.currency, content.language)}`);
		}
	}
	const notes = statutoryLines(content, labels);
	if (notes.length > 0) {
		writer.space(10);
		for (const note of notes) {
			writer.text(note, {font: fonts.bold});
		}
	}
	writer.space(14);
	writer.text(`${labels.references}: ${referenceLine(content)}`, {size: SMALL, color: MUTED_COLOR});

	const footer = `${seller.name} · ${labels.organisationNumber} ${seller.organisationNumber} · ${seller.address} · ${seller.email}`;
	writer.pages.forEach((page, index) => {
		const pageLabel = labels.page(index + 1, writer.pages.length);
		const footerLines = wrap(footer, fonts.regular, SMALL, CONTENT_WIDTH - 80);
		footerLines.forEach((line, lineIndex) => {
			page.drawText(line, {
				x: MARGIN,
				y: MARGIN - 12 - lineIndex * (SMALL + 2),
				size: SMALL,
				font: fonts.regular,
				color: MUTED_COLOR,
			});
		});
		const width = fonts.regular.widthOfTextAtSize(pageLabel, SMALL);
		page.drawText(pageLabel, {
			x: PAGE_WIDTH - MARGIN - width,
			y: MARGIN - 12,
			size: SMALL,
			font: fonts.regular,
			color: MUTED_COLOR,
		});
	});
	return document.save({useObjectStreams: false});
}
