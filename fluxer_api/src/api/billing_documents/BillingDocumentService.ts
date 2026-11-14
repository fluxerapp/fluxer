// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {
	billingDocumentLanguage,
	buildCreditContent,
	buildSaleContent,
	type CreditOriginal,
	chargedTax,
	creditOriginalFromContent,
	creditSeries,
	layoutOf,
	type SaleData,
	type SaleLine,
	saleSeries,
	sekMinor,
	swedishTaxMinor,
	vatTreatmentOf,
} from '@app/api/billing_documents/BillingDocumentBuilder';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {EcbReferenceRates} from '@app/api/billing_documents/EcbReferenceRates';
import {Config} from '@app/api/Config';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import {Logger} from '@app/api/Logger';
import {getKVClient, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	type BillingDocumentAddress,
	type BillingDocumentBuyer,
	type BillingDocumentContent,
	type BillingDocumentCreditReason,
	type BillingDocumentFlow,
	type BillingDocumentLanguage,
	type BillingDocumentSekTax,
	type BillingDocumentSeries,
	type BillingDocumentTaxLine,
	type BillingDocumentVatIdVerification,
	billingDocumentFilename,
	formatBillingDocumentNumber,
	stockholmDate,
} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import {renderBillingDocumentPdf} from '@pkgs/email/src/billing_documents/BillingDocumentPdf';
import type {BillingDocumentAttachment, BillingTotals} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';

const LINE_ITEM_LIMIT = 100;
const YEARLY_PERIOD_MIN_SECONDS = 60 * 24 * 60 * 60;

export interface IssueContext {
	userId: bigint | null;
	locale: string | null;
	donorEmail?: string | null;
}

interface Descriptions {
	operatorPass: string;
	plan: (name: string, yearly: boolean) => string;
	proration: (name: string) => string;
	gift: (name: string, yearly: boolean) => string;
	donation: (interval: 'once' | 'month' | 'year') => string;
}

const DESCRIPTIONS: Record<BillingDocumentLanguage, Descriptions> = {
	en: {
		operatorPass: 'Operator Pass: printable numbered Operator certificate (PDF), delivered digitally',
		plan: (name, yearly) => `${name}, ${yearly ? 'yearly' : 'monthly'} subscription`,
		proration: (name) => `${name}, adjustment for a plan change`,
		gift: (name, yearly) => `${name} gift code, ${yearly ? '1 year' : '1 month'}`,
		donation: (interval) =>
			({month: 'Monthly donation to Fluxer', year: 'Yearly donation to Fluxer', once: 'Donation to Fluxer'})[interval],
	},
	'sv-SE': {
		operatorPass: 'Operator Pass: utskrivbart numrerat Operator-certifikat (PDF), levereras digitalt',
		plan: (name, yearly) => `${name}, ${yearly ? 'årsabonnemang' : 'månadsabonnemang'}`,
		proration: (name) => `${name}, justering vid byte av plan`,
		gift: (name, yearly) => `${name}-presentkod, ${yearly ? '1 år' : '1 månad'}`,
		donation: (interval) =>
			({month: 'Månatlig gåva till Fluxer', year: 'Årlig gåva till Fluxer', once: 'Gåva till Fluxer'})[interval],
	},
	'pt-BR': {
		operatorPass: 'Operator Pass: certificado de Operator numerado para impressão (PDF), entregue digitalmente',
		plan: (name, yearly) => `${name}, assinatura ${yearly ? 'anual' : 'mensal'}`,
		proration: (name) => `${name}, ajuste por mudança de plano`,
		gift: (name, yearly) => `Código de presente ${name}, ${yearly ? '1 ano' : '1 mês'}`,
		donation: (interval) =>
			({month: 'Doação mensal ao Fluxer', year: 'Doação anual ao Fluxer', once: 'Doação ao Fluxer'})[interval],
	},
};

function isoFromUnix(value: number | null | undefined): string | null {
	return value ? new Date(value * 1000).toISOString() : null;
}

function address(value: Stripe.Address | null | undefined): BillingDocumentAddress | null {
	if (!value || !(value.line1 || value.city || value.postal_code)) {
		return null;
	}
	return {
		line1: value.line1 ?? null,
		line2: value.line2 ?? null,
		postalCode: value.postal_code ?? null,
		city: value.city ?? null,
		state: value.state ?? null,
		country: value.country ?? null,
	};
}

function describeCharge(charge: Stripe.Charge | null): string | null {
	const details = charge?.payment_method_details;
	if (!details) {
		return null;
	}
	if (details.type === 'card' && details.card) {
		const brand = details.card.brand
			? details.card.brand.charAt(0).toUpperCase() + details.card.brand.slice(1)
			: 'Card';
		return details.card.last4 ? `${brand} ···· ${details.card.last4}` : brand;
	}
	return details.type === 'pix'
		? 'Pix'
		: details.type.charAt(0).toUpperCase() + details.type.slice(1).replaceAll('_', ' ');
}

function taxRatePercent(rate: Stripe.TaxRate | null | undefined): number | null {
	return rate?.percentage ?? null;
}

function sha256(value: string | Uint8Array): string {
	return createHash('sha256').update(value).digest('hex');
}

export function billingDocumentContent(row: BillingDocumentRow): BillingDocumentContent {
	return JSON.parse(row.content) as BillingDocumentContent;
}

export function billingDocumentAttachment(row: BillingDocumentRow): BillingDocumentAttachment {
	return {
		number: row.display_number,
		issuedAt: row.issued_at,
		pdf: {
			filename: billingDocumentFilename(row.display_number),
			content: new Uint8Array(row.pdf),
			contentType: 'application/pdf',
		},
	};
}

export function billingDocumentTotals(row: BillingDocumentRow): BillingTotals {
	const content = billingDocumentContent(row);
	return {
		totalMinor: content.totalMinor,
		paidMinor: content.totalMinor,
		currency: content.currency.toLowerCase(),
		tax: content.taxMinor !== 0 ? {amountMinor: content.taxMinor, behavior: 'inclusive'} : null,
		reverseCharge: content.vatTreatment === 'reverse_charge_eu',
	};
}

const FRAUD_DISPUTE_REASONS = new Set<string>(['fraudulent', 'unrecognized']);

type CreditSource = Pick<Stripe.Refund, 'id' | 'created' | 'amount' | 'currency'>;

function creditReason(refund: Stripe.Refund): BillingDocumentCreditReason {
	if (refund.reason === 'fraudulent') return 'fraud';
	if (refund.reason === 'duplicate') return 'duplicate';
	return (refund.metadata?.rejection_reason ?? '').startsWith('operator_pass_withdrawal:') ? 'withdrawal' : 'refund';
}

export class BillingDocumentSourceError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'BillingDocumentSourceError';
	}
}

export function createBillingDocumentService(stripe: Stripe | null): BillingDocumentService | null {
	if (!stripe || Config.instance.selfHosted) {
		return null;
	}
	return new BillingDocumentService(stripe, new BillingDocumentRepository(), new EcbReferenceRates(getKVClient()), () =>
		getSnowflakeService().generate(),
	);
}

export class BillingDocumentService {
	private readonly taxRates = new Map<string, Promise<Stripe.TaxRate>>();
	private readonly seriesStarts = new Map<string, Date | null>();

	constructor(
		private readonly stripe: Stripe,
		readonly repository: BillingDocumentRepository,
		private readonly ecb: EcbReferenceRates,
		private readonly nextId: () => Promise<bigint>,
		private readonly now: () => Date = () => new Date(),
	) {}

	async findDocument(documentId: bigint): Promise<BillingDocumentRow | null> {
		return this.repository.findDocument(documentId);
	}

	async issueForCheckoutSession(sessionId: string, context: IssueContext): Promise<BillingDocumentRow> {
		const existing = await this.repository.findDocumentBySource(`cs:${sessionId}`);
		if (existing) {
			return existing;
		}
		const session = await this.stripe.checkout.sessions.retrieve(sessionId);
		const invoiceId = extractId(session.invoice);
		if (session.mode !== 'payment' || invoiceId) {
			throw new BillingDocumentSourceError(
				invoiceId
					? `Checkout session ${sessionId} has Stripe invoice ${invoiceId}; its sale is documented under in:${invoiceId} or before the cutover, not under cs:`
					: `Checkout session ${sessionId} is a ${session.mode} session; its payments are documented under their in: invoices`,
			);
		}
		const language = billingDocumentLanguage(context.locale);
		const sale = await this.loadCheckoutSale(sessionId, language);
		return this.issueSale(`cs:${sessionId}`, sale, language, context);
	}

	async issueForInvoice(invoiceId: string, context: IssueContext): Promise<BillingDocumentRow | null> {
		const existing = await this.repository.findDocumentBySource(`in:${invoiceId}`);
		if (existing) {
			return existing;
		}
		const language = billingDocumentLanguage(context.locale);
		const sale = await this.loadInvoiceSale(invoiceId, language);
		return sale ? this.issueSale(`in:${invoiceId}`, sale, language, context) : null;
	}

	async issueForRefund(refundOrId: Stripe.Refund | string, context: IssueContext): Promise<BillingDocumentRow | null> {
		const refund = typeof refundOrId === 'string' ? await this.stripe.refunds.retrieve(refundOrId) : refundOrId;
		const own = await this.repository.findDocumentBySource(`re:${refund.id}`);
		if (own) {
			return own;
		}
		const retryOf = refund.metadata?.retry_of;
		if (retryOf) {
			const retried = await this.repository.findDocumentBySource(`re:${retryOf}`);
			if (retried) {
				await this.repository.aliasSource(`re:${refund.id}`, retried.document_id);
				return retried;
			}
		}
		if (refund.status !== 'succeeded' || refund.amount <= 0) {
			return null;
		}
		const paymentIntentId = extractId(refund.payment_intent ?? null);
		if (!paymentIntentId) {
			return null;
		}
		const original = await this.resolveOriginal(paymentIntentId, context);
		if (!original) {
			Logger.warn({refundId: refund.id, paymentIntentId}, 'No original sale found for a refund; no credit note issued');
			return null;
		}
		const chargeId = extractId(refund.charge ?? null) ?? original.sale.references.chargeId;
		const charge = chargeId ? await this.stripe.charges.retrieve(chargeId) : null;
		const creditedBefore = await this.creditedBefore(paymentIntentId, refund, charge, original);
		const amountMinor = refundCreditMinor(refund, charge, original, creditedBefore);
		return this.issueCredit(`re:${refund.id}`, original, context, {
			amountMinor,
			creditedBeforeMinor: creditedBefore,
			creditedQuantity: withdrawnPassCount(refund),
			reason: creditReason(refund),
			at: new Date(refund.created * 1000).toISOString(),
			references: {
				...original.sale.references,
				refundId: refund.id,
				disputeId: null,
				chargeId: extractId(refund.charge ?? null) ?? original.sale.references.chargeId,
			},
		});
	}

	async issueForDispute(disputeId: string, context: IssueContext): Promise<BillingDocumentRow | null> {
		const own = await this.repository.findDocumentBySource(`du:${disputeId}`);
		if (own) {
			return own;
		}
		const dispute = await this.stripe.disputes.retrieve(disputeId);
		if (dispute.status !== 'lost') {
			throw new BillingDocumentSourceError(`Dispute ${disputeId} is ${dispute.status}, not lost`);
		}
		if (FRAUD_DISPUTE_REASONS.has(dispute.reason)) {
			throw new BillingDocumentSourceError(
				`Dispute ${disputeId} is a ${dispute.reason} dispute, which the books treat as a bad debt without a credit note`,
			);
		}
		const paymentIntentId = extractId(dispute.payment_intent ?? null);
		if (!paymentIntentId) {
			return null;
		}
		const original = await this.resolveOriginal(paymentIntentId, context);
		if (!original) {
			Logger.warn({disputeId, paymentIntentId}, 'No original sale found for a lost dispute; no credit note issued');
			return null;
		}
		const chargeId = extractId(dispute.charge ?? null) ?? original.sale.references.chargeId;
		const charge = chargeId ? await this.stripe.charges.retrieve(chargeId) : null;
		const current = {id: dispute.id, created: dispute.created, amount: dispute.amount, currency: dispute.currency};
		const creditedBefore = await this.creditedBefore(paymentIntentId, current, charge, original);
		return this.issueCredit(`du:${dispute.id}`, original, context, {
			amountMinor: refundCreditMinor(current, charge, original, creditedBefore),
			creditedBeforeMinor: creditedBefore,
			creditedQuantity: null,
			reason: 'dispute',
			at: new Date().toISOString(),
			references: {
				...original.sale.references,
				refundId: null,
				disputeId: dispute.id,
				chargeId: chargeId ?? null,
			},
		});
	}

	async reissue(sourceKey: string, context: IssueContext): Promise<BillingDocumentRow | null> {
		const separator = sourceKey.indexOf(':');
		const id = sourceKey.slice(separator + 1);
		switch (separator > 0 && id ? sourceKey.slice(0, separator) : null) {
			case 'cs':
				return this.issueForCheckoutSession(id, context);
			case 'in':
				return this.issueForInvoice(id, context);
			case 're':
				return this.issueForRefund(id, context);
			case 'du':
				return this.issueForDispute(id, context);
			default:
				throw new BillingDocumentSourceError(
					`Billing document source ${sourceKey} is not a cs:, in:, re: or du: source`,
				);
		}
	}

	async reissueContext(sourceKey: string): Promise<IssueContext> {
		const id = sourceKey.slice(sourceKey.indexOf(':') + 1);
		const userId = (value: string | null | undefined) => (value && /^\d+$/.test(value) ? BigInt(value) : null);
		if (sourceKey.startsWith('cs:')) {
			const session = await this.stripe.checkout.sessions.retrieve(id);
			const metadata = session.metadata ?? {};
			return {
				userId: userId(session.client_reference_id ?? metadata.user_id),
				locale: metadata.operator_pass_locale ?? metadata.donation_locale ?? null,
				donorEmail:
					metadata.is_donation === 'true' ? (metadata.donation_email ?? session.customer_details?.email ?? null) : null,
			};
		}
		if (sourceKey.startsWith('in:')) {
			const invoice = await this.stripe.invoices.retrieve(id);
			const metadata = {...invoice.metadata, ...invoice.parent?.subscription_details?.metadata};
			return {
				userId: userId(metadata.user_id),
				locale: metadata.donation_locale ?? null,
				donorEmail:
					metadata.is_donation === 'true' ? (metadata.donation_email ?? invoice.customer_email ?? null) : null,
			};
		}
		if (!sourceKey.startsWith('re:') && !sourceKey.startsWith('du:')) {
			throw new BillingDocumentSourceError(`Billing document source ${sourceKey} is not a cs:, in:, re: or du: source`);
		}
		const source = sourceKey.startsWith('re:')
			? await this.stripe.refunds.retrieve(id)
			: await this.stripe.disputes.retrieve(id);
		const paymentIntentId = extractId(source.payment_intent ?? null);
		const original = paymentIntentId ? await this.repository.findDocumentBySource(`pi:${paymentIntentId}`) : null;
		return {userId: original?.user_id ?? null, locale: original?.language ?? null};
	}

	private async issueSale(
		sourceKey: string,
		sale: SaleData,
		language: BillingDocumentLanguage,
		context: IssueContext,
	): Promise<BillingDocumentRow> {
		const sekTax = await this.sekTax(sale);
		const series = saleSeries(sale.flow);
		const missingAddress = layoutOf(sale.buyer) === 'full' && !sale.buyer.address;
		if (missingAddress) {
			Logger.error(
				{billingDocumentAlert: true, sourceKey, series, country: sale.buyer.country},
				'A full invoice goes to a business buyer without an address; add the address with a corrected invoice',
			);
		}
		const row = await this.issue(sourceKey, series, context, (documentId, number, issuedAt) =>
			buildSaleContent(sale, {documentId, number, issuedAt, language}, sekTax),
		);
		if (missingAddress) {
			await alertSupport({
				dedupeKey: `missing-address:${sourceKey}`,
				subject: `Invoice ${row.display_number} has no buyer address`,
				summary:
					'A full invoice was issued to a business buyer without the buyer address. A full invoice must name the buyer and their address, so send the buyer a corrected invoice.',
				facts: {
					invoiceId: sale.references.stripeInvoiceId,
					paymentIntentId: sale.references.paymentIntentId,
					checkoutSessionId: sale.references.checkoutSessionId,
					userId: context.userId,
					documentNumber: row.display_number,
					series,
					amountMinor: sale.totalMinor,
					currency: sale.currency,
					country: sale.buyer.country,
				},
			});
		}
		if (sale.references.paymentIntentId) {
			await this.repository.aliasSource(`pi:${sale.references.paymentIntentId}`, row.document_id);
		}
		return row;
	}

	private async issueCredit(
		sourceKey: string,
		original: CreditOriginal,
		context: IssueContext,
		request: Parameters<typeof buildCreditContent>[1],
	): Promise<BillingDocumentRow> {
		const language = original.language ?? billingDocumentLanguage(context.locale);
		return this.issue(sourceKey, creditSeries(original), context, (documentId, number, issuedAt) =>
			buildCreditContent(original, request, {documentId, number, issuedAt, language}),
		);
	}

	private async issue(
		sourceKey: string,
		series: BillingDocumentSeries,
		context: IssueContext,
		build: (documentId: string, number: string, issuedAt: string) => BillingDocumentContent,
	): Promise<BillingDocumentRow> {
		const source = await this.repository.claimSource(sourceKey, await this.nextId());
		const documentId = source.document_id;
		const done = await this.repository.findDocument(documentId);
		if (done) {
			return done;
		}
		const claimed = await this.repository.claimNumber(series, sourceKey, documentId);
		const issuedAt = this.now();
		const content = build(
			documentId.toString(),
			formatBillingDocumentNumber(series, claimed.number),
			issuedAt.toISOString(),
		);
		const json = JSON.stringify(content);
		const pdf = await renderBillingDocumentPdf(content);
		const original = content.original?.documentId ? BigInt(content.original.documentId) : null;
		const row = await this.repository.createDocument({
			document_id: documentId,
			series,
			number: claimed.number,
			display_number: content.number,
			kind: content.kind,
			flow: content.flow,
			layout: content.layout,
			issued_at: issuedAt,
			supply_date: content.supplyDate,
			period_start: content.periodStart ? new Date(content.periodStart) : null,
			period_end: content.periodEnd ? new Date(content.periodEnd) : null,
			currency: content.currency,
			total_minor: BigInt(content.totalMinor),
			tax_minor: BigInt(content.taxMinor),
			tax_lines: JSON.stringify(content.taxLines),
			sek_tax_minor: content.sekTax ? BigInt(content.sekTax.amountMinor) : null,
			fx_rate: content.sekTax?.rate ?? null,
			fx_date: content.sekTax?.rateDate ?? null,
			buyer: JSON.stringify(content.buyer),
			seller: JSON.stringify(content.seller),
			language: content.language,
			original_document_id: original,
			original_number: content.original?.number ?? null,
			reason: content.reason,
			source_key: sourceKey,
			stripe_refs: JSON.stringify(content.references),
			user_id: context.userId,
			donor_email_hash: context.donorEmail ? sha256(context.donorEmail.trim().toLowerCase()) : null,
			content: json,
			content_sha256: sha256(json),
			pdf: Buffer.from(pdf),
			pdf_sha256: sha256(pdf),
			render_version: content.renderVersion,
			emailed_at: null,
			archived_at: null,
		});
		Logger.info({documentId: row.document_id, number: row.display_number, sourceKey}, 'Issued a billing document');
		return row;
	}

	private async sekTax(sale: SaleData): Promise<BillingDocumentSekTax | null> {
		const seTax = swedishTaxMinor(sale.taxLines);
		if (seTax === 0 || sale.currency.toUpperCase() === 'SEK' || sale.flow === 'gift' || sale.flow === 'donation') {
			return null;
		}
		const rate = await this.ecb.sekRate(sale.currency, sale.payment ? new Date(sale.payment.at) : this.now());
		return {
			amountMinor: sekMinor(seTax, sale.currency, rate.rate),
			rate: rate.rate,
			rateDate: rate.rateDate,
			source: 'ECB',
		};
	}

	private async buyerVerification(
		customerId: string | null,
		vatId: string | null,
	): Promise<BillingDocumentVatIdVerification | null> {
		if (!vatId) {
			return null;
		}
		if (!customerId) {
			return 'unavailable';
		}
		const ids = await this.stripe.customers.listTaxIds(customerId, {limit: 20});
		const match = ids.data.find((id) => id.value === vatId);
		switch (match?.verification?.status) {
			case 'verified':
				return 'verified';
			case 'unverified':
				return 'unverified';
			case 'pending':
				return 'pending';
			default:
				return 'unavailable';
		}
	}

	private async taxRate(id: string): Promise<Stripe.TaxRate> {
		let rate = this.taxRates.get(id);
		if (!rate) {
			rate = this.stripe.taxRates.retrieve(id);
			this.taxRates.set(id, rate);
		}
		return rate;
	}

	async loadCheckoutSale(sessionId: string, language: BillingDocumentLanguage): Promise<SaleData> {
		const session = await this.stripe.checkout.sessions.retrieve(sessionId, {
			expand: ['total_details.breakdown', 'payment_intent.latest_charge'],
		});
		if (session.payment_status !== 'paid' && session.payment_status !== 'no_payment_required') {
			throw new Error(`Checkout session ${sessionId} is not paid yet`);
		}
		const items = await this.stripe.checkout.sessions.listLineItems(sessionId, {
			limit: LINE_ITEM_LIMIT,
			expand: ['data.taxes'],
		});
		const flow = checkoutFlow(session);
		const descriptions = DESCRIPTIONS[language];
		const branding = await getBillingBranding();
		const premium = `${branding.productName} ${branding.premiumName}`;
		const intent = typeof session.payment_intent === 'object' ? session.payment_intent : null;
		const charge = intent && typeof intent.latest_charge === 'object' ? intent.latest_charge : null;
		const paidAt = charge ? new Date(charge.created * 1000) : new Date(session.created * 1000);
		const description = (item: Stripe.LineItem): string => {
			switch (flow) {
				case 'operator_pass':
					return descriptions.operatorPass;
				case 'gift':
					return descriptions.gift(premium, session.metadata?.product_type === ProductType.GIFT_1_YEAR);
				case 'donation':
					return descriptions.donation('once');
				default:
					return item.description ?? premium;
			}
		};
		const lines: Array<SaleLine> = items.data.map((item) => ({
			description: description(item),
			quantity: item.quantity ?? 1,
			amountMinor: item.amount_total,
			taxMinor: item.amount_tax,
			ratePercent: taxRatePercent(item.taxes?.find((tax) => tax.amount > 0)?.rate),
			periodStart: null,
			periodEnd: null,
		}));
		const taxLines = untaxedAtNet(
			(session.total_details?.breakdown?.taxes ?? []).map((tax) => ({
				country: tax.rate.country ?? null,
				jurisdiction: tax.rate.jurisdiction ?? null,
				ratePercent: tax.rate.percentage ?? null,
				taxType: tax.rate.tax_type ?? null,
				taxabilityReason: tax.taxability_reason ?? null,
				taxableMinor: tax.taxable_amount ?? 0,
				taxMinor: tax.amount,
			})),
			lines,
		);
		const details = session.customer_details;
		const vatId = details?.tax_ids?.[0]?.value ?? null;
		const customerId = extractId(session.customer);
		const business = (details?.tax_ids?.length ?? 0) > 0 || Boolean(details?.business_name);
		const buyer: BillingDocumentBuyer = {
			business,
			name: business ? (details?.name ?? null) : null,
			businessName: details?.business_name ?? null,
			email: details?.email ?? session.customer_email ?? null,
			address: business ? address(details?.address) : null,
			country: details?.address?.country ?? taxLines[0]?.country ?? null,
			vatId,
			vatIdVerification: await this.buyerVerification(customerId, vatId),
		};
		return {
			flow,
			currency: session.currency ?? charge?.currency ?? 'usd',
			totalMinor: session.amount_total ?? 0,
			lines,
			taxLines,
			buyer,
			supplyDate: stockholmDate(paidAt.toISOString()),
			periodStart: null,
			periodEnd: null,
			payment: {at: paidAt.toISOString(), method: describeCharge(charge)},
			references: {
				checkoutSessionId: session.id,
				paymentIntentId: intent?.id ?? extractId(session.payment_intent),
				chargeId: charge?.id ?? null,
				stripeInvoiceId: null,
				refundId: null,
				disputeId: null,
			},
		};
	}

	async loadInvoiceSale(invoiceId: string, language: BillingDocumentLanguage): Promise<SaleData | null> {
		const invoice = await this.stripe.invoices.retrieve(invoiceId, {
			expand: ['payments.data.payment.payment_intent'],
		});
		if (invoice.status !== 'paid' || invoice.total <= 0) {
			return null;
		}
		const donation = invoice.parent?.subscription_details?.metadata?.is_donation === 'true';
		const descriptions = DESCRIPTIONS[language];
		const branding = await getBillingBranding();
		const premium = `${branding.productName} ${branding.premiumName}`;
		const payment = (invoice.payments?.data ?? []).find((entry) => entry.status === 'paid')?.payment;
		const intent = typeof payment?.payment_intent === 'object' ? payment.payment_intent : null;
		const latestCharge = intent?.latest_charge ?? null;
		const charge =
			typeof latestCharge === 'string' ? await this.stripe.charges.retrieve(latestCharge) : (latestCharge ?? null);
		const taxLinesById = new Map<string, BillingDocumentTaxLine>();
		for (const tax of invoice.total_taxes ?? []) {
			const id = tax.tax_rate_details?.tax_rate;
			const rate = id ? await this.taxRate(id) : null;
			const key = id ?? `${tax.taxability_reason}`;
			const current = taxLinesById.get(key);
			taxLinesById.set(key, {
				country: rate?.country ?? null,
				jurisdiction: rate?.jurisdiction ?? null,
				ratePercent: rate?.percentage ?? null,
				taxType: rate?.tax_type ?? null,
				taxabilityReason: tax.taxability_reason ?? null,
				taxableMinor: (current?.taxableMinor ?? 0) + (tax.taxable_amount ?? 0),
				taxMinor: (current?.taxMinor ?? 0) + tax.amount,
			});
		}
		const lines: Array<SaleLine> = [];
		for (const line of invoice.lines?.data ?? []) {
			const taxes = line.taxes ?? [];
			const taxMinor = taxes.reduce((sum, tax) => sum + tax.amount, 0);
			const discounts = (line.discount_amounts ?? []).reduce((sum, discount) => sum + discount.amount, 0);
			const exclusive = taxes.some((tax) => tax.tax_behavior === 'exclusive');
			const rateId = taxes.find((tax) => tax.amount > 0)?.tax_rate_details?.tax_rate;
			const proration = Boolean(
				line.parent?.subscription_item_details?.proration || line.parent?.invoice_item_details?.proration,
			);
			const yearly = line.period ? line.period.end - line.period.start > YEARLY_PERIOD_MIN_SECONDS : false;
			const planDescription = proration ? descriptions.proration(premium) : descriptions.plan(premium, yearly);
			lines.push({
				description: donation ? descriptions.donation(yearly ? 'year' : 'month') : planDescription,
				quantity: line.quantity ?? 1,
				amountMinor: line.amount - discounts + (exclusive ? taxMinor : 0),
				taxMinor,
				ratePercent: rateId ? ((await this.taxRate(rateId)).percentage ?? null) : null,
				periodStart: isoFromUnix(line.period?.start),
				periodEnd: isoFromUnix(line.period?.end),
			});
		}
		const linesTotal = lines.reduce((sum, line) => sum + line.amountMinor, 0);
		const last = lines.at(-1);
		if (last && linesTotal !== invoice.total) {
			Logger.warn(
				{invoiceId, linesTotal, total: invoice.total},
				'Invoice lines do not add up to the total; the last line takes the difference',
			);
			last.amountMinor += invoice.total - linesTotal;
		}
		const taxLines = untaxedAtNet([...taxLinesById.values()], lines);
		const vatId = invoice.customer_tax_ids?.[0]?.value ?? null;
		const customerId = extractId(invoice.customer);
		const primary = lines.find((line) => line.periodStart && line.amountMinor > 0) ?? lines[0];
		const paidAt = isoFromUnix(invoice.status_transitions?.paid_at) ?? new Date(invoice.created * 1000).toISOString();
		const business = (invoice.customer_tax_ids?.length ?? 0) > 0;
		const buyer: BillingDocumentBuyer = {
			business,
			name: business ? (invoice.customer_name ?? null) : null,
			businessName: business ? (invoice.customer_name ?? null) : null,
			email: invoice.customer_email ?? null,
			address: business ? address(invoice.customer_address) : null,
			country: invoice.customer_address?.country ?? taxLines[0]?.country ?? null,
			vatId,
			vatIdVerification: await this.buyerVerification(customerId, vatId),
		};
		return {
			flow: donation ? 'donation' : 'plutonium',
			currency: invoice.currency,
			totalMinor: invoice.total,
			lines,
			taxLines,
			buyer,
			supplyDate: null,
			periodStart: primary?.periodStart ?? null,
			periodEnd: primary?.periodEnd ?? null,
			payment: {at: paidAt, method: describeCharge(charge)},
			references: {
				checkoutSessionId: null,
				paymentIntentId: intent?.id ?? null,
				chargeId: charge?.id ?? null,
				stripeInvoiceId: invoice.id ?? null,
				refundId: null,
				disputeId: null,
			},
		};
	}

	private async seriesStartedAt(series: BillingDocumentSeries): Promise<Date | null> {
		if (this.seriesStarts.has(series)) {
			return this.seriesStarts.get(series) ?? null;
		}
		const first = await this.repository.findNumber(series, 1n);
		const document = first?.document_id ? await this.repository.findDocument(first.document_id) : null;
		const startedAt = document?.issued_at ?? null;
		if (startedAt) {
			this.seriesStarts.set(series, startedAt);
		}
		return startedAt;
	}

	private async issuedAfterCutover(flow: BillingDocumentFlow, paidAt: Date): Promise<boolean> {
		const startedAt = await this.seriesStartedAt(saleSeries(flow));
		return startedAt != null && paidAt.getTime() >= startedAt.getTime();
	}

	private async resolveOriginal(paymentIntentId: string, context: IssueContext): Promise<CreditOriginal | null> {
		const known = await this.repository.findDocumentBySource(`pi:${paymentIntentId}`);
		if (known) {
			return creditOriginalFromContent(billingDocumentContent(known));
		}
		const language = billingDocumentLanguage(context.locale);
		const sessions = await this.stripe.checkout.sessions.list({payment_intent: paymentIntentId, limit: 1});
		const session = sessions.data[0];
		if (session && session.mode === 'payment') {
			const flow = checkoutFlow(session);
			const legacyInvoiceId = extractId(session.invoice);
			if (!legacyInvoiceId && (await this.issuedAfterCutover(flow, new Date(session.created * 1000)))) {
				const issued = await this.issueForCheckoutSession(session.id, context);
				return creditOriginalFromContent(billingDocumentContent(issued));
			}
			const sale = await this.loadCheckoutSale(session.id, language);
			const legacy = legacyInvoiceId ? await this.stripe.invoices.retrieve(legacyInvoiceId) : null;
			return this.legacyOriginal(sale, legacy, paymentIntentId);
		}
		const invoice = await this.findSubscriptionInvoice(paymentIntentId);
		if (!invoice?.id) {
			return null;
		}
		const issuedDocument = await this.repository.findDocumentBySource(`in:${invoice.id}`);
		if (issuedDocument) {
			return creditOriginalFromContent(billingDocumentContent(issuedDocument));
		}
		const donation = invoice.parent?.subscription_details?.metadata?.is_donation === 'true';
		const paidAt = new Date((invoice.status_transitions?.paid_at ?? invoice.created) * 1000);
		if (await this.issuedAfterCutover(donation ? 'donation' : 'plutonium', paidAt)) {
			const issued = await this.issueForInvoice(invoice.id, context);
			if (issued) {
				return creditOriginalFromContent(billingDocumentContent(issued));
			}
		}
		const sale = await this.loadInvoiceSale(invoice.id, language);
		return sale ? this.legacyOriginal(sale, invoice, paymentIntentId) : null;
	}

	private legacyOriginal(sale: SaleData, invoice: Stripe.Invoice | null, paymentIntentId: string): CreditOriginal {
		const reference = invoice?.number
			? {
					number: invoice.number,
					issuedAt:
						isoFromUnix(invoice.status_transitions?.finalized_at) ?? new Date(invoice.created * 1000).toISOString(),
					documentId: null,
				}
			: {
					number: sale.references.chargeId ?? paymentIntentId,
					issuedAt: sale.payment?.at ?? this.now().toISOString(),
					documentId: null,
				};
		return {
			sale,
			language: null,
			reference,
			layout: invoice ? layoutOf(sale.buyer) : 'simplified',
			vatTreatment: sale.flow === 'gift' && invoice && chargedTax(sale) > 0 ? 'charged' : vatTreatmentOf(sale),
			sekRate: null,
		};
	}

	private async findSubscriptionInvoice(paymentIntentId: string): Promise<Stripe.Invoice | null> {
		const payments = await this.stripe.invoicePayments.list({
			payment: {type: 'payment_intent', payment_intent: paymentIntentId},
			status: 'paid',
			limit: 1,
		});
		const invoiceId = extractId(payments.data[0]?.invoice ?? null);
		return invoiceId ? this.stripe.invoices.retrieve(invoiceId) : null;
	}

	private async creditedBefore(
		paymentIntentId: string,
		current: CreditSource,
		charge: Stripe.Charge | null,
		original: CreditOriginal,
	): Promise<number> {
		const refunds = await this.stripe.refunds.list({payment_intent: paymentIntentId, limit: 100});
		let total = 0;
		for (const refund of refunds.data) {
			const earlier =
				refund.created < current.created || (refund.created === current.created && refund.id < current.id);
			if (refund.id === current.id || !earlier) {
				continue;
			}
			const credited = await this.repository.findSource(`re:${refund.id}`);
			const retried = refund.metadata?.retry_of
				? await this.repository.findSource(`re:${refund.metadata.retry_of}`)
				: null;
			if (credited && !retried) {
				const document = await this.repository.findDocument(credited.document_id);
				total += document ? -Number(document.total_minor) : 0;
			} else if (!credited && !retried && refund.status === 'succeeded') {
				total += refundCreditMinor(refund, charge, original, total);
			}
		}
		return total;
	}
}

function remainingMinor(original: CreditOriginal, creditedBefore: number): number {
	return Math.max(0, original.sale.totalMinor - creditedBefore);
}

function convertedCharge(refund: CreditSource, charge: Stripe.Charge, original: CreditOriginal): boolean {
	const presentment = charge.presentment_details?.presentment_currency?.toLowerCase();
	return (
		charge.currency !== original.sale.currency.toLowerCase() ||
		(refund.currency != null && refund.currency !== charge.currency) ||
		(presentment != null && presentment !== charge.currency)
	);
}

function refundCreditMinor(
	refund: CreditSource,
	charge: Stripe.Charge | null,
	original: CreditOriginal,
	creditedBefore: number,
): number {
	const remaining = remainingMinor(original, creditedBefore);
	if (!charge || !convertedCharge(refund, charge, original)) {
		return Math.min(refund.amount, remaining);
	}
	if (refund.currency && refund.currency !== charge.currency) {
		Logger.error(
			{
				billingDocumentAlert: true,
				refundId: refund.id,
				refundCurrency: refund.currency,
				chargeCurrency: charge.currency,
			},
			'A refund is in another currency than its charge; check the credit note amount',
		);
		void alertSupport({
			dedupeKey: `credit-currency:${refund.id}`,
			subject: `Check the credit note amount for ${refund.id}`,
			summary: `A refund or dispute is in ${refund.currency.toUpperCase()} but its charge was in ${charge.currency.toUpperCase()}. Check that the credit note amount is right.`,
			facts: {
				...(refund.id.startsWith('re_') ? {refundId: refund.id} : {disputeId: refund.id}),
				paymentIntentId: original.sale.references.paymentIntentId,
				documentNumber: original.reference.number,
				amountMinor: refund.amount,
				currency: refund.currency,
			},
		});
	}
	if (refund.amount >= charge.amount || charge.amount <= 0) {
		return remaining;
	}
	return Math.min(Math.round((refund.amount * original.sale.totalMinor) / charge.amount), remaining);
}

function withdrawnPassCount(refund: Stripe.Refund): number | null {
	const scope = (refund.metadata?.rejection_reason ?? '').match(/^operator_pass_withdrawal:(.+)$/)?.[1];
	return scope && !scope.startsWith('all:') ? 1 : null;
}

function untaxedAtNet(
	taxLines: ReadonlyArray<BillingDocumentTaxLine>,
	lines: ReadonlyArray<SaleLine>,
): Array<BillingDocumentTaxLine> {
	const untaxedMinor = lines.filter((line) => line.taxMinor === 0).reduce((sum, line) => sum + line.amountMinor, 0);
	return taxLines.map((line) =>
		line.taxMinor === 0
			? {...line, ratePercent: null, taxableMinor: line.taxableMinor > 0 ? line.taxableMinor : untaxedMinor}
			: line,
	);
}

function checkoutFlow(session: Stripe.Checkout.Session): BillingDocumentFlow {
	if (session.metadata?.operator_pass === 'true') return 'operator_pass';
	if (session.metadata?.is_donation === 'true') return 'donation';
	if (session.metadata?.is_gift === 'true') return 'gift';
	return 'plutonium';
}
