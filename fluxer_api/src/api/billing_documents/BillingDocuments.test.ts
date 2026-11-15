// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildCreditContent,
	buildSaleContent,
	creditOriginalFromContent,
	type SaleData,
} from '@app/api/billing_documents/BillingDocumentBuilder';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {BillingDocumentService, billingDocumentContent} from '@app/api/billing_documents/BillingDocumentService';
import {EcbReferenceRates, latestPublishedEcbDate} from '@app/api/billing_documents/EcbReferenceRates';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	type BillingDocumentBuyer,
	type BillingDocumentTaxLine,
	stockholmDate,
} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import type Stripe from 'stripe';
import {afterAll, beforeAll, describe, expect, test, vi} from 'vitest';

const loggerError = vi.hoisted(() => vi.fn());

vi.mock('@app/api/Logger', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/api/Logger')>();
	return {
		...actual,
		Logger: new Proxy(actual.Logger, {
			get: (target, prop) => (prop === 'error' ? loggerError : Reflect.get(target, prop)),
		}),
	};
});

let harness: ApiTestHarness;

beforeAll(async () => {
	harness = await createApiTestHarness();
});

afterAll(async () => {
	await harness.shutdown();
});

const HEADER = {documentId: '1', number: 'OP-000001', issuedAt: '2026-11-01T16:05:00.000Z', language: 'en' as const};

function buyer(patch: Partial<BillingDocumentBuyer> = {}): BillingDocumentBuyer {
	return {
		business: false,
		name: null,
		businessName: null,
		email: 'buyer@example.com',
		address: null,
		country: 'SE',
		vatId: null,
		vatIdVerification: null,
		...patch,
	};
}

function tax(country: string, ratePercent: number, taxableMinor: number, taxMinor: number, reason = 'standard_rated') {
	return {
		country,
		jurisdiction: country,
		ratePercent,
		taxType: 'vat',
		taxabilityReason: reason,
		taxableMinor,
		taxMinor,
	} satisfies BillingDocumentTaxLine;
}

function sale(patch: Partial<SaleData> & Pick<SaleData, 'taxLines'>): SaleData {
	const taxMinor = patch.taxLines.reduce((sum, line) => sum + line.taxMinor, 0);
	return {
		flow: 'operator_pass',
		currency: 'sek',
		totalMinor: 179900,
		lines: [
			{
				description: 'Operator Pass',
				quantity: 1,
				amountMinor: patch.totalMinor ?? 179900,
				taxMinor,
				ratePercent: patch.taxLines[0]?.ratePercent ?? null,
				periodStart: null,
				periodEnd: null,
			},
		],
		buyer: buyer(),
		supplyDate: '2026-11-01',
		periodStart: null,
		periodEnd: null,
		payment: {at: '2026-11-01T16:04:00.000Z', method: 'Visa ···· 4242'},
		references: {
			checkoutSessionId: 'cs_1',
			paymentIntentId: 'pi_1',
			chargeId: 'ch_1',
			stripeInvoiceId: null,
			refundId: null,
			disputeId: null,
		},
		...patch,
	};
}

describe('document numbers', () => {
	test('fifty concurrent issues on one series take 1 to 50 with no gap and no repeat', async () => {
		const repository = new BillingDocumentRepository();
		const claims = await Promise.all(
			Array.from({length: 50}, (_, index) => repository.claimNumber('ZZ', `cs:concurrent_${index}`, BigInt(index + 1))),
		);
		const numbers = claims.map((claim) => Number(claim.number)).sort((left, right) => left - right);
		expect(numbers).toEqual(Array.from({length: 50}, (_, index) => index + 1));
		const rows = await repository.listSeriesClaims('ZZ');
		expect(new Set(rows.map((row) => row.source_key)).size).toBe(50);
	});

	test('the same source racing itself and retrying after a crash keeps one number', async () => {
		const repository = new BillingDocumentRepository();
		const racing = await Promise.all(Array.from({length: 8}, () => repository.claimNumber('ZY', 'in:in_same', 7n)));
		expect(new Set(racing.map((claim) => claim.number))).toEqual(new Set([1n]));
		await repository.claimNumber('ZY', 'in:in_other', 8n);
		const retried = await repository.claimNumber('ZY', 'in:in_same', 7n);
		expect(retried.number).toBe(1n);
		expect((await repository.listSeriesClaims('ZY')).map((row) => row.number)).toEqual([1n, 2n]);
	});
});

describe('invoice content', () => {
	test('a Swedish consumer gets a receipt with Swedish VAT and only the Swedish VAT number', () => {
		const content = buildSaleContent(sale({taxLines: [tax('SE', 25, 143920, 35980)]}), HEADER, null);
		expect(content).toMatchObject({kind: 'invoice', layout: 'simplified', vatTreatment: 'charged', taxMinor: 35980});
		expect(content.subtotalExclMinor + content.taxMinor).toBe(content.totalMinor);
		expect(content.lines[0]).toMatchObject({amountExclMinor: 143920, taxMinor: 35980, ratePercent: 25});
		expect(content.seller.vatNumbers).toEqual(['SE559537399301']);
		expect(content.seller).toMatchObject({name: 'Fluxer Platform AB', organisationNumber: '559537-3993'});
	});

	test('an EU business with a VAT ID gets a full invoice under reverse charge', () => {
		const content = buildSaleContent(
			sale({
				currency: 'eur',
				totalMinor: 19900,
				taxLines: [tax('DE', 19, 19900, 0, 'reverse_charge')],
				buyer: buyer({
					business: true,
					businessName: 'Beispiel GmbH',
					country: 'DE',
					vatId: 'DE123456789',
					vatIdVerification: 'verified',
				}),
			}),
			HEADER,
			null,
		);
		expect(content).toMatchObject({layout: 'full', vatTreatment: 'reverse_charge_eu', taxMinor: 0, totalMinor: 19900});
		expect(content.buyer.vatId).toBe('DE123456789');
		expect(content.taxLines).toHaveLength(1);
	});

	test('a US business is outside the scope of Swedish VAT and a US consumer is told no VAT was charged', () => {
		const business = buildSaleContent(
			sale({currency: 'usd', taxLines: [], buyer: buyer({business: true, country: 'US', vatId: 'US12-3456789'})}),
			HEADER,
			null,
		);
		expect(business).toMatchObject({layout: 'full', vatTreatment: 'outside_scope'});
		const consumer = buildSaleContent(
			sale({currency: 'usd', taxLines: [], buyer: buyer({country: 'US'})}),
			HEADER,
			null,
		);
		expect(consumer).toMatchObject({layout: 'simplified', vatTreatment: 'not_charged'});
	});

	test('UK and Norwegian VAT add the GB and VOEC numbers', () => {
		expect(
			buildSaleContent(
				sale({currency: 'gbp', taxLines: [tax('GB', 20, 1666, 333)], buyer: buyer({country: 'GB'})}),
				HEADER,
				null,
			).seller.vatNumbers,
		).toEqual(['SE559537399301', 'GB518363191']);
		expect(
			buildSaleContent(
				sale({currency: 'nok', taxLines: [tax('NO', 25, 4000, 1000)], buyer: buyer({country: 'NO'})}),
				HEADER,
				null,
			).seller.vatNumbers,
		).toEqual(['SE559537399301', 'VOEC 3058085']);
	});

	test('gift and donation documents never show VAT, even when Stripe calculated some', () => {
		const gift = buildSaleContent(sale({flow: 'gift', taxLines: [tax('GB', 20, 4166, 833)]}), HEADER, null);
		expect(gift).toMatchObject({kind: 'receipt', series: 'GC', vatTreatment: 'voucher', taxMinor: 0, taxLines: []});
		expect(gift.lines.every((line) => line.taxMinor === 0 && line.ratePercent === null)).toBe(true);
		const donation = buildSaleContent(sale({flow: 'donation', taxLines: []}), HEADER, null);
		expect(donation).toMatchObject({kind: 'donation_receipt', series: 'DN', vatTreatment: 'donation', taxMinor: 0});
	});
});

describe('document dates', () => {
	test('a document issued just before midnight UTC on New Year is dated in Stockholm time', () => {
		expect(stockholmDate('2026-12-31T23:30:00.000Z')).toBe('2027-01-01');
		expect(stockholmDate('2026-06-30T22:30:00.000Z')).toBe('2026-07-01');
		expect(stockholmDate('2027-01-01')).toBe('2027-01-01');
	});
});

describe('credit notes', () => {
	test('partial refunds credit the invoice VAT per rate and together never credit more than it', () => {
		const invoice = buildSaleContent(
			sale({totalMinor: 3000, taxLines: [tax('SE', 25, 2400, 600)]}),
			{...HEADER, number: 'OP-000009'},
			null,
		);
		const original = creditOriginalFromContent(invoice);
		const refunds = [1001, 999, 1000];
		let before = 0;
		const notes = refunds.map((amount, index) => {
			const note = buildCreditContent(
				original,
				{
					amountMinor: amount,
					creditedBeforeMinor: before,
					reason: 'refund',
					at: '2026-11-03T10:00:00.000Z',
					references: {...invoice.references, refundId: `re_${index}`},
				},
				{...HEADER, number: `KR-00000${index + 1}`},
			);
			before += amount;
			return note;
		});
		for (const note of notes) {
			expect(note.original).toEqual({number: 'OP-000009', issuedAt: invoice.issuedAt, documentId: invoice.documentId});
			expect(note).toMatchObject({kind: 'credit_note', series: 'KR', layout: invoice.layout});
			expect(note.subtotalExclMinor + note.taxMinor).toBe(note.totalMinor);
			expect(note.lines.reduce((sum, line) => sum + line.amountExclMinor + line.taxMinor, 0)).toBe(note.totalMinor);
		}
		expect(notes.map((note) => note.totalMinor)).toEqual([-1001, -999, -1000]);
		expect(notes.reduce((sum, note) => sum + note.taxMinor, 0)).toBe(-600);
		expect(notes.every((note) => Math.abs(note.taxMinor) <= 201)).toBe(true);
	});

	test('withdrawing one of three passes credits one pass at the original unit price', () => {
		const invoice = buildSaleContent(
			sale({
				totalMinor: 62700,
				lines: [
					{
						description: 'Operator Pass',
						quantity: 3,
						amountMinor: 62700,
						taxMinor: 12540,
						ratePercent: 25,
						periodStart: null,
						periodEnd: null,
					},
				],
				taxLines: [tax('SE', 25, 50160, 12540)],
			}),
			HEADER,
			null,
		);
		const original = creditOriginalFromContent(invoice);
		const request = {
			amountMinor: 20900,
			creditedBeforeMinor: 0,
			reason: 'withdrawal' as const,
			at: '2026-11-03T10:00:00.000Z',
			references: invoice.references,
		};
		for (const note of [
			buildCreditContent(original, {...request, creditedQuantity: 1}, {...HEADER, number: 'KR-000002'}),
			buildCreditContent(original, request, {...HEADER, number: 'KR-000003'}),
		]) {
			expect(note.lines[0]).toMatchObject({quantity: 1, unitAmountExclMinor: -16720, amountExclMinor: -16720});
			expect(note.totalMinor).toBe(-20900);
		}
		const reduction = buildCreditContent(original, {...request, amountMinor: 5000}, {...HEADER, number: 'KR-000004'});
		expect(reduction.lines[0]).toMatchObject({quantity: 1, amountExclMinor: -4000});
	});

	test('a credit beyond what is left on the invoice is capped', () => {
		const invoice = buildSaleContent(sale({totalMinor: 3000, taxLines: [tax('SE', 25, 2400, 600)]}), HEADER, null);
		const note = buildCreditContent(
			creditOriginalFromContent(invoice),
			{
				amountMinor: 3000,
				creditedBeforeMinor: 2000,
				reason: 'refund',
				at: '2026-11-20T10:00:00.000Z',
				references: invoice.references,
			},
			{...HEADER, number: 'KR-000004'},
		);
		expect(note.totalMinor).toBe(-1000);
		expect(note.taxMinor).toBe(-200);
	});

	test('a gift sold before the cutover with VAT on its Stripe invoice gets a credit note for that VAT', () => {
		const giftSale = sale({flow: 'gift', currency: 'gbp', totalMinor: 4999, taxLines: [tax('GB', 20, 4166, 833)]});
		const request = {
			amountMinor: 4999,
			creditedBeforeMinor: 0,
			reason: 'refund' as const,
			at: '2026-11-03T10:00:00.000Z',
			references: giftSale.references,
		};
		const original = {
			sale: giftSale,
			language: null,
			reference: {number: 'INV-0001', issuedAt: '2026-10-01T10:00:00.000Z', documentId: null},
			layout: 'simplified' as const,
			sekRate: null,
		};
		const legacy = buildCreditContent({...original, vatTreatment: 'charged'}, request, {
			...HEADER,
			number: 'KR-000005',
		});
		expect(legacy).toMatchObject({kind: 'credit_note', series: 'KR', taxMinor: -833, totalMinor: -4999});
		const voucher = buildCreditContent({...original, vatTreatment: 'voucher'}, request, {
			...HEADER,
			number: 'KG-000001',
		});
		expect(voucher).toMatchObject({kind: 'refund_note', series: 'KG', taxMinor: 0, taxLines: [], totalMinor: -4999});
	});
});

interface FakeCheckout {
	currency?: string;
	amount?: number;
	taxes?: Array<{amount: number; taxable_amount: number; taxability_reason: string; rate: Record<string, unknown>}>;
	lineTax?: number;
	lineTaxes?: Array<{amount: number; rate: {percentage: number}}>;
	customerDetails?: Record<string, unknown>;
	presentment?: {presentment_amount: number; presentment_currency: string} | null;
	tag?: string;
	refunded?: boolean;
	refund?: Partial<Stripe.Refund>;
	chargeAmount?: number;
	session?: Record<string, unknown>;
}

function fakeCheckoutStripe(options: FakeCheckout = {}): Stripe {
	const currency = options.currency ?? 'sek';
	const amount = options.amount ?? 179900;
	const tag = options.tag ?? 'op';
	const session = {
		id: `cs_fake_${tag}`,
		object: 'checkout.session',
		mode: 'payment',
		payment_status: 'paid',
		currency,
		amount_total: amount,
		created: 1793548800,
		metadata: {operator_pass: 'true'},
		customer: null,
		customer_email: 'buyer@example.com',
		customer_details: options.customerDetails ?? {email: 'buyer@example.com', address: {country: 'SE'}, tax_ids: []},
		invoice: null,
		payment_intent: {
			id: `pi_fake_${tag}`,
			latest_charge: {
				id: `ch_fake_${tag}`,
				created: 1793548860,
				currency,
				payment_method_details: {type: 'card', card: {brand: 'visa', last4: '4242'}},
			},
		},
		total_details: {
			breakdown: {
				taxes: options.taxes ?? [
					{
						amount: 35980,
						taxable_amount: 143920,
						taxability_reason: 'standard_rated',
						rate: {percentage: 25, country: 'SE', jurisdiction: 'Sweden', tax_type: 'vat'},
					},
				],
			},
		},
	};
	Object.assign(session, options.session);
	const charge = {
		id: `ch_fake_${tag}`,
		amount: options.chargeAmount ?? amount,
		amount_refunded: 0,
		currency,
		presentment_details: options.presentment ?? null,
	};
	const refund = {
		id: `re_fake_${tag}`,
		object: 'refund',
		status: 'succeeded',
		amount,
		currency,
		created: 1793635200,
		payment_intent: `pi_fake_${tag}`,
		charge: `ch_fake_${tag}`,
		metadata: {rejection_reason: 'operator_pass_withdrawal:all:2026-11-02T10:00:00Z'},
		...options.refund,
	};
	return {
		checkout: {
			sessions: {
				retrieve: async () => session,
				listLineItems: async () => ({
					data: [
						{
							quantity: 1,
							amount_total: amount,
							amount_tax: options.lineTax ?? 35980,
							description: 'Operator Pass',
							taxes: options.lineTaxes ?? [{amount: 35980, rate: {percentage: 25}}],
						},
					],
				}),
				list: async () => ({data: [session]}),
			},
		},
		refunds: {
			retrieve: async () => refund,
			list: async () => ({data: options.refunded === false ? [] : [refund]}),
		},
		charges: {retrieve: async () => charge},
		disputes: {
			retrieve: async (id: string) => ({
				id,
				object: 'dispute',
				status: 'lost',
				reason: id.includes('fraud') ? 'fraudulent' : 'product_not_received',
				amount,
				currency,
				created: 1793721600,
				payment_intent: `pi_fake_${tag}`,
				charge: `ch_fake_${tag}`,
			}),
		},
		customers: {listTaxIds: async () => ({data: []})},
	} as unknown as Stripe;
}

test('an Operator Pass invoice is issued once, stored byte for byte, and its credit note names it', async () => {
	let nextId = 5_000_000n;
	const repository = new BillingDocumentRepository();
	const service = new BillingDocumentService(
		fakeCheckoutStripe(),
		repository,
		new EcbReferenceRates(getKVClient(), async () => ''),
		async () => nextId++,
	);
	const context = {userId: 42n, locale: 'sv-SE'};
	const [first, second] = await Promise.all([
		service.issueForCheckoutSession('cs_fake_op', context),
		service.issueForCheckoutSession('cs_fake_op', context),
	]);
	expect(first.document_id).toBe(second.document_id);
	expect(first.display_number).toMatch(/^OP-\d{6}$/);
	const stored = await repository.findDocument(first.document_id);
	expect(Buffer.compare(stored!.pdf, first.pdf)).toBe(0);
	expect(billingDocumentContent(first)).toMatchObject({
		language: 'sv-SE',
		taxMinor: 35980,
		payment: {method: 'Visa ···· 4242'},
		references: {checkoutSessionId: 'cs_fake_op', paymentIntentId: 'pi_fake_op', chargeId: 'ch_fake_op'},
	});
	const note = await service.issueForRefund('re_fake_op', context);
	expect(note).not.toBeNull();
	expect(note!.display_number).toMatch(/^KR-\d{6}$/);
	expect(billingDocumentContent(note!)).toMatchObject({
		original: {number: first.display_number, documentId: first.document_id.toString()},
		reason: 'withdrawal',
		totalMinor: -179900,
		taxMinor: -35980,
	});
	expect((await service.issueForRefund('re_fake_op', context))!.document_id).toBe(note!.document_id);
});

test('a lost dispute that staff accept gets a credit note naming it, and a fraud dispute gets none', async () => {
	const service = fakeService(fakeCheckoutStripe({refunded: false}), 5_100_000n);
	const context = {userId: 42n, locale: 'en-US'};
	const sale = await service.issueForCheckoutSession('cs_fake_op', context);
	const note = await service.reissue('du:du_fake_op', context);
	expect(note!.display_number).toMatch(/^KR-\d{6}$/);
	expect(billingDocumentContent(note!)).toMatchObject({
		original: {number: sale.display_number},
		reason: 'dispute',
		totalMinor: -179900,
		taxMinor: -35980,
		references: {disputeId: 'du_fake_op', refundId: null},
	});
	expect((await service.issueForDispute('du_fake_op', context))!.document_id).toBe(note!.document_id);
	await expect(service.issueForDispute('du_fake_fraud', context)).rejects.toThrow(/fraudulent/);
});

function fakeService(stripe: Stripe, start: bigint): BillingDocumentService {
	let nextId = start;
	return new BillingDocumentService(
		stripe,
		new BillingDocumentRepository(),
		new EcbReferenceRates(getKVClient(), async () => ''),
		async () => nextId++,
	);
}

test('a reverse-charge sale states the net amount it applies to and no VAT rate', async () => {
	const stripe = fakeCheckoutStripe({
		tag: 'rc',
		currency: 'eur',
		amount: 21276,
		lineTax: 0,
		lineTaxes: [{amount: 0, rate: {percentage: 19}}],
		taxes: [
			{
				amount: 0,
				taxable_amount: 0,
				taxability_reason: 'reverse_charge',
				rate: {percentage: 19, country: 'DE', jurisdiction: 'DE', tax_type: 'vat'},
			},
		],
		customerDetails: {
			email: 'buyer@example.com',
			business_name: 'Beispiel GmbH',
			address: {country: 'DE', line1: 'Hauptstr. 1', postal_code: '10115', city: 'Berlin'},
			tax_ids: [{type: 'eu_vat', value: 'DE123456789'}],
		},
	});
	const service = fakeService(stripe, 6_000_000n);
	const sale = await service.loadCheckoutSale('cs_fake_rc', 'en');
	expect(sale.lines[0]!.ratePercent).toBeNull();
	expect(sale.taxLines[0]).toMatchObject({ratePercent: null, taxableMinor: 21276, taxMinor: 0});
	const content = billingDocumentContent(
		await service.issueForCheckoutSession('cs_fake_rc', {userId: 1n, locale: 'en'}),
	);
	expect(content).toMatchObject({layout: 'full', vatTreatment: 'reverse_charge_eu'});
	expect(content.taxLines[0]).toMatchObject({ratePercent: null, taxableMinor: 21276});
	expect(content.buyer.address).toMatchObject({city: 'Berlin'});
});

test('a self-serve refund of two payments gets one credit note for each, naming its own original', async () => {
	const context = {userId: 5n, locale: 'en'};
	const notes = [];
	for (const [tag, amount, start] of [
		['two_a', 4500, 6_700_000n],
		['two_b', 499, 6_800_000n],
	] as const) {
		const service = fakeService(
			fakeCheckoutStripe({
				tag,
				amount,
				lineTax: Math.round(amount / 5),
				lineTaxes: [{amount: Math.round(amount / 5), rate: {percentage: 25}}],
				taxes: [
					{
						amount: Math.round(amount / 5),
						taxable_amount: amount - Math.round(amount / 5),
						taxability_reason: 'standard_rated',
						rate: {percentage: 25, country: 'SE', jurisdiction: 'Sweden', tax_type: 'vat'},
					},
				],
				refund: {metadata: {refund_kind: 'self_serve'}},
			}),
			start,
		);
		const original = await service.issueForCheckoutSession(`cs_fake_${tag}`, context);
		const note = await service.issueForRefund(`re_fake_${tag}`, context);
		expect(note).not.toBeNull();
		notes.push({original, note: note!});
	}
	for (const {original, note} of notes) {
		expect(note.display_number).toMatch(/^KR-\d{6}$/);
		expect(billingDocumentContent(note)).toMatchObject({
			original: {number: original.display_number},
			reason: 'refund',
			totalMinor: -Number(original.total_minor),
			taxMinor: -Number(original.tax_minor),
		});
	}
	expect(notes[0]!.note.document_id).not.toBe(notes[1]!.note.document_id);
});

test('a business buyer who gave no address still gets a full invoice, and support is alerted to correct it', async () => {
	const stripe = fakeCheckoutStripe({
		tag: 'noaddr',
		customerDetails: {
			email: 'buyer@example.com',
			business_name: 'Exempel AB',
			address: {country: 'SE'},
			tax_ids: [{type: 'eu_vat', value: 'SE556677889901'}],
		},
	});
	loggerError.mockClear();
	const row = await fakeService(stripe, 6_600_000n).issueForCheckoutSession('cs_fake_noaddr', {
		userId: 1n,
		locale: 'en',
	});
	expect(billingDocumentContent(row)).toMatchObject({layout: 'full', buyer: {address: null}});
	expect(loggerError).toHaveBeenCalledWith(
		expect.objectContaining({billingDocumentAlert: true, sourceKey: 'cs:cs_fake_noaddr'}),
		expect.stringContaining('without an address'),
	);
});

test('a US sale prints no rate on a line that charged no VAT', async () => {
	const stripe = fakeCheckoutStripe({
		tag: 'us',
		currency: 'usd',
		amount: 19900,
		lineTax: 0,
		lineTaxes: [{amount: 0, rate: {percentage: 8.875}}],
		taxes: [
			{
				amount: 0,
				taxable_amount: 0,
				taxability_reason: 'not_collecting',
				rate: {percentage: 8.875, country: 'US', jurisdiction: 'NY', tax_type: 'sales_tax'},
			},
		],
		customerDetails: {email: 'buyer@example.com', address: {country: 'US'}, tax_ids: []},
	});
	const sale = await fakeService(stripe, 6_100_000n).loadCheckoutSale('cs_fake_us', 'en');
	expect(sale.lines[0]!.ratePercent).toBeNull();
	expect(sale.taxLines[0]).toMatchObject({ratePercent: null, taxableMinor: 19900});
});

test('a refund of a sale paid partly from the account balance credits only the money returned', async () => {
	const stripe = fakeCheckoutStripe({
		tag: 'bal',
		refunded: false,
		chargeAmount: 100000,
		refund: {amount: 100000, metadata: {}},
	});
	const service = fakeService(stripe, 6_200_000n);
	const context = {userId: 7n, locale: 'en'};
	await service.issueForCheckoutSession('cs_fake_bal', context);
	const note = await service.issueForRefund('re_fake_bal', context);
	expect(billingDocumentContent(note!)).toMatchObject({reason: 'refund', totalMinor: -100000, taxMinor: -20000});
});

test('a converted payment refunded in full credits the whole sale', async () => {
	const stripe = fakeCheckoutStripe({
		tag: 'fxr',
		refunded: false,
		presentment: {presentment_amount: 16500, presentment_currency: 'eur'},
		refund: {metadata: {}},
	});
	const service = fakeService(stripe, 6_300_000n);
	const context = {userId: 8n, locale: 'en'};
	await service.issueForCheckoutSession('cs_fake_fxr', context);
	const note = await service.issueForRefund('re_fake_fxr', context);
	expect(billingDocumentContent(note!)).toMatchObject({totalMinor: -179900, taxMinor: -35980});
});

test('a checkout session with a Stripe invoice or in subscription mode gets no cs: document', async () => {
	const context = {userId: 9n, locale: 'en'};
	const legacy = fakeService(fakeCheckoutStripe({tag: 'leg', session: {invoice: 'in_legacy'}}), 6_400_000n);
	await expect(legacy.reissue('cs:cs_fake_leg', context)).rejects.toThrow(/in:in_legacy/);
	const subscription = fakeService(fakeCheckoutStripe({tag: 'sub', session: {mode: 'subscription'}}), 6_500_000n);
	await expect(subscription.issueForCheckoutSession('cs_fake_sub', context)).rejects.toThrow(/subscription session/);
	expect(await new BillingDocumentRepository().findDocumentBySource('cs:cs_fake_sub')).toBeNull();
});

test('the SEK rate is the latest ECB rate published before the payment, not the issue day', async () => {
	expect(latestPublishedEcbDate(new Date('2026-11-02T14:59:00Z'))).toBe('2026-11-01');
	expect(latestPublishedEcbDate(new Date('2026-11-02T15:00:00Z'))).toBe('2026-11-02');
	const requests: Array<string> = [];
	const csv = [
		'KEY,FREQ,CURRENCY,CURRENCY_DENOM,EXR_TYPE,EXR_SUFFIX,TIME_PERIOD,OBS_VALUE',
		'EXR.D.SEK.EUR.SP00.A,D,SEK,EUR,SP00,A,2026-10-29,11.0',
		'EXR.D.SEK.EUR.SP00.A,D,SEK,EUR,SP00,A,2026-10-30,11.2',
		'EXR.D.USD.EUR.SP00.A,D,USD,EUR,SP00,A,2026-10-29,1.1',
		'EXR.D.USD.EUR.SP00.A,D,USD,EUR,SP00,A,2026-10-30,1.12',
	].join('\n');
	const rates = new EcbReferenceRates(getKVClient(), async (currencies, start, end) => {
		requests.push(`${currencies.join('+')} ${start} ${end}`);
		return csv;
	});
	const paidOnMonday = await rates.sekRate('usd', new Date('2026-11-02T08:00:00Z'));
	expect(paidOnMonday).toEqual({rate: '10.000000', rateDate: '2026-10-30'});
	expect(requests).toEqual(['USD+SEK 2026-10-18 2026-11-01']);
	await rates.sekRate('usd', new Date('2026-11-02T09:00:00Z'));
	expect(requests).toHaveLength(1);
});
