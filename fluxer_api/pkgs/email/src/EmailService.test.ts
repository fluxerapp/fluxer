// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {PDFDocument} from 'pdf-lib';
import {describe, expect, it} from 'vitest';

const CONFIG: EmailConfig = {
	enabled: true,
	fromEmail: 'noreply@example.com',
	fromName: 'Fluxer',
	appBaseUrl: 'https://example.com',
	apiBaseUrl: 'https://api.example.com',
	appealsEmail: 'appeals@fluxer.com',
	privacyEmail: 'privacy@fluxer.com',
	safetyEmail: 'safety@fluxer.com',
	supportEmail: 'support@fluxer.com',
	productName: 'Fluxer',
	marketingBaseUrl: 'https://example.com',
	staticCdnBaseUrl: 'https://static.example.com',
	legalAddress:
		'Fluxer Platform AB (org. no. 559537-3993, registered office Haninge, Sweden), c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden',
	brandLogo: true,
};

function createCapturingServiceFor(config: EmailConfig): {service: EmailService; sent: Array<EmailMessage>} {
	const sent: Array<EmailMessage> = [];
	const provider: IEmailProvider = {
		sendEmail: async (message) => {
			sent.push(message);
			return true;
		},
	};
	return {service: new EmailService(config, provider), sent};
}

describe('EmailService', () => {
	it('keeps the HTML part on a plain-http instance whose links stay on its own origins', async () => {
		const {service, sent} = createCapturingServiceFor({
			...CONFIG,
			appBaseUrl: 'http://chat.example.org',
			apiBaseUrl: 'http://chat.example.org/api',
			marketingBaseUrl: 'http://chat.example.org',
			staticCdnBaseUrl: 'http://static.chat.example.org:8080',
		});
		await service.send('password_reset', 'user@example.com', {resetToken: 'tok.abc'}, 'en-US');
		const html = sent[0].html ?? '';
		expect(html).toContain('href="http://chat.example.org/reset#token=tok.abc"');
		expect(html).toContain('src="http://static.chat.example.org:8080/email/brand/v1/logo-color.png"');
		expect(html).not.toContain('%%');
	});

	it('sends the purchase confirmation and the terms in Portuguese whenever the Brazilian rule applies', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		await service.send(
			'operator_pass_purchase_confirmation',
			'buyer@example.com',
			{
				invoiceNumber: null,
				orderDate: new Date('2026-11-02T14:00:00Z'),
				quantity: 1,
				totals: {totalMinor: 102000, paidMinor: 102000, currency: 'BRL', tax: null, reverseCharge: false},
				document: null,
				business: false,
				consentAt: new Date('2026-11-02T13:59:00Z'),
				withdrawalEndsOn: '2026-11-09',
				withdrawalCountry: 'BR',
				wallName: null,
				unitAmountMinor: 102000,
				paymentMethod: null,
				deliveryBoxText: null,
				termsVersion: '2026-11-01',
			},
			'en-US',
		);
		expect(sent[0].text).toContain('Direito de arrependimento');
		expect(sent[0].attachments?.map((attachment) => attachment.filename)).toEqual([
			'fluxer-operator-pass-terms-2026-11-01-pt-BR.pdf',
		]);
	});
	it('attaches the terms of the version the purchase accepted, or sends nothing when that text is missing', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const params = {
			invoiceNumber: null,
			orderDate: new Date('2026-11-02T14:00:00Z'),
			quantity: 1,
			totals: {totalMinor: 1000, paidMinor: 1000, currency: 'EUR', tax: null, reverseCharge: false},
			document: null,
			business: false,
			consentAt: new Date('2026-11-02T13:59:00Z'),
			withdrawalEndsOn: null,
			withdrawalCountry: null,
			wallName: null,
			unitAmountMinor: 1000,
			paymentMethod: null,
			deliveryBoxText: null,
		};
		expect(
			await service.send(
				'operator_pass_purchase_confirmation',
				'buyer@example.com',
				{...params, termsVersion: '2026-11-01'},
				'en-US',
			),
		).toBe(true);
		const [attachment] = sent[0].attachments ?? [];
		expect(attachment.filename).toBe('fluxer-operator-pass-terms-2026-11-01-en-US.pdf');
		expect((await PDFDocument.load(attachment.content)).getTitle()).toBe('Fluxer Operator Pass terms (2026-11-01)');
		expect(sent[0].text).toContain('(version 2026-11-01)');
		expect(
			await service.send(
				'operator_pass_purchase_confirmation',
				'buyer@example.com',
				{...params, termsVersion: '2026-10-18'},
				'en-US',
			),
		).toBe(false);
		expect(sent).toHaveLength(1);
	});
	it('puts the Fluxer invoice with its tax line and PDF in the purchase confirmation, with no Stripe link', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const invoicePdf = {filename: 'Fluxer-OP-000001.pdf', content: new Uint8Array([1]), contentType: 'application/pdf'};
		await service.send(
			'operator_pass_purchase_confirmation',
			'buyer@example.com',
			{
				invoiceNumber: 'OP-000001',
				orderDate: new Date('2026-11-02T14:00:00Z'),
				quantity: 1,
				totals: {
					totalMinor: 19900,
					paidMinor: 19900,
					currency: 'EUR',
					tax: {amountMinor: 3980, behavior: 'inclusive'},
					reverseCharge: false,
				},
				document: {number: 'OP-000001', issuedAt: new Date('2026-11-02T14:00:00Z'), pdf: invoicePdf},
				business: true,
				consentAt: new Date('2026-11-02T13:59:00Z'),
				withdrawalEndsOn: null,
				withdrawalCountry: null,
				wallName: null,
				unitAmountMinor: 19900,
				paymentMethod: null,
				deliveryBoxText: null,
				termsVersion: '2026-11-01',
			},
			'en-US',
		);
		expect(sent[0].text).toContain('Includes €39.80 tax');
		expect(sent[0].text).toContain('Your invoice OP-000001 is attached to this email as a PDF.');
		expect(sent[0].html).not.toContain('stripe.com');
		expect(sent[0].attachments?.map((attachment) => attachment.filename)).toEqual([
			'fluxer-operator-pass-terms-2026-11-01-en-US.pdf',
			'Fluxer-OP-000001.pdf',
		]);
	});
	it('attaches the credit note to an Operator Pass refund receipt', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const creditNotePdf = {
			filename: 'CreditNote-CN-1.pdf',
			content: new Uint8Array([1]),
			contentType: 'application/pdf',
		};
		await service.send(
			'operator_pass_refund_receipt',
			'buyer@example.com',
			{
				invoiceNumber: 'A1B2-0001',
				orderDate: new Date('2026-11-02T14:00:00Z'),
				refundedAt: new Date('2026-11-03T14:00:00Z'),
				amountMinor: 19900,
				currency: 'EUR',
				delivered: true,
				passCount: 1,
				lastPass: false,
				visionary: false,
				creditNote: {number: 'CN-1', issuedAt: new Date('2026-11-03T14:00:00Z'), pdf: creditNotePdf},
			},
			'en-US',
		);
		expect(sent[0].text).toContain('Credit note number');
		expect(sent[0].text).toContain('CN-1');
		expect(sent[0].text).toContain('The pass has ended. Your other passes stay.');
		expect(sent[0].text).toContain('The credit note for this refund is attached to this email as a PDF.');
		expect(sent[0].attachments?.map((attachment) => attachment.filename)).toEqual(['CreditNote-CN-1.pdf']);
	});
	it.each([
		[
			1,
			true,
			'The pass has ended. Your badge and wall listing have ended too. You keep #operators through your Visionary',
		],
		[2, false, '2 passes have ended. Your other passes stay.'],
		[0, true, "This refund didn't end any of your passes, so nothing else on your account changes."],
	])('says what a refund that ended %i passes did to the account', async (passCount, lastPass, text) => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		await service.send(
			'operator_pass_refund_receipt',
			'buyer@example.com',
			{
				invoiceNumber: 'A1B2-0001',
				orderDate: new Date('2026-11-02T14:00:00Z'),
				refundedAt: new Date('2026-11-03T14:00:00Z'),
				amountMinor: 19900,
				currency: 'EUR',
				delivered: true,
				passCount,
				lastPass,
				visionary: true,
				creditNote: null,
			},
			'en-US',
		);
		expect(sent[0].text).toContain(text);
	});
	it('tells a Visionary whose pass bonus is removed that #operators stays through Visionary', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const params = {
			until: null,
			rule: 'Community guidelines section 4, advertising',
			facts: 'Posted ads in #operators three times.',
			automated: false,
			dispute: null,
		};
		await service.send('operator_pass_perks_removed', 'visionary@example.com', {...params, visionary: true}, 'en-US');
		await service.send('operator_pass_perks_removed', 'operator@example.com', {...params, visionary: false}, 'en-US');
		const [visionary, operator] = sent.map((message) => message.text ?? '');
		expect(visionary).toContain('This includes your wall listing, badge and the Operator role.');
		expect(visionary).toContain('You keep #operators through your Visionary.');
		expect(operator).toContain('This includes your wall listing, badge, #operators and the Operator role.');
		expect(operator).not.toContain('through your Visionary');
	});
	it('explains a dispute bar in the recipient language and says the disputed pages are canceled', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const params = {
			until: null,
			rule: 'Section 9 of the Operator Pass terms',
			facts: 'Payment dispute dp_test was found unjustified',
			automated: false,
			visionary: false,
			dispute: {id: 'dp_test_sv', deadline: new Date('2026-12-01T12:00:00Z')},
		};
		await service.send('operator_pass_perks_removed', 'buyer@example.com', params, 'sv-SE');
		await service.send('operator_pass_perks_removed', 'buyer@example.com', {...params, dispute: null}, 'sv-SE');
		const [dispute, other] = sent.map((message) => message.text ?? '');
		expect(dispute).toContain('Avsnitt 9 i villkoren för Operator Pass');
		expect(dispute).toContain('betalningstvisten dp_test_sv');
		expect(dispute).toContain('1 december 2026');
		expect(dispute).toContain('Certifikatsidorna som den betalningen avsåg har makulerats');
		expect(dispute).not.toContain('Payment dispute');
		expect(dispute).not.toContain('påverkas inte');
		expect(other).toContain('Ditt Operator-certifikat påverkas inte');
		expect(other).toContain('Payment dispute dp_test was found unjustified');
	});
	it('puts the credit note number and attachment in a refusal email', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		await service.send(
			'operator_pass_purchase_refused',
			'buyer@example.com',
			{
				creditNote: {
					number: 'CN-0001',
					issuedAt: new Date('2026-11-03T14:00:00Z'),
					pdf: {filename: 'CN-0001.pdf', content: Buffer.from('%PDF-1.7'), contentType: 'application/pdf'},
				},
			},
			'de',
		);
		expect(sent[0].text).toContain('Gutschriftnummer');
		expect(sent[0].text).toContain('CN-0001');
		expect(sent[0].text).toContain('Die Gutschrift zu dieser Rückerstattung findest du als PDF im Anhang.');
		expect(sent[0].attachments?.map((attachment) => attachment.filename)).toContain('CN-0001.pdf');
	});
	it('keeps the # in #operators when a pass leaves the last one on an account', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		await service.send(
			'operator_pass_offer_accepted',
			'sender@example.com',
			{recipientName: 'Grace', remainingCount: 0, visionary: false},
			'en-US',
		);
		expect(sent[0].text).toContain('your badge, wall listing and #operators are gone.');
	});
	it('tells a buyer who withdraws before delivery that nothing else on the account changes', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		const params = {
			name: 'Ada Lovelace',
			receivedAt: new Date('2026-11-02T14:00:00Z'),
			invoiceNumber: null,
			orderDate: new Date('2026-11-02T13:00:00Z'),
			amountMinor: 59700,
			currency: 'USD',
			passCount: 3,
			lastPass: true,
			visionary: false,
			refund: 'started' as const,
			creditNote: null,
		};
		await service.send('operator_pass_withdrawal_receipt', 'buyer@example.com', {...params, delivered: false}, 'en-US');
		await service.send('operator_pass_withdrawal_receipt', 'holder@example.com', {...params, delivered: true}, 'en-US');
		const [undelivered, delivered] = sent.map((message) => message.text ?? '');
		expect(undelivered).toContain(
			"We hadn't delivered your certificates yet, so nothing else on your account changes.",
		);
		expect(undelivered).not.toContain('passes have ended');
		expect(undelivered).not.toContain('Your badge, wall listing and #operators have ended too.');
		expect(delivered).toContain('3 passes have ended.');
		expect(delivered).toContain('Your badge, wall listing and #operators have ended too.');
	});
	it('says no new refund was needed when the payment was already refunded', async () => {
		const {service, sent} = createCapturingServiceFor(CONFIG);
		await service.send(
			'operator_pass_withdrawal_receipt',
			'buyer@example.com',
			{
				name: 'Ada Lovelace',
				receivedAt: new Date('2026-11-02T14:00:00Z'),
				invoiceNumber: null,
				orderDate: new Date('2026-11-02T13:00:00Z'),
				amountMinor: 19900,
				currency: 'USD',
				delivered: true,
				passCount: 1,
				lastPass: true,
				visionary: false,
				refund: 'already',
				creditNote: null,
			},
			'en-US',
		);
		const text = sent[0]?.text ?? '';
		expect(text).toContain('so no new refund was needed.');
		expect(text).not.toContain('We started a full refund');
	});
});
