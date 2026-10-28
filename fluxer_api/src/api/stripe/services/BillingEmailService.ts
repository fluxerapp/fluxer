// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {getEffectiveBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {type ProductRegistry, ProductType} from '@app/api/stripe/ProductRegistry';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	findChargedLine,
	invoiceLinkUntil,
	invoicePaidAt,
	invoiceTotals,
	isDonationInvoice,
	isOperatorPassInvoice,
	plutoniumPurchaseTerms,
	unixToDate,
} from '@app/api/stripe/services/BillingInvoiceDetails';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getPaidPremiumAccessEndsAt} from '@app/api/user/UserHelpers';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {BillingPaymentProblem, PlutoniumPlan} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {BillingInvoiceDocument, EmailSendParams, IEmailService} from '@pkgs/email/src/IEmailService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms, seconds} from 'itty-time';
import type Stripe from 'stripe';

const SENT_TTL_SECONDS = seconds('400 days');
const LOCK_TTL_SECONDS = seconds('5 minutes');
const INVOICE_PDF_TIMEOUT_MS = ms('10 seconds');
const INVOICE_PDF_MAX_BYTES = 5 * 1024 * 1024;
const REFUND_BACKFILL_WINDOW_SECONDS = seconds('1 day');
const YEARLY_PERIOD_MIN_MS = ms('60 days');
const CLOSED_INVOICE_STATUSES = new Set(['paid', 'void', 'uncollectible', 'draft']);

const PLAN_BY_PRODUCT_TYPE: Record<ProductType, PlutoniumPlan> = {
	[ProductType.MONTHLY_SUBSCRIPTION]: 'monthly',
	[ProductType.YEARLY_SUBSCRIPTION]: 'yearly',
	[ProductType.GIFT_1_MONTH]: 'gift_1_month',
	[ProductType.GIFT_1_YEAR]: 'gift_1_year',
};

export type BillingEmailJob =
	| {kind: 'invoice_paid'; id: string}
	| {kind: 'invoice_payment_problem'; id: string; paymentAttempt: number; actionRequired: boolean}
	| {kind: 'refund'; id: string};

class BillingEmailRetryError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'BillingEmailRetryError';
	}
}

interface Recipient {
	email: string;
	locale: string | null;
	timeZone: string | null;
}

export function billingEmailJobKey(job: BillingEmailJob): string {
	return job.kind === 'invoice_payment_problem'
		? `billing-email:${job.kind}:${job.id}:${job.paymentAttempt}:${job.actionRequired ? 'action' : 'failed'}`
		: `billing-email:${job.kind}:${job.id}`;
}

export async function enqueueBillingEmail(
	workerService: IWorkerService<WorkerTaskName>,
	job: BillingEmailJob,
): Promise<void> {
	if (!getEffectiveBillingConfig().billingEmails) {
		return;
	}
	await workerService.addJob('sendBillingEmail', {...job}, {jobKey: billingEmailJobKey(job)});
}

export function isRecentRefund(refund: Stripe.Refund, eventCreated: number): boolean {
	return refund.created >= eventCreated - REFUND_BACKFILL_WINDOW_SECONDS;
}

function userRecipient(user: User | null): Recipient | null {
	if (!user?.email || user.emailBounced) {
		return null;
	}
	return {email: user.email, locale: user.locale, timeZone: user.timezone};
}

function donationManageUrl(email: string): string {
	return `${Config.endpoints.marketing}/donate/manage?email=${encodeURIComponent(email)}`;
}

export class BillingEmailService {
	constructor(
		private readonly stripe: Stripe,
		private readonly userRepository: UserRepository,
		private readonly emailService: IEmailService,
		private readonly cacheService: ICacheService,
		private readonly productRegistry: ProductRegistry,
		private readonly donationRepository: DonationRepository,
		private readonly operatorPassRepository: OperatorPassRepository,
	) {}

	async deliver(job: BillingEmailJob): Promise<void> {
		switch (job.kind) {
			case 'invoice_paid':
				return this.invoicePaid(job.id);
			case 'invoice_payment_problem':
				return this.invoicePaymentProblem(job);
			case 'refund':
				return this.refund(job.id);
		}
	}

	private async invoicePaid(invoiceId: string): Promise<void> {
		const invoice = await this.stripe.invoices.retrieve(invoiceId);
		if (invoice.status !== 'paid' || invoice.total <= 0) {
			return;
		}
		const line = findChargedLine(invoice, this.productRegistry);
		if (line) {
			const recipient = userRecipient(await this.resolveUser(invoice));
			if (!recipient) {
				Logger.warn({invoiceId}, 'No deliverable account email for a paid Plutonium invoice; receipt not sent');
				return;
			}
			const {premiumName} = await getBillingBranding();
			await this.sendOnce(`receipt:${invoiceId}`, recipient, 'plutonium_payment_receipt', {
				premiumName,
				plan: PLAN_BY_PRODUCT_TYPE[line.product.type],
				invoiceNumber: invoice.number ?? null,
				paidAt: invoicePaidAt(invoice),
				period: line.product.isGift ? null : line.period,
				totals: invoiceTotals(invoice),
				invoice: await this.invoiceDocument(invoice),
				purchase: plutoniumPurchaseTerms(invoice, line),
				timeZone: recipient.timeZone,
			});
			return;
		}
		if (isOperatorPassInvoice(invoice)) {
			await this.operatorPassInvoicePaid(invoice);
			return;
		}
		if (isDonationInvoice(invoice)) {
			await this.donationInvoicePaid(invoice);
		}
	}

	private async operatorPassInvoicePaid(invoice: Stripe.Invoice): Promise<void> {
		const document = await this.invoiceDocument(invoice);
		const purchaserId =
			invoice.metadata?.operator_pass_purchaser_id ??
			invoice.parent?.subscription_details?.metadata?.operator_pass_purchaser_id;
		const recipient =
			purchaserId && /^\d+$/.test(purchaserId)
				? userRecipient(await this.userRepository.findUnique(createUserID(BigInt(purchaserId))))
				: null;
		if (!document || !recipient) {
			Logger.warn({invoiceId: invoice.id}, 'No invoice link or purchaser email for an Operator Pass invoice');
			return;
		}
		await this.sendOnce(`op_invoice:${invoice.id}`, recipient, 'operator_pass_invoice', {
			invoiceNumber: invoice.number ?? null,
			paidAt: invoicePaidAt(invoice),
			totals: invoiceTotals(invoice),
			invoice: document,
			timeZone: recipient.timeZone,
		});
	}

	private async donationInvoicePaid(invoice: Stripe.Invoice): Promise<void> {
		const recipient = await this.resolveDonor(invoice);
		if (!recipient) {
			Logger.warn({invoiceId: invoice.id}, 'No donor email for a paid donation invoice; receipt not sent');
			return;
		}
		const firstLine = invoice.lines?.data?.[0];
		const start = unixToDate(firstLine?.period?.start);
		const end = unixToDate(firstLine?.period?.end);
		const period = start && end && end > start ? {start, end} : null;
		const interval = period && period.end.getTime() - period.start.getTime() > YEARLY_PERIOD_MIN_MS ? 'year' : 'month';
		await this.sendOnce(`donation_receipt:${invoice.id}`, recipient, 'donation_receipt', {
			interval,
			invoiceNumber: invoice.number ?? null,
			paidAt: invoicePaidAt(invoice),
			period,
			totals: invoiceTotals(invoice),
			invoice: await this.invoiceDocument(invoice),
			manageUrl: donationManageUrl(recipient.email),
		});
	}

	private async invoicePaymentProblem(job: Extract<BillingEmailJob, {kind: 'invoice_payment_problem'}>) {
		const invoice = await this.stripe.invoices.retrieve(job.id, {expand: ['payments.data.payment.payment_intent']});
		if (
			invoice.billing_reason !== 'subscription_cycle' ||
			CLOSED_INVOICE_STATUSES.has(invoice.status ?? 'draft') ||
			invoice.amount_remaining <= 0
		) {
			return;
		}
		const latestPayment = [...(invoice.payments?.data ?? [])].sort((left, right) => right.created - left.created)[0];
		const intent = latestPayment?.payment?.payment_intent;
		const reason: BillingPaymentProblem =
			job.actionRequired || (typeof intent === 'object' && intent?.status === 'requires_action')
				? 'action_required'
				: 'declined';
		const key = `payment_problem:${invoice.id}:${job.paymentAttempt}:${reason}`;
		const attemptedAt = unixToDate(latestPayment?.created) ?? new Date();
		if (findChargedLine(invoice, this.productRegistry)) {
			const user = await this.resolveUser(invoice);
			const recipient = userRecipient(user);
			if (!user || !recipient) {
				Logger.warn({invoiceId: invoice.id}, 'No deliverable account email for a Plutonium payment problem');
				return;
			}
			const accessEndsAt = getPaidPremiumAccessEndsAt(user);
			const {premiumName} = await getBillingBranding();
			await this.sendOnce(key, recipient, 'plutonium_payment_failed', {
				premiumName,
				reason,
				invoiceNumber: invoice.number ?? null,
				attemptedAt,
				amountMinor: invoice.amount_remaining,
				currency: invoice.currency,
				invoiceUrl: invoice.hosted_invoice_url ?? null,
				accessEndsAt: accessEndsAt && accessEndsAt.getTime() > Date.now() ? accessEndsAt : null,
				timeZone: recipient.timeZone,
			});
			return;
		}
		if (!isDonationInvoice(invoice)) {
			return;
		}
		const recipient = await this.resolveDonor(invoice);
		if (!recipient) {
			Logger.warn({invoiceId: invoice.id}, 'No donor email for a donation payment problem');
			return;
		}
		await this.sendOnce(`donation_${key}`, recipient, 'donation_payment_failed', {
			reason,
			invoiceNumber: invoice.number ?? null,
			attemptedAt,
			amountMinor: invoice.amount_remaining,
			currency: invoice.currency,
			invoiceUrl: invoice.hosted_invoice_url ?? null,
			manageUrl: donationManageUrl(recipient.email),
		});
	}

	private async refund(refundId: string): Promise<void> {
		const refund = await this.stripe.refunds.retrieve(refundId, {expand: ['charge', 'payment_intent']});
		if (refund.status !== 'succeeded' || refund.amount <= 0) {
			return;
		}
		const paymentIntentId = extractId(refund.payment_intent);
		const charge = typeof refund.charge === 'object' ? refund.charge : null;
		if (!paymentIntentId) {
			return;
		}
		const purchase = await this.operatorPassRepository.findPurchaseByPaymentIntent(paymentIntentId);
		if (purchase) {
			await this.operatorPassRefund(refund, charge, purchase);
			return;
		}
		const invoice = await this.findPaidInvoice(paymentIntentId);
		const line = invoice ? findChargedLine(invoice, this.productRegistry) : null;
		if (invoice && line) {
			const recipient = userRecipient(await this.resolveUser(invoice));
			if (!recipient) {
				Logger.warn({refundId, invoiceId: invoice.id}, 'No deliverable account email for a Plutonium refund');
				return;
			}
			const {premiumName} = await getBillingBranding();
			await this.sendOnce(`refund:${refund.id}`, recipient, 'plutonium_refund_receipt', {
				premiumName,
				plan: PLAN_BY_PRODUCT_TYPE[line.product.type],
				invoiceNumber: invoice.number ?? null,
				refundedAt: new Date(refund.created * 1000),
				amountMinor: refund.amount,
				paidMinor: invoice.amount_paid,
				paidAt: invoicePaidAt(invoice),
				currency: refund.currency,
				refundUrl: charge?.receipt_url ?? null,
				timeZone: recipient.timeZone,
			});
			return;
		}
		const intent = typeof refund.payment_intent === 'object' ? refund.payment_intent : null;
		const recipient = invoice && isDonationInvoice(invoice) ? await this.resolveDonor(invoice) : null;
		const oneTimeDonor =
			!invoice && intent?.metadata?.is_donation === 'true' && intent.metadata.donation_email
				? {
						email: intent.metadata.donation_email,
						locale: intent.metadata.donation_locale ?? null,
						timeZone: null,
					}
				: null;
		const donor = recipient ?? oneTimeDonor;
		if (donor) {
			await this.donationRefund(refund, charge, donor, invoice?.number ?? charge?.receipt_number ?? null);
		}
	}

	private async operatorPassRefund(
		refund: Stripe.Refund,
		charge: Stripe.Charge | null,
		purchase: NonNullable<Awaited<ReturnType<OperatorPassRepository['findPurchaseByPaymentIntent']>>>,
	): Promise<void> {
		const fluxerWithdrawal =
			refund.metadata?.checkout_session_id === purchase.checkout_session_id &&
			(refund.metadata?.rejection_reason ?? '').startsWith('operator_pass_withdrawal');
		if (fluxerWithdrawal) {
			return;
		}
		const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
		const recipient = userRecipient(purchaser);
		if (!recipient) {
			Logger.warn({refundId: refund.id}, 'No deliverable purchaser email for an Operator Pass refund');
			return;
		}
		const donationAmount = charge ? Math.max(0, charge.amount - purchase.amount_minor) : 0;
		const donationPart =
			purchase.subscription_id != null &&
			!refund.metadata?.checkout_session_id &&
			(refund.metadata?.subscription_id === purchase.subscription_id || refund.amount === donationAmount);
		if (donationPart) {
			await this.donationRefund(refund, charge, recipient, purchase.invoice_number ?? null);
			return;
		}
		await this.sendOnce(`refund:${refund.id}`, recipient, 'operator_pass_refund_receipt', {
			invoiceNumber: purchase.invoice_number ?? null,
			orderDate: purchase.purchased_at ?? purchase.created_at,
			refundedAt: new Date(refund.created * 1000),
			amountMinor: refund.amount,
			currency: refund.currency,
			full: Boolean(charge?.refunded) || (charge?.amount_refunded ?? 0) >= purchase.amount_minor,
			timeZone: recipient.timeZone,
		});
	}

	private async donationRefund(
		refund: Stripe.Refund,
		charge: Stripe.Charge | null,
		recipient: Recipient,
		reference: string | null,
	): Promise<void> {
		await this.sendOnce(`refund:${refund.id}`, recipient, 'donation_refund_receipt', {
			reference,
			refundedAt: new Date(refund.created * 1000),
			amountMinor: refund.amount,
			currency: refund.currency,
			refundUrl: charge?.receipt_url ?? null,
		});
	}

	private async findPaidInvoice(paymentIntentId: string): Promise<Stripe.Invoice | null> {
		const payments = this.stripe.invoicePayments.list({
			payment: {type: 'payment_intent', payment_intent: paymentIntentId},
			status: 'paid',
			expand: ['data.invoice'],
			limit: 10,
		});
		let fallback: Stripe.Invoice | null = null;
		for await (const payment of payments) {
			const invoice =
				typeof payment.invoice === 'string' ? await this.stripe.invoices.retrieve(payment.invoice) : payment.invoice;
			if (!invoice || ('deleted' in invoice && invoice.deleted)) {
				continue;
			}
			const full = invoice as Stripe.Invoice;
			if (findChargedLine(full, this.productRegistry)) {
				return full;
			}
			fallback ??= full;
		}
		return fallback;
	}

	private async invoiceDocument(invoice: Stripe.Invoice): Promise<BillingInvoiceDocument | null> {
		if (!invoice.hosted_invoice_url) {
			return null;
		}
		return {
			url: invoice.hosted_invoice_url,
			linkUntil: invoiceLinkUntil(invoice),
			pdf: await this.fetchInvoicePdf(invoice),
		};
	}

	private async fetchInvoicePdf(invoice: Stripe.Invoice): Promise<BillingInvoiceDocument['pdf']> {
		if (!invoice.invoice_pdf) {
			return null;
		}
		try {
			const response = await fetch(invoice.invoice_pdf, {signal: AbortSignal.timeout(INVOICE_PDF_TIMEOUT_MS)});
			if (!response.ok) {
				throw new Error(`Invoice PDF request failed with ${response.status}`);
			}
			const content = new Uint8Array(await response.arrayBuffer());
			if (content.byteLength === 0 || content.byteLength > INVOICE_PDF_MAX_BYTES) {
				throw new Error(`Invoice PDF has an unexpected size of ${content.byteLength} bytes`);
			}
			const name = (invoice.number ?? invoice.id ?? 'invoice').replace(/[^\w-]/g, '');
			return {filename: `Invoice-${name}.pdf`, content, contentType: 'application/pdf'};
		} catch (error) {
			Logger.warn({error, invoiceId: invoice.id}, 'Could not attach the invoice PDF; the email links to the invoice');
			return null;
		}
	}

	private async resolveUser(invoice: Stripe.Invoice): Promise<User | null> {
		const customerId = extractId(invoice.customer);
		if (customerId) {
			const user = await this.userRepository.findByStripeCustomerId(customerId);
			if (user) {
				return user;
			}
		}
		const subscriptionId = extractId(invoice.parent?.subscription_details?.subscription ?? null);
		if (subscriptionId) {
			const user = await this.userRepository.findByStripeSubscriptionId(subscriptionId);
			if (user) {
				return user;
			}
		}
		const metadataUserId =
			invoice.parent?.subscription_details?.metadata?.user_id ?? invoice.metadata?.user_id ?? undefined;
		if (metadataUserId && /^\d+$/.test(metadataUserId)) {
			return this.userRepository.findUnique(createUserID(BigInt(metadataUserId)));
		}
		return null;
	}

	private async resolveDonor(invoice: Stripe.Invoice): Promise<Recipient | null> {
		const metadata = invoice.parent?.subscription_details?.metadata ?? {};
		const subscriptionId = extractId(invoice.parent?.subscription_details?.subscription ?? null);
		const donor = subscriptionId ? await this.donationRepository.findDonorByStripeSubscriptionId(subscriptionId) : null;
		const email = donor?.email ?? metadata.donation_email ?? invoice.customer_email ?? null;
		return email ? {email, locale: metadata.donation_locale ?? null, timeZone: null} : null;
	}

	private sentKey(key: string): string {
		return `stripe:billing:email:sent:${key}`;
	}

	private async sendOnce<K extends keyof EmailSendParams>(
		key: string,
		recipient: Recipient,
		templateKey: K,
		params: EmailSendParams[K],
	): Promise<void> {
		const sentKey = this.sentKey(key);
		if (await this.cacheService.get<boolean>(sentKey)) {
			return;
		}
		const lockKey = `stripe:billing:email:lock:${key}`;
		const token = await this.cacheService.acquireLock(lockKey, LOCK_TTL_SECONDS);
		if (!token) {
			Logger.info({key}, 'Another job is sending this billing email');
			return;
		}
		try {
			if (await this.cacheService.get<boolean>(sentKey)) {
				return;
			}
			if (await this.emailService.isEmailBounced(recipient.email)) {
				Logger.warn({key, templateKey}, 'Not sending a billing email to a hard-bounced address');
				return;
			}
			if (!(await this.emailService.send(templateKey, recipient.email, params, recipient.locale))) {
				throw new BillingEmailRetryError(`Failed to send the billing email ${templateKey}`);
			}
			try {
				await this.cacheService.set(sentKey, true, SENT_TTL_SECONDS);
			} catch (error) {
				Logger.error({error, key}, 'Sent a billing email but could not record it; a retry may send it again');
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, token);
		}
	}
}
