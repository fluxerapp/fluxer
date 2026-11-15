// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {
	type BillingDocumentService,
	billingDocumentAttachment,
	billingDocumentContent,
	billingDocumentTotals,
} from '@app/api/billing_documents/BillingDocumentService';
import {Config} from '@app/api/Config';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {getEffectiveBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {type ProductRegistry, ProductType} from '@app/api/stripe/ProductRegistry';
import {
	getPrimarySubscriptionItem,
	getSubscriptionBillingCycle,
	loadSubscriptionSchedule,
} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	billingSellerShown,
	findChargedLine,
	invoicePaidAt,
	invoiceTotals,
	isDonationInvoice,
	plainTotals,
	plutoniumPurchaseTerms,
	purchaseTermsFromMetadata,
	subscriptionBillingProduct,
	unixToDate,
} from '@app/api/stripe/services/BillingInvoiceDetails';
import {
	annualNoticeAnniversary,
	BILLING_NOTICES,
	type BillingNotice,
	type BillingNoticeProduct,
	dueSubscriptionNotices,
	type NoticeInput,
	noticeStillValid,
	PAYMENT_CONFIRMATION_REMINDERS,
} from '@app/api/stripe/services/BillingNoticeSchedule';
import {
	billingPaymentMethod,
	freshCancelSource,
	type SubscriptionNoticeContext,
	subscriptionCancelling,
	subscriptionNoticeContext,
	upcomingChargeAmounts,
} from '@app/api/stripe/services/BillingSubscriptionDetails';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getPaidPremiumAccessEndsAt} from '@app/api/user/UserHelpers';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {BillingPaymentProblem, PlutoniumPlan} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {BillingEndedInvoiceStatus, EmailSendParams, IEmailService} from '@pkgs/email/src/IEmailService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms, seconds} from 'itty-time';
import type Stripe from 'stripe';
import {z} from 'zod';

const SENT_TTL_SECONDS = seconds('400 days');
const LOCK_TTL_SECONDS = seconds('5 minutes');
const REFUND_BACKFILL_WINDOW_SECONDS = seconds('1 day');
const YEARLY_PERIOD_MIN_MS = ms('60 days');
const CLOSED_INVOICE_STATUSES = new Set(['paid', 'void', 'uncollectible', 'draft']);
const PAYMENT_PROBLEM_BILLING_REASONS = new Set(['subscription_cycle', 'subscription_update']);
const NOTICE_SCAN_HORIZON_SECONDS = seconds('41 days');
const ENDED_INVOICE_STATUSES = new Set<string>(['void', 'uncollectible', 'open', 'paid']);
const REFUND_CANCEL_WAIT_RETRIES = 4;
const ENDED_SUBSCRIPTION_STATUSES = new Set<string>(['canceled', 'incomplete_expired']);

const PLAN_BY_PRODUCT_TYPE: Record<ProductType, PlutoniumPlan> = {
	[ProductType.MONTHLY_SUBSCRIPTION]: 'monthly',
	[ProductType.YEARLY_SUBSCRIPTION]: 'yearly',
	[ProductType.GIFT_1_MONTH]: 'gift_1_month',
	[ProductType.GIFT_1_YEAR]: 'gift_1_year',
};

export type BillingCancelSource = 'customer_request' | 'gift_past_trial_limit' | 'payment_dispute';

const jobId = z.string().min(1);
const unixSeconds = z.number().int().positive();

export const BillingEmailJobSchema = z.discriminatedUnion('kind', [
	z.object({kind: z.literal('invoice_paid'), id: jobId}),
	z.object({
		kind: z.literal('invoice_payment_problem'),
		id: jobId,
		paymentAttempt: z.number().int().nonnegative().default(0),
		actionRequired: z.boolean().default(false),
		reminder: z.union([z.literal(1), z.literal(2)]).optional(),
	}),
	z.object({kind: z.literal('refund'), id: jobId}),
	z.object({
		kind: z.literal('subscription_notice'),
		id: jobId,
		notice: z.enum(BILLING_NOTICES),
		chargeAt: unixSeconds,
	}),
	z.object({kind: z.literal('subscription_cancel_scheduled'), id: jobId, requestedAt: unixSeconds}),
	z.object({kind: z.literal('subscription_ended_unpaid'), id: jobId}),
	z.object({kind: z.literal('gift_applied'), id: jobId, trialEnd: unixSeconds}),
	z.object({kind: z.literal('gift_paid'), id: jobId}),
	z.object({
		kind: z.literal('plutonium_withdrawal'),
		id: jobId,
		userId: z.string().regex(/^\d+$/),
		name: z.string().min(1).max(200),
		receivedAt: z.iso.datetime(),
		purchasedAt: z.iso.datetime(),
		gift: z.boolean(),
		amountMinor: z.number().int().nonnegative(),
		currency: z.string().min(1),
		refund: z.enum(['started', 'manual']),
		brazil: z.boolean(),
		proRata: z.boolean().optional(),
	}),
]);

export type BillingEmailJob = z.infer<typeof BillingEmailJobSchema>;

type JobOf<K extends BillingEmailJob['kind']> = Extract<BillingEmailJob, {kind: K}>;

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
	const base = `billing-email:${job.kind}:${job.id}`;
	switch (job.kind) {
		case 'invoice_payment_problem':
			return `${base}:${job.paymentAttempt}:${job.actionRequired ? 'action' : 'failed'}${job.reminder ? `:r${job.reminder}` : ''}`;
		case 'subscription_notice':
			return `${base}:${job.notice}:${job.chargeAt}`;
		case 'subscription_cancel_scheduled':
			return `${base}:${job.requestedAt}`;
		case 'gift_applied':
			return `${base}:${job.trialEnd}`;
		default:
			return base;
	}
}

export async function enqueueBillingEmail(
	workerService: IWorkerService<WorkerTaskName>,
	job: BillingEmailJob,
	later?: {keySuffix: string; runAt: Date},
): Promise<void> {
	if (!getEffectiveBillingConfig().billingEmails) {
		return;
	}
	const jobKey = billingEmailJobKey(job);
	await workerService.addJob(
		'sendBillingEmail',
		{...job},
		later ? {jobKey: `${jobKey}:${later.keySuffix}`, runAt: later.runAt} : {jobKey},
	);
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

function nowSeconds(): number {
	return Math.floor(Date.now() / 1000);
}

function noticeSentKey(notice: BillingNotice, subscriptionId: string, chargeAt: number, input: NoticeInput): string {
	const at = notice === 'annual' ? (annualNoticeAnniversary(input, chargeAt) ?? chargeAt) : chargeAt;
	return `notice:${notice}:${subscriptionId}:${at}`;
}

function endedInvoiceStatus(invoice: Stripe.Invoice | null): BillingEndedInvoiceStatus {
	const status = invoice?.status ?? 'other';
	return ENDED_INVOICE_STATUSES.has(status) ? (status as BillingEndedInvoiceStatus) : 'other';
}

function paymentProblemKey(
	invoice: Stripe.Invoice,
	job: JobOf<'invoice_payment_problem'>,
	reason: BillingPaymentProblem,
): string {
	const base = `payment_problem:${invoice.id}`;
	if (reason === 'action_required') {
		return job.reminder ? `${base}:action_required:r${job.reminder}` : `${base}:action_required`;
	}
	return invoice.next_payment_attempt === null ? `${base}:declined:final` : `${base}:declined:first`;
}

export class BillingEmailService {
	constructor(
		private readonly stripe: Stripe,
		private readonly userRepository: UserRepository,
		private readonly emailService: IEmailService,
		private readonly cacheService: ICacheService,
		private readonly productRegistry: ProductRegistry,
		private readonly donationRepository: DonationRepository,
		private readonly operatorPassFulfilment: OperatorPassFulfilmentService | null,
		private readonly addJob: WorkerTaskHelpers['addJob'],
		private readonly documents: BillingDocumentService | null,
	) {}

	async deliver(job: BillingEmailJob, attempt = 0): Promise<void> {
		switch (job.kind) {
			case 'invoice_paid':
				return this.invoicePaid(job.id);
			case 'invoice_payment_problem':
				return this.invoicePaymentProblem(job);
			case 'refund':
				return this.refund(job.id, attempt);
			case 'subscription_notice':
				return this.subscriptionNotice(job);
			case 'subscription_cancel_scheduled':
				return this.subscriptionCancelScheduled(job);
			case 'subscription_ended_unpaid':
				return this.subscriptionEndedUnpaid(job.id);
			case 'gift_applied':
				return this.giftApplied(job);
			case 'gift_paid':
				return this.giftPaid(job.id);
			case 'plutonium_withdrawal':
				return this.plutoniumWithdrawal(job);
		}
	}

	async scanSubscriptionNotices(workerService: IWorkerService<WorkerTaskName>, now: number): Promise<void> {
		for (const status of ['active', 'trialing'] as const) {
			const subscriptions = this.stripe.subscriptions.list({
				status,
				current_period_end: {lte: now + NOTICE_SCAN_HORIZON_SECONDS},
				expand: ['data.schedule', 'data.default_payment_method'],
				limit: 100,
			});
			for await (const subscription of subscriptions) {
				try {
					await this.queueDueNotices(workerService, subscription, now);
				} catch (error) {
					Logger.warn({error, subscriptionId: subscription.id}, 'Could not check a subscription for billing notices');
				}
			}
		}
	}

	private async queueDueNotices(
		workerService: IWorkerService<WorkerTaskName>,
		subscription: Stripe.Subscription,
		now: number,
	): Promise<void> {
		const context = await subscriptionNoticeContext(this.stripe, subscription, this.productRegistry, now, 'when_due');
		for (const due of context ? dueSubscriptionNotices(context.input) : []) {
			if (due.late) {
				Logger.info({subscriptionId: subscription.id, ...due}, 'Sending a billing notice later than usual');
			}
			await enqueueBillingEmail(workerService, {
				kind: 'subscription_notice',
				id: subscription.id,
				notice: due.notice,
				chargeAt: due.chargeAt,
			});
		}
	}

	private async invoicePaid(invoiceId: string): Promise<void> {
		const invoice = await this.stripe.invoices.retrieve(invoiceId);
		if (invoice.status !== 'paid' || invoice.total <= 0) {
			return;
		}
		const line = findChargedLine(invoice, this.productRegistry);
		if (!line && isDonationInvoice(invoice)) {
			await this.donationInvoicePaid(invoice);
			return;
		}
		if (!line && !extractId(invoice.parent?.subscription_details?.subscription ?? null)) {
			if (!this.documents) {
				return;
			}
			Logger.error(
				{billingDocumentAlert: true, invoiceId},
				'A paid invoice is not a subscription, a donation or a registered product; no Fluxer document was issued',
			);
			await alertSupport({
				dedupeKey: `unknown-invoice:${invoiceId}`,
				subject: `Paid invoice ${invoiceId} has no Fluxer document`,
				summary:
					'A paid Stripe invoice is not a subscription, a donation or a registered product, so no Fluxer document was issued. Check what was sold and issue the document by hand.',
				facts: {
					invoiceId,
					amountMinor: invoice.total,
					currency: invoice.currency,
					country: invoice.customer_address?.country ?? null,
				},
			});
			return;
		}
		const user = await this.resolveUser(invoice);
		const document = await this.documents?.issueForInvoice(invoiceId, {
			userId: user?.id ?? null,
			locale: user?.locale ?? null,
		});
		if (!line) {
			if (!document) {
				return;
			}
			Logger.error(
				{billingDocumentAlert: true, invoiceId, number: document.display_number},
				'A paid subscription invoice has no registered product; the document was issued but no receipt was emailed',
			);
			await alertSupport({
				dedupeKey: `unregistered-product:${invoiceId}`,
				subject: `No receipt sent for ${document.display_number}`,
				summary:
					'A paid subscription invoice has no registered product. The Fluxer document was issued, but no receipt was emailed to the buyer. Send the document to the buyer from the help desk.',
				facts: {
					invoiceId,
					userId: user?.id ?? null,
					documentNumber: document.display_number,
					series: document.series,
					amountMinor: invoice.total,
					currency: invoice.currency,
					country: invoice.customer_address?.country ?? null,
				},
			});
			return;
		}
		const recipient = userRecipient(user);
		if (!recipient) {
			Logger.warn({invoiceId}, 'No deliverable account email for a paid Plutonium invoice; receipt not sent');
			return;
		}
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(
			`receipt:${invoiceId}`,
			recipient,
			'plutonium_payment_receipt',
			{
				premiumName,
				plan: PLAN_BY_PRODUCT_TYPE[line.product.type],
				invoiceNumber: document?.display_number ?? (this.documents ? null : (invoice.number ?? null)),
				paidAt: invoicePaidAt(invoice),
				period: line.product.isGift ? null : line.period,
				totals: invoiceTotals(invoice),
				document: document ? billingDocumentAttachment(document) : null,
				business: document ? billingDocumentContent(document).buyer.business : false,
				purchase: plutoniumPurchaseTerms(invoice, line),
				timeZone: recipient.timeZone,
			},
			`in:${invoiceId}`,
		);
	}

	private async donationInvoicePaid(invoice: Stripe.Invoice): Promise<void> {
		const recipient = await this.resolveDonor(invoice);
		const document = invoice.id
			? await this.documents?.issueForInvoice(invoice.id, {
					userId: null,
					locale: recipient?.locale ?? null,
					donorEmail: recipient?.email ?? null,
				})
			: null;
		if (!recipient) {
			Logger.warn({invoiceId: invoice.id}, 'No donor email for a paid donation invoice; receipt not sent');
			return;
		}
		const firstLine = invoice.lines?.data?.[0];
		const start = unixToDate(firstLine?.period?.start);
		const end = unixToDate(firstLine?.period?.end);
		const period = start && end && end > start ? {start, end} : null;
		const interval = period && period.end.getTime() - period.start.getTime() > YEARLY_PERIOD_MIN_MS ? 'year' : 'month';
		await this.sendOnce(
			`donation_receipt:${invoice.id}`,
			recipient,
			'donation_receipt',
			{
				interval,
				paidAt: invoicePaidAt(invoice),
				period,
				totals: invoiceTotals(invoice),
				document: document ? billingDocumentAttachment(document) : null,
				manageUrl: donationManageUrl(recipient.email),
			},
			`in:${invoice.id}`,
		);
	}

	private async invoicePaymentProblem(job: Extract<BillingEmailJob, {kind: 'invoice_payment_problem'}>) {
		const invoice = await this.stripe.invoices.retrieve(job.id, {expand: ['payments.data.payment.payment_intent']});
		if (
			!PAYMENT_PROBLEM_BILLING_REASONS.has(invoice.billing_reason ?? '') ||
			CLOSED_INVOICE_STATUSES.has(invoice.status ?? 'draft') ||
			invoice.amount_remaining <= 0
		) {
			return;
		}
		const latestPayment = [...(invoice.payments?.data ?? [])].sort((left, right) => right.created - left.created)[0];
		const intent = latestPayment?.payment?.payment_intent;
		const needsAction = typeof intent === 'object' && intent?.status === 'requires_action';
		if (job.reminder && !needsAction) {
			return;
		}
		const reason: BillingPaymentProblem = job.actionRequired || needsAction ? 'action_required' : 'declined';
		const key = paymentProblemKey(invoice, job, reason);
		if (key.endsWith(':declined:final') && (await this.invoiceSubscriptionEnded(invoice))) {
			return;
		}
		const planChange = invoice.billing_reason === 'subscription_update';
		const reminder = job.reminder != null;
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
				attemptedAt,
				amountMinor: invoice.amount_remaining,
				currency: invoice.currency,
				accessEndsAt: accessEndsAt && accessEndsAt.getTime() > Date.now() ? accessEndsAt : null,
				reminder,
				planChange,
				timeZone: recipient.timeZone,
			});
			await this.scheduleConfirmationReminders(job, reason, key);
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
			attemptedAt,
			amountMinor: invoice.amount_remaining,
			currency: invoice.currency,
			manageUrl: donationManageUrl(recipient.email),
			reminder,
		});
		await this.scheduleConfirmationReminders(job, reason, `donation_${key}`);
	}

	private async invoiceSubscriptionEnded(invoice: Stripe.Invoice): Promise<boolean> {
		const subscriptionId = extractId(invoice.parent?.subscription_details?.subscription ?? null);
		if (!subscriptionId) {
			return false;
		}
		const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
		return ENDED_SUBSCRIPTION_STATUSES.has(subscription.status);
	}

	private async scheduleConfirmationReminders(
		job: JobOf<'invoice_payment_problem'>,
		reason: BillingPaymentProblem,
		sendKey: string,
	): Promise<void> {
		if (reason !== 'action_required' || job.reminder || !(await this.wasSent(sendKey))) {
			return;
		}
		const scheduledKey = this.sentKey(`${sendKey}:reminders`);
		if (await this.cacheService.get<boolean>(scheduledKey)) {
			return;
		}
		const now = Date.now();
		for (const {reminder, delaySeconds} of PAYMENT_CONFIRMATION_REMINDERS) {
			const reminderJob: JobOf<'invoice_payment_problem'> = {...job, actionRequired: true, reminder};
			await this.addJob('sendBillingEmail', reminderJob, {
				runAt: new Date(now + delaySeconds * 1000),
				jobKey: billingEmailJobKey(reminderJob),
			});
		}
		await this.cacheService.set(scheduledKey, true, SENT_TTL_SECONDS);
	}

	private async refund(refundId: string, attempt: number): Promise<void> {
		const refund = await this.stripe.refunds.retrieve(refundId, {expand: ['charge', 'payment_intent']});
		if (refund.status === 'pending' || refund.status === 'requires_action') {
			throw new BillingEmailRetryError(`Refund ${refundId} has not settled yet`);
		}
		if (refund.status !== 'succeeded' || refund.amount <= 0) {
			return;
		}
		const paymentIntentId = extractId(refund.payment_intent);
		const charge = typeof refund.charge === 'object' ? refund.charge : null;
		if (!paymentIntentId) {
			return;
		}
		const intent = typeof refund.payment_intent === 'object' ? refund.payment_intent : null;
		const operatorPassPayment = intent?.metadata?.operator_pass === 'true';
		const purchase = await this.operatorPassFulfilment?.findRefundedPurchase(paymentIntentId, operatorPassPayment);
		if (!purchase && this.operatorPassFulfilment && operatorPassPayment) {
			throw new BillingEmailRetryError(`The Operator Pass purchase of refund ${refundId} is not recorded yet`);
		}
		if (purchase && this.operatorPassFulfilment) {
			const receipt = await this.operatorPassFulfilment.prepareRefundReceipt(purchase, refund);
			if (receipt) {
				await this.sendOnce(
					`refund:${refund.id}`,
					receipt.recipient,
					'operator_pass_refund_receipt',
					receipt.params,
					`re:${refund.id}`,
				);
			}
			return;
		}
		const invoice = await this.findPaidInvoice(paymentIntentId);
		const line = invoice ? findChargedLine(invoice, this.productRegistry) : null;
		const gift = invoice ? null : await this.findGiftSession(paymentIntentId);
		if ((invoice && line) || gift) {
			const user = invoice ? await this.resolveUser(invoice) : await this.giftBuyer(gift);
			const recipient = userRecipient(user);
			const creditNote = await this.documents?.issueForRefund(refund, {
				userId: user?.id ?? null,
				locale: user?.locale ?? null,
			});
			if (!recipient) {
				Logger.warn({refundId, invoiceId: invoice?.id}, 'No deliverable account email for a Plutonium refund');
				return;
			}
			const endedSubscriptionId =
				refund.metadata?.refund_kind === 'self_serve' ? refund.metadata.subscription_id : undefined;
			const subscriptionEnded = endedSubscriptionId
				? await this.refundEndedSubscription(refund.id, endedSubscriptionId, attempt)
				: false;
			const {premiumName} = await getBillingBranding();
			const giftProduct = gift?.metadata?.price_id ? this.productRegistry.getProduct(gift.metadata.price_id) : null;
			const productType = line?.product.type ?? giftProduct?.type ?? ProductType.GIFT_1_MONTH;
			await this.sendOnce(
				`refund:${refund.id}`,
				recipient,
				'plutonium_refund_receipt',
				{
					premiumName,
					plan: PLAN_BY_PRODUCT_TYPE[productType],
					invoiceNumber: originalNumber(creditNote) ?? invoice?.number ?? null,
					refundedAt: new Date(refund.created * 1000),
					amountMinor: refund.amount,
					paidMinor: invoice?.amount_paid ?? gift?.amount_total ?? refund.amount,
					paidAt: invoice ? invoicePaidAt(invoice) : new Date((gift?.created ?? refund.created) * 1000),
					currency: refund.currency,
					creditNote: creditNote ? billingDocumentAttachment(creditNote) : null,
					subscriptionEnded:
						subscriptionEnded && endedSubscriptionId
							? {requestedAt: new Date(refund.created * 1000), reference: endedSubscriptionId}
							: null,
					timeZone: recipient.timeZone,
				},
				`re:${refund.id}`,
			);
			return;
		}
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
			const creditNote = await this.documents?.issueForRefund(refund, {
				userId: null,
				locale: donor.locale,
				donorEmail: donor.email,
			});
			await this.sendOnce(
				`refund:${refund.id}`,
				donor,
				'donation_refund_receipt',
				{
					reference: originalNumber(creditNote) ?? invoice?.number ?? charge?.receipt_number ?? null,
					refundedAt: new Date(refund.created * 1000),
					amountMinor: refund.amount,
					currency: refund.currency,
					creditNote: creditNote ? billingDocumentAttachment(creditNote) : null,
				},
				`re:${refund.id}`,
			);
			return;
		}
		const buyer = invoice ? await this.resolveUser(invoice) : await this.paymentIntentBuyer(intent);
		await this.documents?.issueForRefund(refund, {userId: buyer?.id ?? null, locale: buyer?.locale ?? null});
	}

	private async plutoniumWithdrawal(job: JobOf<'plutonium_withdrawal'>): Promise<void> {
		const user = await this.userRepository.findUnique(createUserID(BigInt(job.userId)));
		const recipient = userRecipient(user);
		if (!recipient) {
			Logger.error(
				{purchaseId: job.id, userId: job.userId},
				'No deliverable account email for a Plutonium withdrawal receipt',
			);
			await alertSupport({
				dedupeKey: `plutonium-withdrawal-receipt:${job.id}`,
				subject: `No withdrawal receipt sent for ${job.id}`,
				summary: `A buyer withdrew from a Plutonium purchase at ${job.receivedAt}, but the account has no email address we can send to. Send the buyer the withdrawal confirmation another way.`,
				facts: {
					...(job.id.startsWith('sub_') ? {subscriptionId: job.id} : {checkoutSessionId: job.id}),
					userId: job.userId,
					amountMinor: job.amountMinor,
					currency: job.currency,
				},
			});
			return;
		}
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(`plutonium_withdrawal:${job.id}`, recipient, 'plutonium_withdrawal_receipt', {
			premiumName,
			name: job.name,
			receivedAt: new Date(job.receivedAt),
			purchasedAt: new Date(job.purchasedAt),
			gift: job.gift,
			amountMinor: job.amountMinor,
			currency: job.currency,
			refund: job.refund,
			brazil: job.brazil,
			proRata: job.proRata === true,
			reference: job.id,
			timeZone: recipient.timeZone,
		});
	}

	private async paymentIntentBuyer(intent: Stripe.PaymentIntent | null): Promise<User | null> {
		const customerId = extractId(intent?.customer ?? null);
		if (customerId) {
			const user = await this.userRepository.findByStripeCustomerId(customerId);
			if (user) {
				return user;
			}
		}
		const userId = intent?.metadata?.user_id ?? null;
		return userId && /^\d+$/.test(userId) ? this.userRepository.findUnique(createUserID(BigInt(userId))) : null;
	}

	private async refundEndedSubscription(refundId: string, subscriptionId: string, attempt: number): Promise<boolean> {
		const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
		if (subscription.status === 'canceled') {
			return true;
		}
		if (attempt < REFUND_CANCEL_WAIT_RETRIES) {
			throw new BillingEmailRetryError(`Waiting for the refunded subscription ${subscriptionId} to be canceled`);
		}
		Logger.warn(
			{refundId, subscriptionId, status: subscription.status},
			'The refunded subscription is still not canceled; sending the refund receipt without saying it ended',
		);
		return false;
	}

	private async subscriptionUser(subscription: Stripe.Subscription): Promise<User | null> {
		const user = await this.userRepository.findByStripeSubscriptionId(subscription.id);
		const customerId = extractId(subscription.customer);
		return user ?? (customerId ? await this.userRepository.findByStripeCustomerId(customerId) : null);
	}

	private async subscriptionRecipient(
		subscription: Stripe.Subscription,
		product: BillingNoticeProduct,
	): Promise<Recipient | null> {
		if (product === 'plutonium') {
			return userRecipient(await this.subscriptionUser(subscription));
		}
		const donor = await this.donationRepository.findDonorByStripeSubscriptionId(subscription.id);
		const email = donor?.email ?? subscription.metadata?.donation_email ?? null;
		return email ? {email, locale: subscription.metadata?.donation_locale ?? null, timeZone: null} : null;
	}

	private warnRequiredNotice(subscriptionId: string, notice: string): void {
		Logger.warn(
			{requiredNotice: true, subscriptionId, notice},
			'No deliverable email for a required billing notice; the customer was not told',
		);
	}

	private async subscriptionNotice(job: JobOf<'subscription_notice'>): Promise<void> {
		const now = nowSeconds();
		const subscription = await this.stripe.subscriptions.retrieve(job.id, {
			expand: [
				'schedule',
				'default_payment_method',
				'customer.invoice_settings.default_payment_method',
				'discounts.source.coupon',
			],
		});
		const context = await subscriptionNoticeContext(this.stripe, subscription, this.productRegistry, now, 'always');
		const product = context?.input.product;
		if (!context || !product || !noticeStillValid(context.input, job.notice, job.chargeAt)) {
			Logger.debug({job}, 'Billing notice no longer applies; not sending it');
			return;
		}
		if (job.notice === 'card_expiring') {
			await this.cardExpiringNotice(subscription, context, product, job.chargeAt);
			return;
		}
		const recipient = await this.subscriptionRecipient(subscription, product);
		if (!recipient) {
			this.warnRequiredNotice(subscription.id, job.notice);
			return;
		}
		const nextChargeAt = job.notice === 'annual' ? context.input.chargeAt : job.chargeAt;
		const amounts = await upcomingChargeAmounts(this.stripe, subscription, nextChargeAt);
		const charge = {
			chargeAt: new Date(nextChargeAt * 1000),
			next: amounts.next,
			regular: amounts.regular,
			taxIncluded: amounts.taxIncluded,
			paymentMethod: billingPaymentMethod(context.paymentMethod),
			seller: billingSellerShown(),
			timeZone: recipient.timeZone,
		};
		const key = noticeSentKey(job.notice, subscription.id, job.chargeAt, context.input);
		if (product === 'donation') {
			await this.sendOnce(`donation_${key}`, recipient, 'donation_renewal_notice', {
				...charge,
				manageUrl: donationManageUrl(recipient.email),
			});
			return;
		}
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(key, recipient, 'plutonium_renewal_notice', {
			...charge,
			variant: job.notice,
			premiumName,
			plan: context.input.plan,
			giftEndsAt: context.input.trialing && subscription.trial_end ? new Date(subscription.trial_end * 1000) : null,
			yearStartsAt: nextChargeAt === job.chargeAt ? null : new Date(job.chargeAt * 1000),
		});
	}

	private async cardExpiringNotice(
		subscription: Stripe.Subscription,
		context: SubscriptionNoticeContext,
		product: BillingNoticeProduct,
		chargeAtUnix: number,
	): Promise<void> {
		const paymentMethod = context.paymentMethod;
		const card = paymentMethod?.card;
		if (!paymentMethod || !card) {
			return;
		}
		const recipient = await this.subscriptionRecipient(subscription, product);
		if (!recipient) {
			Logger.warn({subscriptionId: subscription.id}, 'No deliverable email for an expiring card notice');
			return;
		}
		const amounts = await upcomingChargeAmounts(this.stripe, subscription, chargeAtUnix);
		const params = {
			brand: card.brand,
			last4: card.last4,
			expMonth: card.exp_month,
			expYear: card.exp_year,
			chargeAt: new Date(chargeAtUnix * 1000),
			amount: amounts.next ?? amounts.regular,
			taxIncluded: amounts.taxIncluded,
			seller: billingSellerShown(),
			timeZone: recipient.timeZone,
		};
		const key = `card_expiring:${paymentMethod.id}:${chargeAtUnix}`;
		if (product === 'donation') {
			await this.sendOnce(`donation_${key}`, recipient, 'donation_card_expiring', {
				...params,
				manageUrl: donationManageUrl(recipient.email),
			});
			return;
		}
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(key, recipient, 'plutonium_card_expiring', {...params, premiumName});
	}

	private async subscriptionCancelScheduled(job: JobOf<'subscription_cancel_scheduled'>): Promise<void> {
		const subscription = await this.stripe.subscriptions.retrieve(job.id, {expand: ['schedule', 'latest_invoice']});
		const schedule = await loadSubscriptionSchedule(this.stripe, subscription.schedule);
		const source = freshCancelSource(subscription, job.requestedAt);
		const product = subscriptionBillingProduct(subscription, this.productRegistry);
		if (
			!product ||
			subscription.status === 'canceled' ||
			!subscriptionCancelling(subscription, schedule) ||
			(source && source !== 'customer_request')
		) {
			return;
		}
		const recipient = await this.subscriptionRecipient(subscription, product);
		if (!recipient) {
			this.warnRequiredNotice(subscription.id, 'cancel_scheduled');
			return;
		}
		const item = getPrimarySubscriptionItem(subscription);
		const scheduleEnd = schedule?.end_behavior === 'cancel' ? schedule.phases.at(-1)?.end_date : null;
		const endsAt = subscription.cancel_at ?? scheduleEnd ?? item?.current_period_end ?? job.requestedAt;
		const params = {
			receivedAt: new Date(job.requestedAt * 1000),
			endsAt: new Date(endsAt * 1000),
			reference: subscription.id,
			seller: billingSellerShown(),
			timeZone: recipient.timeZone,
		};
		const key = `cancel_scheduled:${subscription.id}:${job.requestedAt}`;
		if (product === 'donation') {
			await this.sendOnce(`donation_${key}`, recipient, 'donation_cancellation_confirmation', {
				...params,
				manageUrl: donationManageUrl(recipient.email),
			});
			return;
		}
		const latest = typeof subscription.latest_invoice === 'object' ? subscription.latest_invoice : null;
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(key, recipient, 'plutonium_cancellation_confirmation', {
			...params,
			premiumName,
			plan: getSubscriptionBillingCycle(subscription) ?? 'monthly',
			unpaid:
				latest?.status === 'open' && latest.amount_remaining > 0
					? plainTotals(latest.amount_remaining, latest.currency)
					: null,
		});
	}

	private async subscriptionEndedUnpaid(subscriptionId: string): Promise<void> {
		const subscription = await this.stripe.subscriptions.retrieve(subscriptionId, {expand: ['latest_invoice']});
		if (
			subscription.cancellation_details?.reason !== 'payment_failed' ||
			subscriptionBillingProduct(subscription, this.productRegistry) !== 'plutonium'
		) {
			return;
		}
		const recipient = userRecipient(await this.subscriptionUser(subscription));
		if (!recipient) {
			Logger.warn({subscriptionId}, 'No deliverable account email for a Plutonium plan that ended unpaid');
			return;
		}
		const invoiceRef = subscription.latest_invoice;
		const invoice = typeof invoiceRef === 'string' ? await this.stripe.invoices.retrieve(invoiceRef) : invoiceRef;
		const status = endedInvoiceStatus(invoice);
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(`ended_unpaid:${subscription.id}`, recipient, 'plutonium_ended_unpaid', {
			premiumName,
			invoiceStatus: status,
			amountMinor: status === 'open' ? (invoice?.amount_remaining ?? 0) : (invoice?.total ?? 0),
			currency: invoice?.currency ?? subscription.currency,
			seller: billingSellerShown(),
		});
	}

	private async giftApplied(job: JobOf<'gift_applied'>): Promise<void> {
		const now = nowSeconds();
		const subscription = await this.stripe.subscriptions.retrieve(job.id, {
			expand: ['schedule', 'default_payment_method', 'discounts.source.coupon'],
		});
		const context = await subscriptionNoticeContext(this.stripe, subscription, this.productRegistry, now, 'always');
		if (
			context?.input.product !== 'plutonium' ||
			context.input.cancelling ||
			!context.input.trialing ||
			subscription.trial_end !== job.trialEnd
		) {
			return;
		}
		const recipient = userRecipient(await this.subscriptionUser(subscription));
		if (!recipient) {
			this.warnRequiredNotice(subscription.id, 'gift_applied');
			return;
		}
		const amounts = await upcomingChargeAmounts(this.stripe, subscription, job.trialEnd);
		const {premiumName} = await getBillingBranding();
		const key = `gift_applied:${subscription.id}:${job.trialEnd}`;
		await this.sendOnce(key, recipient, 'plutonium_gift_applied', {
			premiumName,
			plan: context.input.plan,
			trialEnd: new Date(job.trialEnd * 1000),
			amount: amounts.next ?? amounts.regular,
			taxIncluded: amounts.taxIncluded,
			paymentMethod: billingPaymentMethod(context.paymentMethod),
			seller: billingSellerShown(),
			timeZone: recipient.timeZone,
		});
		if (await this.wasSent(key)) {
			await this.markGiftNoticesSent(subscription.id, context.input, job.trialEnd);
		}
	}

	private async markGiftNoticesSent(subscriptionId: string, input: NoticeInput, trialEnd: number): Promise<void> {
		for (const due of dueSubscriptionNotices(input)) {
			if ((due.notice === 'gift' || due.notice === 'renewal') && due.chargeAt === trialEnd) {
				await this.cacheService.set(
					this.sentKey(noticeSentKey(due.notice, subscriptionId, trialEnd, input)),
					true,
					SENT_TTL_SECONDS,
				);
			}
		}
	}

	private async giftPaid(sessionId: string): Promise<void> {
		const session = await this.stripe.checkout.sessions.retrieve(sessionId);
		const product = session.metadata?.price_id ? this.productRegistry.getProduct(session.metadata.price_id) : null;
		if (
			session.mode !== 'payment' ||
			session.payment_status !== 'paid' ||
			(session.amount_total ?? 0) <= 0 ||
			!product?.isGift ||
			session.invoice
		) {
			return;
		}
		const user = await this.giftBuyer(session);
		const document = await this.documents?.issueForCheckoutSession(session.id, {
			userId: user?.id ?? null,
			locale: user?.locale ?? null,
		});
		const recipient = userRecipient(user);
		if (!recipient) {
			Logger.warn({sessionId}, 'No deliverable account email for a paid gift; receipt not sent');
			return;
		}
		const paidAt = document ? new Date(billingDocumentContent(document).payment?.at ?? document.issued_at) : new Date();
		const {premiumName} = await getBillingBranding();
		await this.sendOnce(
			`gift_receipt:${session.id}`,
			recipient,
			'plutonium_payment_receipt',
			{
				premiumName,
				plan: PLAN_BY_PRODUCT_TYPE[product.type],
				invoiceNumber: document?.display_number ?? null,
				paidAt,
				period: null,
				totals: document
					? billingDocumentTotals(document)
					: plainTotals(session.amount_total ?? 0, session.currency ?? product.currency),
				document: document ? billingDocumentAttachment(document) : null,
				business: false,
				purchase: purchaseTermsFromMetadata(session.metadata ?? {}, paidAt, null),
				timeZone: recipient.timeZone,
			},
			`cs:${session.id}`,
		);
	}

	private async findGiftSession(paymentIntentId: string): Promise<Stripe.Checkout.Session | null> {
		const sessions = await this.stripe.checkout.sessions.list({payment_intent: paymentIntentId, limit: 1});
		const session = sessions.data[0];
		return session?.metadata?.is_gift === 'true' ? session : null;
	}

	private async giftBuyer(session: Stripe.Checkout.Session | null): Promise<User | null> {
		const userId = session?.metadata?.user_id ?? session?.client_reference_id ?? null;
		return userId && /^\d+$/.test(userId) ? this.userRepository.findUnique(createUserID(BigInt(userId))) : null;
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

	private async recordDocumentEmailed(documentSource: string | null): Promise<void> {
		if (!documentSource || !this.documents) {
			return;
		}
		try {
			const document = await this.documents.repository.findDocumentBySource(documentSource);
			if (document && document.emailed_at == null) {
				await this.documents.repository.markEmailed(document.document_id, new Date());
			}
		} catch (error) {
			Logger.error({error, documentSource}, 'Sent a billing document but could not record when it was emailed');
		}
	}

	private async wasSent(key: string): Promise<boolean> {
		return Boolean(await this.cacheService.get<boolean>(this.sentKey(key)));
	}

	private async sendOnce<K extends keyof EmailSendParams>(
		key: string,
		recipient: Recipient,
		templateKey: K,
		params: EmailSendParams[K],
		documentSource: string | null = null,
	): Promise<boolean> {
		const sentKey = this.sentKey(key);
		if (await this.cacheService.get<boolean>(sentKey)) {
			return false;
		}
		const lockKey = `stripe:billing:email:lock:${key}`;
		const token = await this.cacheService.acquireLock(lockKey, LOCK_TTL_SECONDS);
		if (!token) {
			Logger.info({key}, 'Another job is sending this billing email');
			return false;
		}
		try {
			if (await this.cacheService.get<boolean>(sentKey)) {
				return false;
			}
			if (await this.emailService.isEmailBounced(recipient.email)) {
				Logger.warn({key, templateKey}, 'Not sending a billing email to a hard-bounced address');
				return false;
			}
			if (!(await this.emailService.send(templateKey, recipient.email, params, recipient.locale))) {
				throw new BillingEmailRetryError(`Failed to send the billing email ${templateKey}`);
			}
			try {
				await this.cacheService.set(sentKey, true, SENT_TTL_SECONDS);
			} catch (error) {
				Logger.error({error, key}, 'Sent a billing email but could not record it; a retry may send it again');
			}
			await this.recordDocumentEmailed(documentSource);
			return true;
		} finally {
			await this.cacheService.releaseLock(lockKey, token);
		}
	}
}

function originalNumber(document: BillingDocumentRow | null | undefined): string | null {
	return document ? (billingDocumentContent(document).original?.number ?? null) : null;
}
