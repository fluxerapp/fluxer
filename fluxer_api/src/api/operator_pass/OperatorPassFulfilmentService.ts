// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {
	type BillingDocumentService,
	billingDocumentAttachment,
	billingDocumentTotals,
	createBillingDocumentService,
} from '@app/api/billing_documents/BillingDocumentService';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassCertificateRow,
	OperatorPassOfferRow,
	OperatorPassPurchaseRow,
	OperatorPassUnitRow,
	OperatorPassWithdrawalChannel,
} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import type {OperatorPassServices} from '@app/api/middleware/OperatorPassServiceFactory';
import {getBillingRepository, getKVClient, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {findOperatorPassBusinessPurchase} from '@app/api/operator_pass/OperatorPassBusinessListing';
import {
	type OperatorPassCertificateReissueReason,
	OperatorPassCertificateService,
} from '@app/api/operator_pass/OperatorPassCertificateService';
import {creditOperatorPassRefunds, operatorPassDocumentContext} from '@app/api/operator_pass/OperatorPassCreditNotes';
import {
	isOperatorPassPermanentlyRemoved,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {
	OperatorPassHoldingService,
	OperatorPassOfferParams,
} from '@app/api/operator_pass/OperatorPassHoldingService';
import {
	keepsOperatorsThroughVisionary,
	OPERATOR_PASS_LABS_INVITE_URL,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import {
	type OperatorPassLockLease,
	operatorPassHolderLockKey,
	operatorPassPurchaseLockKey,
	withOperatorPassLock,
} from '@app/api/operator_pass/OperatorPassLocks';
import {
	OPERATOR_PASS_WITHDRAWAL_RULE_VERSION,
	type OperatorPassWithdrawalRuleMatch,
	resolveOperatorPassWithdrawalRule,
} from '@app/api/operator_pass/OperatorPassPrices';
import {
	getOperatorPassCertificateUrl,
	getOperatorPassWithdrawalEndsAt,
	getOperatorPassWithdrawalEndsOn,
	isOperatorPassCertificatePageCancelled,
	isOperatorPassPurchaseWithdrawable,
	isOperatorPassSinglePassWithdrawable,
	isOperatorPassWelcomeSettled,
	isOperatorPassWithinWithdrawalDeadline,
	listOperatorPassWithdrawableUnits,
	replaceOperatorPassCertificateName,
	wasOperatorPassPurchaseUndeliveredAt,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {isOperatorPassCurrency, operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {OperatorPassVisionaryService} from '@app/api/operator_pass/OperatorPassVisionaryService';
import type {OperatorPassWallService} from '@app/api/operator_pass/OperatorPassWallService';
import {extractId} from '@app/api/stripe/StripeUtils';
import {enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {OperatorPassOfferBusyError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassOfferBusyError';
import {OperatorPassWithdrawalUnavailableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWithdrawalUnavailableError';
import {UnknownOperatorPassPurchaseError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassPurchaseError';
import {UnknownOperatorPassUnitError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassUnitError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {OPERATOR_PASS_PHASES} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {
	BillingDocumentAttachment,
	BillingTotals,
	EmailAttachment,
	EmailSendParams,
	IEmailService,
	OperatorPassWithdrawalRefund,
} from '@pkgs/email/src/IEmailService';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';
import Stripe from 'stripe';

const APPLIED_TTL_SECONDS = seconds('365 days');
const WITHDRAWAL_RECEIPT_REQUEUE_AFTER_MS = ms('10 minutes');
const WITHDRAWAL_RECEIPT_ALERT_MS = ms('24 hours');
const WITHDRAWAL_RECEIPT_RETRY_MS = ms('7 days');
const OWED_CREDIT_NOTE_DELAY_MS = ms('1 hour');
const BILLING_RECORD_ATTEMPTS = 3;

interface ReturnedPassPayment {
	refundId: string | null;
	viaDispute: boolean;
	amountMinor: number;
}

interface AcceptedWithdrawalDispute {
	receivedAt: Date;
	passCount: number;
	amountMinor: number;
}

interface CheckoutPaymentReferences {
	paymentIntentId: string | null;
}

export interface OperatorPassWithdrawParams {
	purchaser: User;
	purchaseId: string;
	name: string;
	channel: OperatorPassWithdrawalChannel;
	receivedAt: Date;
}

export interface OperatorPassWithdrawalReceipt {
	purchaseId: string;
	passId: string | null;
	recipientUserId: UserID;
	name: string;
	receivedAt: Date;
	recordedAt: Date;
	amountMinor: number;
	refund: OperatorPassWithdrawalRefund;
	delivered: boolean;
	passCount: number;
	lastPass: boolean;
	visionary: boolean;
	refundId: string | null;
}

export interface OperatorPassRefundReceipt {
	recipient: {email: string; locale: string | null; timeZone: string | null};
	params: EmailSendParams['operator_pass_refund_receipt'];
}

export const OPERATOR_PASS_WITHDRAWAL_RECEIPT_TASK = 'sendOperatorPassWithdrawalReceipt';
export const OPERATOR_PASS_DELIVERY_RECORD_TASK = 'recordOperatorPassDelivery';

function encodeOperatorPassWithdrawalReceipt(receipt: OperatorPassWithdrawalReceipt): Record<string, unknown> {
	return {
		purchase_id: receipt.purchaseId,
		pass_id: receipt.passId ?? 'all',
		recipient_user_id: receipt.recipientUserId.toString(),
		name: receipt.name,
		received_at: receipt.receivedAt.toISOString(),
		recorded_at: receipt.recordedAt.toISOString(),
		amount_minor: receipt.amountMinor,
		refund: receipt.refund,
		delivered: receipt.delivered,
		pass_count: receipt.passCount,
		last_pass: receipt.lastPass,
		visionary: receipt.visionary,
		refund_id: receipt.refundId ?? '',
	};
}

function startedWithdrawalRefund(returned: {viaDispute: boolean}): OperatorPassWithdrawalRefund {
	return returned.viaDispute ? 'dispute' : 'started';
}

function decodeWithdrawalRefund(value: unknown): OperatorPassWithdrawalRefund {
	return value === 'dispute' || value === 'already' ? value : 'started';
}

export function decodeOperatorPassWithdrawalReceipt(payload: Record<string, unknown>): OperatorPassWithdrawalReceipt {
	const passId = String(payload.pass_id);
	return {
		purchaseId: String(payload.purchase_id),
		passId: passId === 'all' ? null : passId,
		recipientUserId: createUserID(BigInt(String(payload.recipient_user_id))),
		name: String(payload.name),
		receivedAt: new Date(String(payload.received_at)),
		recordedAt: new Date(String(payload.recorded_at)),
		amountMinor: Number(payload.amount_minor),
		refund: decodeWithdrawalRefund(payload.refund),
		delivered: payload.delivered !== false,
		passCount: Math.max(1, Number(payload.pass_count) || 1),
		lastPass: payload.last_pass === true,
		visionary: payload.visionary === true,
		refundId: typeof payload.refund_id === 'string' && payload.refund_id !== '' ? payload.refund_id : null,
	};
}

export interface OperatorPassUnitWithdrawParams {
	holder: User;
	passId: string;
	name: string;
	receivedAt: Date;
}

export async function recordOperatorPassDeliveryOnPayment(
	stripe: Stripe | null,
	purchase: Pick<
		OperatorPassPurchaseRow,
		'payment_intent_id' | 'checkout_session_id' | 'purchased_at' | 'withdrawal_country'
	>,
	deliveredAt: Date,
): Promise<void> {
	if (!purchase.payment_intent_id) {
		return;
	}
	if (!stripe) {
		throw new StripeError('Stripe is not available to record the Operator Pass certificate delivery time');
	}
	const metadata: Record<string, string> = {
		operator_pass_delivered_at: deliveredAt.toISOString(),
		operator_pass_withdrawal_country: purchase.withdrawal_country ?? '',
	};
	if (purchase.purchased_at) {
		metadata.operator_pass_paid_at = purchase.purchased_at.toISOString();
	}
	for (let attempt = 1; attempt <= BILLING_RECORD_ATTEMPTS; attempt++) {
		try {
			await stripe.paymentIntents.update(purchase.payment_intent_id, {metadata});
			return;
		} catch (error) {
			Logger.warn(
				{error, attempt, sessionId: purchase.checkout_session_id},
				'Failed to record the Operator Pass certificate delivery time on the payment',
			);
		}
	}
	throw new StripeError('Could not record the Operator Pass certificate delivery time on the payment');
}

export async function syncOperatorPassDeliveryRecord(
	stripe: Stripe | null,
	repository: Pick<OperatorPassRepository, 'updatePurchase'>,
	purchase: OperatorPassPurchaseRow,
): Promise<OperatorPassPurchaseRow> {
	if (purchase.delivered_at == null || purchase.delivery_recorded_at != null) {
		return purchase;
	}
	await recordOperatorPassDeliveryOnPayment(stripe, purchase, purchase.delivered_at);
	return repository.updatePurchase(purchase, {delivery_recorded_at: Db.set(operatorPassNow())});
}

const OPERATOR_PASS_REFUSALS = [
	'operator_pass_removed',
	'operator_pass_names_missing',
	'operator_pass_email_undeliverable',
] as const;

type OperatorPassRefusal = (typeof OPERATOR_PASS_REFUSALS)[number];

function refusalOfRefund(refund: Stripe.Refund, purchase: OperatorPassPurchaseRow): OperatorPassRefusal | null {
	if (refund.metadata?.checkout_session_id !== purchase.checkout_session_id) {
		return null;
	}
	const reason = refund.metadata?.rejection_reason ?? '';
	return OPERATOR_PASS_REFUSALS.find((refusal) => refusal === reason) ?? null;
}

function alertWithdrawalReceiptNotSent(
	purchase: OperatorPassPurchaseRow,
	receipt: OperatorPassWithdrawalReceipt,
	dedupeKey: string,
	summary: string,
): Promise<void> {
	return alertSupport({
		dedupeKey,
		subject: 'Operator Pass withdrawal receipt not sent',
		summary,
		facts: {
			refundId: receipt.refundId,
			paymentIntentId: purchase.payment_intent_id,
			checkoutSessionId: purchase.checkout_session_id,
			userId: receipt.recipientUserId,
			documentNumber: purchase.invoice_number,
			amountMinor: receipt.amountMinor,
			currency: purchase.currency,
		},
	});
}

function alertOperatorPassRefundNotSent(
	purchase: OperatorPassPurchaseRow,
	refund: Stripe.Refund,
	summary: string,
	error: unknown = refund.failure_reason,
): Promise<void> {
	return alertSupport({
		dedupeKey: `operator-pass-refund:${refund.id}`,
		subject: `Operator Pass refund ${refund.id} did not reach the buyer`,
		summary,
		facts: {
			refundId: refund.id,
			paymentIntentId: purchase.payment_intent_id,
			checkoutSessionId: purchase.checkout_session_id,
			userId: purchase.purchaser_id,
			documentNumber: purchase.invoice_number,
			amountMinor: refund.amount,
			currency: refund.currency,
			error,
		},
	});
}

const EMAILED_REFUSALS: ReadonlySet<OperatorPassRefusal> = new Set(['operator_pass_names_missing']);

const WITHDRAWAL_REFUND_PREFIX = 'operator_pass_withdrawal:';

const CONSUMER_ONLY_COUNTRIES: ReadonlySet<string> = new Set(['BR']);

export function isOperatorPassBusinessCheckout(session: Stripe.Checkout.Session): boolean {
	const details = session.customer_details;
	return Boolean(details?.business_name) || (details?.tax_ids?.length ?? 0) > 0;
}

export function resolveOperatorPassPurchaseWithdrawalRule(
	session: Stripe.Checkout.Session,
	purchase: Pick<OperatorPassPurchaseRow, 'withdrawal_country' | 'currency'>,
): OperatorPassWithdrawalRuleMatch | null {
	if (session.metadata?.operator_pass_cnpj) {
		return null;
	}
	const rule = resolveOperatorPassWithdrawalRule(
		Config.operatorPass.withdrawalCountries,
		[purchase.withdrawal_country, session.customer_details?.address?.country],
		purchase.currency,
	);
	if (rule && CONSUMER_ONLY_COUNTRIES.has(rule.country)) {
		return rule;
	}
	return isOperatorPassBusinessCheckout(session) ? null : rule;
}

export function isOperatorPassCheckoutSession(session: Stripe.Checkout.Session): boolean {
	return session.metadata?.operator_pass === 'true';
}

interface OperatorPassFulfilmentDependencies {
	stripe: Stripe | null;
	userRepository: UserRepository;
	emailService: IEmailService;
	cacheService: ICacheService;
	services: OperatorPassServices;
}

export function createOperatorPassFulfilmentService(
	dependencies: OperatorPassFulfilmentDependencies,
): OperatorPassFulfilmentService | null {
	if (Config.instance.selfHosted) {
		return null;
	}
	return new OperatorPassFulfilmentService(
		dependencies.stripe,
		dependencies.services.repository,
		dependencies.userRepository,
		dependencies.emailService,
		dependencies.cacheService,
		getKVClient(),
		dependencies.services,
	);
}

export class OperatorPassFulfilmentService {
	readonly wallService: OperatorPassWallService;
	readonly holdingService: OperatorPassHoldingService;
	readonly certificates: OperatorPassCertificateService;
	readonly visionary: OperatorPassVisionaryService;

	constructor(
		private readonly stripe: Stripe | null,
		readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly emailService: IEmailService,
		private readonly cacheService: ICacheService,
		kvClient: IKVProvider,
		private readonly services: OperatorPassServices,
		private readonly documents: BillingDocumentService | null = createBillingDocumentService(stripe),
	) {
		this.wallService = services.wallService;
		this.holdingService = services.holdingService;
		this.certificates = new OperatorPassCertificateService(repository, cacheService, kvClient);
		this.visionary = new OperatorPassVisionaryService({
			repository,
			userRepository,
			holdingService: services.holdingService,
			wallService: services.wallService,
			roleService: services.roleService,
			certificates: this.certificates,
			emailService,
			cacheService,
		});
	}

	get removalService(): OperatorPassServices['removalService'] {
		return this.services.removalService;
	}

	async handleCheckoutSessionCompleted(session: Stripe.Checkout.Session): Promise<void> {
		if (session.payment_status === 'unpaid') {
			Logger.info(
				{sessionId: session.id, paymentStatus: session.payment_status},
				'Operator Pass checkout completed with unpaid status; deferring fulfilment until settlement',
			);
			return;
		}
		await this.fulfilCheckoutSession(session, null);
	}

	async handleAsyncPaymentSucceeded(session: Stripe.Checkout.Session, paidAt: Date): Promise<void> {
		await this.fulfilCheckoutSession(session, paidAt);
	}

	async handleCheckoutSessionExpired(session: Stripe.Checkout.Session): Promise<void> {
		await this.withPurchaseLock(session.id, async () => {
			const purchase = await this.repository.findPurchase(session.id);
			if (purchase?.status !== 'pending') {
				return;
			}
			await this.repository.deletePurchase(purchase);
			Logger.info(
				{sessionId: session.id, purchaserId: purchase.purchaser_id},
				'Deleted the pending Operator Pass purchase of an expired checkout session',
			);
		});
	}

	async handleAsyncPaymentFailed(session: Stripe.Checkout.Session): Promise<void> {
		await this.withPurchaseLock(session.id, async () => {
			const purchase = await this.repository.findPurchase(session.id);
			if (purchase?.status !== 'pending' || purchase.purchased_at != null) {
				return;
			}
			await this.repository.deletePurchase(purchase);
			Logger.warn(
				{sessionId: session.id, purchaserId: purchase.purchaser_id, paymentStatus: session.payment_status},
				'Deleted the pending Operator Pass purchase of a failed async payment',
			);
		});
	}

	async findPurchaseByPaymentIntent(paymentIntentId: string): Promise<OperatorPassPurchaseRow | null> {
		return this.repository.findPurchaseByPaymentIntent(paymentIntentId);
	}

	async findRefundedPurchase(
		paymentIntentId: string,
		operatorPassPayment: boolean,
	): Promise<OperatorPassPurchaseRow | null> {
		const purchase = await this.repository.findPurchaseByPaymentIntent(paymentIntentId);
		if (purchase) {
			return this.settlePendingPurchase(purchase);
		}
		if (!this.stripe || !operatorPassPayment) {
			return null;
		}
		const sessions = await this.stripe.checkout.sessions.list({payment_intent: paymentIntentId, limit: 1});
		const session = sessions.data[0];
		if (session?.metadata?.operator_pass !== 'true') {
			return null;
		}
		return this.settleCheckoutSession(session);
	}

	async settlePendingPurchase(purchase: OperatorPassPurchaseRow): Promise<OperatorPassPurchaseRow> {
		if (purchase.status !== 'pending' || !this.stripe) {
			return purchase;
		}
		const session = await this.stripe.checkout.sessions.retrieve(purchase.checkout_session_id);
		return (await this.settleCheckoutSession(session)) ?? purchase;
	}

	async settleReturnedCheckout(purchaserId: UserID, sessionId: string): Promise<void> {
		const purchase = await this.repository.findPurchase(sessionId);
		if (purchase?.purchaser_id !== purchaserId || purchase.status !== 'pending') {
			return;
		}
		try {
			await this.settlePendingPurchase(purchase);
		} catch (error) {
			Logger.warn(
				{error, sessionId},
				'Could not fulfil a returned Operator Pass checkout right away; the webhook or the sweep will',
			);
		}
	}

	private async settleCheckoutSession(session: Stripe.Checkout.Session): Promise<OperatorPassPurchaseRow | null> {
		if (session.payment_status !== 'unpaid') {
			await this.fulfilCheckoutSession(session, null);
		}
		return this.repository.findPurchase(session.id);
	}

	async markFraudWarning(paymentIntentId: string): Promise<void> {
		const purchase =
			(await this.repository.findPurchaseByPaymentIntent(paymentIntentId)) ??
			(await this.findPurchaseBySessionPaymentIntent(paymentIntentId));
		if (!purchase || purchase.fraud_warning_at) {
			return;
		}
		await this.withCurrentPurchase(purchase, (current) =>
			this.repository.updatePurchase(current, {fraud_warning_at: Db.set(operatorPassNow())}),
		);
	}

	private async findPurchaseBySessionPaymentIntent(paymentIntentId: string): Promise<OperatorPassPurchaseRow | null> {
		if (!this.stripe) {
			return null;
		}
		const sessions = await this.stripe.checkout.sessions.list({payment_intent: paymentIntentId, limit: 1});
		const session = sessions.data[0];
		return session?.metadata?.operator_pass === 'true' ? this.repository.findPurchase(session.id) : null;
	}

	async listPurchases(purchaserId: UserID): Promise<Array<OperatorPassPurchaseRow>> {
		const purchases = await this.repository.listPurchasesByPurchaser(purchaserId);
		return purchases
			.filter((purchase) => purchase.status !== 'pending' || purchase.purchased_at != null)
			.sort((left, right) => right.created_at.getTime() - left.created_at.getTime());
	}

	async handleChargeRefunded(purchase: OperatorPassPurchaseRow, charge: Stripe.Charge): Promise<void> {
		await this.withCurrentPurchase(purchase, async (current) => {
			if (current.status === 'pending') {
				Logger.info(
					{sessionId: current.checkout_session_id, chargeId: charge.id},
					'Operator Pass refund arrived before fulfilment finished, so the webhook job retries it',
				);
				throw new StripeError('Operator Pass purchase is not fulfilled yet; retrying the refund');
			}
			await this.handleChargeRefundedLocked(current, charge);
			if (this.stripe && this.documents) {
				await creditOperatorPassRefunds(this.stripe, this.documents, current);
			}
		});
	}

	private async creditRefundsMadeHere(purchase: OperatorPassPurchaseRow): Promise<void> {
		if (!this.stripe || !this.documents) return;
		try {
			await creditOperatorPassRefunds(this.stripe, this.documents, purchase);
		} catch (error) {
			Logger.error(
				{error, sessionId: purchase.checkout_session_id},
				'Could not issue the credit note for an Operator Pass refund; the charge.refunded webhook will retry',
			);
		}
	}

	async handleDisputeCreated(purchase: OperatorPassPurchaseRow, dispute: Stripe.Dispute): Promise<void> {
		await this.withCurrentPurchase(purchase, (current) => this.handleDisputeCreatedLocked(current, dispute));
	}

	async handleDisputeReleased(purchase: OperatorPassPurchaseRow, dispute: Stripe.Dispute): Promise<void> {
		await this.withCurrentPurchase(purchase, (current) => this.handleDisputeReleasedLocked(current, dispute));
	}

	async handleDisputeLost(
		purchase: OperatorPassPurchaseRow,
		dispute: Stripe.Dispute,
	): Promise<OperatorPassPurchaseRow> {
		return this.withCurrentPurchase(purchase, (current) => this.handleDisputeLostLocked(current, dispute));
	}

	private async handleChargeRefundedLocked(purchase: OperatorPassPurchaseRow, charge: Stripe.Charge): Promise<void> {
		if (
			purchase.status === 'duplicate_refunded' ||
			purchase.status === 'withdrawn' ||
			purchase.status === 'dispute_lost'
		) {
			Logger.debug(
				{sessionId: purchase.checkout_session_id, status: purchase.status, chargeId: charge.id},
				'Operator Pass refund already handled',
			);
			return;
		}
		const passRefunded = charge.refunded ? purchase.amount_minor : charge.amount_refunded;
		const quantity = Math.max(1, purchase.quantity ?? 1);
		if (passRefunded < purchase.amount_minor) {
			const unitAmount = Math.round(purchase.amount_minor / quantity);
			const units = await this.repository.listUnitsByPurchase(purchase.checkout_session_id);
			const ended = units.filter(isOperatorPassCertificatePageCancelled).length;
			const covered = unitAmount > 0 ? Math.floor(passRefunded / unitAmount) : 0;
			const toEnd = Math.max(0, covered - ended);
			if (toEnd > 0) {
				await this.holdingService.endPurchaseUnits(purchase, 'refund', 'revoked', toEnd);
			}
			await this.certificates.cancelReversedPages(purchase, 'refund', covered);
			const cancelled = (await this.repository.listUnitsByPurchase(purchase.checkout_session_id)).filter(
				isOperatorPassCertificatePageCancelled,
			).length;
			if (cancelled >= quantity) {
				await this.markRefunded(purchase);
			}
			Logger.info(
				{
					sessionId: purchase.checkout_session_id,
					chargeId: charge.id,
					amountRefunded: charge.amount_refunded,
					passRefunded,
					passAmount: purchase.amount_minor,
					revoked: toEnd,
					cancelled,
				},
				'Partial refund of an Operator Pass purchase; revoked the passes it covers',
			);
			return;
		}
		await this.revokeWholePurchase(purchase);
		Logger.info(
			{sessionId: purchase.checkout_session_id, purchaserId: purchase.purchaser_id},
			'Operator Pass revoked after a refund',
		);
	}

	private async revokeWholePurchase(purchase: OperatorPassPurchaseRow): Promise<void> {
		await this.holdingService.endPurchaseUnits(purchase, 'refund', 'revoked');
		await this.certificates.cancelReversedPages(purchase, 'refund');
		await this.markRefunded(purchase);
	}

	async cancelPreviewPurchase(purchaseId: string): Promise<OperatorPassPurchaseRow> {
		return this.withPurchaseLock(
			purchaseId,
			async () => {
				const purchase = await this.repository.findPurchase(purchaseId);
				if (!purchase || purchase.preview_label == null) {
					throw new UnknownOperatorPassPurchaseError();
				}
				if (purchase.status === 'pending') {
					if (this.stripe) {
						const session = await this.stripe.checkout.sessions.retrieve(purchaseId);
						if (session.status === 'open') {
							await this.stripe.checkout.sessions.expire(purchaseId);
						} else if (session.status === 'complete') {
							throw new StripeError('The preview checkout is complete but not delivered yet; retry in a moment');
						}
					}
					await this.repository.deletePurchase(purchase);
					return purchase;
				}
				if (purchase.status === 'completed' || purchase.status === 'disputed') {
					await this.revokeWholePurchase(purchase);
					Logger.info(
						{sessionId: purchaseId, purchaserId: purchase.purchaser_id, label: purchase.preview_label},
						'Operator Pass preview purchase cancelled',
					);
				}
				return (await this.repository.findPurchase(purchaseId)) ?? purchase;
			},
			purchaseBusyError,
		);
	}

	private async markRefunded(purchase: OperatorPassPurchaseRow): Promise<void> {
		if (purchase.status === 'refunded') {
			return;
		}
		await this.repository.updatePurchase(purchase, {status: Db.set('refunded'), refunded_at: Db.set(new Date())});
		await this.reviewBusinessListing(purchase);
	}

	private async reviewBusinessListing(purchase: OperatorPassPurchaseRow): Promise<void> {
		const userId = purchase.purchaser_id;
		if (!(await this.wallService.getEntry(userId))?.live_business) {
			return;
		}
		let qualifying: OperatorPassPurchaseRow | null = null;
		if (this.stripe) {
			try {
				qualifying = await findOperatorPassBusinessPurchase({
					stripe: this.stripe,
					repository: this.repository,
					userId,
					now: operatorPassNow(),
				});
			} catch (error) {
				Logger.warn({error, userId}, 'Could not check a business listing again, so it goes back to personal');
			}
		}
		if (!qualifying) {
			await this.wallService.endBusinessListing(userId);
		}
	}

	private async applyRefundsMadeElsewhere(purchase: OperatorPassPurchaseRow): Promise<OperatorPassPurchaseRow> {
		if (!this.stripe || !purchase.payment_intent_id || !['completed', 'disputed'].includes(purchase.status)) {
			return purchase;
		}
		const intent = await this.stripe.paymentIntents.retrieve(purchase.payment_intent_id, {expand: ['latest_charge']});
		const charge = typeof intent.latest_charge === 'object' ? intent.latest_charge : null;
		if (!charge || charge.amount_refunded === 0) {
			return purchase;
		}
		await this.handleChargeRefundedLocked(purchase, charge);
		return (await this.repository.findPurchase(purchase.checkout_session_id)) ?? purchase;
	}

	private async handleDisputeCreatedLocked(purchase: OperatorPassPurchaseRow, dispute: Stripe.Dispute): Promise<void> {
		const disputeFields = {
			dispute_id: Db.set(dispute.id),
			dispute_reason: Db.set(dispute.reason ?? null),
			dispute_status: Db.set(dispute.status),
			disputed_at: Db.set(purchase.disputed_at ?? new Date(dispute.created * 1000)),
		};
		if (purchase.dispute_id === dispute.id && purchase.dispute_closed_at != null) {
			Logger.info(
				{sessionId: purchase.checkout_session_id, status: purchase.status, disputeId: dispute.id},
				'Dispute opened event arrived after the dispute closed; leaving the Operator Pass as it is',
			);
			return;
		}
		if (purchase.status !== 'completed') {
			Logger.info(
				{sessionId: purchase.checkout_session_id, status: purchase.status, disputeId: dispute.id},
				'Dispute on an Operator Pass purchase that has no active pass to pause',
			);
			await this.repository.updatePurchase(purchase, disputeFields);
			return;
		}
		await this.holdingService.setPurchasePaused(purchase, true);
		await this.repository.updatePurchase(purchase, {...disputeFields, status: Db.set('disputed')});
		await this.reviewBusinessListing(purchase);
		Logger.info(
			{sessionId: purchase.checkout_session_id, purchaserId: purchase.purchaser_id, disputeId: dispute.id},
			'Operator Pass paused while a payment dispute is open',
		);
	}

	private async handleDisputeReleasedLocked(purchase: OperatorPassPurchaseRow, dispute: Stripe.Dispute): Promise<void> {
		if (purchase.status !== 'disputed') {
			Logger.debug(
				{sessionId: purchase.checkout_session_id, status: purchase.status, disputeId: dispute.id},
				'Closed dispute on an Operator Pass purchase that the dispute did not pause',
			);
			const current = await this.repository.updatePurchase(purchase, {
				dispute_id: Db.set(dispute.id),
				dispute_status: Db.set(dispute.status),
				dispute_closed_at: Db.set(new Date()),
			});
			await this.sendDueWelcome(current);
			return;
		}
		const updated = await this.repository.updatePurchase(purchase, {
			status: Db.set('completed'),
			dispute_status: Db.set(dispute.status),
			dispute_closed_at: Db.set(new Date()),
		});
		await this.holdingService.setPurchasePaused(updated, false);
		Logger.info(
			{sessionId: purchase.checkout_session_id, purchaserId: purchase.purchaser_id, disputeStatus: dispute.status},
			'Operator Pass restored after a payment dispute closed without the money being returned',
		);
		await this.sendDueWelcome(updated);
	}

	private async sendDueWelcome(purchase: OperatorPassPurchaseRow): Promise<void> {
		if (purchase.status !== 'completed' || purchase.delivered_at == null || isOperatorPassWelcomeSettled(purchase)) {
			return;
		}
		const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
		if (!purchaser) {
			Logger.error(
				{sessionId: purchase.checkout_session_id, userId: purchase.purchaser_id},
				'Operator Pass purchaser not found for the certificate email',
			);
			return;
		}
		try {
			await this.sendWelcomeOnce(purchase, purchaser);
		} catch (error) {
			Logger.warn(
				{error, sessionId: purchase.checkout_session_id},
				'Operator Pass certificate email failed after a dispute closed, the fulfilment sweep will retry it',
			);
		}
	}

	private async handleDisputeLostLocked(
		purchase: OperatorPassPurchaseRow,
		dispute: Stripe.Dispute,
	): Promise<OperatorPassPurchaseRow> {
		if (purchase.status === 'dispute_lost') {
			await this.certificates.cancelReversedPages(purchase, 'dispute');
			return purchase;
		}
		const active = purchase.status === 'disputed' || purchase.status === 'completed';
		if (active) {
			await this.holdingService.endPurchaseUnits(purchase, 'dispute', 'revoked');
		}
		await this.certificates.cancelReversedPages(purchase, 'dispute');
		const updated = await this.repository.updatePurchase(purchase, {
			status: Db.set(active || purchase.status === 'pending' ? 'dispute_lost' : purchase.status),
			dispute_id: Db.set(dispute.id),
			dispute_reason: Db.set(dispute.reason ?? null),
			dispute_status: Db.set(dispute.status),
			disputed_at: Db.set(purchase.disputed_at ?? new Date(dispute.created * 1000)),
			dispute_closed_at: Db.set(new Date()),
		});
		await this.reviewBusinessListing(purchase);
		Logger.info(
			{sessionId: purchase.checkout_session_id, purchaserId: purchase.purchaser_id, disputeId: dispute.id},
			'Operator Pass ended after a payment dispute returned the money',
		);
		return updated;
	}

	async withdraw({
		purchaser,
		purchaseId,
		name,
		channel,
		receivedAt: requestedAt,
	}: OperatorPassWithdrawParams): Promise<OperatorPassPurchaseRow> {
		const found = await this.repository.findPurchase(purchaseId);
		if (!found || found.purchaser_id !== purchaser.id) {
			throw new UnknownOperatorPassPurchaseError();
		}
		return this.withPurchaseLock(
			purchaseId,
			async () => {
				const purchase = await this.applyRefundsMadeElsewhere(
					(await this.repository.findPurchase(purchaseId)) ?? found,
				);
				const accepted = await this.findAcceptedWithdrawalDispute(purchase);
				const receivedAt = accepted?.receivedAt ?? requestedAt;
				const refundedBefore = purchase.status === 'refunded' && channel !== 'button';
				const units = await this.repository.listUnitsByPurchase(purchase.checkout_session_id);
				const undeliveredAtReceipt =
					channel !== 'button' && !refundedBefore && wasOperatorPassPurchaseUndeliveredAt(purchase, receivedAt);
				const heldUnits = units.filter((unit) => unit.status === 'held');
				const withdrawableUnits = undeliveredAtReceipt
					? heldUnits.filter((unit) => unit.holder_id === purchase.purchaser_id)
					: listOperatorPassWithdrawableUnits(purchase, units, receivedAt);
				const movedUnits = undeliveredAtReceipt
					? heldUnits.filter((unit) => unit.holder_id !== purchase.purchaser_id)
					: await this.listUnitsPassedOnAfterReceipt(purchase, units, receivedAt, channel, refundedBefore);
				const withinPeriod =
					accepted !== null ||
					(refundedBefore
						? isOperatorPassWithinWithdrawalDeadline(purchase, receivedAt)
						: undeliveredAtReceipt ||
							isOperatorPassPurchaseWithdrawable(purchase, units, receivedAt) ||
							withdrawableUnits.length + movedUnits.length > 0);
				if (!withinPeriod || !purchase.payment_intent_id) {
					throw new OperatorPassWithdrawalUnavailableError();
				}
				const quantity = Math.max(1, purchase.quantity ?? 1);
				const unitPrice = Math.round(purchase.amount_minor / quantity);
				const refundedUnits = units.filter((unit) => unit.end_reason === 'refund').length;
				let amountMinor = purchase.amount_minor;
				let passCount = quantity;
				if (accepted) {
					amountMinor = accepted.amountMinor;
					passCount = accepted.passCount;
				} else if (refundedBefore) {
					if (refundedUnits > 0 && refundedUnits < quantity) {
						amountMinor = unitPrice * refundedUnits;
						passCount = refundedUnits;
					}
				} else if (undeliveredAtReceipt) {
					if (refundedUnits > 0) amountMinor = unitPrice * (quantity - refundedUnits);
					if (units.length > 0) passCount = withdrawableUnits.length + movedUnits.length;
				} else if (units.length > 0) {
					amountMinor = unitPrice * (withdrawableUnits.length + movedUnits.length);
					passCount = withdrawableUnits.length + movedUnits.length;
				}
				let returned: ReturnedPassPayment | null = null;
				if (accepted) {
					returned = {refundId: null, viaDispute: true, amountMinor};
				} else if (!refundedBefore) {
					returned = await this.returnPassPayment(purchase, amountMinor, `all:${receivedAt.toISOString()}`, {
						receivedAt,
						passCount,
					});
				}
				if (returned?.refundId) {
					await this.creditRefundsMadeHere(purchase);
				}
				if (!refundedBefore) {
					await this.holdingService.endPurchaseUnits(
						purchase,
						'withdrawal',
						'withdrawn',
						Number.POSITIVE_INFINITY,
						units.length === 0 ? null : new Set(withdrawableUnits.map((unit) => unit.pass_id)),
					);
					if (movedUnits.length > 0) {
						await this.holdingService.endPurchaseUnits(
							purchase,
							'withdrawal',
							'revoked',
							Number.POSITIVE_INFINITY,
							new Set(movedUnits.map((unit) => unit.pass_id)),
						);
					}
					await this.certificates.syncCancellations(purchase);
				}
				const remaining = (await this.repository.listUnitsByPurchase(purchase.checkout_session_id)).filter(
					(unit) => unit.status === 'held',
				);
				const receipt: OperatorPassWithdrawalReceipt = {
					purchaseId: purchase.checkout_session_id,
					passId: null,
					recipientUserId: purchaser.id,
					name,
					receivedAt,
					recordedAt: operatorPassNow(),
					amountMinor: returned?.amountMinor ?? amountMinor,
					refund: returned ? startedWithdrawalRefund(returned) : 'already',
					delivered: purchase.delivered_at != null || units.length > 0,
					passCount: Math.max(1, passCount),
					lastPass: (await this.holdingService.queries.listHeldUnits(purchaser.id)).length === 0,
					visionary: keepsOperatorsThroughVisionary(purchaser),
					refundId: returned?.refundId ?? null,
				};
				const withdrawn = await this.repository.updatePurchase(purchase, {
					...(remaining.length === 0
						? {status: Db.set('withdrawn'), refunded_at: Db.set(purchase.refunded_at ?? new Date())}
						: {}),
					...(remaining.length === 0 && purchase.delivered_at == null
						? {certificate_names: Db.clear(), wall_name: Db.clear()}
						: {}),
					withdrawn_at: Db.set(receivedAt),
					withdrawal_name: Db.set(name),
					withdrawal_refund_id: Db.set(returned?.refundId ?? null),
					withdrawal_channel: Db.set(channel),
					withdrawal_receipts_owed: Db.set(withOwedWithdrawalReceipt(purchase, receipt)),
				});
				await this.reviewBusinessListing(purchase);
				Logger.info(
					{sessionId: purchase.checkout_session_id, purchaserId: purchaser.id, channel},
					'Operator Pass purchase withdrawn',
				);
				await this.queueWithdrawalReceipt(receipt);
				return withdrawn;
			},
			purchaseBusyError,
		);
	}

	async withdrawUnit({holder, passId, name, receivedAt}: OperatorPassUnitWithdrawParams): Promise<OperatorPassUnitRow> {
		const unit = await this.repository.findUnit(passId);
		if (!unit || unit.holder_id !== holder.id || !unit.purchase_id) {
			throw new UnknownOperatorPassUnitError();
		}
		const purchaseId = unit.purchase_id;
		return this.withPurchaseLock(
			purchaseId,
			async () => {
				const found = await this.repository.findPurchase(purchaseId);
				const purchase = found ? await this.applyRefundsMadeElsewhere(found) : null;
				const current = await this.repository.findUnit(passId);
				if (!purchase || !current || purchase.purchaser_id !== holder.id) {
					throw new UnknownOperatorPassUnitError();
				}
				if (!isOperatorPassSinglePassWithdrawable(purchase, current, receivedAt) || !purchase.payment_intent_id) {
					throw new OperatorPassWithdrawalUnavailableError();
				}
				const amountMinor = Math.round(purchase.amount_minor / Math.max(1, purchase.quantity ?? 1));
				const returned = await this.returnPassPayment(purchase, amountMinor, current.pass_id, null);
				if (returned.refundId) {
					await this.creditRefundsMadeHere(purchase);
				}
				await this.holdingService.withdrawUnit(current, purchase);
				await this.certificates.syncCancellations(purchase);
				const remaining = (await this.repository.listUnitsByPurchase(purchaseId)).filter(
					(unit) => unit.status === 'held',
				);
				const receipt: OperatorPassWithdrawalReceipt = {
					purchaseId,
					passId,
					recipientUserId: holder.id,
					name,
					receivedAt,
					recordedAt: operatorPassNow(),
					amountMinor: returned.amountMinor,
					refund: startedWithdrawalRefund(returned),
					delivered: true,
					passCount: 1,
					lastPass: (await this.holdingService.queries.listHeldUnits(holder.id)).length === 0,
					visionary: keepsOperatorsThroughVisionary(holder),
					refundId: returned.refundId,
				};
				await this.repository.updatePurchase(purchase, {
					...(remaining.length === 0
						? {status: Db.set('withdrawn'), refunded_at: Db.set(purchase.refunded_at ?? new Date())}
						: {}),
					withdrawn_at: Db.set(receivedAt),
					withdrawal_name: Db.set(name),
					withdrawal_refund_id: Db.set(returned.refundId),
					withdrawal_channel: Db.set('button'),
					withdrawal_receipts_owed: Db.set(withOwedWithdrawalReceipt(purchase, receipt)),
				});
				await this.reviewBusinessListing(purchase);
				Logger.info({sessionId: purchaseId, passId, holderId: holder.id}, 'Operator Pass withdrawn');
				await this.queueWithdrawalReceipt(receipt);
				return (await this.repository.findUnit(passId)) ?? current;
			},
			purchaseBusyError,
		);
	}

	private async returnPassPayment(
		purchase: OperatorPassPurchaseRow,
		amountMinor: number,
		idempotencyScope: string,
		acceptance: Omit<AcceptedWithdrawalDispute, 'amountMinor'> | null,
	): Promise<ReturnedPassPayment> {
		try {
			const refund = await this.refundPassAmount(
				purchase,
				amountMinor,
				`${WITHDRAWAL_REFUND_PREFIX}${idempotencyScope}`,
			);
			return {refundId: refund.id, viaDispute: false, amountMinor};
		} catch (error) {
			if (
				!acceptance ||
				purchase.status !== 'disputed' ||
				!purchase.dispute_id ||
				!isChargeDisputedError(error) ||
				!this.stripe
			) {
				throw error;
			}
			const current = await this.stripe.disputes.retrieve(purchase.dispute_id);
			const dispute = current.status === 'lost' ? current : await this.stripe.disputes.close(purchase.dispute_id);
			await this.cacheService.set(
				withdrawalDisputeAcceptedKey(purchase.dispute_id),
				{
					receivedAt: acceptance.receivedAt.toISOString(),
					passCount: acceptance.passCount,
					amountMinor: dispute.amount,
				},
				APPLIED_TTL_SECONDS,
			);
			Logger.info(
				{sessionId: purchase.checkout_session_id, disputeId: purchase.dispute_id, closedBefore: dispute === current},
				'Accepted the payment dispute so the withdrawal returns the money',
			);
			return {refundId: null, viaDispute: true, amountMinor: dispute.amount};
		}
	}

	private async findAcceptedWithdrawalDispute(
		purchase: OperatorPassPurchaseRow,
	): Promise<AcceptedWithdrawalDispute | null> {
		if (!purchase.dispute_id || purchase.withdrawn_at != null) {
			return null;
		}
		const stored = await this.cacheService.get<Record<string, unknown>>(
			withdrawalDisputeAcceptedKey(purchase.dispute_id),
		);
		if (
			!stored ||
			typeof stored.receivedAt !== 'string' ||
			typeof stored.passCount !== 'number' ||
			typeof stored.amountMinor !== 'number'
		) {
			return null;
		}
		return {receivedAt: new Date(stored.receivedAt), passCount: stored.passCount, amountMinor: stored.amountMinor};
	}

	private async listUnitsPassedOnAfterReceipt(
		purchase: OperatorPassPurchaseRow,
		units: ReadonlyArray<OperatorPassUnitRow>,
		receivedAt: Date,
		channel: OperatorPassWithdrawalChannel,
		refundedBefore: boolean,
	): Promise<Array<OperatorPassUnitRow>> {
		if (
			channel === 'button' ||
			refundedBefore ||
			purchase.withdrawal_days == null ||
			!['completed', 'disputed'].includes(purchase.status) ||
			!isOperatorPassWithinWithdrawalDeadline(purchase, receivedAt)
		) {
			return [];
		}
		const moved: Array<OperatorPassUnitRow> = [];
		for (const unit of units) {
			if (unit.status !== 'held' || unit.source !== 'transfer' || unit.holder_id === purchase.purchaser_id) {
				continue;
			}
			const events = await this.repository.listUnitEvents(unit.pass_id);
			const passedOn = events.find(
				(event) => event.kind === 'transferred' && event.sender_id === purchase.purchaser_id,
			);
			if (passedOn && passedOn.occurred_at.getTime() > receivedAt.getTime()) {
				moved.push(unit);
			}
		}
		return moved;
	}

	private async queueWithdrawalReceipt(receipt: OperatorPassWithdrawalReceipt): Promise<void> {
		try {
			await getWorkerService().addJob(
				OPERATOR_PASS_WITHDRAWAL_RECEIPT_TASK,
				encodeOperatorPassWithdrawalReceipt(receipt),
				{
					jobKey: withdrawalReceiptKey(receipt),
				},
			);
		} catch (error) {
			Logger.error(
				{error, sessionId: receipt.purchaseId},
				'Could not queue the Operator Pass withdrawal receipt, sending it directly',
			);
			try {
				await this.sendWithdrawalReceipt(receipt);
			} catch (sendError) {
				Logger.error(
					{error: sendError, sessionId: receipt.purchaseId},
					'Failed to send the Operator Pass withdrawal receipt',
				);
			}
		}
	}

	async requeueOwedWithdrawalReceipts(purchase: OperatorPassPurchaseRow, now: Date): Promise<void> {
		for (const [key, encoded] of purchase.withdrawal_receipts_owed ?? []) {
			if ((await this.cacheService.get<boolean>(withdrawalReceiptSentKey(key))) === true) {
				await this.settleWithdrawalReceipt(purchase.checkout_session_id, key);
				continue;
			}
			const receipt = decodeOperatorPassWithdrawalReceipt(JSON.parse(encoded) as Record<string, unknown>);
			const ageMs = now.getTime() - receipt.recordedAt.getTime();
			if (ageMs < WITHDRAWAL_RECEIPT_REQUEUE_AFTER_MS) {
				continue;
			}
			if (ageMs >= WITHDRAWAL_RECEIPT_RETRY_MS) {
				Logger.error(
					{sessionId: purchase.checkout_session_id, receipt: key},
					'Operator Pass withdrawal receipt was never sent, giving up a week after the withdrawal was recorded',
				);
				await alertWithdrawalReceiptNotSent(
					purchase,
					receipt,
					`withdrawal-receipt-given-up:${key}`,
					'An Operator Pass withdrawal receipt was never sent, and the code stopped retrying it a week after the withdrawal. Send the buyer the withdrawal confirmation by hand.',
				);
				await this.settleWithdrawalReceipt(purchase.checkout_session_id, key);
				continue;
			}
			if (ageMs >= WITHDRAWAL_RECEIPT_ALERT_MS) {
				Logger.error(
					{sessionId: purchase.checkout_session_id, receipt: key},
					'Operator Pass withdrawal receipt is still not sent a day after the withdrawal was recorded',
				);
				await alertWithdrawalReceiptNotSent(
					purchase,
					receipt,
					`withdrawal-receipt-late:${key}`,
					'An Operator Pass withdrawal receipt is still not sent a day after the withdrawal. The code keeps retrying for a week, but the buyer is waiting for the confirmation.',
				);
			}
			await this.queueWithdrawalReceipt(receipt);
		}
	}

	private async settleWithdrawalReceipt(purchaseId: string, key: string): Promise<void> {
		try {
			await this.withPurchaseLock(purchaseId, async () => {
				const current = await this.repository.findPurchase(purchaseId);
				if (!current?.withdrawal_receipts_owed?.has(key)) {
					return;
				}
				const owed = new Map(current.withdrawal_receipts_owed);
				owed.delete(key);
				await this.repository.updatePurchase(current, {
					withdrawal_receipts_owed: owed.size > 0 ? Db.set(owed) : Db.clear(),
				});
			});
		} catch (error) {
			Logger.warn(
				{error, sessionId: purchaseId, receipt: key},
				'Could not mark the Operator Pass withdrawal receipt as sent, the fulfilment sweep will',
			);
		}
	}

	async sendWithdrawalReceipt(receipt: OperatorPassWithdrawalReceipt, waitForRefund = false): Promise<void> {
		const key = withdrawalReceiptKey(receipt);
		if ((await this.cacheService.get<boolean>(withdrawalReceiptSentKey(key))) === true) {
			await this.settleWithdrawalReceipt(receipt.purchaseId, key);
			return;
		}
		const purchase = await this.repository.findPurchase(receipt.purchaseId);
		if (!purchase) {
			Logger.error({sessionId: receipt.purchaseId}, 'No purchase for the Operator Pass withdrawal receipt');
			return;
		}
		const recipient = await this.userRepository.findUnique(receipt.recipientUserId);
		const email = recipient?.email ?? (await this.findCheckoutEmail(purchase.checkout_session_id));
		if (!email) {
			Logger.error({sessionId: purchase.checkout_session_id}, 'No address for the Operator Pass withdrawal receipt');
			return;
		}
		if (await this.emailService.isEmailBounced(email)) {
			Logger.warn(
				{sessionId: purchase.checkout_session_id, receipt: key},
				'Operator Pass withdrawal receipt address has bounced; skipping the receipt',
			);
			await this.cacheService.set(withdrawalReceiptSentKey(key), true, APPLIED_TTL_SECONDS);
			await this.settleWithdrawalReceipt(purchase.checkout_session_id, key);
			return;
		}
		let creditNote: BillingDocumentAttachment | null;
		try {
			creditNote = await this.loadWithdrawalCreditNote(purchase, receipt.refundId, waitForRefund);
		} catch (error) {
			if (waitForRefund) {
				throw error;
			}
			Logger.warn(
				{error, sessionId: purchase.checkout_session_id, refundId: receipt.refundId},
				'Sending the Operator Pass withdrawal receipt without its credit note, which stays owed',
			);
			creditNote = null;
		}
		if (receipt.refundId && this.documents && this.stripe) {
			const owedKey = withdrawalCreditNoteOwedKey(receipt.refundId);
			if (creditNote) {
				await this.cacheService.delete(owedKey);
			} else {
				await this.cacheService.set(owedKey, encodeOperatorPassWithdrawalReceipt(receipt), APPLIED_TTL_SECONDS);
				try {
					await enqueueBillingEmail(
						getWorkerService(),
						{kind: 'refund', id: receipt.refundId},
						{keySuffix: 'credit-note-owed', runAt: new Date(Date.now() + OWED_CREDIT_NOTE_DELAY_MS)},
					);
				} catch (error) {
					Logger.error(
						{error, sessionId: purchase.checkout_session_id, refundId: receipt.refundId},
						'Could not queue the owed credit note of an Operator Pass withdrawal',
					);
				}
			}
		}
		const sent = await this.emailService.send(
			'operator_pass_withdrawal_receipt',
			email,
			{
				name: receipt.name,
				receivedAt: receipt.receivedAt,
				invoiceNumber: purchase.invoice_number ?? null,
				orderDate: purchase.purchased_at ?? purchase.created_at,
				amountMinor: receipt.amountMinor,
				currency: purchase.currency,
				delivered: receipt.delivered,
				passCount: receipt.passCount,
				lastPass: receipt.lastPass,
				visionary: receipt.visionary,
				refund: receipt.refund,
				creditNote,
				timeZone: recipient?.timezone ?? null,
			},
			recipient?.locale ?? null,
		);
		if (!sent) {
			throw new Error('Failed to send the Operator Pass withdrawal receipt');
		}
		await this.cacheService.set(withdrawalReceiptSentKey(key), true, APPLIED_TTL_SECONDS);
		await this.settleWithdrawalReceipt(purchase.checkout_session_id, key);
	}

	private async loadWithdrawalCreditNote(
		purchase: OperatorPassPurchaseRow,
		refundId: string | null,
		waitForRefund: boolean,
	): Promise<BillingDocumentAttachment | null> {
		if (!refundId || !this.stripe || !this.documents) {
			return null;
		}
		const refund = await this.findWithdrawalRefundInEffect(this.stripe, refundId);
		if (
			waitForRefund &&
			(refund.status === 'pending' || refund.status === 'requires_action' || refund.status === 'failed')
		) {
			throw new Error('The Operator Pass withdrawal refund has not settled yet, so its credit note does not exist yet');
		}
		return this.loadRefundCreditNote(purchase, refund);
	}

	private async findWithdrawalRefundInEffect(stripe: Stripe, refundId: string): Promise<Stripe.Refund> {
		const refund = await stripe.refunds.retrieve(refundId);
		const paymentIntentId = extractId(refund.payment_intent ?? null);
		if (refund.status !== 'failed' || !paymentIntentId) {
			return refund;
		}
		const refunds = await stripe.refunds.list({payment_intent: paymentIntentId, limit: 100});
		return refunds.data.find((candidate) => candidate.metadata?.retry_of === refund.id) ?? refund;
	}

	private async countPassesCoveredByRefund(purchase: OperatorPassPurchaseRow, refund: Stripe.Refund): Promise<number> {
		const quantity = Math.max(1, purchase.quantity ?? 1);
		const unitPrice = Math.round(purchase.amount_minor / quantity);
		const covered = (refunded: number) => {
			if (refunded >= purchase.amount_minor) return quantity;
			return unitPrice > 0 ? Math.min(quantity, Math.floor(refunded / unitPrice)) : 0;
		};
		if (!this.stripe || !purchase.payment_intent_id) {
			return covered(refund.amount);
		}
		const refunds = await this.stripe.refunds.list({payment_intent: purchase.payment_intent_id, limit: 100});
		const counted = refunds.data
			.filter(
				(candidate) => candidate.status === 'succeeded' || candidate.status === 'pending' || candidate.id === refund.id,
			)
			.sort((left, right) => left.created - right.created || left.id.localeCompare(right.id));
		let before = 0;
		for (const candidate of counted) {
			if (candidate.id === refund.id) {
				return Math.max(0, covered(before + refund.amount) - covered(before));
			}
			before += candidate.amount;
		}
		return Math.max(0, covered(before + refund.amount) - covered(before));
	}

	async prepareRefundReceipt(
		found: OperatorPassPurchaseRow,
		refund: Stripe.Refund,
	): Promise<OperatorPassRefundReceipt | null> {
		const applied = await this.withCurrentPurchase(found, async (current) => {
			if (await this.refundSendsItsOwnEmail(current, refund)) {
				if (this.stripe && this.documents) {
					await creditOperatorPassRefunds(this.stripe, this.documents, current);
				}
				return {purchase: current, units: null};
			}
			const purchase = await this.applyRefundsMadeElsewhere(current);
			return {purchase, units: await this.repository.listUnitsByPurchase(purchase.checkout_session_id)};
		});
		if (!applied) {
			return null;
		}
		const {purchase, units} = applied;
		if (!units) {
			return this.prepareOwedWithdrawalCreditNote(purchase, refund);
		}
		const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
		if (!purchaser?.email || purchaser.emailBounced) {
			Logger.warn({refundId: refund.id}, 'No deliverable purchaser email for an Operator Pass refund');
			return null;
		}
		const quantity = Math.max(1, purchase.quantity ?? 1);
		const endedByRefund = units.filter((unit) => unit.end_reason === 'refund').length;
		const endedThisRefund = Math.min(endedByRefund, await this.countPassesCoveredByRefund(purchase, refund));
		const delivered = purchase.delivered_at != null;
		return {
			recipient: {email: purchaser.email, locale: purchaser.locale, timeZone: purchaser.timezone},
			params: {
				invoiceNumber: purchase.invoice_number ?? null,
				orderDate: purchase.purchased_at ?? purchase.created_at,
				refundedAt: new Date(refund.created * 1000),
				amountMinor: refund.amount,
				currency: refund.currency,
				delivered,
				passCount: delivered ? endedThisRefund : quantity,
				lastPass: (await this.holdingService.queries.listHeldUnits(purchaser.id)).length === 0,
				visionary: keepsOperatorsThroughVisionary(purchaser),
				creditNote: await this.loadRefundCreditNote(purchase, refund),
				timeZone: purchaser.timezone,
			},
		};
	}

	private async prepareOwedWithdrawalCreditNote(
		purchase: OperatorPassPurchaseRow,
		refund: Stripe.Refund,
	): Promise<OperatorPassRefundReceipt | null> {
		const owed = await this.cacheService.get<Record<string, unknown>>(
			withdrawalCreditNoteOwedKey(refund.metadata?.retry_of ?? refund.id),
		);
		if (!owed) {
			return null;
		}
		const withdrawal = decodeOperatorPassWithdrawalReceipt(owed);
		const creditNote = await this.loadRefundCreditNote(purchase, refund);
		if (!creditNote) {
			Logger.error(
				{sessionId: purchase.checkout_session_id, refundId: refund.id},
				'The settled Operator Pass withdrawal refund still has no credit note to send',
			);
			return null;
		}
		const recipient = await this.userRepository.findUnique(withdrawal.recipientUserId);
		if (!recipient?.email || recipient.emailBounced) {
			Logger.warn({refundId: refund.id}, 'No deliverable email for the credit note of an Operator Pass withdrawal');
			return null;
		}
		return {
			recipient: {email: recipient.email, locale: recipient.locale, timeZone: recipient.timezone},
			params: {
				invoiceNumber: purchase.invoice_number ?? null,
				orderDate: purchase.purchased_at ?? purchase.created_at,
				refundedAt: new Date(refund.created * 1000),
				amountMinor: refund.amount,
				currency: refund.currency,
				delivered: withdrawal.delivered,
				passCount: withdrawal.passCount,
				lastPass: withdrawal.lastPass,
				visionary: withdrawal.visionary,
				creditNote,
				timeZone: recipient.timezone,
			},
		};
	}

	private async refundSendsItsOwnEmail(purchase: OperatorPassPurchaseRow, refund: Stripe.Refund): Promise<boolean> {
		if (refund.metadata?.checkout_session_id !== purchase.checkout_session_id) {
			return false;
		}
		const refusal = refusalOfRefund(refund, purchase);
		if (refusal) {
			if (!EMAILED_REFUSALS.has(refusal)) {
				return false;
			}
			if (purchase.status === 'pending') {
				return true;
			}
			return (
				purchase.status === 'duplicate_refunded' &&
				((!purchase.invoice_document_id && !purchase.invoice_id) ||
					(await this.cacheService.get<boolean>(refusalCreditNoteKey(purchase.checkout_session_id))) === true)
			);
		}
		const reason = refund.metadata?.rejection_reason ?? '';
		if (!reason.startsWith(WITHDRAWAL_REFUND_PREFIX)) {
			return false;
		}
		const scope = reason.slice(WITHDRAWAL_REFUND_PREFIX.length);
		if (scope.startsWith('all:')) {
			const key = `withdrawal-receipt:${purchase.checkout_session_id}:${scope}`;
			return (
				purchase.withdrawal_refund_id === (refund.metadata?.retry_of ?? refund.id) ||
				purchase.withdrawal_receipts_owed?.has(key) === true ||
				(await this.cacheService.get<boolean>(withdrawalReceiptSentKey(key))) === true
			);
		}
		const unit = await this.repository.findUnit(scope);
		return unit?.purchase_id === purchase.checkout_session_id && unit.status === 'withdrawn';
	}

	private async loadRefundCreditNote(
		purchase: OperatorPassPurchaseRow,
		refund: Stripe.Refund,
	): Promise<BillingDocumentAttachment | null> {
		if (!this.documents) {
			return null;
		}
		const document = await this.documents.issueForRefund(refund, operatorPassDocumentContext(purchase));
		return document ? billingDocumentAttachment(document) : null;
	}

	private async findCheckoutEmail(sessionId: string): Promise<string | null> {
		if (!this.stripe) {
			return null;
		}
		const session = await this.stripe.checkout.sessions.retrieve(sessionId);
		return session.customer_details?.email ?? session.customer_email ?? null;
	}

	async refundPassAmount(
		purchase: OperatorPassPurchaseRow,
		amountMinor: number,
		reason: string,
	): Promise<Stripe.Refund> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for an Operator Pass refund');
		}
		if (!purchase.payment_intent_id) {
			throw new StripeError('Operator Pass purchase has no payment intent to refund');
		}
		const refund = await this.stripe.refunds.create(
			{
				payment_intent: purchase.payment_intent_id,
				amount: amountMinor,
				reason: 'requested_by_customer',
				metadata: {checkout_session_id: purchase.checkout_session_id, rejection_reason: reason},
			},
			{idempotencyKey: `operator-pass-${reason}:${purchase.checkout_session_id}`},
		);
		try {
			await getBillingRepository().refunds.upsertFromStripe(refund);
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, refundId: refund.id},
				'Mirror upsert failed for an Operator Pass refund; reconciler will heal',
			);
		}
		return refund;
	}

	async handleRefundFailed(refund: Stripe.Refund, eventType: string): Promise<void> {
		const announcesEnd =
			(refund.status === 'failed' && eventType === 'refund.failed') ||
			(refund.status === 'canceled' && eventType === 'refund.updated');
		if (!announcesEnd) return;
		const sessionId = refund.metadata?.checkout_session_id;
		if (!sessionId || !this.stripe) return;
		const purchase = await this.repository.findPurchase(sessionId);
		if (!purchase) return;
		const stripe = this.stripe;
		const isRetry = refund.metadata?.retry_of != null;
		const canceled = refund.status === 'canceled';
		let message = 'An Operator Pass refund failed; retrying once';
		if (canceled) {
			message = 'An Operator Pass refund was canceled; not retrying, support has to decide whether to send the money';
		} else if (isRetry) {
			message =
				'The retry of a failed Operator Pass refund also failed; the passes stay ended and support has to send the money another way';
		}
		Logger.error(
			{
				sessionId,
				refundId: refund.id,
				amount: refund.amount,
				currency: refund.currency,
				status: refund.status,
				failureReason: refund.failure_reason ?? null,
				retryOf: refund.metadata?.retry_of ?? null,
			},
			message,
		);
		if (canceled) {
			await alertOperatorPassRefundNotSent(
				purchase,
				refund,
				'An Operator Pass refund was canceled at Stripe and was not retried. Decide whether to send the money another way.',
			);
		} else if (isRetry) {
			await alertOperatorPassRefundNotSent(
				purchase,
				refund,
				'The retry of a failed Operator Pass refund also failed. The passes stay ended, so send the money another way and tell the buyer.',
			);
		}
		await this.withCurrentPurchase(purchase, async (current) => {
			if (canceled || isRetry || !current.payment_intent_id) return;
			const existing = await stripe.refunds.list({payment_intent: current.payment_intent_id, limit: 100});
			const earlierRetry = existing.data.find((candidate) => candidate.metadata?.retry_of === refund.id);
			if (earlierRetry) {
				Logger.warn(
					{sessionId, failedRefundId: refund.id, retryRefundId: earlierRetry.id},
					'A failed Operator Pass refund was already retried; not retrying again',
				);
				return;
			}
			let retry: Stripe.Refund;
			try {
				retry = await stripe.refunds.create(
					{
						payment_intent: current.payment_intent_id,
						amount: refund.amount,
						...(refund.reason === 'requested_by_customer' ? {reason: refund.reason} : {}),
						metadata: {...refund.metadata, checkout_session_id: sessionId, retry_of: refund.id},
					},
					{idempotencyKey: `operator-pass-refund-retry:${refund.id}`},
				);
			} catch (error) {
				if (!(error instanceof Stripe.errors.StripeInvalidRequestError)) throw error;
				Logger.error(
					{sessionId, refundId: refund.id, amount: refund.amount, currency: refund.currency, error: error.message},
					'Stripe refused to retry the failed Operator Pass refund; support has to send the money another way',
				);
				await alertOperatorPassRefundNotSent(
					current,
					refund,
					'An Operator Pass refund failed and Stripe refused to retry it. The passes stay ended, so send the money another way and tell the buyer.',
					error,
				);
				return;
			}
			try {
				await getBillingRepository().refunds.upsertFromStripe(retry);
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, refundId: retry.id},
					'Mirror upsert failed for an Operator Pass refund retry; reconciler will heal',
				);
			}
			Logger.warn(
				{sessionId, failedRefundId: refund.id, retryRefundId: retry.id, amount: retry.amount},
				'Retried a failed Operator Pass refund',
			);
			await this.creditRefundsMadeHere(current);
		});
	}

	private async fulfilCheckoutSession(session: Stripe.Checkout.Session, paidAt: Date | null): Promise<void> {
		const appliedKey = getAppliedKey(session.id);
		if (await this.cacheService.get<boolean>(appliedKey)) {
			Logger.debug({sessionId: session.id}, 'Operator Pass checkout already fulfilled');
			return;
		}
		await this.withPurchaseLock(session.id, async (lease) => {
			const purchase = (await this.repository.findPurchase(session.id)) ?? (await this.recoverPurchase(session));
			if (!purchase) {
				Logger.error({sessionId: session.id}, 'No Operator Pass purchase found for checkout session, skipping event');
				return;
			}
			const awaitingWelcome = purchase.status === 'completed' && !isOperatorPassWelcomeSettled(purchase);
			if (purchase.status !== 'pending' && !awaitingWelcome) {
				if (!isWelcomeOutstanding(purchase)) {
					await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
				}
				return;
			}
			const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
			if (!purchaser) {
				Logger.error({sessionId: session.id, userId: purchase.purchaser_id}, 'Operator Pass purchaser not found');
				throw new StripeError('Operator Pass purchaser not found');
			}
			if (awaitingWelcome) {
				await this.sendWelcomeOnce(purchase, purchaser);
				await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
				return;
			}
			await this.fulfilPendingPurchase(session, purchase, purchaser, paidAt, lease);
		});
	}

	private async fulfilPendingPurchase(
		session: Stripe.Checkout.Session,
		pending: OperatorPassPurchaseRow,
		purchaser: User,
		paidAt: Date | null,
		lease: OperatorPassLockLease,
	): Promise<void> {
		const appliedKey = getAppliedKey(session.id);
		let purchase = pending;
		const references = this.resolvePaymentReferences(session);
		purchase = await this.repository.updatePurchase(purchase, {
			payment_intent_id: Db.set(references.paymentIntentId ?? purchase.payment_intent_id),
		});
		const charge = await this.retrieveLatestCharge(purchase.payment_intent_id ?? null);
		purchase = await this.recordPayment(purchase, session, charge, paidAt);
		if (charge && charge.amount_refunded > 0 && (charge.refunded || charge.amount_refunded >= purchase.amount_minor)) {
			const recordedRefusal = await this.findRefusalRefund(purchase);
			if (recordedRefusal) {
				await this.finishRefusal(purchase, purchaser, session, recordedRefusal);
			} else {
				await this.handleChargeRefundedLocked(purchase, charge);
				await this.queueRefundReceipts(purchase);
			}
			await this.creditRefundsMadeHere(purchase);
			await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
			return;
		}
		const dispute = charge?.disputed ? await this.retrieveChargeDispute(charge) : null;
		if (dispute?.status === 'lost') {
			await this.handleDisputeLostLocked(purchase, dispute);
			await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
			return;
		}
		let refusal: OperatorPassRefusal | null = null;
		if (await this.isRefusedPurchase(purchaser)) {
			refusal = 'operator_pass_removed';
		} else if (purchase.delivered_at == null && !purchase.certificate_names?.length) {
			refusal = 'operator_pass_names_missing';
		} else if (
			purchase.delivered_at == null &&
			purchase.confirmation_sent_at == null &&
			(await this.isConfirmationUndeliverable(purchaser, session))
		) {
			refusal = 'operator_pass_email_undeliverable';
		}
		if (refusal) {
			const recordedRefusal = await this.findRefusalRefund(purchase);
			if (!recordedRefusal) {
				await this.refundRefusedPurchase(purchase, refusal);
			}
			await this.finishRefusal(purchase, purchaser, session, recordedRefusal ?? refusal);
			await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
			return;
		}
		purchase = await this.recordPurchaseTerms(purchase, session);
		if (purchase.delivered_at == null && purchase.confirmation_sent_at == null) {
			purchase = await this.extendCountryWithdrawalDeadline(purchase, operatorPassNow());
		}
		await this.sendConfirmationOnce(purchase, purchaser, session);
		purchase = await this.repository.updatePurchase(purchase, {
			confirmation_sent_at: Db.set(purchase.confirmation_sent_at ?? new Date()),
		});
		await lease.assertHeld();
		await this.certificates.issueForPurchase(purchase);
		const deliveredAt = purchase.delivered_at ?? operatorPassNow();
		purchase = await this.repository.updatePurchase(purchase, {delivered_at: Db.set(deliveredAt)});
		purchase = await this.extendCountryWithdrawalDeadline(purchase, deliveredAt);
		purchase = await this.recordDeliveryOnPayment(purchase);
		await lease.assertHeld();
		purchase = await this.withLock(
			operatorPassHolderLockKey(purchase.purchaser_id),
			async () => {
				const recorded = await this.recordHolderBeforeDelivery(purchase);
				const delivery: Parameters<OperatorPassHoldingService['deliverPurchase']>[0] = {
					purchase: recorded,
					buyer: purchaser,
					deliveredAt,
					choice: null,
				};
				if (recorded.wall_opt_out != null) {
					delivery.choice = {
						name: recorded.wall_opt_out ? null : (recorded.wall_name ?? null),
						optedOut: recorded.wall_opt_out || !recorded.wall_name,
					};
				}
				await this.holdingService.deliverPurchase(delivery);
				return recorded;
			},
			() => new StripeError('Another Operator Pass delivery to this account is underway; retrying'),
		);
		const bonusAt = purchase.perks_granted_at ?? operatorPassNow();
		await lease.assertHeld();
		purchase = await this.repository.updatePurchase(purchase, {
			status: Db.set('completed'),
			completed_at: Db.set(purchase.completed_at ?? new Date()),
			perks_granted_at: Db.set(bonusAt),
			wall_name: Db.clear(),
		});
		if (charge && charge.amount_refunded > 0) {
			await this.handleChargeRefundedLocked(purchase, charge);
			await this.creditRefundsMadeHere(purchase);
		}
		if (dispute) {
			const current = (await this.repository.findPurchase(purchase.checkout_session_id)) ?? purchase;
			if (dispute.status === 'won' || dispute.status === 'warning_closed' || dispute.status === 'prevented') {
				await this.handleDisputeReleasedLocked(current, dispute);
			} else {
				await this.handleDisputeCreatedLocked(current, dispute);
				Logger.warn(
					{sessionId: session.id, purchaserId: purchase.purchaser_id, disputeId: dispute.id},
					'Operator Pass delivered after a dispute was already open, so it is paused; check that the dispute review exists',
				);
			}
		}
		purchase = (await this.repository.findPurchase(purchase.checkout_session_id)) ?? purchase;
		if (purchase.status === 'completed') {
			await this.sendWelcomeOnce(purchase, purchaser);
			purchase = (await this.repository.findPurchase(purchase.checkout_session_id)) ?? purchase;
		}
		if (!isWelcomeOutstanding(purchase)) {
			await this.cacheService.set(appliedKey, true, APPLIED_TTL_SECONDS);
		}
		Logger.info(
			{sessionId: session.id, purchaserId: purchase.purchaser_id, quantity: purchase.quantity ?? 1},
			'Operator Pass checkout fulfilled',
		);
	}

	private async recordDeliveryOnPayment(purchase: OperatorPassPurchaseRow): Promise<OperatorPassPurchaseRow> {
		try {
			return await syncOperatorPassDeliveryRecord(this.stripe, this.repository, purchase);
		} catch (error) {
			Logger.warn(
				{error, sessionId: purchase.checkout_session_id},
				'Could not record the Operator Pass certificate delivery time on the payment yet, retrying in the background',
			);
		}
		try {
			await getWorkerService().addJob(
				OPERATOR_PASS_DELIVERY_RECORD_TASK,
				{purchase_id: purchase.checkout_session_id},
				{jobKey: `operator-pass-delivery-record:${purchase.checkout_session_id}`},
			);
		} catch (error) {
			Logger.error(
				{error, sessionId: purchase.checkout_session_id},
				'Could not queue the Operator Pass certificate delivery time for the payment, the fulfilment sweep will record it',
			);
		}
		return purchase;
	}

	private async isConfirmationUndeliverable(purchaser: User, session: Stripe.Checkout.Session): Promise<boolean> {
		const email = confirmationAddress(purchaser, session);
		return email != null && (await this.emailService.isEmailBounced(email));
	}

	private async isRefusedPurchase(purchaser: User): Promise<boolean> {
		const {removal} = await loadOperatorPassEntitlementState(this.repository, purchaser.id);
		return isOperatorPassPermanentlyRemoved(removal);
	}

	private async extendCountryWithdrawalDeadline(
		purchase: OperatorPassPurchaseRow,
		startsAt: Date,
	): Promise<OperatorPassPurchaseRow> {
		if (purchase.withdrawal_days == null) {
			return purchase;
		}
		const endsAt = getOperatorPassWithdrawalEndsAt(startsAt, purchase.withdrawal_days);
		if (purchase.withdrawal_ends_at && endsAt.getTime() <= purchase.withdrawal_ends_at.getTime()) {
			return purchase;
		}
		return this.repository.updatePurchase(purchase, {withdrawal_ends_at: Db.set(endsAt)});
	}

	private async recordPayment(
		purchase: OperatorPassPurchaseRow,
		session: Stripe.Checkout.Session,
		charge: Stripe.Charge | null,
		paidAt: Date | null,
	): Promise<OperatorPassPurchaseRow> {
		if (purchase.purchased_at && purchase.invoice_number && purchase.charge_id && purchase.business_buyer != null) {
			return purchase;
		}
		const legacyInvoiceId = extractId(session.invoice);
		const legacyInvoice =
			legacyInvoiceId && this.stripe && !purchase.invoice_number
				? await this.stripe.invoices.retrieve(legacyInvoiceId)
				: null;
		const invoice =
			purchase.invoice_number || legacyInvoiceId || purchase.preview_label != null
				? null
				: await this.issuePurchaseInvoice(purchase, session);
		return this.repository.updatePurchase(purchase, {
			purchased_at: Db.set(purchase.purchased_at ?? paidAt ?? resolveOperatorPassPaidAt(charge, operatorPassNow())),
			invoice_id: Db.set(purchase.invoice_id ?? legacyInvoiceId),
			invoice_number: Db.set(purchase.invoice_number ?? invoice?.display_number ?? legacyInvoice?.number ?? null),
			invoice_document_id: Db.set(purchase.invoice_document_id ?? invoice?.document_id ?? null),
			charge_id: Db.set(purchase.charge_id ?? charge?.id ?? null),
			business_buyer: Db.set(purchase.business_buyer ?? (session.customer_details?.tax_ids?.length ?? 0) > 0),
		});
	}

	private async recordHolderBeforeDelivery(purchase: OperatorPassPurchaseRow): Promise<OperatorPassPurchaseRow> {
		if (purchase.holder_had_pass != null) {
			return purchase;
		}
		const held = await this.holdingService.queries.listHeldUnits(purchase.purchaser_id);
		return this.repository.updatePurchase(purchase, {
			holder_had_pass: Db.set(held.some((unit) => unit.purchase_id !== purchase.checkout_session_id)),
		});
	}

	private async queueRefundReceipts(purchase: OperatorPassPurchaseRow): Promise<void> {
		if (!this.stripe || !purchase.payment_intent_id) {
			return;
		}
		const refunds = await this.stripe.refunds.list({payment_intent: purchase.payment_intent_id, limit: 100});
		for (const refund of refunds.data) {
			if (refund.status !== 'succeeded') continue;
			try {
				await enqueueBillingEmail(
					getWorkerService(),
					{kind: 'refund', id: refund.id},
					{keySuffix: 'fulfilment', runAt: new Date()},
				);
			} catch (error) {
				Logger.error(
					{error, sessionId: purchase.checkout_session_id, refundId: refund.id},
					'Could not queue the receipt for an Operator Pass refund made before fulfilment',
				);
			}
		}
	}

	private async recordPurchaseTerms(
		purchase: OperatorPassPurchaseRow,
		session: Stripe.Checkout.Session,
	): Promise<OperatorPassPurchaseRow> {
		if (purchase.withdrawal_ends_at) {
			return purchase;
		}
		const purchasedAt = purchase.purchased_at ?? operatorPassNow();
		const rule = resolveOperatorPassPurchaseWithdrawalRule(session, purchase);
		return this.repository.updatePurchase(purchase, {
			withdrawal_country: Db.set(rule?.country ?? null),
			withdrawal_days: Db.set(rule?.days ?? null),
			withdrawal_rule_version: Db.set(rule ? OPERATOR_PASS_WITHDRAWAL_RULE_VERSION : null),
			withdrawal_ends_at: Db.set(
				getOperatorPassWithdrawalEndsAt(rule ? operatorPassNow() : purchasedAt, rule?.days ?? null),
			),
			stripe_terms_consent: Db.set(session.consent?.terms_of_service ?? null),
			operator_pass_terms_version: Db.set(purchase.operator_pass_terms_version ?? OPERATOR_PASS_TERMS_VERSION),
		});
	}

	private async describePaymentMethod(paymentIntentId: string | null): Promise<string | null> {
		if (!this.stripe || !paymentIntentId) {
			return null;
		}
		try {
			const intent = await this.stripe.paymentIntents.retrieve(paymentIntentId, {expand: ['latest_charge']});
			const details = typeof intent.latest_charge === 'object' ? intent.latest_charge?.payment_method_details : null;
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
		} catch (error) {
			Logger.warn({error, paymentIntentId}, 'Failed to describe the payment method of an Operator Pass payment');
			return null;
		}
	}

	private async retrieveLatestCharge(paymentIntentId: string | null): Promise<Stripe.Charge | null> {
		if (!this.stripe || !paymentIntentId) {
			return null;
		}
		const intent = await this.stripe.paymentIntents.retrieve(paymentIntentId, {expand: ['latest_charge']});
		return typeof intent.latest_charge === 'object' ? intent.latest_charge : null;
	}

	private async retrieveChargeDispute(charge: Stripe.Charge): Promise<Stripe.Dispute> {
		const disputes = this.stripe ? await this.stripe.disputes.list({charge: charge.id, limit: 1}) : null;
		const dispute = disputes?.data[0];
		if (!dispute) {
			throw new StripeError('Operator Pass charge is marked disputed but Stripe returned no dispute');
		}
		return dispute;
	}

	private async issuePurchaseInvoice(purchase: OperatorPassPurchaseRow, session: Stripe.Checkout.Session) {
		if (!this.documents) {
			return null;
		}
		const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
		return this.documents.issueForCheckoutSession(
			session.id,
			operatorPassDocumentContext(purchase, purchaser?.locale ?? session.metadata?.operator_pass_locale ?? null),
		);
	}

	private async sendConfirmationOnce(
		purchase: OperatorPassPurchaseRow,
		purchaser: User,
		session: Stripe.Checkout.Session,
	): Promise<void> {
		const sentKey = confirmationSentKey(purchase.checkout_session_id);
		if (purchase.confirmation_sent_at || (await this.cacheService.get<boolean>(sentKey))) {
			return;
		}
		const email = confirmationAddress(purchaser, session);
		if (!email) {
			throw new StripeError('Operator Pass buyer has no email address for the purchase confirmation');
		}
		const invoice =
			purchase.invoice_document_id && this.documents
				? await this.documents.findDocument(purchase.invoice_document_id)
				: null;
		const sent = await this.emailService.send(
			'operator_pass_purchase_confirmation',
			email,
			{
				invoiceNumber: purchase.invoice_number ?? null,
				orderDate: purchase.purchased_at ?? operatorPassNow(),
				quantity: Math.max(1, purchase.quantity ?? 1),
				totals: invoice ? billingDocumentTotals(invoice) : plainPurchaseTotals(purchase),
				document: invoice ? billingDocumentAttachment(invoice) : null,
				business: purchase.business_buyer === true,
				consentAt: purchase.waiver_consent_at ?? null,
				withdrawalEndsOn:
					purchase.withdrawal_days != null && purchase.withdrawal_ends_at
						? getOperatorPassWithdrawalEndsOn(purchase.withdrawal_ends_at)
						: null,
				withdrawalCountry: purchase.withdrawal_country ?? null,
				wallName: await this.resolveConfirmationWallName(purchase),
				unitAmountMinor: Math.round(purchase.amount_minor / Math.max(1, purchase.quantity ?? 1)),
				paymentMethod: await this.describePaymentMethod(purchase.payment_intent_id ?? null),
				deliveryBoxText: purchase.waiver_consent_text ?? null,
				termsVersion: purchase.operator_pass_terms_version ?? OPERATOR_PASS_TERMS_VERSION,
				timeZone: purchaser.timezone,
			},
			purchaser.locale ?? session.metadata?.operator_pass_locale ?? null,
		);
		if (!sent) {
			throw new StripeError('Failed to send the Operator Pass purchase confirmation; the pass is not granted');
		}
		await this.cacheService.set(sentKey, true, APPLIED_TTL_SECONDS);
		if (invoice && invoice.emailed_at == null && this.documents) {
			try {
				await this.documents.repository.markEmailed(invoice.document_id, new Date());
			} catch (error) {
				Logger.error(
					{error, sessionId: purchase.checkout_session_id},
					'Sent the Operator Pass invoice but could not record when it was emailed',
				);
			}
		}
	}

	private async resolveConfirmationWallName(purchase: OperatorPassPurchaseRow): Promise<string | null> {
		if (purchase.wall_opt_out == null) {
			const entry = await this.wallService.getEntry(purchase.purchaser_id);
			return entry && !entry.opted_out ? (entry.name ?? null) : null;
		}
		return purchase.wall_opt_out ? null : (purchase.wall_name ?? null);
	}

	private async sendPurchaseRefused(
		purchase: OperatorPassPurchaseRow,
		purchaser: User,
		session: Stripe.Checkout.Session,
		refusal: OperatorPassRefusal,
	): Promise<void> {
		const sentKey = refusalSentKey(purchase.checkout_session_id);
		if (await this.cacheService.get<boolean>(sentKey)) {
			return;
		}
		const email = purchaser.email ?? session.customer_details?.email ?? session.customer_email;
		if (!email) {
			throw new StripeError('Operator Pass buyer has no email address for the refusal email');
		}
		if (await this.emailService.isEmailBounced(email)) {
			Logger.warn(
				{sessionId: purchase.checkout_session_id, refusal},
				'Operator Pass buyer address has bounced; skipping the refusal email',
			);
			await this.cacheService.set(sentKey, true, APPLIED_TTL_SECONDS);
			return;
		}
		const refund = await this.findRefusalRefundRecord(purchase);
		const creditNote = refund ? await this.loadRefundCreditNote(purchase, refund) : null;
		const sent = await this.emailService.send('operator_pass_purchase_refused', email, {creditNote}, purchaser.locale);
		if (!sent) {
			throw new StripeError('Failed to send the Operator Pass refusal email');
		}
		if (creditNote) {
			await this.cacheService.set(refusalCreditNoteKey(purchase.checkout_session_id), true, APPLIED_TTL_SECONDS);
		}
		await this.cacheService.set(sentKey, true, APPLIED_TTL_SECONDS);
	}

	private async finishRefusal(
		purchase: OperatorPassPurchaseRow,
		purchaser: User,
		session: Stripe.Checkout.Session,
		refusal: OperatorPassRefusal,
	): Promise<void> {
		if (EMAILED_REFUSALS.has(refusal)) {
			await this.sendPurchaseRefused(purchase, purchaser, session, refusal);
		}
		await this.repository.updatePurchase(purchase, {
			status: Db.set('duplicate_refunded'),
			completed_at: Db.set(new Date()),
			certificate_names: Db.clear(),
			wall_name: Db.clear(),
			refusal_reason: Db.set(refusal),
		});
	}

	private async findRefusalRefund(purchase: OperatorPassPurchaseRow): Promise<OperatorPassRefusal | null> {
		const refund = await this.findRefusalRefundRecord(purchase);
		return refund ? refusalOfRefund(refund, purchase) : null;
	}

	private async findRefusalRefundRecord(purchase: OperatorPassPurchaseRow): Promise<Stripe.Refund | null> {
		if (!this.stripe || !purchase.payment_intent_id) {
			return null;
		}
		const refunds = await this.stripe.refunds.list({payment_intent: purchase.payment_intent_id, limit: 100});
		return refunds.data.find((refund) => refusalOfRefund(refund, purchase) != null) ?? null;
	}

	private async refundRefusedPurchase(purchase: OperatorPassPurchaseRow, reason: OperatorPassRefusal): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for an Operator Pass refusal refund');
		}
		if (!purchase.payment_intent_id) {
			throw new StripeError('Operator Pass refused purchase has no payment intent to refund');
		}
		Logger.warn(
			{sessionId: purchase.checkout_session_id, purchaserId: purchase.purchaser_id, reason},
			'Refusing the Operator Pass purchase before delivery; refunding it',
		);
		const refund = await this.stripe.refunds.create(
			{
				payment_intent: purchase.payment_intent_id,
				metadata: {
					checkout_session_id: purchase.checkout_session_id,
					rejection_reason: reason,
				},
			},
			{idempotencyKey: `operator-pass-duplicate-refund:${purchase.checkout_session_id}`},
		);
		try {
			await getBillingRepository().refunds.upsertFromStripe(refund);
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, refundId: refund.id},
				'Mirror upsert failed for an Operator Pass refusal refund; reconciler will heal',
			);
		}
		await this.creditRefundsMadeHere(purchase);
	}

	private async withCurrentPurchase<T>(
		purchase: OperatorPassPurchaseRow,
		action: (current: OperatorPassPurchaseRow) => Promise<T>,
	): Promise<T> {
		return this.withPurchaseLock(purchase.checkout_session_id, async () =>
			action((await this.repository.findPurchase(purchase.checkout_session_id)) ?? purchase),
		);
	}

	async createOffer(params: OperatorPassOfferParams): Promise<OperatorPassOfferRow> {
		return this.withLock(
			`operator_pass:offer_lock:${params.sender.id}`,
			() => this.holdingService.createOffer(params),
			() => new OperatorPassOfferBusyError(),
		);
	}

	private async withPurchaseLock<T>(
		checkoutSessionId: string,
		action: (lease: OperatorPassLockLease) => Promise<T>,
		busyError: () => Error = () => new StripeError('Operator Pass purchase is busy; retrying'),
	): Promise<T> {
		return this.withLock(operatorPassPurchaseLockKey(checkoutSessionId), action, busyError);
	}

	private async withLock<T>(
		lockKey: string,
		action: (lease: OperatorPassLockLease) => Promise<T>,
		busyError: () => Error,
	): Promise<T> {
		return withOperatorPassLock(this.cacheService, lockKey, action, busyError);
	}

	private resolvePaymentReferences(session: Stripe.Checkout.Session): CheckoutPaymentReferences {
		return {paymentIntentId: extractId(session.payment_intent)};
	}

	private async recoverPurchase(session: Stripe.Checkout.Session): Promise<OperatorPassPurchaseRow | null> {
		const metadata = session.metadata ?? {};
		const purchaserId = metadata.operator_pass_purchaser_id;
		const amountMinor = Number(metadata.operator_pass_amount_minor);
		const quantity = Number(metadata.operator_pass_quantity ?? '1');
		const currency = metadata.operator_pass_currency;
		const phase = metadata.operator_pass_phase;
		if (
			!purchaserId ||
			!/^\d{1,20}$/u.test(purchaserId) ||
			!Number.isSafeInteger(amountMinor) ||
			!Number.isSafeInteger(quantity) ||
			quantity < 1 ||
			!currency ||
			!isOperatorPassCurrency(currency) ||
			!phase ||
			!(OPERATOR_PASS_PHASES as ReadonlyArray<string>).includes(phase)
		) {
			return null;
		}
		const purchase: OperatorPassPurchaseRow = {
			checkout_session_id: session.id,
			purchaser_id: createUserID(BigInt(purchaserId)),
			quantity,
			status: 'pending',
			phase,
			amount_minor: amountMinor,
			currency,
			quote_honored: metadata.operator_pass_quote_honored === 'true',
			payment_intent_id: null,
			invoice_id: null,
			created_at: new Date((session.created ?? Math.floor(Date.now() / 1000)) * 1000),
			completed_at: null,
			refunded_at: null,
			waiver_consent_at: parseDate(metadata.operator_pass_waiver_consent_at),
			waiver_consent_version: metadata.operator_pass_waiver_consent_version ?? null,
			waiver_consent_text: session.custom_text?.terms_of_service_acceptance?.message ?? null,
			waiver_consent_ip: null,
			waiver_consent_user_agent: null,
			wall_name: null,
			wall_opt_out:
				metadata.operator_pass_wall_opt_out === undefined ? null : metadata.operator_pass_wall_opt_out === 'true',
			withdrawal_country: metadata.operator_pass_withdrawal_country ?? null,
			withdrawal_days: null,
			withdrawal_rule_version: null,
			fraud_warning_at: null,
			stripe_terms_consent: null,
			operator_pass_terms_version: metadata.operator_pass_terms_version ?? OPERATOR_PASS_TERMS_VERSION,
			purchased_at: null,
			withdrawal_ends_at: null,
			perks_granted_at: null,
			confirmation_sent_at: null,
			withdrawn_at: null,
			withdrawal_name: null,
			withdrawal_refund_id: null,
			withdrawal_channel: null,
			withdrawal_receipts_owed: null,
			invoice_number: null,
			invoice_document_id: null,
			dispute_id: null,
			dispute_reason: null,
			dispute_status: null,
			disputed_at: null,
			dispute_closed_at: null,
			certificate_names: null,
			age_confirmed_at:
				metadata.operator_pass_age_confirmed === 'true' ? parseDate(metadata.operator_pass_waiver_consent_at) : null,
			delivered_at: null,
			delivery_recorded_at: null,
			welcome_sent_at: null,
			welcome_skipped_at: null,
			refusal_reason: null,
			charge_id: null,
			business_buyer: null,
			holder_had_pass: null,
			labs_greeting:
				metadata.operator_pass_labs_greeting === undefined ? null : metadata.operator_pass_labs_greeting === 'true',
			preview_label: metadata.operator_pass_preview === 'true' ? (metadata.operator_pass_preview_label ?? '') : null,
			version: 1,
		};
		await this.repository.createPurchase(purchase);
		Logger.warn({sessionId: session.id, purchaserId}, 'Recovered Operator Pass purchase from checkout metadata');
		return purchase;
	}

	private async loadCertificateAttachment(purchase: OperatorPassPurchaseRow): Promise<EmailAttachment | null> {
		const file = await this.certificates.loadPurchaseFile(purchase);
		return file ? {filename: file.filename, content: Buffer.from(file.bytes), contentType: 'application/pdf'} : null;
	}

	async reissueCertificatePage(params: {
		passId: string;
		holderName: string;
		reason: OperatorPassCertificateReissueReason;
	}): Promise<OperatorPassCertificateRow> {
		const latest = await this.repository.findLatestCertificate(params.passId);
		if (latest?.source === 'visionary') {
			return this.reissueVisionaryPage(params);
		}
		if (!latest?.purchase_id) {
			throw new UnknownOperatorPassPurchaseError();
		}
		const purchaseId = latest.purchase_id;
		return this.withPurchaseLock(
			purchaseId,
			async () => {
				const purchase = await this.repository.findPurchase(purchaseId);
				if (!purchase) {
					throw new UnknownOperatorPassPurchaseError();
				}
				const unit = await this.repository.findUnit(params.passId);
				const page = await this.repository.findLatestCertificate(params.passId);
				if (!unit || isOperatorPassCertificatePageCancelled(unit) || page?.cancelled_at != null) {
					await this.certificates.syncCancellations(purchase);
					if (params.reason !== 'objection') {
						throw InputValidationError.create('pass_id', 'This certificate page is cancelled');
					}
					return this.eraseCancelledCertificatePage(purchase, params.passId);
				}
				const purchaser = await this.userRepository.findUnique(purchase.purchaser_id);
				if (!purchaser || purchaser.deletionStartedAt) {
					throw InputValidationError.create('pass_id', 'The buyer of this certificate page is deleting their account');
				}
				const row = await this.certificates.reissuePage(params);
				await this.repository.updatePurchase(purchase, {
					certificate_names: Db.set(replaceOperatorPassCertificateName(purchase, row.pass_id, row.holder_name)),
				});
				await this.certificates.syncCancellations(purchase);
				return row;
			},
			purchaseBusyError,
		);
	}

	private async reissueVisionaryPage(params: {
		passId: string;
		holderName: string;
		reason: OperatorPassCertificateReissueReason;
	}): Promise<OperatorPassCertificateRow> {
		const unit = await this.repository.findUnit(params.passId);
		const page = await this.repository.findLatestCertificate(params.passId);
		if (unit?.status !== 'held' || page?.cancelled_at != null) {
			if (params.reason !== 'objection') {
				throw InputValidationError.create('pass_id', 'This certificate page is cancelled');
			}
			await this.repository.clearCertificatePersonalData(params.passId);
			const erased = await this.repository.findLatestCertificate(params.passId);
			if (!erased) {
				throw InputValidationError.create('pass_id', 'This pass has no certificate page');
			}
			return erased;
		}
		const holder = await this.userRepository.findUnique(unit.holder_id);
		if (!holder || holder.deletionStartedAt) {
			throw InputValidationError.create('pass_id', 'The holder of this certificate page is deleting their account');
		}
		return this.certificates.reissuePage(params);
	}

	private async eraseCancelledCertificatePage(
		purchase: OperatorPassPurchaseRow,
		passId: string,
	): Promise<OperatorPassCertificateRow> {
		await this.repository.clearCertificatePersonalData(passId);
		if (purchase.certificate_names != null) {
			await this.repository.updatePurchase(purchase, {
				certificate_names: Db.set(replaceOperatorPassCertificateName(purchase, passId, '')),
			});
		}
		const erased = await this.repository.findLatestCertificate(passId);
		if (!erased) {
			throw InputValidationError.create('pass_id', 'This pass has no certificate page');
		}
		Logger.info(
			{passId, number: erased.number, version: erased.version},
			'Erased the name and PDF of a cancelled Operator certificate page after an objection',
		);
		return erased;
	}

	async resetCertificateLink(user: User, passId: string): Promise<string> {
		const latest = await this.repository.findLatestCertificate(passId);
		if (!latest) {
			throw new UnknownOperatorPassUnitError();
		}
		const unit = await this.repository.findUnit(passId);
		if (latest.source === 'visionary') {
			if (unit?.holder_id !== user.id) {
				throw new UnknownOperatorPassUnitError();
			}
		} else {
			const purchaseId = unit?.purchase_id ?? latest.purchase_id;
			const purchase = purchaseId ? await this.repository.findPurchase(purchaseId) : null;
			if (purchase?.purchaser_id !== user.id) {
				throw new UnknownOperatorPassUnitError();
			}
		}
		const row = await this.reissueCertificatePage({passId, holderName: latest.holder_name, reason: 'link'});
		if (!row.link_token) {
			throw new Error(`Operator certificate page ${passId} has no link after a link reset`);
		}
		return getOperatorPassCertificateUrl(row.link_token);
	}

	private async sendWelcomeOnce(purchase: OperatorPassPurchaseRow, user: User): Promise<void> {
		if (isOperatorPassWelcomeSettled(purchase)) {
			return;
		}
		if (!user.email || (await this.emailService.isEmailBounced(user.email))) {
			Logger.warn(
				{userId: user.id, sessionId: purchase.checkout_session_id},
				'Operator Pass buyer has no deliverable email; skipping welcome email',
			);
			await this.repository.updatePurchase(purchase, {welcome_skipped_at: Db.set(operatorPassNow())});
			return;
		}
		const attachment = await this.loadCertificateAttachment(purchase);
		const units = await this.repository.listUnitsByPurchase(purchase.checkout_session_id);
		const livePages = units.filter((unit) => !isOperatorPassCertificatePageCancelled(unit)).length;
		if (!attachment) {
			if (livePages > 0) {
				throw new StripeError('Operator Pass certificate is missing for the welcome email; retrying');
			}
			Logger.warn(
				{sessionId: purchase.checkout_session_id},
				'The Operator Pass purchase has no pass left with a certificate page; skipping welcome email',
			);
			await this.repository.updatePurchase(purchase, {welcome_skipped_at: Db.set(operatorPassNow())});
			return;
		}
		const wallEntry = await this.wallService.getEntry(user.id);
		const sent = await this.emailService.send(
			'operator_pass_welcome',
			user.email,
			{
				labsInviteUrl: OPERATOR_PASS_LABS_INVITE_URL,
				labsMember: await this.services.roleService.isLabsMember(user.id),
				invoiceNumber: purchase.invoice_number ?? null,
				deliveredAt: purchase.delivered_at ?? purchase.perks_granted_at ?? operatorPassNow(),
				wallName: wallEntry && !wallEntry.opted_out ? (wallEntry.name ?? null) : null,
				passCount: units.length > 0 ? livePages : Math.max(1, purchase.quantity ?? 1),
				certificate: attachment,
				timeZone: user.timezone,
				visionary: keepsOperatorsThroughVisionary(user),
			},
			user.locale,
		);
		if (!sent) {
			throw new StripeError('Failed to send the Operator Pass welcome email with the certificate; retrying');
		}
		await this.repository.updatePurchase(purchase, {welcome_sent_at: Db.set(operatorPassNow())});
	}
}

function plainPurchaseTotals(purchase: OperatorPassPurchaseRow): BillingTotals {
	return {
		totalMinor: purchase.amount_minor,
		paidMinor: purchase.amount_minor,
		currency: purchase.currency,
		tax: null,
		reverseCharge: false,
	};
}

export function resolveOperatorPassPaidAt(charge: Stripe.Charge | null, fallback: Date): Date {
	return charge?.created ? new Date(charge.created * 1000) : fallback;
}

const SETTLEMENT_EVENT_SCAN_LIMIT = 1000;

export async function resolveOperatorPassSettledAt(
	stripe: Stripe,
	session: Stripe.Checkout.Session,
	fallback: Date,
): Promise<Date> {
	const intent = typeof session.payment_intent === 'object' ? session.payment_intent : null;
	if (!intent) {
		return fallback;
	}
	let scanned = 0;
	for await (const event of stripe.events.list({
		type: 'payment_intent.succeeded',
		created: {gte: intent.created},
		limit: 100,
	})) {
		if ((event.data.object as {id?: string}).id === intent.id) {
			return new Date(event.created * 1000);
		}
		scanned += 1;
		if (scanned >= SETTLEMENT_EVENT_SCAN_LIMIT) {
			break;
		}
	}
	const charge = typeof intent.latest_charge === 'object' ? intent.latest_charge : null;
	return charge?.payment_method_details?.type === 'card' ? resolveOperatorPassPaidAt(charge, fallback) : fallback;
}

function isChargeDisputedError(error: unknown): boolean {
	return typeof error === 'object' && error !== null && 'code' in error && error.code === 'charge_disputed';
}

function parseDate(value: string | undefined): Date | null {
	if (!value) {
		return null;
	}
	const parsed = new Date(value);
	return Number.isNaN(parsed.getTime()) ? null : parsed;
}

function confirmationAddress(purchaser: User, session: Stripe.Checkout.Session): string | null {
	return purchaser.email ?? session.customer_details?.email ?? session.customer_email ?? null;
}

function isWelcomeOutstanding(purchase: OperatorPassPurchaseRow): boolean {
	return (
		(purchase.status === 'completed' || purchase.status === 'disputed') &&
		purchase.delivered_at != null &&
		!isOperatorPassWelcomeSettled(purchase)
	);
}

function purchaseBusyError(): ConflictError {
	return new ConflictError({
		code: APIErrorCodes.CONFLICT,
		message: 'This purchase is being updated, try again in a moment',
	});
}

function withdrawalReceiptKey(receipt: OperatorPassWithdrawalReceipt): string {
	return `withdrawal-receipt:${receipt.purchaseId}:${receipt.passId ?? 'all'}:${receipt.receivedAt.toISOString()}`;
}

function withdrawalDisputeAcceptedKey(disputeId: string): string {
	return `operator_pass:withdrawal_dispute_accepted:${disputeId}`;
}

function withdrawalReceiptSentKey(key: string): string {
	return `operator_pass:${key}:sent`;
}

function withOwedWithdrawalReceipt(
	purchase: OperatorPassPurchaseRow,
	receipt: OperatorPassWithdrawalReceipt,
): Map<string, string> {
	const owed = new Map(purchase.withdrawal_receipts_owed ?? []);
	owed.set(withdrawalReceiptKey(receipt), JSON.stringify(encodeOperatorPassWithdrawalReceipt(receipt)));
	return owed;
}

function withdrawalCreditNoteOwedKey(refundId: string): string {
	return `operator_pass:withdrawal:credit_note_owed:${refundId}`;
}

function refusalCreditNoteKey(checkoutSessionId: string): string {
	return `operator_pass:refusal:credit_note:${checkoutSessionId}`;
}

export function operatorPassPurchaseCacheKeys(checkoutSessionId: string): Array<string> {
	return [getAppliedKey(checkoutSessionId), confirmationSentKey(checkoutSessionId), refusalSentKey(checkoutSessionId)];
}

function confirmationSentKey(checkoutSessionId: string): string {
	return `operator_pass:confirmation:sent:${checkoutSessionId}`;
}

function refusalSentKey(checkoutSessionId: string): string {
	return `operator_pass:refusal:sent:${checkoutSessionId}`;
}

function getAppliedKey(checkoutSessionId: string): string {
	return `stripe:checkout:operator_pass:applied:${checkoutSessionId}`;
}
