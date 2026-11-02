// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {decisionAuditMetadata, type EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {mirrorBestEffort} from '@app/api/stripe/StripeMirror';
import {extractId, isMissingOrCancelledSubscriptionError} from '@app/api/stripe/StripeUtils';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms, seconds} from 'itty-time';
import type Stripe from 'stripe';

type RadarValueListCreateParams = Stripe.Radar.ValueListCreateParams;
type RadarValueListItemType = RadarValueListCreateParams['item_type'];

interface StripePaymentFraudServiceDeps {
	stripe: Stripe | null;
	userRepository: UserRepository;
	donationRepository: DonationRepository;
	cacheService: ICacheService;
	auditService: AdminAuditService;
	enforcement: EnforcementDecisionService;
	operatorPassFulfilment?: Pick<
		OperatorPassFulfilmentService,
		'findPurchaseByPaymentIntent' | 'markFraudWarning'
	> | null;
}

interface FraudSignalParams {
	userId: UserID;
	source: 'early_fraud_warning';
	signalId: string;
	chargeId: string | null;
	paymentIntentId: string | null;
	customerId: string | null;
	fraudType: string | null;
	paymentAt: Date;
	extraMetadata?: Map<string, string>;
}

interface ResolvedFraudUserTarget {
	customerId: string | null;
	kind: 'direct_purchase' | 'donation' | 'gift_purchase' | 'operator_pass_purchase' | 'unknown';
	paymentIntentId: string | null;
	paymentAt: Date;
	userId: UserID | null;
}

interface RadarValueListConfig {
	alias: string;
	itemType: RadarValueListItemType;
	name: string;
}

const OPERATION_COMPLETION_TTL_SECONDS = seconds('30 days');
const OPERATION_LOCK_TTL_SECONDS = seconds('2 minutes');
const OPERATION_LOCK_MAX_WAIT_MS = ms('20 seconds');
const OPERATION_LOCK_RETRY_DELAY_MS = 100;
const EARLY_FRAUD_WARNING_REASON = 'Payment fraud - Stripe early fraud warning';
const CARD_FINGERPRINT_VALUE_LIST: RadarValueListConfig = {
	alias: 'card_fingerprint_blocklist',
	itemType: 'card_fingerprint',
	name: 'Card fingerprint block list',
};

function chargeTime(charge: Stripe.Charge): Date {
	return new Date(charge.created * 1000);
}

export class StripePaymentFraudService {
	constructor(private readonly deps: StripePaymentFraudServiceDeps) {}

	async handleFraudulentDispute(dispute: Stripe.Dispute): Promise<void> {
		if (dispute.reason !== 'fraudulent') {
			return;
		}
		const chargeId = extractId(dispute.charge);
		if (!chargeId) {
			Logger.warn(
				{disputeId: dispute.id},
				'Fraudulent Stripe dispute missing charge id; skipping Stripe-side fraud blocks',
			);
			return;
		}
		const charge = await this.retrieveCharge(chargeId);
		await this.ensureChargeReportedAsFraud(chargeId);
		await this.ensurePaymentMethodBlocked(charge);
	}

	async handleEarlyFraudWarning(warning: Stripe.Radar.EarlyFraudWarning): Promise<void> {
		if (!warning.actionable) {
			Logger.debug({warningId: warning.id}, 'Skipping non-actionable Stripe early fraud warning');
			return;
		}
		const chargeId = extractId(warning.charge);
		if (!chargeId) {
			throw new StripeError('Early fraud warning missing charge id');
		}
		const charge = await this.retrieveCharge(chargeId);
		const paymentIntentId = extractId(warning.payment_intent) ?? extractId(charge.payment_intent);
		const customerId = extractId(charge.customer);
		const userTarget = await this.resolveFraudUserTarget({
			charge,
			paymentIntentId,
			customerId,
		});
		if (userTarget.kind === 'operator_pass_purchase' && paymentIntentId) {
			await this.deps.operatorPassFulfilment?.markFraudWarning(paymentIntentId);
		}
		await this.ensureChargeReportedAsFraud(charge.id);
		await this.ensureFraudulentRefund(charge, warning.id);
		await this.ensurePaymentMethodBlocked(charge);
		if (userTarget.kind === 'operator_pass_purchase') {
			Logger.info(
				{chargeId, paymentIntentId, warningId: warning.id},
				'Stripe early fraud warning on an Operator Pass payment; refunded with no account action',
			);
			return;
		}
		if (!userTarget.userId) {
			Logger.warn(
				{
					chargeId,
					customerId,
					paymentIntentId,
					kind: userTarget.kind,
					warningId: warning.id,
				},
				'Stripe early fraud warning had no internal user target; Stripe-side actions were still applied',
			);
			return;
		}
		if (userTarget.kind === 'direct_purchase' && paymentIntentId) {
			await this.cancelSubscriptionPaidByIntent(paymentIntentId, userTarget.userId);
		}
		await this.limitPurchasesForFraudSignal({
			userId: userTarget.userId,
			source: 'early_fraud_warning',
			signalId: warning.id,
			chargeId,
			paymentIntentId: userTarget.paymentIntentId,
			customerId: userTarget.customerId,
			fraudType: warning.fraud_type,
			paymentAt: userTarget.paymentAt,
			extraMetadata: new Map([['target_kind', userTarget.kind]]),
		});
	}

	private async resolvePaymentTime(paymentIntentId: string | null, fallback: Date): Promise<Date> {
		if (!paymentIntentId) return fallback;
		const payment = await this.deps.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		return payment?.createdAt ?? fallback;
	}

	private async limitPurchasesForFraudSignal({
		userId,
		source,
		signalId,
		chargeId,
		paymentIntentId,
		customerId,
		fraudType,
		paymentAt,
		extraMetadata,
	}: FraudSignalParams): Promise<void> {
		const actionId = paymentIntentId ?? chargeId ?? `${source}:${signalId}`;
		await this.runOnce(
			'stripe:payment_fraud:account_action',
			actionId,
			{
				actionId,
				chargeId,
				customerId,
				fraudType,
				paymentIntentId,
				signalId,
				source,
				userId: userId.toString(),
			},
			async () => {
				const user = await this.deps.userRepository.findUnique(userId);
				if (!user) {
					throw new StripeError('User not found for payment fraud action');
				}
				const auditReason = this.getAuditReason(fraudType);
				const metadata = new Map<string, string>([['source', source]]);
				if (chargeId) {
					metadata.set('charge_id', chargeId);
				}
				if (paymentIntentId) {
					metadata.set('payment_intent_id', paymentIntentId);
				}
				if (customerId) {
					metadata.set('customer_id', customerId);
				}
				if (fraudType) {
					metadata.set('fraud_type', fraudType);
				}
				for (const [key, value] of extraMetadata ?? []) {
					metadata.set(key, value);
				}
				const decision = await this.deps.enforcement.recordAutomatedPurchaseLimit({
					subject: user,
					reasonCode: DeletionReasons.PAYMENT_FRAUD,
					paymentAt,
					internalNote: [auditReason, ...[...metadata].map(([key, value]) => `${key}: ${value}`)].join('\n'),
				});
				await this.deps.auditService.createAuditLog({
					adminUserId: SYSTEM_USER_ID,
					targetType: 'user',
					targetId: BigInt(userId),
					action: 'limit_features',
					auditLogReason: auditReason,
					metadata: new Map([...metadata, ['limit', 'purchases'], ...decisionAuditMetadata(decision)]),
				});
				Logger.info(
					{chargeId, customerId, fraudType, paymentIntentId, signalId, source, userId},
					'Limited purchases after a payment fraud signal pending review',
				);
			},
		);
	}

	private async resolveFraudUserTarget({
		charge,
		paymentIntentId,
		customerId,
	}: {
		charge: Stripe.Charge;
		customerId: string | null;
		paymentIntentId: string | null;
	}): Promise<ResolvedFraudUserTarget> {
		if (paymentIntentId) {
			const operatorPassPurchase = await this.deps.operatorPassFulfilment?.findPurchaseByPaymentIntent(paymentIntentId);
			if (operatorPassPurchase) {
				return {
					customerId,
					kind: 'operator_pass_purchase',
					paymentIntentId,
					paymentAt: operatorPassPurchase.purchased_at ?? operatorPassPurchase.created_at,
					userId: operatorPassPurchase.purchaser_id,
				};
			}
			const purchaserId =
				charge.metadata?.operator_pass === 'true' ? charge.metadata.operator_pass_purchaser_id : undefined;
			if (purchaserId && /^\d+$/.test(purchaserId)) {
				return {
					customerId,
					kind: 'operator_pass_purchase',
					paymentIntentId,
					paymentAt: chargeTime(charge),
					userId: createUserID(BigInt(purchaserId)),
				};
			}
			const giftCode = await this.deps.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
			if (giftCode) {
				return {
					customerId,
					kind: 'gift_purchase',
					paymentIntentId,
					paymentAt: await this.resolvePaymentTime(paymentIntentId, giftCode.createdAt),
					userId: giftCode.createdByUserId,
				};
			}
			const payment = await this.deps.userRepository.getPaymentByPaymentIntent(paymentIntentId);
			if (payment) {
				return {
					customerId: customerId ?? payment.stripeCustomerId,
					kind: 'direct_purchase',
					paymentIntentId,
					paymentAt: payment.createdAt,
					userId: payment.userId,
				};
			}
		}
		if (customerId) {
			const donationCustomer = await this.deps.donationRepository.findDonorByStripeCustomerId(customerId);
			if (donationCustomer) {
				return {
					customerId,
					kind: 'donation',
					paymentIntentId,
					paymentAt: chargeTime(charge),
					userId: null,
				};
			}
			const user = await this.deps.userRepository.findByStripeCustomerId(customerId);
			if (user) {
				return {
					customerId,
					kind: 'direct_purchase',
					paymentIntentId,
					paymentAt: chargeTime(charge),
					userId: user.id,
				};
			}
		}
		Logger.error(
			{
				billingEmail: charge.billing_details.email,
				chargeId: charge.id,
				customerId,
				paymentIntentId,
			},
			'Unable to resolve internal user for Stripe payment fraud signal',
		);
		return {
			customerId,
			kind: 'unknown',
			paymentIntentId,
			paymentAt: chargeTime(charge),
			userId: null,
		};
	}

	private async ensureFraudulentRefund(charge: Stripe.Charge, warningId: string): Promise<void> {
		await this.runOnce('stripe:payment_fraud:refund', charge.id, {chargeId: charge.id, warningId}, async () => {
			if (this.isChargeFullyRefunded(charge)) {
				Logger.info({chargeId: charge.id}, 'Stripe charge already fully refunded before early fraud warning refund');
				return;
			}
			const stripe = this.getStripe();
			try {
				const refund = await stripe.refunds.create(
					{
						charge: charge.id,
						metadata: {
							rejection_reason: 'stripe_early_fraud_warning',
							source: 'stripe_early_fraud_warning',
							warning_id: warningId,
						},
						reason: 'fraudulent',
					},
					{idempotencyKey: `payment-fraud-refund:${charge.id}`},
				);
				await mirrorBestEffort(
					() =>
						getBillingRepository().refunds.upsertFromStripe(refund, {
							customerId: extractId(charge.customer) ?? undefined,
						}),
					{refundId: refund.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
				Logger.info({chargeId: charge.id, warningId}, 'Created fraudulent refund for Stripe early fraud warning');
			} catch (error: unknown) {
				const refreshedCharge = await this.retrieveCharge(charge.id);
				if (this.isChargeFullyRefunded(refreshedCharge)) {
					Logger.info(
						{chargeId: charge.id, warningId},
						'Stripe charge became fully refunded while processing early fraud warning',
					);
					return;
				}
				Logger.error({chargeId: charge.id, error, warningId}, 'Failed to create Stripe fraudulent refund');
				throw error;
			}
		});
	}

	private async ensureChargeReportedAsFraud(chargeId: string): Promise<void> {
		await this.runOnce('stripe:payment_fraud:charge_report', chargeId, {chargeId}, async () => {
			try {
				const updatedCharge = await this.getStripe().charges.update(chargeId, {
					fraud_details: {
						user_report: 'fraudulent',
					},
				});
				await mirrorBestEffort(
					() => getBillingRepository().charges.upsertFromStripe(updatedCharge),
					{chargeId: updatedCharge.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
				Logger.info({chargeId}, 'Reported Stripe charge as fraudulent');
			} catch (error: unknown) {
				Logger.warn({chargeId, error}, 'Failed to mark Stripe charge as fraudulent; continuing');
			}
		});
	}

	private async ensurePaymentMethodBlocked(charge: Stripe.Charge): Promise<void> {
		await this.runOnce('stripe:payment_fraud:radar_blocks', charge.id, {chargeId: charge.id}, async () => {
			const cardFingerprint = this.getChargeFingerprint(charge);
			if (!cardFingerprint) {
				return;
			}
			try {
				await this.ensureRadarValueListed(CARD_FINGERPRINT_VALUE_LIST, cardFingerprint);
			} catch (error: unknown) {
				Logger.warn(
					{cardFingerprint, chargeId: charge.id, error},
					'Failed to update the Stripe Radar card fingerprint list, continuing',
				);
			}
		});
	}

	private async ensureRadarValueListed(config: RadarValueListConfig, value: string): Promise<void> {
		const trimmedValue = value.trim();
		if (trimmedValue.length === 0) {
			return;
		}
		const stripe = this.getStripe();
		const valueList = await this.ensureRadarValueList(config);
		if (!valueList) {
			return;
		}
		const existingItems = await stripe.radar.valueListItems.list({
			limit: 1,
			value: trimmedValue,
			value_list: valueList.id,
		});
		const alreadyListed = existingItems.data.some((item) => item.value === trimmedValue);
		if (alreadyListed) {
			return;
		}
		await stripe.radar.valueListItems.create({
			value: trimmedValue,
			value_list: valueList.id,
		});
	}

	private async ensureRadarValueList(config: RadarValueListConfig): Promise<Stripe.Radar.ValueList | null> {
		const stripe = this.getStripe();
		const existingLists = await stripe.radar.valueLists.list({
			alias: config.alias,
			limit: 1,
		});
		const existingList = existingLists.data[0] ?? null;
		if (existingList) {
			if (existingList.item_type !== config.itemType) {
				Logger.error(
					{
						alias: config.alias,
						configuredItemType: existingList.item_type,
						expectedItemType: config.itemType,
					},
					'Stripe Radar value list alias already exists with the wrong item type',
				);
				return null;
			}
			return existingList;
		}
		return await stripe.radar.valueLists.create({
			alias: config.alias,
			item_type: config.itemType,
			name: config.name,
		});
	}

	private getChargeFingerprint(charge: Stripe.Charge): string | null {
		const paymentMethodDetails = charge.payment_method_details;
		if (!paymentMethodDetails) {
			return null;
		}
		return (
			paymentMethodDetails.card?.fingerprint ??
			paymentMethodDetails.sepa_debit?.fingerprint ??
			paymentMethodDetails.us_bank_account?.fingerprint ??
			null
		);
	}

	private async cancelSubscriptionPaidByIntent(paymentIntentId: string, userId: UserID): Promise<void> {
		const stripe = this.getStripe();
		const payments = await stripe.invoicePayments.list({
			payment: {type: 'payment_intent', payment_intent: paymentIntentId},
			expand: ['data.invoice'],
			limit: 10,
		});
		for (const payment of payments.data) {
			const invoice = typeof payment.invoice === 'string' ? null : payment.invoice;
			const subscriptionId =
				invoice && !('deleted' in invoice && invoice.deleted)
					? extractId((invoice as Stripe.Invoice).parent?.subscription_details?.subscription ?? null)
					: null;
			if (!subscriptionId) continue;
			try {
				const subscription = await stripe.subscriptions.retrieve(subscriptionId);
				if (extractId(subscription.latest_invoice) !== invoice?.id) {
					Logger.info(
						{paymentIntentId, subscriptionId, userId},
						'Flagged payment did not pay for the current subscription period; leaving the subscription running',
					);
					continue;
				}
				await stripe.subscriptions.cancel(
					subscriptionId,
					{invoice_now: false, prorate: false},
					{idempotencyKey: `payment-fraud-cancel:${subscriptionId}`},
				);
				Logger.info(
					{paymentIntentId, subscriptionId, userId},
					'Cancelled the subscription paid by a payment with an early fraud warning',
				);
			} catch (error: unknown) {
				if (!isMissingOrCancelledSubscriptionError(error)) throw error;
			}
		}
	}

	private getAuditReason(fraudType: string | null): string {
		return fraudType ? `${EARLY_FRAUD_WARNING_REASON} (${fraudType})` : EARLY_FRAUD_WARNING_REASON;
	}

	private isChargeFullyRefunded(charge: Stripe.Charge): boolean {
		if (charge.refunded) {
			return true;
		}
		const refundableAmount = charge.captured ? charge.amount_captured : charge.amount;
		return charge.amount_refunded >= refundableAmount;
	}

	private async retrieveCharge(chargeId: string): Promise<Stripe.Charge> {
		return await this.getStripe().charges.retrieve(chargeId);
	}

	private getStripe(): Stripe {
		if (!this.deps.stripe) {
			throw new StripeError('Stripe client not available for payment fraud handling');
		}
		return this.deps.stripe;
	}

	private async runOnce(
		scope: string,
		actionId: string,
		logContext: Record<string, unknown>,
		action: () => Promise<void>,
	): Promise<void> {
		const completionKey = this.getCompletionKey(scope, actionId);
		if (await this.deps.cacheService.get<boolean>(completionKey)) {
			return;
		}
		const lockKey = this.getLockKey(scope, actionId);
		const token = await this.acquireLockWithRetry({
			completionKey,
			lockKey,
			logContext,
		});
		if (!token) {
			throw new StripeError(`Timed out acquiring payment fraud lock for ${scope}`);
		}
		try {
			if (await this.deps.cacheService.get<boolean>(completionKey)) {
				return;
			}
			await action();
			await this.deps.cacheService.set(completionKey, true, OPERATION_COMPLETION_TTL_SECONDS);
		} finally {
			await this.releaseLock(lockKey, token, logContext);
		}
	}

	private getCompletionKey(scope: string, actionId: string): string {
		return `${scope}:applied:${actionId}`;
	}

	private getLockKey(scope: string, actionId: string): string {
		return `${scope}:${actionId}`;
	}

	private async acquireLockWithRetry({
		lockKey,
		completionKey,
		logContext,
	}: {
		completionKey: string;
		lockKey: string;
		logContext: Record<string, unknown>;
	}): Promise<string | null> {
		const startTime = Date.now();
		while (Date.now() - startTime < OPERATION_LOCK_MAX_WAIT_MS) {
			const token = await this.deps.cacheService.acquireLock(lockKey, OPERATION_LOCK_TTL_SECONDS);
			if (token) {
				return token;
			}
			if (await this.deps.cacheService.get<boolean>(completionKey)) {
				return null;
			}
			await this.sleep(OPERATION_LOCK_RETRY_DELAY_MS);
		}
		Logger.warn({...logContext, lockKey}, 'Timed out waiting for Stripe payment fraud lock');
		return null;
	}

	private async releaseLock(lockKey: string, token: string, logContext: Record<string, unknown>): Promise<void> {
		try {
			const released = await this.deps.cacheService.releaseLock(lockKey, token);
			if (!released) {
				Logger.warn({...logContext, lockKey}, 'Stripe payment fraud lock token no longer matched on release');
			}
		} catch (error: unknown) {
			Logger.error({error, ...logContext, lockKey}, 'Failed to release Stripe payment fraud lock');
		}
	}

	private sleep(delayMs: number): Promise<void> {
		return new Promise((resolve) => {
			const timeout = setTimeout(resolve, delayMs);
			timeout.unref?.();
		});
	}
}
