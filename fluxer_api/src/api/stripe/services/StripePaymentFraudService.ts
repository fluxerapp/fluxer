// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {ISessionTerminator} from '@app/api/auth/ISessionTerminator';
import type {UserID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {IDonationRepository} from '@app/api/donation/IDonationRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {extractId} from '@app/api/stripe/StripeUtils';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms, seconds} from 'itty-time';
import type Stripe from 'stripe';

type RadarValueListCreateParams = Stripe.Radar.ValueListCreateParams;
type RadarValueListItemType = RadarValueListCreateParams['item_type'];

interface StripePaymentFraudServiceDeps {
	stripe: Stripe | null;
	userRepository: IUserRepository;
	userCacheService: UserCacheService;
	sessionTerminator: ISessionTerminator;
	emailService: IEmailService;
	gatewayService: IGatewayService;
	donationRepository: IDonationRepository;
	cacheService: ICacheService;
	auditService: AdminAuditService;
	kvDeletionQueue: KVAccountDeletionQueueService;
}

interface FraudAccountActionParams {
	userId: UserID;
	source: 'chargeback' | 'early_fraud_warning';
	signalId: string;
	chargeId: string | null;
	paymentIntentId: string | null;
	customerId: string | null;
	fraudType: string | null;
	extraMetadata?: Map<string, string>;
}

interface ResolvedFraudUserTarget {
	customerId: string | null;
	kind: 'direct_purchase' | 'donation' | 'gift_purchase' | 'unknown';
	paymentIntentId: string | null;
	userId: UserID | null;
}

interface RadarValueListConfig {
	alias: string;
	itemType: RadarValueListItemType;
	name: string;
}

const FRAUD_DELETION_DELAY_MS = ms('60 days');
const OPERATION_COMPLETION_TTL_SECONDS = seconds('30 days');
const OPERATION_LOCK_TTL_SECONDS = seconds('2 minutes');
const OPERATION_LOCK_MAX_WAIT_MS = ms('20 seconds');
const OPERATION_LOCK_RETRY_DELAY_MS = 100;
const EARLY_FRAUD_WARNING_REASON = 'Payment fraud - Stripe early fraud warning';
const CHARGEBACK_REASON = 'Payment fraud - Stripe chargeback filed';
const RADAR_VALUE_LISTS: Record<'cardFingerprint' | 'customerId' | 'email' | 'ipAddress', RadarValueListConfig> = {
	cardFingerprint: {
		alias: 'card_fingerprint_blocklist',
		itemType: 'card_fingerprint',
		name: 'Card fingerprint block list',
	},
	customerId: {
		alias: 'customer_id_blocklist',
		itemType: 'customer_id',
		name: 'Customer ID block list',
	},
	email: {
		alias: 'email_blocklist',
		itemType: 'email',
		name: 'Email block list',
	},
	ipAddress: {
		alias: 'client_ip_address_blocklist',
		itemType: 'ip_address',
		name: 'Client IP address block list',
	},
};

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
		await this.ensureRadarBlocksForCharge(charge, {
			additionalEmails: [dispute.evidence.customer_email_address ?? null],
			ipAddress: dispute.evidence.customer_purchase_ip ?? null,
		});
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
		await this.ensureChargeReportedAsFraud(charge.id);
		await this.ensureFraudulentRefund(charge, warning.id);
		await this.ensureRadarBlocksForCharge(charge);
		const userTarget = await this.resolveFraudUserTarget({
			charge,
			paymentIntentId,
			customerId,
		});
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
		await this.enforceAccountFraudAction({
			userId: userTarget.userId,
			source: 'early_fraud_warning',
			signalId: warning.id,
			chargeId,
			paymentIntentId: userTarget.paymentIntentId,
			customerId: userTarget.customerId,
			fraudType: warning.fraud_type,
			extraMetadata: new Map([['target_kind', userTarget.kind]]),
		});
	}

	async enforceAccountFraudAction({
		userId,
		source,
		signalId,
		chargeId,
		paymentIntentId,
		customerId,
		fraudType,
		extraMetadata,
	}: FraudAccountActionParams): Promise<void> {
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
				const pendingDeletionAt = new Date(Date.now() + FRAUD_DELETION_DELAY_MS);
				const cancelledUser = await this.cancelStripeSubscriptionImmediately(user);
				const subscriptionCancelled = cancelledUser !== null;
				const currentUser = cancelledUser ?? user;
				const auditReason = this.getAuditReason({source, fraudType});
				const updatedUser = await this.deps.userRepository.updateDeletionSchedule(currentUser, {
					flags: (currentUser.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED,
					pending_deletion_at: pendingDeletionAt,
					deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
					deletion_public_reason: 'Payment fraud',
					deletion_audit_log_reason: auditReason,
				});
				await reschedulePendingDeletion({
					userId,
					currentPendingDeletionAt: currentUser.pendingDeletionAt,
					nextPendingDeletionAt: pendingDeletionAt,
					deletionReasonCode: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
					userRepository: this.deps.userRepository,
					deletionQueue: this.deps.kvDeletionQueue,
				});
				await this.deps.userCacheService.setUserPartialResponseFromUser(updatedUser);
				await this.dispatchUser(updatedUser);
				await this.deps.sessionTerminator.terminateAllUserSessions(userId);
				if (updatedUser.email) {
					await this.deps.emailService.sendScheduledDeletionNotification(
						updatedUser.email,
						updatedUser.username,
						pendingDeletionAt,
						auditReason,
						updatedUser.locale,
					);
				}
				const metadata = new Map<string, string>([
					['days', '60'],
					['source', source],
					['scheduled_for', pendingDeletionAt.toISOString()],
					['subscription_cancelled', subscriptionCancelled ? 'true' : 'false'],
				]);
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
				if (extraMetadata) {
					for (const [key, value] of extraMetadata) {
						metadata.set(key, value);
					}
				}
				await this.deps.auditService.createAuditLog({
					adminUserId: SYSTEM_USER_ID,
					targetType: 'user',
					targetId: BigInt(userId),
					action: 'schedule_deletion',
					auditLogReason: auditReason,
					metadata,
				});
				Logger.info(
					{
						chargeId,
						customerId,
						fraudType,
						paymentIntentId,
						pendingDeletionAt,
						signalId,
						source,
						subscriptionCancelled,
						userId,
					},
					'Applied payment fraud account enforcement',
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
			const giftCode = await this.deps.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
			if (giftCode) {
				return {
					customerId,
					kind: 'gift_purchase',
					paymentIntentId,
					userId: giftCode.createdByUserId,
				};
			}
			const payment = await this.deps.userRepository.getPaymentByPaymentIntent(paymentIntentId);
			if (payment) {
				return {
					customerId: customerId ?? payment.stripeCustomerId,
					kind: 'direct_purchase',
					paymentIntentId,
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
					userId: null,
				};
			}
			const user = await this.deps.userRepository.findByStripeCustomerId(customerId);
			if (user) {
				return {
					customerId,
					kind: 'direct_purchase',
					paymentIntentId,
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
							source: 'stripe_early_fraud_warning',
							warning_id: warningId,
						},
						reason: 'fraudulent',
					},
					{idempotencyKey: `payment-fraud-refund:${charge.id}`},
				);
				try {
					await getBillingRepository().refunds.upsertFromStripe(refund, {
						customerId: extractId(charge.customer) ?? undefined,
					});
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, refundId: refund.id},
						'Mirror upsert failed after Stripe write; reconciler will heal',
					);
				}
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
				try {
					await getBillingRepository().charges.upsertFromStripe(updatedCharge);
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, chargeId: updatedCharge.id},
						'Mirror upsert failed after Stripe write; reconciler will heal',
					);
				}
				Logger.info({chargeId}, 'Reported Stripe charge as fraudulent');
			} catch (error: unknown) {
				Logger.warn({chargeId, error}, 'Failed to mark Stripe charge as fraudulent; continuing');
			}
		});
	}

	private async ensureRadarBlocksForCharge(
		charge: Stripe.Charge,
		{
			additionalEmails = [],
			ipAddress = null,
		}: {
			additionalEmails?: Array<string | null>;
			ipAddress?: string | null;
		} = {},
	): Promise<void> {
		await this.runOnce('stripe:payment_fraud:radar_blocks', charge.id, {chargeId: charge.id}, async () => {
			const customerId = extractId(charge.customer);
			const billingEmail = await this.resolveChargeEmail(charge, customerId);
			const cardFingerprint = this.getChargeFingerprint(charge);
			const emails = new Set([billingEmail, ...additionalEmails].filter((value): value is string => Boolean(value)));
			try {
				if (customerId) {
					await this.ensureRadarValueListed(RADAR_VALUE_LISTS.customerId, customerId);
				}
				for (const email of emails) {
					await this.ensureRadarValueListed(RADAR_VALUE_LISTS.email, email);
				}
				if (cardFingerprint) {
					await this.ensureRadarValueListed(RADAR_VALUE_LISTS.cardFingerprint, cardFingerprint);
				}
				if (ipAddress) {
					await this.ensureRadarValueListed(RADAR_VALUE_LISTS.ipAddress, ipAddress);
				}
			} catch (error: unknown) {
				Logger.warn(
					{
						cardFingerprint,
						chargeId: charge.id,
						customerId,
						emails: Array.from(emails),
						error,
						ipAddress,
					},
					'Failed to update Stripe Radar value lists; continuing with account enforcement',
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

	private async resolveChargeEmail(charge: Stripe.Charge, customerId: string | null): Promise<string | null> {
		const directEmail = charge.billing_details.email ?? charge.receipt_email;
		if (directEmail) {
			return directEmail;
		}
		if (!customerId) {
			return null;
		}
		try {
			const customer = await this.getStripe().customers.retrieve(customerId);
			if (customer && !customer.deleted) {
				return customer.email ?? null;
			}
		} catch (error: unknown) {
			Logger.warn({customerId, error}, 'Failed to load Stripe customer email for Radar blocking');
		}
		return null;
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

	private async cancelStripeSubscriptionImmediately(user: User): Promise<User | null> {
		if (!user.stripeSubscriptionId) {
			return null;
		}
		try {
			await this.getStripe().subscriptions.cancel(
				user.stripeSubscriptionId,
				{invoice_now: false, prorate: false},
				{idempotencyKey: `payment-fraud-cancel:${user.stripeSubscriptionId}`},
			);
		} catch (error: unknown) {
			if (!this.isMissingOrCancelledSubscriptionError(error)) {
				Logger.error(
					{error, subscriptionId: user.stripeSubscriptionId, userId: user.id},
					'Failed to cancel Stripe subscription during payment fraud enforcement',
				);
				throw error;
			}
			Logger.warn(
				{subscriptionId: user.stripeSubscriptionId, userId: user.id},
				'Stripe subscription was already unavailable during payment fraud enforcement; clearing local state',
			);
		}
		const updatedUser = await this.deps.userRepository.patchUpsert(
			user.id,
			{
				premium_billing_cycle: null,
				premium_will_cancel: false,
				stripe_subscription_id: null,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
		return updatedUser;
	}

	private isMissingOrCancelledSubscriptionError(error: unknown): boolean {
		if (!(error instanceof Error)) {
			return false;
		}
		const normalisedMessage = error.message.toLowerCase();
		return (
			normalisedMessage.includes('no such subscription') ||
			normalisedMessage.includes('cannot cancel a canceled subscription') ||
			normalisedMessage.includes('cannot cancel a cancelled subscription') ||
			normalisedMessage.includes('subscription is canceled') ||
			normalisedMessage.includes('subscription is cancelled')
		);
	}

	private getAuditReason({
		source,
		fraudType,
	}: {
		fraudType: string | null;
		source: 'chargeback' | 'early_fraud_warning';
	}): string {
		const baseReason = source === 'early_fraud_warning' ? EARLY_FRAUD_WARNING_REASON : CHARGEBACK_REASON;
		if (!fraudType) {
			return baseReason;
		}
		return `${baseReason} (${fraudType})`;
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

	private async dispatchUser(user: User): Promise<void> {
		await this.deps.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
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
