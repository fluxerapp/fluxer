// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';
import {createUserID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {BillingSubscriptionRow} from '../../database/types/BillingTypes';
import type {UserRow} from '../../database/types/UserTypes';
import type {IDonationRepository} from '../../donation/IDonationRepository';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {Payment} from '../../models/Payment';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import type {ProductInfo, ProductRegistry} from '../ProductRegistry';
import {
	getFirstInvoicePaymentIntentId,
	getPrimarySubscriptionItem,
	getSubscriptionItemPeriodEnd,
	getSubscriptionPremiumPeriodEnd,
} from '../StripeSubscriptionPeriod';
import {extractId} from '../StripeUtils';
import {EU_WITHDRAWAL_WAIVER_TEXT_VERSION} from './StripeCheckoutService';
import type {StripeGiftService} from './StripeGiftService';
import type {StripePremiumService} from './StripePremiumService';

interface DonationCustomerDetails {
	businessName: string | null;
	taxId: string | null;
	taxIdType: string | null;
}

interface DonationSubscriptionDetails {
	amountCents: number | null;
	currency: string | null;
	interval: string | null;
	currentPeriodEnd: Date | null;
	cancelAt: Date | null;
}

interface CheckoutChargeDetails {
	chargeId: string | null;
	paymentMethodType: string | null;
	cardCountry: string | null;
}

type CheckoutPremiumApplyResult = 'granted' | 'refunded_duplicate_subscription';
type CheckoutSideEffectResult = 'continue' | 'stop';

interface CheckoutPremiumGrantRecovery {
	expectedPremiumUntil?: Date | null;
}

interface CheckoutFulfilmentContext {
	session: Stripe.Checkout.Session;
	payment: Payment;
	user: User;
	productInfo: ProductInfo;
	subscriptionId: string | null;
	customerId: string | null;
	isRecurring: boolean;
	amountTotal: number;
	currency: string;
}

interface CheckoutPremiumGrantContext extends CheckoutFulfilmentContext {
	recovery: CheckoutPremiumGrantRecovery;
}

export class StripeCheckoutWebhookHandler {
	static readonly CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS = seconds('365 days');

	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private productRegistry: ProductRegistry,
		private cacheService: ICacheService,
		private giftService: StripeGiftService,
		private premiumService: StripePremiumService,
		private donationRepository: IDonationRepository,
	) {}

	async handleCheckoutSessionCompleted(session: Stripe.Checkout.Session): Promise<void> {
		if (session.metadata?.is_donation === 'true') {
			await this.handleDonationCheckoutCompleted(session);
			return;
		}
		if (session.payment_status === 'unpaid') {
			const payment = await this.userRepository.getPaymentByCheckoutSession(session.id);
			if (payment && payment.status === 'pending') {
				await this.userRepository.updatePayment({
					...payment.toRow(),
					status: 'awaiting_settlement',
				});
			}
			Logger.info(
				{sessionId: session.id, paymentStatus: session.payment_status},
				'Checkout session completed with unpaid status; deferring fulfilment until settlement',
			);
			return;
		}
		await this.fulfilCheckoutSession(session);
	}

	async handleAsyncPaymentSucceeded(session: Stripe.Checkout.Session): Promise<void> {
		if (session.metadata?.is_donation === 'true') {
			return;
		}
		Logger.info({sessionId: session.id}, 'Processing async payment succeeded for checkout session');
		await this.fulfilCheckoutSession(session);
	}

	async handleAsyncPaymentFailed(session: Stripe.Checkout.Session): Promise<void> {
		if (session.metadata?.is_donation === 'true') {
			return;
		}
		const payment = await this.userRepository.getPaymentByCheckoutSession(session.id);
		if (payment && (payment.status === 'pending' || payment.status === 'awaiting_settlement')) {
			await this.userRepository.updatePayment({
				...payment.toRow(),
				status: 'failed',
			});
		}
		Logger.warn(
			{sessionId: session.id, paymentStatus: session.payment_status},
			'Async payment failed for checkout session',
		);
	}

	private async fulfilCheckoutSession(session: Stripe.Checkout.Session): Promise<void> {
		let payment = await this.userRepository.getPaymentByCheckoutSession(session.id);
		if (!payment) {
			payment = await this.tryRecoverPaymentFromSessionMetadata(session);
		}
		if (!payment) {
			Logger.error(
				{
					sessionId: session.id,
					paymentLinkId: extractId(session.payment_link),
					mode: session.mode,
					submitType: session.submit_type,
					paymentStatus: session.payment_status,
					clientReferenceId: session.client_reference_id,
				},
				'No payment record found for checkout session and recovery failed, skipping event',
			);
			return;
		}
		if (payment.status === 'completed' && payment.isGift && !payment.giftCode) {
			Logger.warn(
				{sessionId: session.id, userId: payment.userId},
				'Legacy completed gift payment with no gift code — skipping (no longer recoverable via webhook)',
			);
			return;
		}
		if (payment.status === 'completed') {
			Logger.debug({sessionId: session.id, status: payment.status}, 'Payment already processed');
			return;
		}
		if (payment.status !== 'pending' && payment.status !== 'awaiting_settlement') {
			Logger.debug({sessionId: session.id, status: payment.status}, 'Payment already processed');
			return;
		}
		const productInfo = this.productRegistry.getProduct(payment.priceId!);
		if (!productInfo) {
			Logger.error({sessionId: session.id, priceId: payment.priceId}, 'Unknown price ID');
			throw new StripeError('Unknown price ID for checkout session');
		}
		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			Logger.error({userId: payment.userId, sessionId: session.id}, 'User not found');
			throw new StripeError('User not found for checkout session');
		}
		if (session.amount_total == null || !session.currency) {
			Logger.error(
				{sessionId: session.id, amountTotal: session.amount_total, currency: session.currency},
				'Checkout session missing amount or currency',
			);
			throw new StripeError('Checkout session missing amount or currency');
		}
		const cardEligible = await this.validateLocalizedCardEligibility(session, payment, productInfo, user);
		if (!cardEligible) {
			return;
		}
		const customerId = extractId(session.customer);
		const subscriptionId = extractId(session.subscription);
		const isRecurring = this.productRegistry.isRecurringSubscription(productInfo);
		let giftCode: string | null = payment.giftCode;
		if (payment.isGift && !giftCode) {
			const paymentIntentId = extractId(session.payment_intent);
			giftCode = await this.giftService.prepareGiftCode(session.id, user, productInfo, paymentIntentId);
		}
		const sideEffectResult = await this.applyCheckoutSideEffects({
			session,
			payment,
			user,
			productInfo,
			subscriptionId,
			customerId,
			isRecurring,
			amountTotal: session.amount_total,
			currency: session.currency,
		});
		if (sideEffectResult === 'stop') {
			return;
		}
		const metadataWaiverAccepted = session.metadata?.eu_withdrawal_waiver_accepted === 'true';
		const stripeTermsAccepted = session.consent?.terms_of_service === 'accepted';
		const metadataWaiverAcceptedAt = session.metadata?.eu_withdrawal_waiver_accepted_at
			? new Date(session.metadata.eu_withdrawal_waiver_accepted_at)
			: null;
		const resolvedWaiverAccepted = payment.euWithdrawalWaiverAccepted || metadataWaiverAccepted || stripeTermsAccepted;
		const resolvedWaiverAcceptedAt =
			payment.euWithdrawalWaiverAcceptedAt ??
			(Number.isNaN(metadataWaiverAcceptedAt?.getTime() ?? Number.NaN) ? null : metadataWaiverAcceptedAt) ??
			(stripeTermsAccepted ? new Date() : null);
		await this.userRepository.updatePayment({
			...payment.toRow(),
			stripe_customer_id: customerId,
			payment_intent_id: extractId(session.payment_intent),
			subscription_id: subscriptionId,
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: session.amount_total,
			currency: session.currency,
			status: 'completed',
			completed_at: payment.completedAt ?? new Date(),
			gift_code: giftCode,
			eu_withdrawal_waiver_accepted: resolvedWaiverAccepted,
			eu_withdrawal_waiver_accepted_at: resolvedWaiverAcceptedAt,
			eu_withdrawal_waiver_text_version:
				payment.euWithdrawalWaiverTextVersion ??
				session.metadata?.eu_withdrawal_waiver_text_version ??
				EU_WITHDRAWAL_WAIVER_TEXT_VERSION,
		});
		Logger.debug(
			{
				userId: payment.userId,
				sessionId: session.id,
				productType: productInfo.type,
				isGift: payment.isGift,
			},
			'Checkout session completed and processed',
		);
	}

	private async validateLocalizedCardEligibility(
		session: Stripe.Checkout.Session,
		payment: Payment,
		productInfo: ProductInfo,
		user: User,
	): Promise<boolean> {
		const requestedCountryCode = session.metadata?.country_code?.trim().toUpperCase() ?? null;
		if (!requestedCountryCode || !this.requiresLocalizedCardEligibility(productInfo)) {
			return true;
		}
		const paymentIntentId = extractId(session.payment_intent);
		if (!paymentIntentId) {
			const inferredPaymentMethodType = this.getDeclaredCheckoutPaymentMethodType(session);
			if (inferredPaymentMethodType && inferredPaymentMethodType !== 'card') {
				Logger.debug(
					{
						sessionId: session.id,
						requestedCountryCode,
						currency: productInfo.currency,
						inferredPaymentMethodType,
					},
					'Skipping localized card eligibility validation because checkout explicitly used a non-card payment method',
				);
				return true;
			}
			const fallbackChargeContext = await this.getLocalizedCheckoutChargeDetailsFromSubscription(session);
			if (fallbackChargeContext?.chargeDetails) {
				const fallbackChargeDetails = fallbackChargeContext.chargeDetails;
				if (fallbackChargeDetails.paymentMethodType !== 'card') {
					Logger.debug(
						{
							sessionId: session.id,
							requestedCountryCode,
							currency: productInfo.currency,
							fallbackPaymentMethodType: fallbackChargeDetails.paymentMethodType,
						},
						'Skipping localized card eligibility validation because subscription fallback resolved to a non-card payment method',
					);
					return true;
				}
				const normalizedFallbackCardCountry = fallbackChargeDetails.cardCountry?.trim().toUpperCase() ?? null;
				if (normalizedFallbackCardCountry === requestedCountryCode) {
					Logger.debug(
						{
							sessionId: session.id,
							requestedCountryCode,
							currency: productInfo.currency,
							cardCountry: normalizedFallbackCardCountry,
						},
						'Validated localized card eligibility from subscription fallback after checkout.session.completed omitted payment_intent',
					);
					return true;
				}
				if (fallbackChargeContext.paymentIntentId && fallbackChargeDetails.chargeId) {
					Logger.warn(
						{
							sessionId: session.id,
							userId: payment.userId,
							paymentIntentId: fallbackChargeContext.paymentIntentId,
							chargeId: fallbackChargeDetails.chargeId,
							requestedCountryCode,
							cardCountry: normalizedFallbackCardCountry,
							currency: productInfo.currency,
						},
						'Rejecting localized checkout because subscription fallback resolved to a card issued outside the requested country',
					);
					await this.rejectLocalizedCardPayment({
						session,
						payment,
						user,
						chargeDetails: fallbackChargeDetails,
						paymentIntentId: fallbackChargeContext.paymentIntentId,
						requestedCountryCode,
						cardCountry: normalizedFallbackCardCountry,
					});
					return false;
				}
			}
			Logger.error(
				{
					sessionId: session.id,
					requestedCountryCode,
					currency: productInfo.currency,
					inferredPaymentMethodType,
					fallbackResolved: Boolean(fallbackChargeContext?.chargeDetails),
					fallbackPaymentIntentId: fallbackChargeContext?.paymentIntentId ?? null,
					fallbackPaymentMethodType: fallbackChargeContext?.chargeDetails?.paymentMethodType ?? null,
					fallbackCardCountry: fallbackChargeContext?.chargeDetails?.cardCountry ?? null,
				},
				'Localized checkout missing payment intent for card eligibility validation',
			);
			throw new StripeError('Localized checkout missing payment intent');
		}
		const chargeDetails = await this.getCheckoutChargeDetails(paymentIntentId);
		if (chargeDetails.paymentMethodType !== 'card') {
			return true;
		}
		const normalizedCardCountry = chargeDetails.cardCountry?.trim().toUpperCase() ?? null;
		if (normalizedCardCountry === requestedCountryCode) {
			return true;
		}
		Logger.warn(
			{
				sessionId: session.id,
				userId: payment.userId,
				paymentIntentId,
				chargeId: chargeDetails.chargeId,
				requestedCountryCode,
				cardCountry: normalizedCardCountry,
				currency: productInfo.currency,
			},
			'Rejecting localized checkout because card issuing country did not match requested country',
		);
		await this.rejectLocalizedCardPayment({
			session,
			payment,
			user,
			chargeDetails,
			paymentIntentId,
			requestedCountryCode,
			cardCountry: normalizedCardCountry,
		});
		return false;
	}

	private async applyCheckoutSideEffects(context: CheckoutFulfilmentContext): Promise<CheckoutSideEffectResult> {
		const checkoutEffectsAppliedKey = this.getCheckoutEffectsAppliedKey(context.session.id);
		if (await this.cacheService.get<boolean>(checkoutEffectsAppliedKey)) {
			Logger.debug({sessionId: context.session.id}, 'Checkout side effects already applied, finishing payment commit');
			return 'continue';
		}
		await this.syncCheckoutUserFields(context);
		const result = context.payment.isGift
			? await this.applyGiftCheckoutEffects(context)
			: await this.applyPremiumCheckoutEffects(context);
		if (result === 'stop') {
			return 'stop';
		}
		await this.cacheService.set(
			checkoutEffectsAppliedKey,
			true,
			StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
		);
		return 'continue';
	}

	private async syncCheckoutUserFields(context: CheckoutFulfilmentContext): Promise<void> {
		const {payment, user, customerId, subscriptionId, isRecurring, productInfo} = context;
		const userUpdates: Partial<UserRow> = {};
		if (customerId && !user.stripeCustomerId) {
			userUpdates.stripe_customer_id = customerId;
		}
		if (subscriptionId && isRecurring) {
			userUpdates.stripe_subscription_id = subscriptionId;
			userUpdates.premium_billing_cycle = productInfo.billingCycle || null;
		}
		if (payment.isGift) {
			userUpdates.has_ever_purchased = true;
		}
		if (Object.keys(userUpdates).length > 0) {
			await this.userRepository.patchUpsert(payment.userId, userUpdates, user.toRow());
		}
	}

	private async applyGiftCheckoutEffects(context: CheckoutFulfilmentContext): Promise<CheckoutSideEffectResult> {
		const {session, payment, user} = context;
		const giftFinalisedKey = this.getCheckoutGiftFinalisedKey(session.id);
		if (await this.cacheService.get<boolean>(giftFinalisedKey)) {
			return 'continue';
		}
		try {
			await this.giftService.finaliseGiftCode(payment.userId);
			await this.cacheService.set(
				giftFinalisedKey,
				true,
				StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
			);
		} catch (error) {
			const latestUser = await this.userRepository.findUnique(payment.userId);
			if (this.didGiftFinalisationApply(user, latestUser)) {
				await this.cacheService.set(
					giftFinalisedKey,
					true,
					StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
				);
				Logger.warn(
					{sessionId: session.id, userId: payment.userId},
					'Gift finalisation threw after state update; marking as applied for idempotency',
				);
			}
			throw error;
		}
		return 'continue';
	}

	private async applyPremiumCheckoutEffects(context: CheckoutFulfilmentContext): Promise<CheckoutSideEffectResult> {
		const premiumAppliedKey = this.getCheckoutPremiumAppliedKey(context.session.id);
		if (await this.cacheService.get<boolean>(premiumAppliedKey)) {
			return 'continue';
		}
		const recovery: CheckoutPremiumGrantRecovery = {};
		try {
			const result = await this.applyCheckoutPremiumGrant({...context, recovery});
			await this.cacheService.set(
				premiumAppliedKey,
				true,
				StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
			);
			return result === 'refunded_duplicate_subscription' ? 'stop' : 'continue';
		} catch (error) {
			const latestUser = await this.userRepository.findUnique(context.payment.userId);
			if (this.didCheckoutPremiumGrantApply(context.user, latestUser, context.productInfo, recovery)) {
				await this.cacheService.set(
					premiumAppliedKey,
					true,
					StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
				);
				Logger.warn(
					{sessionId: context.session.id, userId: context.payment.userId},
					'Premium grant threw after state update; marking as applied for idempotency',
				);
			}
			throw error;
		}
	}

	private async applyCheckoutPremiumGrant(context: CheckoutPremiumGrantContext): Promise<CheckoutPremiumApplyResult> {
		const {session, payment, user, productInfo, subscriptionId, recovery} = context;
		if (productInfo.premiumType === UserPremiumTypes.LIFETIME && user.stripeSubscriptionId && this.stripe) {
			await this.cancelStripeSubscriptionImmediately(user);
		}
		if (this.productRegistry.isRecurringSubscription(productInfo) && subscriptionId) {
			const duplicate = await this.detectDuplicateRecurringSubscription(payment.userId, subscriptionId, productInfo);
			if (duplicate) {
				await this.refundDuplicateRecurringSubscriptionCheckout(context, duplicate.existingSubscriptionId);
				return 'refunded_duplicate_subscription';
			}
		}
		if (productInfo.premiumType === UserPremiumTypes.LIFETIME) {
			await this.premiumService.setPremiumLifetime(payment.userId, true);
			return 'granted';
		}
		if (this.productRegistry.isRecurringSubscription(productInfo)) {
			const periodEnd = await this.resolveCheckoutSubscriptionPeriodEnd(session, productInfo);
			recovery.expectedPremiumUntil = periodEnd;
			await this.premiumService.setPremiumFromSubscriptionPeriod(
				payment.userId,
				productInfo.premiumType,
				periodEnd,
				productInfo.billingCycle || null,
				true,
			);
			return 'granted';
		}
		await this.premiumService.extendPremiumByGift(
			payment.userId,
			productInfo.premiumType,
			'months',
			productInfo.durationMonths,
			true,
		);
		return 'granted';
	}

	private async detectDuplicateRecurringSubscription(
		userId: UserID,
		newSubscriptionId: string,
		productInfo: ProductInfo,
	): Promise<{existingSubscriptionId: string} | null> {
		const billingRepo = getBillingRepository();
		const customers = await billingRepo.customers.findByUserId(userId).catch(() => []);
		for (const customer of customers) {
			const subs = await billingRepo.subscriptions.listByCustomer(customer.provider_id).catch(() => []);
			const duplicate = subs.find((sub) =>
				this.isDuplicateRecurringSubscriptionCandidate(sub, newSubscriptionId, productInfo),
			);
			if (duplicate) {
				return {existingSubscriptionId: duplicate.provider_id};
			}
		}
		return null;
	}

	private isDuplicateRecurringSubscriptionCandidate(
		subscription: BillingSubscriptionRow,
		newSubscriptionId: string,
		productInfo: ProductInfo,
	): boolean {
		if (subscription.provider_id === newSubscriptionId) {
			return false;
		}
		if (!this.isActiveDuplicateSubscriptionStatus(subscription.status)) {
			return false;
		}
		return this.isSameProductRecurring(subscription.primary_price_id, productInfo);
	}

	private isActiveDuplicateSubscriptionStatus(status: BillingSubscriptionRow['status']): boolean {
		return status === 'active' || status === 'trialing' || status === 'past_due';
	}

	private isSameProductRecurring(existingPriceId: string | null, productInfo: ProductInfo): boolean {
		if (!existingPriceId) return false;
		const existingProduct = this.productRegistry.getProduct(existingPriceId);
		if (!existingProduct) return false;
		return existingProduct.type === productInfo.type;
	}

	private async refundDuplicateRecurringSubscriptionCheckout(
		context: CheckoutPremiumGrantContext,
		existingSubscriptionId: string,
	): Promise<void> {
		const {session, payment, productInfo, subscriptionId, customerId, amountTotal, currency} = context;
		if (!subscriptionId) {
			throw new StripeError('Duplicate subscription checkout missing subscription id');
		}
		Logger.warn(
			{
				sessionId: session.id,
				userId: payment.userId,
				newSubscriptionId: subscriptionId,
				existingSubscriptionId,
				productType: productInfo.type,
			},
			'Refusing duplicate recurring subscription checkout; cancelling new Stripe subscription and refunding',
		);
		await this.cancelStripeSubscriptionById(subscriptionId, session.id, payment.userId.toString());
		const chargeId = await this.tryGetLatestChargeForSubscription(subscriptionId);
		if (chargeId) {
			await this.refundChargeForDuplicateSubscription(chargeId, session.id);
		}
		await this.userRepository.updatePayment({
			...payment.toRow(),
			stripe_customer_id: customerId,
			payment_intent_id: extractId(session.payment_intent),
			subscription_id: subscriptionId,
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: amountTotal,
			currency,
			status: 'refunded_duplicate_subscription',
			completed_at: payment.completedAt ?? new Date(),
		});
	}

	private async tryGetLatestChargeForSubscription(subscriptionId: string): Promise<string | null> {
		if (!this.stripe) return null;
		try {
			const subscription = await this.stripe.subscriptions.retrieve(subscriptionId, {
				expand: ['latest_invoice.payments.data.payment'],
			});
			const latestInvoice =
				typeof subscription.latest_invoice === 'string' ? null : (subscription.latest_invoice ?? null);
			const paymentIntentId = getFirstInvoicePaymentIntentId(latestInvoice);
			if (!paymentIntentId) {
				return null;
			}
			const paymentIntent = await this.stripe.paymentIntents.retrieve(paymentIntentId);
			return extractId(paymentIntent.latest_charge);
		} catch (err) {
			Logger.warn({err, subscriptionId}, 'Failed to resolve latest charge for duplicate-subscription refund');
			return null;
		}
	}

	private async refundChargeForDuplicateSubscription(chargeId: string, checkoutSessionId: string): Promise<void> {
		if (!this.stripe) return;
		const refund = await this.stripe.refunds.create(
			{
				charge: chargeId,
				metadata: {
					checkout_session_id: checkoutSessionId,
					rejection_reason: 'duplicate_active_subscription',
				},
			},
			{idempotencyKey: `duplicate-sub-refund:${checkoutSessionId}`},
		);
		try {
			await getBillingRepository().refunds.upsertFromStripe(refund);
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, refundId: refund.id},
				'Mirror upsert failed for duplicate-sub refund; reconciler will heal',
			);
		}
	}

	private async resolveCheckoutSubscriptionPeriodEnd(
		session: Stripe.Checkout.Session,
		productInfo: ProductInfo,
	): Promise<Date> {
		const subscriptionId = extractId(session.subscription);
		if (subscriptionId && this.stripe) {
			try {
				const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
				const candidate = getSubscriptionPremiumPeriodEnd(subscription);
				if (candidate) {
					return candidate;
				}
			} catch (error) {
				Logger.warn(
					{error, sessionId: session.id, subscriptionId},
					'Failed to retrieve Stripe subscription for checkout period_end; falling back to product duration',
				);
			}
		}
		const fallback = new Date(Date.now() + productInfo.durationMonths * 30 * 24 * 60 * 60 * 1000);
		Logger.warn(
			{sessionId: session.id, subscriptionId, durationMonths: productInfo.durationMonths},
			'Using product duration fallback for subscription period_end on checkout fulfilment',
		);
		return fallback;
	}

	private didCheckoutPremiumGrantApply(
		initialUser: User,
		latestUser: User | null,
		productInfo: ProductInfo,
		recovery: CheckoutPremiumGrantRecovery,
	): boolean {
		if (!latestUser) {
			return false;
		}
		if (productInfo.premiumType === UserPremiumTypes.LIFETIME) {
			return latestUser.premiumType === UserPremiumTypes.LIFETIME && latestUser.premiumUntil === null;
		}
		if (latestUser.premiumType !== productInfo.premiumType) {
			return false;
		}
		if (this.productRegistry.isRecurringSubscription(productInfo)) {
			const targetPremiumUntil = recovery.expectedPremiumUntil?.getTime() ?? null;
			const latestPremiumUntil = latestUser.premiumUntil?.getTime() ?? null;
			return targetPremiumUntil !== null && latestPremiumUntil !== null && latestPremiumUntil >= targetPremiumUntil;
		}
		const initialGiftEnd = initialUser.premiumGiftExtensionEndsAt?.getTime() ?? 0;
		const latestGiftEnd = latestUser.premiumGiftExtensionEndsAt?.getTime() ?? 0;
		return productInfo.durationMonths > 0 && latestGiftEnd > initialGiftEnd;
	}

	private didGiftFinalisationApply(initialUser: User, latestUser: User | null): boolean {
		if (!latestUser) {
			return false;
		}
		const initialServerSeq = initialUser.giftInventoryServerSeq ?? 0;
		const latestServerSeq = latestUser.giftInventoryServerSeq ?? 0;
		return latestServerSeq > initialServerSeq;
	}

	private requiresLocalizedCardEligibility(productInfo: ProductInfo): boolean {
		return productInfo.currency !== 'USD' && productInfo.currency !== 'EUR';
	}

	private getDeclaredCheckoutPaymentMethodType(session: Stripe.Checkout.Session): string | null {
		const metadataPaymentMethod = session.metadata?.payment_method?.trim().toLowerCase() ?? null;
		if (metadataPaymentMethod) {
			return metadataPaymentMethod;
		}
		const paymentMethodTypes = session.payment_method_types ?? [];
		if (paymentMethodTypes.length === 1) {
			return paymentMethodTypes[0]?.trim().toLowerCase() ?? null;
		}
		if (paymentMethodTypes.length > 1 && !paymentMethodTypes.some((type) => type.toLowerCase() === 'card')) {
			return paymentMethodTypes[0]?.trim().toLowerCase() ?? null;
		}
		return null;
	}

	private async getLocalizedCheckoutChargeDetailsFromSubscription(session: Stripe.Checkout.Session): Promise<{
		paymentIntentId: string | null;
		chargeDetails: CheckoutChargeDetails | null;
	} | null> {
		if (!this.stripe) {
			return null;
		}
		const subscriptionId = extractId(session.subscription);
		if (!subscriptionId) {
			return null;
		}
		type StripeSubscriptionWithFallbackPaymentState = Stripe.Subscription & {
			default_payment_method?:
				| {
						id?: string;
						type?: string | null;
						card?: {
							country?: string | null;
						} | null;
				  }
				| string
				| null;
			latest_invoice?: Stripe.Invoice | string | null;
		};
		try {
			const subscription = (await this.stripe.subscriptions.retrieve(subscriptionId, {
				expand: ['default_payment_method', 'latest_invoice.payments.data.payment'],
			})) as StripeSubscriptionWithFallbackPaymentState;
			const latestInvoice =
				typeof subscription.latest_invoice === 'string' ? null : (subscription.latest_invoice ?? null);
			const invoicePaymentIntentId = getFirstInvoicePaymentIntentId(latestInvoice);
			if (invoicePaymentIntentId) {
				return {
					paymentIntentId: invoicePaymentIntentId,
					chargeDetails: await this.getCheckoutChargeDetails(invoicePaymentIntentId),
				};
			}
			const defaultPaymentMethod =
				typeof subscription.default_payment_method === 'string' ? null : subscription.default_payment_method;
			if (!defaultPaymentMethod) {
				return null;
			}
			return {
				paymentIntentId: null,
				chargeDetails: {
					chargeId: null,
					paymentMethodType: defaultPaymentMethod.type ?? null,
					cardCountry: defaultPaymentMethod.card?.country ?? null,
				},
			};
		} catch (error) {
			Logger.warn(
				{
					error,
					sessionId: session.id,
					subscriptionId,
				},
				'Failed to load subscription fallback payment details for localized card eligibility',
			);
			return null;
		}
	}

	private async getCheckoutChargeDetails(paymentIntentId: string): Promise<CheckoutChargeDetails> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for localized card eligibility checks');
		}
		try {
			const paymentIntent = await this.stripe.paymentIntents.retrieve(paymentIntentId, {
				expand: ['latest_charge'],
			});
			const latestCharge = paymentIntent.latest_charge;
			if (!latestCharge) {
				throw new StripeError('Payment intent missing latest charge');
			}
			const charge = typeof latestCharge === 'string' ? await this.stripe.charges.retrieve(latestCharge) : latestCharge;
			const paymentMethodDetails = charge.payment_method_details;
			return {
				chargeId: charge.id,
				paymentMethodType: this.getChargePaymentMethodType(paymentMethodDetails),
				cardCountry: paymentMethodDetails?.card?.country ?? null,
			};
		} catch (error) {
			Logger.error({error, paymentIntentId}, 'Failed to load Stripe charge details for localized card eligibility');
			throw error;
		}
	}

	private getChargePaymentMethodType(paymentMethodDetails: Stripe.Charge.PaymentMethodDetails | null): string | null {
		if (!paymentMethodDetails) {
			return null;
		}
		if (paymentMethodDetails.type) {
			return paymentMethodDetails.type;
		}
		if (paymentMethodDetails.card) {
			return 'card';
		}
		if ('pix' in paymentMethodDetails && paymentMethodDetails.pix) {
			return 'pix';
		}
		if ('upi' in paymentMethodDetails && paymentMethodDetails.upi) {
			return 'upi';
		}
		return null;
	}

	private async rejectLocalizedCardPayment({
		session,
		payment,
		user,
		chargeDetails,
		paymentIntentId,
		requestedCountryCode,
		cardCountry,
	}: {
		session: Stripe.Checkout.Session;
		payment: Payment;
		user: User;
		chargeDetails: CheckoutChargeDetails;
		paymentIntentId: string;
		requestedCountryCode: string;
		cardCountry: string | null;
	}): Promise<void> {
		const subscriptionId = extractId(session.subscription);
		if (subscriptionId) {
			await this.cancelStripeSubscriptionById(subscriptionId, session.id, user.id.toString());
		}
		if (chargeDetails.chargeId) {
			await this.refundChargeForLocalizedCardMismatch({
				chargeId: chargeDetails.chargeId,
				checkoutSessionId: session.id,
				requestedCountryCode,
				cardCountry,
			});
		} else {
			Logger.warn(
				{
					sessionId: session.id,
					userId: user.id.toString(),
					paymentIntentId,
					requestedCountryCode,
					cardCountry,
				},
				'Skipping localized card refund because Stripe did not surface a charge id',
			);
		}
		await this.userRepository.updatePayment({
			...payment.toRow(),
			stripe_customer_id: extractId(session.customer),
			payment_intent_id: paymentIntentId,
			subscription_id: subscriptionId,
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: session.amount_total ?? payment.amountCents,
			currency: session.currency ?? payment.currency,
			status: 'failed',
			completed_at: payment.completedAt ?? new Date(),
		});
	}

	private async handleDonationCheckoutCompleted(session: Stripe.Checkout.Session): Promise<void> {
		const email = session.metadata?.donation_email;
		if (!email) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing email in metadata');
			throw new StripeError('Donation checkout missing email');
		}
		const customerId = extractId(session.customer);
		if (!customerId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing customer');
			throw new StripeError('Donation checkout missing customer id');
		}
		const isRecurring = session.mode === 'subscription';
		const subscriptionId = extractId(session.subscription);
		if (isRecurring && !subscriptionId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing subscription id');
			throw new StripeError('Donation checkout missing subscription id');
		}
		const customerDetails = await this.loadDonationCustomerDetails(customerId);
		const subscriptionDetails = isRecurring
			? await this.loadDonationSubscriptionDetails(subscriptionId)
			: {amountCents: null, currency: null, interval: null, currentPeriodEnd: null, cancelAt: null};
		const existingDonor = await this.donationRepository.findDonorByEmail(email);
		if (existingDonor) {
			await this.donationRepository.updateDonorSubscription(email, {
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				stripeSubscriptionId: subscriptionId,
				subscriptionAmountCents: subscriptionDetails.amountCents,
				subscriptionCurrency: subscriptionDetails.currency,
				subscriptionInterval: subscriptionDetails.interval,
				subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
				subscriptionCancelAt: subscriptionDetails.cancelAt,
			});
		} else {
			await this.donationRepository.createDonor({
				email,
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				stripeSubscriptionId: subscriptionId,
				subscriptionAmountCents: subscriptionDetails.amountCents,
				subscriptionCurrency: subscriptionDetails.currency,
				subscriptionInterval: subscriptionDetails.interval,
				subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
				subscriptionCancelAt: subscriptionDetails.cancelAt,
			});
		}
		const encodedEmail = encodeURIComponent(email);
		const manageUrl = `${Config.endpoints.marketing}/donate/manage?email=${encodedEmail}`;
		if (isRecurring) {
			const recurringAmountCents = subscriptionDetails.amountCents;
			const recurringCurrency = subscriptionDetails.currency;
			const recurringInterval = subscriptionDetails.interval;
			if (recurringAmountCents == null || !recurringCurrency || !recurringInterval) {
				Logger.error({sessionId: session.id, subscriptionId}, 'Donation subscription details incomplete');
				throw new StripeError('Donation subscription details incomplete');
			}
			await this.emailService.sendDonationConfirmation(
				email,
				recurringAmountCents,
				recurringCurrency,
				recurringInterval,
				manageUrl,
				null,
			);
		} else {
			const oneTimeAmountCents = session.amount_total;
			const oneTimeCurrency = session.currency;
			if (oneTimeAmountCents == null || !oneTimeCurrency) {
				Logger.error(
					{sessionId: session.id, amountTotal: session.amount_total, currency: session.currency},
					'Donation checkout missing amount or currency',
				);
				throw new StripeError('Donation checkout missing amount or currency');
			}
			await this.emailService.sendDonationConfirmation(
				email,
				oneTimeAmountCents,
				oneTimeCurrency,
				'once',
				manageUrl,
				null,
			);
		}
		Logger.info(
			{
				email,
				customerId,
				subscriptionId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				isRecurring,
				amountCents: isRecurring ? subscriptionDetails.amountCents : session.amount_total,
				currency: isRecurring ? subscriptionDetails.currency : session.currency,
				interval: subscriptionDetails.interval,
			},
			'Donation checkout completed',
		);
	}

	private async cancelStripeSubscriptionImmediately(user: User): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for immediate cancellation');
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeError('User missing subscription id for immediate cancellation');
		}
		const canceledSubscription = await this.stripe.subscriptions.cancel(user.stripeSubscriptionId, {
			invoice_now: false,
			prorate: false,
		});
		try {
			await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
				knownUserId: user.id,
				snapshotCapturedAt: new Date(),
			});
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, subId: canceledSubscription.id},
				'Mirror upsert failed after Stripe write; reconciler will heal',
			);
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, {
			stripe_subscription_id: null,
			premium_billing_cycle: null,
			premium_will_cancel: false,
		});
		await this.dispatchUser(updatedUser);
		Logger.debug({userId: user.id}, 'Canceled active subscription due to lifetime grant');
	}

	private async cancelStripeSubscriptionById(
		subscriptionId: string,
		checkoutSessionId: string,
		userId: string,
	): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for subscription cancellation');
		}
		try {
			const canceledSubscription = await this.stripe.subscriptions.cancel(
				subscriptionId,
				{invoice_now: false, prorate: false},
				{idempotencyKey: `localized-card-country-cancel:${checkoutSessionId}`},
			);
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: canceledSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
		} catch (error) {
			if (this.isMissingOrCancelledSubscriptionError(error)) {
				Logger.warn(
					{subscriptionId, checkoutSessionId, userId},
					'Stripe subscription was already unavailable while rejecting localized card payment',
				);
				return;
			}
			Logger.error(
				{error, subscriptionId, checkoutSessionId, userId},
				'Failed to cancel localized card payment subscription',
			);
			throw error;
		}
	}

	private async refundChargeForLocalizedCardMismatch({
		chargeId,
		checkoutSessionId,
		requestedCountryCode,
		cardCountry,
	}: {
		chargeId: string;
		checkoutSessionId: string;
		requestedCountryCode: string;
		cardCountry: string | null;
	}): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for localized card refund');
		}
		const refund = await this.stripe.refunds.create(
			{
				charge: chargeId,
				metadata: {
					checkout_session_id: checkoutSessionId,
					rejection_reason: 'localized_card_country_mismatch',
					expected_country: requestedCountryCode,
					actual_country: cardCountry ?? 'unknown',
				},
			},
			{idempotencyKey: `localized-card-country-refund:${checkoutSessionId}`},
		);
		try {
			await getBillingRepository().refunds.upsertFromStripe(refund);
		} catch (mirrorErr) {
			Logger.error({mirrorErr, refundId: refund.id}, 'Mirror upsert failed after Stripe write; reconciler will heal');
		}
	}

	private isMissingOrCancelledSubscriptionError(error: unknown): boolean {
		if (!(error instanceof Error)) {
			return false;
		}
		const normalizedMessage = error.message.toLowerCase();
		return (
			normalizedMessage.includes('no such subscription') ||
			normalizedMessage.includes('cannot cancel a canceled subscription') ||
			normalizedMessage.includes('cannot cancel a cancelled subscription') ||
			normalizedMessage.includes('subscription is canceled') ||
			normalizedMessage.includes('subscription is cancelled')
		);
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}

	private async tryRecoverPaymentFromSessionMetadata(session: Stripe.Checkout.Session): Promise<Payment | null> {
		const userIdStr = session.client_reference_id ?? session.metadata?.user_id;
		const priceId = session.metadata?.price_id;
		const productType = session.metadata?.product_type;
		const isGift = session.metadata?.is_gift === 'true';
		const euWithdrawalWaiverRequired = session.metadata?.eu_withdrawal_waiver_required === 'true';
		const euWithdrawalWaiverAccepted =
			session.metadata?.eu_withdrawal_waiver_accepted === 'true' || session.consent?.terms_of_service === 'accepted';
		const euWithdrawalWaiverAcceptedAt = session.metadata?.eu_withdrawal_waiver_accepted_at
			? new Date(session.metadata.eu_withdrawal_waiver_accepted_at)
			: null;
		if (!userIdStr || !priceId || !productType) {
			return null;
		}
		let userId: UserID;
		try {
			userId = createUserID(BigInt(userIdStr));
		} catch {
			Logger.error({sessionId: session.id, userIdStr}, 'Invalid user ID in checkout session metadata');
			return null;
		}
		try {
			await this.userRepository.createPayment({
				checkout_session_id: session.id,
				user_id: userId,
				price_id: priceId,
				product_type: productType,
				status: 'pending',
				is_gift: isGift,
				created_at: new Date(),
				purchase_geoip_country_code: session.metadata?.purchase_geoip_country_code ?? null,
				purchase_client_country_code: session.metadata?.purchase_client_country_code ?? null,
				eu_withdrawal_waiver_required: euWithdrawalWaiverRequired,
				eu_withdrawal_waiver_accepted: euWithdrawalWaiverAccepted,
				eu_withdrawal_waiver_accepted_at: Number.isNaN(euWithdrawalWaiverAcceptedAt?.getTime() ?? Number.NaN)
					? session.consent?.terms_of_service === 'accepted'
						? new Date()
						: null
					: euWithdrawalWaiverAcceptedAt,
				eu_withdrawal_waiver_text_version:
					session.metadata?.eu_withdrawal_waiver_text_version ?? EU_WITHDRAWAL_WAIVER_TEXT_VERSION,
			});
			Logger.warn(
				{sessionId: session.id, userId: userId.toString(), priceId, productType},
				'Recovered missing payment record from checkout session metadata',
			);
			return await this.userRepository.getPaymentByCheckoutSession(session.id);
		} catch (error) {
			Logger.error(
				{sessionId: session.id, userId: userId.toString(), error},
				'Failed to recover payment record from checkout session metadata',
			);
			return null;
		}
	}

	private getCheckoutEffectsAppliedKey(checkoutSessionId: string): string {
		return `stripe:checkout:effects:applied:${checkoutSessionId}`;
	}

	private getCheckoutPremiumAppliedKey(checkoutSessionId: string): string {
		return `stripe:checkout:premium:applied:${checkoutSessionId}`;
	}

	private getCheckoutGiftFinalisedKey(checkoutSessionId: string): string {
		return `stripe:checkout:gift:finalised:${checkoutSessionId}`;
	}

	private async loadDonationCustomerDetails(customerId: string): Promise<DonationCustomerDetails> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation customer lookup');
		}
		try {
			const customer = await this.stripe.customers.retrieve(customerId);
			if (customer && !customer.deleted) {
				const businessName = customer.name ?? null;
				const primaryTaxId = customer.tax_ids?.data?.[0] ?? null;
				return {
					businessName,
					taxId: primaryTaxId?.value ?? null,
					taxIdType: primaryTaxId?.type ?? null,
				};
			}
		} catch (error) {
			Logger.error({error, customerId}, 'Failed to retrieve customer details');
			throw error;
		}
		throw new StripeError('Donation customer not found');
	}

	private async loadDonationSubscriptionDetails(subscriptionId: string | null): Promise<DonationSubscriptionDetails> {
		if (!subscriptionId) {
			throw new StripeError('Donation subscription id missing for lookup');
		}
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation subscription lookup');
		}
		try {
			const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
			const item = getPrimarySubscriptionItem(subscription);
			if (!item) {
				Logger.error({subscriptionId}, 'Subscription has no items for donation checkout');
				throw new StripeError('Donation subscription has no items');
			}
			if (!item.price?.recurring || item.price.unit_amount == null || !item.price.currency) {
				Logger.error({subscriptionId}, 'Donation subscription missing pricing details');
				throw new StripeError('Donation subscription missing pricing details');
			}
			const currentPeriodEnd = getSubscriptionItemPeriodEnd(item);
			if (!currentPeriodEnd) {
				Logger.error({subscriptionId}, 'Donation subscription missing period end');
				throw new StripeError('Donation subscription missing period end');
			}
			const cancelAt = subscription.cancel_at ? new Date(subscription.cancel_at * 1000) : null;
			return {
				amountCents: item.price.unit_amount,
				currency: item.price.currency,
				interval: item.price.recurring.interval,
				currentPeriodEnd,
				cancelAt,
			};
		} catch (error) {
			Logger.error({error, subscriptionId}, 'Failed to retrieve subscription details');
			throw error;
		}
	}
}
