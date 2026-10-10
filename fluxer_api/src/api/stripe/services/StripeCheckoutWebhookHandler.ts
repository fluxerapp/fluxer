// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {BillingSubscriptionRow} from '@app/api/database/types/BillingTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {Payment} from '@app/api/models/Payment';
import type {User} from '@app/api/models/User';
import type {ProductInfo, ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {
	getFirstInvoicePaymentIntentId,
	getPrimarySubscriptionItem,
	getSubscriptionCurrentPeriodStart,
	getSubscriptionItemPeriodEnd,
	getSubscriptionPremiumPeriodEnd,
} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {EU_WITHDRAWAL_WAIVER_TEXT_VERSION} from '@app/api/stripe/services/StripeCheckoutService';
import type {StripeGiftService} from '@app/api/stripe/services/StripeGiftService';
import type {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

interface DonationCustomerDetails {
	businessName: string | undefined;
	taxId: string | undefined;
	taxIdType: string | undefined;
}

interface DonationSubscriptionDetails {
	amountCents: number | null;
	currency: string | null;
	interval: string | null;
	currentPeriodEnd: Date | null;
	cancelAt: Date | null;
	status: string | null;
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
		private userRepository: UserRepository,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private productRegistry: ProductRegistry,
		private cacheService: ICacheService,
		private giftService: StripeGiftService,
		private premiumService: StripePremiumService,
		private donationRepository: DonationRepository,
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
			Logger.info({sessionId: session.id}, 'Processing async payment succeeded for donation checkout session');
			await this.handleDonationCheckoutCompleted(session);
			return;
		}
		Logger.info({sessionId: session.id}, 'Processing async payment succeeded for checkout session');
		await this.fulfilCheckoutSession(session);
	}

	async handleAsyncPaymentFailed(session: Stripe.Checkout.Session): Promise<void> {
		if (session.metadata?.is_donation === 'true') {
			Logger.warn(
				{
					sessionId: session.id,
					email: session.metadata?.donation_email,
					paymentStatus: session.payment_status,
				},
				'Async payment failed for donation checkout session; no confirmation sent',
			);
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
			const period = await this.resolveCheckoutSubscriptionPeriod(session, productInfo);
			recovery.expectedPremiumUntil = period.end;
			await this.premiumService.setPremiumFromSubscriptionPeriod(
				payment.userId,
				productInfo.premiumType,
				period.end,
				productInfo.billingCycle || null,
				true,
				null,
				period.start,
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
		const latestUser = await this.userRepository.findUnique(payment.userId);
		if (latestUser && latestUser.stripeSubscriptionId === subscriptionId) {
			const restoredUser = await this.userRepository.patchUpsert(
				payment.userId,
				{stripe_subscription_id: existingSubscriptionId},
				latestUser.toRow(),
			);
			await this.dispatchUser(restoredUser);
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

	private async resolveCheckoutSubscriptionPeriod(
		session: Stripe.Checkout.Session,
		productInfo: ProductInfo,
	): Promise<{start: Date | null; end: Date}> {
		const subscriptionId = extractId(session.subscription);
		if (subscriptionId && this.stripe) {
			try {
				const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
				const candidate = getSubscriptionPremiumPeriodEnd(subscription);
				if (candidate) {
					return {start: getSubscriptionCurrentPeriodStart(subscription), end: candidate};
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
		return {start: null, end: fallback};
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

	private async handleDonationCheckoutCompleted(session: Stripe.Checkout.Session): Promise<void> {
		const email = session.metadata?.donation_email?.trim().toLowerCase();
		if (!email) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing email in metadata');
			throw new StripeError('Donation checkout missing email');
		}
		const customerId = extractId(session.customer);
		if (!customerId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing customer');
			throw new StripeError('Donation checkout missing customer id');
		}
		const donationLocale = session.metadata?.donation_locale ?? null;
		const isRecurring = session.mode === 'subscription';
		const subscriptionId = extractId(session.subscription);
		if (isRecurring && !subscriptionId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing subscription id');
			throw new StripeError('Donation checkout missing subscription id');
		}
		const customerDetails = await this.loadDonationCustomerDetails(
			customerId,
			session.metadata?.is_business === 'true',
		);
		const subscriptionDetails = isRecurring
			? await this.loadDonationSubscriptionDetails(subscriptionId)
			: {amountCents: null, currency: null, interval: null, currentPeriodEnd: null, cancelAt: null, status: null};
		const existingDonor = await this.donationRepository.findDonorByEmail(email);
		const subscriptionUpdate = {
			stripeSubscriptionId: subscriptionId,
			subscriptionAmountCents: subscriptionDetails.amountCents,
			subscriptionCurrency: subscriptionDetails.currency,
			subscriptionInterval: subscriptionDetails.interval,
			subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
			subscriptionCancelAt: subscriptionDetails.cancelAt,
			subscriptionStatus: subscriptionDetails.status,
		};
		if (existingDonor && isRecurring) {
			await this.donationRepository.updateDonorSubscription(email, {
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				...subscriptionUpdate,
			});
		} else if (existingDonor) {
			const subscriptionCustomerId = existingDonor.stripeSubscriptionId ? existingDonor.stripeCustomerId : null;
			await this.donationRepository.updateDonorCustomerDetails(email, {
				stripeCustomerId: subscriptionCustomerId ?? customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
			});
			await this.donationRepository.linkDonorStripeCustomer(email, customerId);
		} else {
			await this.donationRepository.createDonor({
				email,
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				...subscriptionUpdate,
			});
		}
		if (session.payment_status === 'unpaid') {
			Logger.info(
				{sessionId: session.id, email, isRecurring, paymentStatus: session.payment_status},
				'Donation checkout completed with unpaid status; deferring confirmation until settlement',
			);
			return;
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
			await this.sendDonationConfirmationOnce({
				sessionId: session.id,
				email,
				amountCents: recurringAmountCents,
				currency: recurringCurrency,
				interval: recurringInterval,
				manageUrl,
				locale: donationLocale,
			});
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
			await this.sendDonationConfirmationOnce({
				sessionId: session.id,
				email,
				amountCents: oneTimeAmountCents,
				currency: oneTimeCurrency,
				interval: 'once',
				manageUrl,
				locale: donationLocale,
			});
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

	private async sendDonationConfirmationOnce({
		sessionId,
		email,
		amountCents,
		currency,
		interval,
		manageUrl,
		locale,
	}: {
		sessionId: string;
		email: string;
		amountCents: number;
		currency: string;
		interval: string;
		manageUrl: string;
		locale: string | null;
	}): Promise<void> {
		const confirmationSentKey = this.getDonationConfirmationSentKey(sessionId);
		if (await this.cacheService.get<boolean>(confirmationSentKey)) {
			Logger.debug({sessionId}, 'Donation confirmation already sent for checkout session');
			return;
		}
		const sent = await this.emailService.sendDonationConfirmation(
			email,
			amountCents,
			currency,
			interval,
			manageUrl,
			locale,
		);
		if (!sent) {
			Logger.error({sessionId, email, interval}, 'Failed to send donation confirmation email');
			throw new StripeError('Failed to send donation confirmation email');
		}
		await this.cacheService.set(
			confirmationSentKey,
			true,
			StripeCheckoutWebhookHandler.CHECKOUT_EFFECTS_APPLIED_TTL_SECONDS,
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

	private getDonationConfirmationSentKey(checkoutSessionId: string): string {
		return `stripe:checkout:donation:confirmation:sent:${checkoutSessionId}`;
	}

	private async loadDonationCustomerDetails(customerId: string, isBusiness: boolean): Promise<DonationCustomerDetails> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation customer lookup');
		}
		try {
			const customer = await this.stripe.customers.retrieve(customerId, {expand: ['tax_ids']});
			if (customer && !customer.deleted) {
				const primaryTaxId = customer.tax_ids?.data?.[0] ?? null;
				const isBusinessDonor = isBusiness || primaryTaxId !== null;
				return {
					businessName: isBusinessDonor ? (customer.name ?? undefined) : undefined,
					taxId: primaryTaxId?.value ?? undefined,
					taxIdType: primaryTaxId?.type ?? undefined,
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
				status: subscription.status,
			};
		} catch (error) {
			Logger.error({error, subscriptionId}, 'Failed to retrieve subscription details');
			throw error;
		}
	}
}
