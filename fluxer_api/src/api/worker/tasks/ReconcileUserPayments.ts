// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {createGiftCodeRow, generateUniqueGiftCode, mapGiftDurationMonthsToFields} from '@app/api/models/GiftCode';
import type {Payment} from '@app/api/models/Payment';
import type {User} from '@app/api/models/User';
import type {ProductInfo} from '@app/api/stripe/ProductRegistry';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {syncStripeCustomerEmail} from '@app/api/stripe/StripeCustomer';
import {extractId} from '@app/api/stripe/StripeUtils';
import {bumpGiftInventory} from '@app/api/user/GiftInventory';
import type {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';

const STALE_PENDING_THRESHOLD_MS = 10 * 60 * 1000;

async function reconcileCompletedGiftWithoutCode(payment: Payment, purchaser: User, stripe: Stripe): Promise<void> {
	const {userRepository, gatewayService} = getWorkerDependencies();
	if (payment.paymentIntentId) {
		const existingGift = await userRepository.findGiftCodeByPaymentIntent(payment.paymentIntentId);
		if (existingGift) {
			await userRepository.updatePayment({
				checkout_session_id: payment.checkoutSessionId,
				gift_code: existingGift.code,
			});
			Logger.info(
				{checkoutSessionId: payment.checkoutSessionId, giftCode: existingGift.code},
				'Linked existing gift code to payment during reconciliation',
			);
			return;
		}
	}
	let paymentIntentId = payment.paymentIntentId;
	if (!paymentIntentId) {
		try {
			const session = await stripe.checkout.sessions.retrieve(payment.checkoutSessionId);
			paymentIntentId = extractId(session.payment_intent);
		} catch (error) {
			Logger.warn(
				{checkoutSessionId: payment.checkoutSessionId, error},
				'Failed to retrieve checkout session from Stripe during gift code reconciliation',
			);
			return;
		}
	}
	if (paymentIntentId) {
		const existingGift = await userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (existingGift) {
			await userRepository.updatePayment({
				checkout_session_id: payment.checkoutSessionId,
				gift_code: existingGift.code,
			});
			Logger.info(
				{checkoutSessionId: payment.checkoutSessionId, paymentIntentId, giftCode: existingGift.code},
				'Linked existing gift code (found via payment intent) to payment during reconciliation',
			);
			return;
		}
	}
	const productRegistry = getProductRegistry();
	const productInfo = payment.priceId ? productRegistry.getProduct(payment.priceId) : null;
	if (!productInfo) {
		Logger.warn(
			{checkoutSessionId: payment.checkoutSessionId, priceId: payment.priceId},
			'Cannot reconcile gift code: unknown price ID',
		);
		return;
	}
	const code = await createGiftCodeForPayment(payment, purchaser, paymentIntentId, productInfo);
	await userRepository.updatePayment({
		checkout_session_id: payment.checkoutSessionId,
		gift_code: code,
	});
	await bumpGiftInventory(userRepository, gatewayService, purchaser.id);
	Logger.info(
		{checkoutSessionId: payment.checkoutSessionId, code, userId: purchaser.id.toString()},
		'Gift code created during payment reconciliation',
	);
}

async function reconcileStuckGiftPayment(payment: Payment, purchaser: User, stripe: Stripe): Promise<void> {
	const {userRepository} = getWorkerDependencies();
	const timeSinceCreation = Date.now() - payment.createdAt.getTime();
	if (timeSinceCreation < STALE_PENDING_THRESHOLD_MS) {
		return;
	}
	let session: Stripe.Checkout.Session;
	try {
		session = await stripe.checkout.sessions.retrieve(payment.checkoutSessionId);
	} catch (error) {
		Logger.warn(
			{checkoutSessionId: payment.checkoutSessionId, error},
			'Failed to retrieve Stripe checkout session during stuck payment reconciliation',
		);
		return;
	}
	if (session.payment_status !== 'paid') {
		if (session.status === 'expired') {
			await userRepository.updatePayment({
				checkout_session_id: payment.checkoutSessionId,
				status: 'failed',
			});
			Logger.info(
				{checkoutSessionId: payment.checkoutSessionId},
				'Marked expired checkout session as failed during reconciliation',
			);
		}
		return;
	}
	const paymentIntentId = extractId(session.payment_intent);
	const customerId = extractId(session.customer);
	let giftCode: string | null = null;
	if (paymentIntentId) {
		const existingGift = await userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (existingGift) {
			giftCode = existingGift.code;
		}
	}
	if (!giftCode) {
		const productRegistry = getProductRegistry();
		const productInfo = payment.priceId ? productRegistry.getProduct(payment.priceId) : null;
		if (!productInfo) {
			Logger.warn(
				{checkoutSessionId: payment.checkoutSessionId, priceId: payment.priceId},
				'Cannot reconcile stuck gift payment: unknown price ID',
			);
			return;
		}
		giftCode = await createGiftCodeForPayment(payment, purchaser, paymentIntentId, productInfo);
	}
	await userRepository.updatePayment({
		checkout_session_id: payment.checkoutSessionId,
		stripe_customer_id: customerId,
		payment_intent_id: paymentIntentId,
		amount_cents: session.amount_total ?? payment.amountCents,
		currency: session.currency ?? payment.currency,
		status: 'completed',
		completed_at: new Date(),
		gift_code: giftCode,
	});
	if (customerId && !purchaser.stripeCustomerId) {
		await userRepository.patchUpsert(purchaser.id, {stripe_customer_id: customerId}, purchaser.toRow());
	}
	if (giftCode) {
		await bumpGiftInventory(userRepository, getWorkerDependencies().gatewayService, purchaser.id, {
			has_ever_purchased: true,
		});
	}
	Logger.info(
		{checkoutSessionId: payment.checkoutSessionId, giftCode, userId: purchaser.id.toString()},
		'Reconciled stuck gift payment',
	);
}

async function reconcileMissingPaymentRecords(
	user: User,
	existingPayments: Array<Payment>,
	stripe: Stripe,
	paymentRepository: PaymentRepository,
): Promise<void> {
	if (!user.stripeCustomerId) {
		return;
	}
	const existingCheckoutSessionIds = new Set(existingPayments.map((p) => p.checkoutSessionId));
	let sessions: Stripe.ApiList<Stripe.Checkout.Session>;
	try {
		sessions = await stripe.checkout.sessions.list({
			customer: user.stripeCustomerId,
			status: 'complete',
			limit: 50,
		});
	} catch (error) {
		Logger.warn(
			{userId: user.id.toString(), stripeCustomerId: user.stripeCustomerId, error},
			'Failed to list Stripe checkout sessions during payment reconciliation',
		);
		return;
	}
	for (const session of sessions.data) {
		if (existingCheckoutSessionIds.has(session.id)) {
			continue;
		}
		if (session.payment_status !== 'paid') {
			continue;
		}
		const existingPayment = await paymentRepository.getPaymentByCheckoutSession(session.id);
		if (existingPayment) {
			continue;
		}
		const priceId = session.metadata?.price_id;
		const productType = session.metadata?.product_type;
		const isGift = session.metadata?.is_gift === 'true';
		if (!priceId || !productType) {
			continue;
		}
		const paymentIntentId = extractId(session.payment_intent);
		const customerId = extractId(session.customer);
		const subscriptionId = extractId(session.subscription);
		let giftCode: string | null = null;
		if (isGift && paymentIntentId) {
			const existingGift = await getWorkerDependencies().userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
			if (existingGift) {
				giftCode = existingGift.code;
			}
		}
		await paymentRepository.createPayment({
			checkout_session_id: session.id,
			user_id: user.id,
			price_id: priceId,
			product_type: productType,
			status: 'pending',
			is_gift: isGift,
			created_at: new Date(session.created * 1000),
		});
		await paymentRepository.updatePayment({
			checkout_session_id: session.id,
			stripe_customer_id: customerId,
			payment_intent_id: paymentIntentId,
			subscription_id: subscriptionId,
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: session.amount_total ?? 0,
			currency: session.currency ?? '',
			status: 'completed',
			completed_at: new Date(),
			gift_code: giftCode,
		});
		Logger.info(
			{checkoutSessionId: session.id, userId: user.id.toString(), isGift, giftCode},
			'Created missing payment record during reconciliation',
		);
		if (isGift && !giftCode) {
			const recoveredPayment = await paymentRepository.getPaymentByCheckoutSession(session.id);
			if (recoveredPayment) {
				await reconcileCompletedGiftWithoutCode(recoveredPayment, user, stripe);
			}
		}
	}
}

const reconcileUserPayments: WorkerTaskHandler = async (payload, helpers) => {
	const {paymentRepository, userRepository, stripe} = getWorkerDependencies();
	if (!stripe) {
		helpers.logger.debug('Stripe is disabled, skipping user payment reconciliation');
		return;
	}
	const userIdStr = payload.userId as string;
	if (!userIdStr) {
		helpers.logger.warn({payload}, 'Payment reconciliation task missing userId');
		return;
	}
	const userId = createUserID(BigInt(userIdStr));
	const user = await userRepository.findUnique(userId);
	if (!user) {
		helpers.logger.debug({userId: userIdStr}, 'User not found for payment reconciliation');
		return;
	}
	if (user.isBot) {
		return;
	}
	if (!user.stripeCustomerId) {
		return;
	}
	try {
		await syncStripeCustomerEmail(stripe, user);
	} catch (error) {
		Logger.warn({userId: userIdStr, error}, 'Failed to sync Stripe customer email during payment reconciliation');
	}
	const payments = await paymentRepository.findPaymentsByUserId(userId);
	let reconciledCount = 0;
	for (const payment of payments) {
		if (!payment.isGift) {
			continue;
		}
		try {
			if (payment.status === 'completed' && !payment.giftCode) {
				await reconcileCompletedGiftWithoutCode(payment, user, stripe);
				reconciledCount += 1;
			} else if (payment.status === 'pending' || payment.status === 'awaiting_settlement') {
				await reconcileStuckGiftPayment(payment, user, stripe);
				reconciledCount += 1;
			}
		} catch (error) {
			Logger.error(
				{checkoutSessionId: payment.checkoutSessionId, userId: userIdStr, error},
				'Failed to reconcile individual gift payment',
			);
		}
	}
	try {
		await reconcileMissingPaymentRecords(user, payments, stripe, paymentRepository);
	} catch (error) {
		Logger.error({userId: userIdStr, error}, 'Failed to reconcile missing payment records from Stripe');
	}
	if (reconciledCount > 0) {
		helpers.logger.info({userId: userIdStr, reconciledCount}, 'Finished user payment reconciliation');
	}
};

export default reconcileUserPayments;

async function createGiftCodeForPayment(
	payment: Payment,
	purchaser: User,
	paymentIntentId: string | null,
	productInfo: ProductInfo,
): Promise<string> {
	const {userRepository} = getWorkerDependencies();
	const code = await generateUniqueGiftCode((c) => userRepository.findGiftCode(c));
	const duration = mapGiftDurationMonthsToFields(productInfo.durationMonths);
	await userRepository.createGiftCode(
		createGiftCodeRow({
			code: code,
			durationType: duration.durationType,
			durationQuantity: duration.durationQuantity,
			createdAt: payment.createdAt,
			createdByUserId: purchaser.id,
			paymentIntentId: paymentIntentId,
			checkoutSessionId: payment.checkoutSessionId,
		}),
	);
	await userRepository.linkGiftCodeToCheckoutSession(code, payment.checkoutSessionId);
	return code;
}
