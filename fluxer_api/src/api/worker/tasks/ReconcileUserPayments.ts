// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {mapGiftDurationMonthsToFields} from '@app/api/models/GiftCode';
import type {Payment} from '@app/api/models/Payment';
import type {User} from '@app/api/models/User';
import {ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {extractId} from '@app/api/stripe/StripeUtils';
import type {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';

const STALE_PENDING_THRESHOLD_MS = 10 * 60 * 1000;

async function generateUniqueGiftCode(findGiftCode: (code: string) => Promise<unknown>): Promise<string> {
	let code: string;
	let exists = true;
	while (exists) {
		code = RandomUtils.randomString(32);
		const existing = await findGiftCode(code);
		exists = !!existing;
	}
	return code!;
}

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
	const productRegistry = new ProductRegistry();
	const productInfo = payment.priceId ? productRegistry.getProduct(payment.priceId) : null;
	if (!productInfo) {
		Logger.warn(
			{checkoutSessionId: payment.checkoutSessionId, priceId: payment.priceId},
			'Cannot reconcile gift code: unknown price ID',
		);
		return;
	}
	const code = await generateUniqueGiftCode((c) => userRepository.findGiftCode(c));
	const duration = mapGiftDurationMonthsToFields(productInfo.durationMonths);
	await userRepository.createGiftCode({
		code,
		duration_months: null,
		duration_type: duration.durationType,
		duration_quantity: duration.durationQuantity,
		created_at: payment.createdAt,
		created_by_user_id: purchaser.id,
		redeemed_at: null,
		redeemed_by_user_id: null,
		stripe_payment_intent_id: paymentIntentId,
		visionary_sequence_number: null,
		checkout_session_id: payment.checkoutSessionId,
		version: 1,
	});
	await userRepository.linkGiftCodeToCheckoutSession(code, payment.checkoutSessionId);
	await userRepository.updatePayment({
		checkout_session_id: payment.checkoutSessionId,
		gift_code: code,
	});
	const currentUser = await userRepository.findUnique(purchaser.id);
	if (currentUser) {
		const updatedUser = await userRepository.patchUpsert(
			purchaser.id,
			{gift_inventory_server_seq: (currentUser.giftInventoryServerSeq ?? 0) + 1},
			currentUser.toRow(),
		);
		await gatewayService.dispatchPresence({
			userId: updatedUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser),
		});
	}
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
		const productRegistry = new ProductRegistry();
		const productInfo = payment.priceId ? productRegistry.getProduct(payment.priceId) : null;
		if (!productInfo) {
			Logger.warn(
				{checkoutSessionId: payment.checkoutSessionId, priceId: payment.priceId},
				'Cannot reconcile stuck gift payment: unknown price ID',
			);
			return;
		}
		const code = await generateUniqueGiftCode((c) => userRepository.findGiftCode(c));
		const duration = mapGiftDurationMonthsToFields(productInfo.durationMonths);
		await userRepository.createGiftCode({
			code,
			duration_months: null,
			duration_type: duration.durationType,
			duration_quantity: duration.durationQuantity,
			created_at: payment.createdAt,
			created_by_user_id: purchaser.id,
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: paymentIntentId,
			visionary_sequence_number: null,
			checkout_session_id: payment.checkoutSessionId,
			version: 1,
		});
		giftCode = code;
		await userRepository.linkGiftCodeToCheckoutSession(code, payment.checkoutSessionId);
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
		const currentUser = await userRepository.findUnique(purchaser.id);
		if (currentUser) {
			const updatedUser = await userRepository.patchUpsert(
				purchaser.id,
				{
					has_ever_purchased: true,
					gift_inventory_server_seq: (currentUser.giftInventoryServerSeq ?? 0) + 1,
				},
				currentUser.toRow(),
			);
			const {gatewayService} = getWorkerDependencies();
			await gatewayService.dispatchPresence({
				userId: updatedUser.id,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser),
			});
		}
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
	if (!Config.stripe.enabled) {
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
