// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	createGiftCodeRow,
	type GiftCode,
	generateUniqueGiftCode,
	mapGiftDurationMonthsToFields,
} from '@app/api/models/GiftCode';
import type {User} from '@app/api/models/User';
import type {ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {extractId} from '@app/api/stripe/StripeUtils';
import {enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import {bumpGiftInventory} from '@app/api/user/GiftInventory';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

export const MANUAL_GIFT_ORDER_PRODUCT_TYPE = 'manual_gift_order';
const MAX_MANUAL_GIFT_ORDER_CODES = 200;
const LINE_ITEM_LIMIT = 100;

interface ManualGiftOrderLine {
	priceId: string;
	durationMonths: number;
	quantity: number;
	amountTotal: number;
}

export function isManualGiftOrderSession(session: Stripe.Checkout.Session): boolean {
	return session.metadata?.fluxer_manual_gift_order === 'true';
}

export async function readManualGiftOrderLines(
	stripe: Stripe,
	productRegistry: ProductRegistry,
	sessionId: string,
): Promise<Array<ManualGiftOrderLine> | null> {
	const items = await stripe.checkout.sessions.listLineItems(sessionId, {limit: LINE_ITEM_LIMIT});
	const lines: Array<ManualGiftOrderLine> = [];
	for (const item of items.data) {
		const priceId = item.price?.id ?? null;
		const product = item.price ? productRegistry.getGiftProductForPrice(item.price) : null;
		if (!priceId || !product || !item.quantity) {
			Logger.error(
				{sessionId, priceId, quantity: item.quantity},
				'Manual gift order has a line that is not a known gift price, so no codes were created',
			);
			return null;
		}
		lines.push({
			priceId,
			durationMonths: product.durationMonths,
			quantity: item.quantity,
			amountTotal: item.amount_total,
		});
	}
	const total = lines.reduce((sum, line) => sum + line.quantity, 0);
	if (items.has_more || total === 0 || total > MAX_MANUAL_GIFT_ORDER_CODES) {
		Logger.error(
			{sessionId, total, max: MAX_MANUAL_GIFT_ORDER_CODES},
			'Manual gift order has no codes or more than the limit, so no codes were created',
		);
		return null;
	}
	return lines;
}

export function pickGiftCodesCoveringRefund(
	codes: Array<GiftCode>,
	lines: Array<ManualGiftOrderLine>,
	amountRefunded: number,
): Array<GiftCode> {
	const unitPrice = new Map<number, number>();
	for (const months of new Set(lines.map((line) => line.durationMonths))) {
		const matching = lines.filter((line) => line.durationMonths === months);
		const amount = matching.reduce((sum, line) => sum + line.amountTotal, 0);
		const quantity = matching.reduce((sum, line) => sum + line.quantity, 0);
		unitPrice.set(months, Math.round(amount / quantity));
	}
	const priceOf = (code: GiftCode) => unitPrice.get(code.durationMonths ?? 0) ?? 0;
	const isReversed = (code: GiftCode) => code.revokedAt !== null || code.premiumReversedSeconds !== null;
	let remaining = amountRefunded - codes.filter(isReversed).reduce((sum, code) => sum + priceOf(code), 0);
	const live = codes.filter((code) => !isReversed(code));
	const picked: Array<GiftCode> = [];
	for (const group of [live.filter((code) => !code.redeemedByUserId), live.filter((code) => code.redeemedByUserId)]) {
		const pool = group.sort((left, right) => priceOf(right) - priceOf(left));
		while (remaining > 0 && pool.length > 0) {
			const fitting = pool.findIndex((code) => priceOf(code) <= remaining);
			const [code] = pool.splice(fitting === -1 ? pool.length - 1 : fitting, 1);
			picked.push(code);
			remaining -= priceOf(code);
		}
	}
	return picked;
}

export class StripeManualGiftOrderHandler {
	constructor(
		private stripe: Stripe | null,
		private userRepository: UserRepository,
		private productRegistry: ProductRegistry,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
	) {}

	async fulfil(session: Stripe.Checkout.Session): Promise<void> {
		if (session.payment_status === 'unpaid') {
			Logger.info({sessionId: session.id}, 'Manual gift order is not paid yet, deferring until settlement');
			return;
		}
		if ((await this.userRepository.getPaymentByCheckoutSession(session.id))?.status === 'completed') {
			Logger.debug({sessionId: session.id}, 'Manual gift order already fulfilled');
			return;
		}
		const buyer = await this.findBuyer(session);
		const paymentIntentId = extractId(session.payment_intent);
		if (!this.stripe || !buyer || !paymentIntentId || session.amount_total == null || !session.currency) {
			Logger.error(
				{sessionId: session.id, userId: session.metadata?.user_id, paymentIntentId, hasBuyer: buyer !== null},
				'Manual gift order has no known buyer or payment, so no codes were created',
			);
			return;
		}
		const lines = await readManualGiftOrderLines(this.stripe, this.productRegistry, session.id);
		if (!lines) {
			return;
		}
		const lockKey = `manual_gift_order_lock:${session.id}`;
		const lockToken = await this.cacheService.acquireLock(lockKey, seconds('5 minutes'));
		if (!lockToken) {
			throw new StripeError('Manual gift order is already being fulfilled');
		}
		try {
			await this.createMissingCodes(session.id, paymentIntentId, buyer, lines);
			await bumpGiftInventory(this.userRepository, this.gatewayService, buyer.id, {has_ever_purchased: true});
			await enqueueBillingEmail(getWorkerService(), {kind: 'gift_paid', id: session.id});
			await this.recordPayment(session, buyer, paymentIntentId, lines[0].priceId);
			Logger.info(
				{
					sessionId: session.id,
					userId: buyer.id.toString(),
					codes: lines.reduce((sum, line) => sum + line.quantity, 0),
				},
				'Manual gift order fulfilled',
			);
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private async findBuyer(session: Stripe.Checkout.Session): Promise<User | null> {
		const userId = session.metadata?.user_id;
		return userId && /^\d+$/.test(userId) ? this.userRepository.findUnique(createUserID(BigInt(userId))) : null;
	}

	private async createMissingCodes(
		checkoutSessionId: string,
		paymentIntentId: string,
		buyer: User,
		lines: Array<ManualGiftOrderLine>,
	): Promise<void> {
		const existing = await this.userRepository.findGiftCodesByPaymentIntent(paymentIntentId);
		for (const months of new Set(lines.map((line) => line.durationMonths))) {
			const wanted = lines
				.filter((line) => line.durationMonths === months)
				.reduce((sum, line) => sum + line.quantity, 0);
			const duration = mapGiftDurationMonthsToFields(months);
			for (let count = existing.filter((code) => code.durationMonths === months).length; count < wanted; count++) {
				await this.userRepository.createGiftCode(
					createGiftCodeRow({
						code: await generateUniqueGiftCode((code) => this.userRepository.findGiftCode(code)),
						durationType: duration.durationType,
						durationQuantity: duration.durationQuantity,
						createdAt: new Date(),
						createdByUserId: buyer.id,
						paymentIntentId,
						checkoutSessionId,
					}),
				);
			}
		}
	}

	private async recordPayment(
		session: Stripe.Checkout.Session,
		buyer: User,
		paymentIntentId: string,
		priceId: string,
	): Promise<void> {
		if (!(await this.userRepository.getPaymentByCheckoutSession(session.id))) {
			await this.userRepository.createPayment({
				checkout_session_id: session.id,
				user_id: buyer.id,
				price_id: priceId,
				product_type: MANUAL_GIFT_ORDER_PRODUCT_TYPE,
				status: 'pending',
				is_gift: true,
				created_at: new Date(),
			});
		}
		await this.userRepository.updatePayment({
			checkout_session_id: session.id,
			stripe_customer_id: extractId(session.customer),
			payment_intent_id: paymentIntentId,
			amount_cents: session.amount_total ?? 0,
			currency: session.currency ?? '',
			status: 'completed',
			completed_at: new Date(),
			tax_amount_minor: session.total_details?.amount_tax ?? null,
			tax_country: session.customer_details?.address?.country ?? null,
		});
	}
}
