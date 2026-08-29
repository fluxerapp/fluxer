// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {Db, type DbOp} from '../../database/CassandraTypes';
import type {GiftCodeRow} from '../../database/types/PaymentTypes';
import {GiftCode, mapGiftCodeDurationToMonths, mapGiftDurationMonthsToFields} from '../../models/GiftCode';
import {GiftCodes, GiftCodesByCreator, GiftCodesByPaymentIntent, GiftCodesByRedeemer} from '../../Tables';

const FETCH_GIFT_CODES_BY_CREATOR_QUERY = GiftCodesByCreator.selectCql({
	where: GiftCodesByCreator.where.eq('created_by_user_id'),
});
const FETCH_GIFT_CODES_BY_REDEEMER_QUERY = GiftCodesByRedeemer.selectCql({
	where: GiftCodesByRedeemer.where.eq('redeemed_by_user_id'),
});
const FETCH_GIFT_CODE_BY_PAYMENT_INTENT_QUERY = GiftCodesByPaymentIntent.selectCql({
	columns: ['code'],
	where: GiftCodesByPaymentIntent.where.eq('stripe_payment_intent_id'),
	limit: 1,
});
const FETCH_GIFT_CODE_QUERY = GiftCodes.selectCql({
	where: GiftCodes.where.eq('code'),
	limit: 1,
});

function normaliseGiftCodeRowForWrite(data: GiftCodeRow): GiftCodeRow {
	let durationType = data.duration_type ?? null;
	let durationQuantity = data.duration_quantity ?? null;
	let durationMonths = data.duration_months ?? null;
	if (durationType === null || durationQuantity === null) {
		if (durationType !== null || durationQuantity !== null) {
			throw new Error('Gift code duration_type and duration_quantity must both be set when either is present');
		}
		if (durationMonths === null) {
			throw new Error('Gift code duration is missing from both duration_type/duration_quantity and duration_months');
		}
		const derivedDuration = mapGiftDurationMonthsToFields(durationMonths);
		durationType = derivedDuration.durationType;
		durationQuantity = derivedDuration.durationQuantity;
	}
	const normalisedDurationMonths = mapGiftCodeDurationToMonths(durationType, durationQuantity);
	if (normalisedDurationMonths !== null) {
		durationMonths = normalisedDurationMonths;
	}
	return {
		...data,
		duration_type: durationType,
		duration_quantity: durationQuantity,
		duration_months: durationMonths,
		revoked_at: data.revoked_at ?? null,
	};
}

export class GiftCodeRepository {
	async createGiftCode(data: GiftCodeRow): Promise<void> {
		const normalisedData = normaliseGiftCodeRowForWrite(data);
		await upsertOne(GiftCodes.insert(normalisedData));
		const batch = new BatchBuilder();
		batch.addPrepared(
			GiftCodesByCreator.upsertAll({
				created_by_user_id: normalisedData.created_by_user_id,
				code: normalisedData.code,
			}),
		);
		if (normalisedData.stripe_payment_intent_id) {
			batch.addPrepared(
				GiftCodesByPaymentIntent.upsertAll({
					stripe_payment_intent_id: normalisedData.stripe_payment_intent_id,
					code: normalisedData.code,
				}),
			);
		}
		if (normalisedData.redeemed_by_user_id) {
			batch.addPrepared(
				GiftCodesByRedeemer.upsertAll({
					redeemed_by_user_id: normalisedData.redeemed_by_user_id,
					code: normalisedData.code,
				}),
			);
		}
		await batch.execute();
	}

	async findGiftCode(code: string): Promise<GiftCode | null> {
		const row = await fetchOne<GiftCodeRow>(FETCH_GIFT_CODE_QUERY, {code});
		if (!row) {
			return null;
		}
		return new GiftCode(row);
	}

	async findGiftCodeByPaymentIntent(paymentIntentId: string): Promise<GiftCode | null> {
		const row = await fetchOne<{
			code: string;
		}>(FETCH_GIFT_CODE_BY_PAYMENT_INTENT_QUERY, {
			stripe_payment_intent_id: paymentIntentId,
		});
		if (!row) {
			return null;
		}
		return this.findGiftCode(row.code);
	}

	async findGiftCodesByCreator(userId: UserID): Promise<Array<GiftCode>> {
		const codes = await fetchMany<{
			code: string;
		}>(FETCH_GIFT_CODES_BY_CREATOR_QUERY, {
			created_by_user_id: userId,
		});
		if (codes.length === 0) {
			return [];
		}
		const gifts: Array<GiftCode> = [];
		for (const {code} of codes) {
			const gift = await this.findGiftCode(code);
			if (gift) {
				gifts.push(gift);
			}
		}
		return gifts;
	}

	async findGiftCodesByRedeemer(userId: UserID): Promise<Array<GiftCode>> {
		const codes = await fetchMany<{
			code: string;
		}>(FETCH_GIFT_CODES_BY_REDEEMER_QUERY, {
			redeemed_by_user_id: userId,
		});
		if (codes.length === 0) {
			return [];
		}
		const gifts: Array<GiftCode> = [];
		for (const {code} of codes) {
			const gift = await this.findGiftCode(code);
			if (gift) {
				gifts.push(gift);
			}
		}
		return gifts;
	}

	async redeemGiftCode(code: string, userId: UserID): Promise<void> {
		await upsertOne(
			GiftCodes.patchByPk(
				{code},
				{
					redeemed_by_user_id: Db.set(userId),
					redeemed_at: Db.set(new Date()),
				},
			),
		);
		await upsertOne(
			GiftCodesByRedeemer.upsertAll({
				redeemed_by_user_id: userId,
				code,
			}),
		);
	}

	async unredeemGiftCode(code: string, userId: UserID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GiftCodes.patchByPk(
				{code},
				{
					redeemed_by_user_id: Db.set(null),
					redeemed_at: Db.set(null),
				},
			),
		);
		batch.addPrepared(GiftCodesByRedeemer.deleteByPk({redeemed_by_user_id: userId, code}));
		await batch.execute();
	}

	async revokeGiftCode(code: string): Promise<void> {
		await upsertOne(GiftCodes.patchByPk({code}, {revoked_at: Db.set(new Date())}));
	}

	async updateGiftCode(code: string, data: Partial<GiftCodeRow>): Promise<void> {
		const batch = new BatchBuilder();
		const patch: Record<string, DbOp<unknown>> = {};
		if (data['redeemed_at'] !== undefined) {
			patch['redeemed_at'] = Db.set(data['redeemed_at']);
		}
		if (data['redeemed_by_user_id'] !== undefined) {
			patch['redeemed_by_user_id'] = Db.set(data['redeemed_by_user_id']);
		}
		if (Object.keys(patch).length > 0) {
			batch.addPrepared(GiftCodes.patchByPk({code}, patch));
		}
		if (data.redeemed_by_user_id) {
			batch.addPrepared(
				GiftCodesByRedeemer.upsertAll({
					redeemed_by_user_id: data.redeemed_by_user_id,
					code,
				}),
			);
		}
		await batch.execute();
	}

	async linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void> {
		await upsertOne(
			GiftCodes.patchByPk(
				{code},
				{
					checkout_session_id: Db.set(checkoutSessionId),
				},
			),
		);
	}
}
