// SPDX-License-Identifier: AGPL-3.0-or-later

import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingActionIntentRow, BillingActionType} from '@app/api/database/types/BillingTypes';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {BillingActionIntents} from '@app/api/Tables';

const FETCH_BY_ID = BillingActionIntents.selectCql({
	where: BillingActionIntents.where.eq('intent_id'),
	limit: 1,
});

export class BillingActionIntentRepository {
	constructor(private snowflakeService: ISnowflakeService) {}

	async create(params: {
		userId: bigint;
		actorAdminId: bigint;
		actionType: BillingActionType;
		subscriptionId?: string | null;
		invoiceId?: string | null;
		paymentIntentId?: string | null;
		refundAmount?: bigint | null;
		refundReason?: string | null;
	}): Promise<bigint> {
		const intentId = await this.snowflakeService.generate();
		const row: BillingActionIntentRow = {
			intent_id: intentId,
			user_id: params.userId,
			actor_admin_id: params.actorAdminId,
			action_type: params.actionType,
			subscription_id: params.subscriptionId ?? null,
			invoice_id: params.invoiceId ?? null,
			payment_intent_id: params.paymentIntentId ?? null,
			refund_amount: params.refundAmount ?? null,
			refund_reason: params.refundReason ?? null,
			status: 'pending',
			error_message: null,
			started_at: new Date(),
			sub_canceled_at: null,
			refund_created_at: null,
			completed_at: null,
			refund_id: null,
		};
		await upsertOne(BillingActionIntents.upsertAll(row));
		return intentId;
	}

	async findById(intentId: bigint): Promise<BillingActionIntentRow | null> {
		return fetchOne<BillingActionIntentRow>(FETCH_BY_ID, {intent_id: intentId});
	}
}
