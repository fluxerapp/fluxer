// SPDX-License-Identifier: AGPL-3.0-or-later

import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {
	BillingActionIntentRow,
	BillingActionIntentStatus,
	BillingActionType,
} from '@app/api/database/types/BillingTypes';
import {BILLING_ACTION_INTENT_COLUMNS} from '@app/api/database/types/BillingTypes';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {BillingActionIntents} from '@app/api/Tables';

const FETCH_BY_ID = BillingActionIntents.selectCql({
	where: BillingActionIntents.where.eq('intent_id'),
	limit: 1,
});
const FETCH_STUCK = BillingActionIntents.select({
	columns: BILLING_ACTION_INTENT_COLUMNS,
	where: [BillingActionIntents.where.eq('status'), BillingActionIntents.where.lt('started_at', 'older_than')],
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

	async markStage(
		intentId: bigint,
		stage: BillingActionIntentStatus,
		fields: Partial<BillingActionIntentRow>,
	): Promise<void> {
		const current = await this.findById(intentId);
		if (!current) {
			throw new Error(`BillingActionIntent ${intentId} not found`);
		}
		const merged: BillingActionIntentRow = {
			...current,
			...fields,
			intent_id: intentId,
			status: stage,
		};
		await upsertOne(BillingActionIntents.upsertAll(merged));
	}

	async findStuck(olderThan: Date): Promise<Array<BillingActionIntentRow>> {
		const nonTerminalStatuses: Array<BillingActionIntentStatus> = ['pending', 'sub_canceled', 'refund_created'];
		const all: Array<BillingActionIntentRow> = [];
		for (const status of nonTerminalStatuses) {
			const rows = await fetchMany<BillingActionIntentRow>(
				FETCH_STUCK.bind({
					status,
					older_than: olderThan,
				}),
			);
			all.push(...rows);
		}
		return all;
	}
}
