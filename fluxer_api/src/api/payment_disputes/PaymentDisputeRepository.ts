// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {
	executeConditional,
	fetchMany,
	fetchOne,
	fetchPage,
	type PagedQueryResult,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp, nextVersion, type PatchObject} from '@app/api/database/CassandraTypes';
import {applyPatchToRow} from '@app/api/database/CassandraVersionedUpdate';
import type {
	PaymentDisputeReviewByPurchaserRow,
	PaymentDisputeReviewRow,
} from '@app/api/database/types/PaymentDisputeTypes';
import {PaymentDisputeReviews, PaymentDisputeReviewsByPurchaser} from '@app/api/Tables';
import {BACKGROUND_READ_TIMEOUT_MS} from '@pkgs/cassandra/src/Client';

export type PaymentDisputeReviewPatch = Partial<{
	[K in Exclude<keyof PaymentDisputeReviewRow, 'dispute_id' | 'version'>]: DbOp<PaymentDisputeReviewRow[K]>;
}>;

const FETCH_REVIEW = PaymentDisputeReviews.selectCql({
	where: PaymentDisputeReviews.where.eq('dispute_id'),
	limit: 1,
});
const SCAN_REVIEWS = PaymentDisputeReviews.selectCql();
const FETCH_DISPUTE_IDS_BY_PURCHASER = PaymentDisputeReviewsByPurchaser.selectCql({
	columns: ['dispute_id'],
	where: PaymentDisputeReviewsByPurchaser.where.eq('purchaser_id'),
});
const SCAN_PAGE_SIZE = 200;

export class PaymentDisputeRepository {
	async findReview(disputeId: string): Promise<PaymentDisputeReviewRow | null> {
		return fetchOne<PaymentDisputeReviewRow>(FETCH_REVIEW, {dispute_id: disputeId});
	}

	async createReview(row: PaymentDisputeReviewRow): Promise<boolean> {
		await upsertOne(
			PaymentDisputeReviewsByPurchaser.upsertAll({purchaser_id: row.purchaser_id, dispute_id: row.dispute_id}),
		);
		return executeConditional(PaymentDisputeReviews.insertIfNotExists(row));
	}

	async updateReview(
		current: PaymentDisputeReviewRow,
		patch: PaymentDisputeReviewPatch,
	): Promise<PaymentDisputeReviewRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			PaymentDisputeReviews.conditionalPatchByPk(
				{dispute_id: current.dispute_id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		if (!applied) {
			return null;
		}
		return {
			...applyPatchToRow<PaymentDisputeReviewRow>(current, patch as PatchObject),
			version,
		} as PaymentDisputeReviewRow;
	}

	async listReviewsByPurchaser(purchaserId: UserID): Promise<Array<PaymentDisputeReviewRow>> {
		const mappings = await fetchMany<Pick<PaymentDisputeReviewByPurchaserRow, 'dispute_id'>>(
			FETCH_DISPUTE_IDS_BY_PURCHASER,
			{purchaser_id: purchaserId},
		);
		const reviews = await Promise.all(mappings.map((mapping) => this.findReview(mapping.dispute_id)));
		return reviews.filter((review): review is PaymentDisputeReviewRow => review !== null);
	}

	async listReviews(): Promise<Array<PaymentDisputeReviewRow>> {
		const rows: Array<PaymentDisputeReviewRow> = [];
		let pageState: string | null = null;
		do {
			const page: PagedQueryResult<PaymentDisputeReviewRow> = await fetchPage<PaymentDisputeReviewRow>(
				SCAN_REVIEWS,
				{},
				{pageSize: SCAN_PAGE_SIZE, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
			);
			rows.push(...page.rows);
			pageState = page.pageState;
		} while (pageState);
		return rows;
	}
}
