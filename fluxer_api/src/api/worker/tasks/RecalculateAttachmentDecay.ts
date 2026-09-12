// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {BatchBuilder, fetchPage, type PagedQueryResult} from '../../database/CassandraQueryExecution';
import type {InstanceAttachmentDecayEffectiveConfig} from '../../instance/InstanceConfigRepository';
import {Logger} from '../../Logger';
import {AttachmentDecayByExpiry, AttachmentDecayById} from '../../Tables';
import type {AttachmentDecayRow} from '../../types/AttachmentDecayTypes';
import type {AttachmentDecayRules} from '../../utils/AttachmentDecay';
import {computeDecay, getExpiryBucket, maybeRenewExpiry} from '../../utils/AttachmentDecay';

function buildDecayRulesFromConfig(config: InstanceAttachmentDecayEffectiveConfig): AttachmentDecayRules {
	return {
		minMb: config.min_size_mb,
		maxMb: config.max_size_mb,
		maxEligibleMb: config.max_eligible_size_mb,
		minDays: config.min_lifetime_days,
		maxDays: config.max_lifetime_days,
		curve: config.curve,
	};
}

export async function recalculateAttachmentDecay(): Promise<void> {
	const {getInstanceConfigRepository} = await import('../../middleware/ServiceSingletons');
	const instanceConfigRepository = getInstanceConfigRepository();
	const config = await instanceConfigRepository.getEffectiveAttachmentDecayConfig();
	let pageState: string | null = null;
	let totalUpdated = 0;
	let totalProcessed = 0;

	const rules = buildDecayRulesFromConfig(config);

	do {
		const result: PagedQueryResult<AttachmentDecayRow> = await fetchPage<AttachmentDecayRow>(
			AttachmentDecayById.selectCql(),
			{},
			{pageSize: 100, pageState},
		);
		pageState = result.pageState;

		for (const row of result.rows) {
			totalProcessed++;

			let expiresAt: Date | null = null;
			let cost = 0;
			let lifetimeDays = 0;

			if (config.enabled) {
				const decay = computeDecay({
					sizeBytes: row.size_bytes,
					uploadedAt: row.uploaded_at,
					rules,
				});
				if (decay) {
					expiresAt = decay.expiresAt;
					cost = decay.cost;
					lifetimeDays = decay.days;

					if (row.last_accessed_at && row.last_accessed_at.getTime() > row.uploaded_at.getTime()) {
						const renewed = maybeRenewExpiry({
							currentExpiry: expiresAt,
							now: row.last_accessed_at,
							thresholdDays: config.renew_threshold_days,
							windowDays: config.renew_window_days,
						});
						if (renewed) {
							expiresAt = renewed;
							lifetimeDays = Math.round((expiresAt.getTime() - row.uploaded_at.getTime()) / ms('1 day'));
							const sizeTB = Number(row.size_bytes) / 1024 / 1024 / 1024 / 1024;
							const lifetimeMonths = lifetimeDays / 30;
							cost = sizeTB * (rules.pricePerTBPerMonth ?? 0.0081103 * 1000) * lifetimeMonths;
						}
					}
				}
			}

			const oldExpiresAt = row.expires_at ? new Date(row.expires_at) : null;
			const expiresAtTime = expiresAt?.getTime() ?? null;
			const oldExpiresAtTime = oldExpiresAt?.getTime() ?? null;

			if (expiresAtTime !== oldExpiresAtTime) {
				totalUpdated++;

				const batch = new BatchBuilder();

				if (oldExpiresAt) {
					batch.addPrepared(
						AttachmentDecayByExpiry.deleteByPk({
							expiry_bucket: getExpiryBucket(oldExpiresAt),
							expires_at: oldExpiresAt,
							attachment_id: row.attachment_id,
						}),
					);
				}

				if (expiresAt) {
					const expiryBucket = getExpiryBucket(expiresAt);
					batch.addPrepared(
						AttachmentDecayById.upsertAll({
							...row,
							expires_at: expiresAt,
							cost,
							lifetime_days: lifetimeDays,
						}),
					);
					batch.addPrepared(
						AttachmentDecayByExpiry.upsertAll({
							expiry_bucket: expiryBucket,
							expires_at: expiresAt,
							attachment_id: row.attachment_id,
							channel_id: row.channel_id,
							message_id: row.message_id,
						}),
					);
				} else {
					batch.addPrepared(
						AttachmentDecayById.upsertAll({
							...row,
							expires_at: null as any,
							cost: 0,
							lifetime_days: 0,
						}),
					);
				}

				await batch.execute();
			}
		}
	} while (pageState);

	Logger.info(
		{totalProcessed, totalUpdated},
		'Finished recalculating attachment decay expiries based on updated configuration',
	);
}

const recalculateAttachmentDecayTask: WorkerTaskHandler = async () => {
	await recalculateAttachmentDecay();
};

export default recalculateAttachmentDecayTask;
