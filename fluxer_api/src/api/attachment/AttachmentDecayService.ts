// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import type {AttachmentID, ChannelID, MessageID} from '@app/api/BrandedTypes';
import type {InstanceAttachmentDecayEffectiveConfig} from '@app/api/instance/InstanceConfigRepository';
import type {AttachmentDecayRow} from '@app/api/types/AttachmentDecayTypes';
import {
	type AttachmentDecayRules,
	computeCost,
	computeDecay,
	DEFAULT_DECAY_CONSTANTS,
	extendExpiry,
	getExpiryBucket,
	maybeRenewExpiry,
} from '@app/api/utils/AttachmentDecay';
import {ms} from 'itty-time';

export interface AttachmentDecayPayload {
	attachmentId: AttachmentID;
	channelId: ChannelID;
	messageId: MessageID;
	filename: string;
	sizeBytes: bigint;
	uploadedAt: Date;
	currentExpiresAt?: Date | null;
}

type AttachmentDecayConfigResolver = () => Promise<InstanceAttachmentDecayEffectiveConfig>;

export async function resolveDefaultAttachmentDecayConfig(): Promise<InstanceAttachmentDecayEffectiveConfig> {
	const {getInstanceConfigRepository} = await import('@app/api/middleware/ServiceSingletons');
	return getInstanceConfigRepository().getEffectiveAttachmentDecayConfig();
}

function buildDecayRules(config: InstanceAttachmentDecayEffectiveConfig): AttachmentDecayRules {
	return {
		minMb: config.min_size_mb,
		maxMb: config.max_size_mb,
		maxEligibleMb: config.max_eligible_size_mb,
		minDays: config.min_lifetime_days,
		maxDays: config.max_lifetime_days,
		curve: config.curve,
	};
}

export class AttachmentDecayService {
	constructor(
		private readonly repo: AttachmentDecayRepository = new AttachmentDecayRepository(),
		private readonly resolveConfig: AttachmentDecayConfigResolver = resolveDefaultAttachmentDecayConfig,
	) {}

	async upsertMany(payloads: Array<AttachmentDecayPayload>): Promise<void> {
		const config = await this.resolveConfig();
		if (!config.enabled) return;
		const rules = buildDecayRules(config);
		for (const payload of payloads) {
			const decay = computeDecay({sizeBytes: payload.sizeBytes, uploadedAt: payload.uploadedAt, rules});
			if (!decay) continue;
			const expiresAt = extendExpiry(payload.currentExpiresAt ?? null, decay.expiresAt);
			const record: AttachmentDecayRow & {
				expiry_bucket: number;
			} = {
				attachment_id: payload.attachmentId,
				channel_id: payload.channelId,
				message_id: payload.messageId,
				filename: payload.filename,
				size_bytes: payload.sizeBytes,
				uploaded_at: payload.uploadedAt,
				expires_at: expiresAt,
				last_accessed_at: payload.uploadedAt,
				cost: decay.cost,
				lifetime_days: decay.days,
				status: null,
				expiry_bucket: getExpiryBucket(expiresAt),
			};
			await this.repo.upsert(record);
		}
	}

	async extendForAttachments(attachments: Array<AttachmentDecayPayload>): Promise<void> {
		if (attachments.length === 0) return;
		const config = await this.resolveConfig();
		if (!config.enabled) return;
		const rules = buildDecayRules(config);
		const attachmentIds = attachments.map((a) => a.attachmentId);
		const existingRecords = await this.repo.fetchByIds(attachmentIds);
		const now = new Date();
		for (const attachment of attachments) {
			const existing = existingRecords.get(attachment.attachmentId);
			if (existing && existing.expires_at.getTime() <= now.getTime()) {
				continue;
			}
			const uploadedAt = existing?.uploaded_at ?? attachment.uploadedAt;
			const decay = computeDecay({sizeBytes: attachment.sizeBytes, uploadedAt, rules});
			if (!decay) continue;
			let expiresAt = extendExpiry(existing?.expires_at ?? attachment.currentExpiresAt ?? null, decay.expiresAt);
			if (!existing && expiresAt.getTime() <= now.getTime()) {
				expiresAt = now;
			}
			if (expiresAt.getTime() > now.getTime()) {
				const renewed = maybeRenewExpiry({
					currentExpiry: expiresAt,
					now,
					thresholdDays: config.renew_threshold_days,
					windowDays: config.renew_window_days,
				});
				if (renewed) {
					expiresAt = renewed;
				}
			}
			await this.writeRecord(attachment, {
				uploadedAt,
				expiresAt,
				lastAccessedAt: now,
				status: existing?.status ?? null,
			});
		}
	}

	async reconcile(
		attachments: Array<AttachmentDecayPayload>,
		fileExists: (attachment: AttachmentDecayPayload) => Promise<boolean>,
	): Promise<number> {
		if (attachments.length === 0) return 0;
		const config = await this.resolveConfig();
		if (!config.enabled) return 0;
		const rules = buildDecayRules(config);
		const existingRecords = await this.repo.fetchByIds(attachments.map((a) => a.attachmentId));
		const now = new Date();
		const windowEnd = new Date(now.getTime() + config.renew_window_days * ms('1 day'));
		const seen = new Set<AttachmentID>();
		let changed = 0;
		for (const attachment of attachments) {
			if (seen.has(attachment.attachmentId)) continue;
			seen.add(attachment.attachmentId);
			const existing = existingRecords.get(attachment.attachmentId);
			const uploadedAt = existing?.uploaded_at ?? attachment.uploadedAt;
			const decay = computeDecay({sizeBytes: attachment.sizeBytes, uploadedAt, rules});
			if (!decay) {
				if (existing) {
					await this.repo.deleteRecords({
						expiry_bucket: getExpiryBucket(existing.expires_at),
						expires_at: existing.expires_at,
						attachment_id: existing.attachment_id,
					});
					changed++;
				}
				continue;
			}
			const stillPending = existing != null && existing.expires_at.getTime() > now.getTime();
			if (!stillPending && !(await fileExists(attachment))) continue;
			const floor =
				stillPending && existing.expires_at.getTime() < windowEnd.getTime() ? existing.expires_at : windowEnd;
			const expiresAt = extendExpiry(floor, decay.expiresAt);
			if (existing && existing.expires_at.getTime() === expiresAt.getTime()) continue;
			await this.writeRecord(attachment, {
				uploadedAt,
				expiresAt,
				lastAccessedAt: existing?.last_accessed_at ?? uploadedAt,
				status: existing?.status ?? null,
			});
			changed++;
		}
		return changed;
	}

	private async writeRecord(
		attachment: AttachmentDecayPayload,
		state: {uploadedAt: Date; expiresAt: Date; lastAccessedAt: Date; status: string | null},
	): Promise<void> {
		const lifetimeDays = Math.round((state.expiresAt.getTime() - state.uploadedAt.getTime()) / ms('1 day'));
		const cost = computeCost({
			sizeBytes: attachment.sizeBytes,
			lifetimeDays,
			pricePerTBPerMonth: DEFAULT_DECAY_CONSTANTS.PRICE_PER_TB_PER_MONTH,
		});
		await this.repo.upsert({
			attachment_id: attachment.attachmentId,
			channel_id: attachment.channelId,
			message_id: attachment.messageId,
			filename: attachment.filename,
			size_bytes: attachment.sizeBytes,
			uploaded_at: state.uploadedAt,
			expires_at: state.expiresAt,
			last_accessed_at: state.lastAccessedAt,
			cost,
			lifetime_days: lifetimeDays,
			status: state.status,
			expiry_bucket: getExpiryBucket(state.expiresAt),
		});
	}
}
