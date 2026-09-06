// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import type {AttachmentID, ChannelID, MessageID} from '../BrandedTypes';
import type {InstanceAttachmentDecayEffectiveConfig} from '../instance/InstanceConfigRepository';
import type {AttachmentDecayRow} from '../types/AttachmentDecayTypes';
import {
	type AttachmentDecayRules,
	computeCost,
	computeDecay,
	DEFAULT_DECAY_CONSTANTS,
	extendExpiry,
	getExpiryBucket,
	maybeRenewExpiry,
} from '../utils/AttachmentDecay';
import {AttachmentDecayRepository} from './AttachmentDecayRepository';

interface AttachmentDecayPayload {
	attachmentId: AttachmentID;
	channelId: ChannelID;
	messageId: MessageID;
	filename: string;
	sizeBytes: bigint;
	uploadedAt: Date;
	currentExpiresAt?: Date | null;
}

type AttachmentDecayConfigResolver = () => Promise<InstanceAttachmentDecayEffectiveConfig>;

async function resolveDefaultAttachmentDecayConfig(): Promise<InstanceAttachmentDecayEffectiveConfig> {
	const {getInstanceConfigRepository} = await import('../middleware/ServiceSingletons');
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
			const lifetimeDays = Math.round((expiresAt.getTime() - uploadedAt.getTime()) / ms('1 day'));
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
				uploaded_at: uploadedAt,
				expires_at: expiresAt,
				last_accessed_at: now,
				cost,
				lifetime_days: lifetimeDays,
				status: existing?.status ?? null,
				expiry_bucket: getExpiryBucket(expiresAt),
			});
		}
	}

	async fetchMetadata(
		attachments: Array<Pick<AttachmentDecayPayload, 'attachmentId'>>,
	): Promise<Map<string, AttachmentDecayRow>> {
		const config = await this.resolveConfig();
		if (!config.enabled) return new Map();
		if (attachments.length === 0) return new Map();
		const attachmentIds = attachments.map((a) => a.attachmentId);
		const recordsMap = await this.repo.fetchByIds(attachmentIds);
		const result = new Map<string, AttachmentDecayRow>();
		for (const [id, row] of recordsMap.entries()) {
			result.set(id.toString(), row);
		}
		return result;
	}
}
