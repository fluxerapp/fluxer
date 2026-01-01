/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AttachmentID, ChannelID, MessageID} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {AttachmentDecayRow} from '~/types/AttachmentDecayTypes';
import {
	computeCost,
	computeDecay,
	DEFAULT_DECAY_CONSTANTS,
	DEFAULT_RENEWAL_CONSTANTS,
	extendExpiry,
	getExpiryBucket,
	maybeRenewExpiry,
} from '~/utils/AttachmentDecay';
import {AttachmentDecayRepository} from './AttachmentDecayRepository';

export interface AttachmentDecayPayload {
	attachmentId: AttachmentID;
	channelId: ChannelID;
	messageId: MessageID;
	filename: string;
	sizeBytes: bigint;
	uploadedAt: Date;
	currentExpiresAt?: Date | null;
}

export class AttachmentDecayService {
	constructor(private readonly repo: AttachmentDecayRepository = new AttachmentDecayRepository()) {}

	async upsertMany(payloads: Array<AttachmentDecayPayload>): Promise<void> {
		if (!Config.attachmentDecayEnabled) return;

		for (const payload of payloads) {
			const decay = computeDecay({sizeBytes: payload.sizeBytes, uploadedAt: payload.uploadedAt});
			if (!decay) continue;

			const expiresAt = extendExpiry(payload.currentExpiresAt ?? null, decay.expiresAt);
			const record: AttachmentDecayRow & {expiry_bucket: number} = {
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
		if (!Config.attachmentDecayEnabled) return;

		for (const attachment of attachments) {
			const existing = await this.repo.fetchById(attachment.attachmentId);
			if (!existing) continue;
			const now = new Date();
			if (existing.expires_at.getTime() <= now.getTime()) {
				continue;
			}

			const uploadedAt = existing.uploaded_at;
			const decay = computeDecay({sizeBytes: attachment.sizeBytes, uploadedAt});
			if (!decay) continue;

			let expiresAt = extendExpiry(existing.expires_at ?? null, decay.expiresAt);

			const windowDays = DEFAULT_RENEWAL_CONSTANTS.RENEW_WINDOW_DAYS;
			const thresholdDays = DEFAULT_RENEWAL_CONSTANTS.RENEW_THRESHOLD_DAYS;

			const renewed = maybeRenewExpiry({
				currentExpiry: expiresAt,
				now,
				thresholdDays,
				windowDays,
			});

			if (renewed) {
				expiresAt = renewed;
			}

			const lifetimeDays = Math.round((expiresAt.getTime() - uploadedAt.getTime()) / (1000 * 60 * 60 * 24));
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
				status: existing.status ?? null,
				expiry_bucket: getExpiryBucket(expiresAt),
			});
		}
	}

	async fetchMetadata(
		attachments: Array<Pick<AttachmentDecayPayload, 'attachmentId'>>,
	): Promise<Map<string, AttachmentDecayRow>> {
		if (!Config.attachmentDecayEnabled) return new Map();

		const map = new Map<string, AttachmentDecayRow>();
		for (const att of attachments) {
			const row = await this.repo.fetchById(att.attachmentId);
			if (row) {
				map.set(att.attachmentId.toString(), row);
			}
		}
		return map;
	}
}
