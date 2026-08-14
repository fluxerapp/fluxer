// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, UserID} from '../../../BrandedTypes';
import {fetchOne, upsertOne} from '../../../database/CassandraQueryExecution';
import type {
	AttachmentUploadTraceByAttachmentRow,
	AttachmentUploadTraceByKeyRow,
} from '../../../database/types/AttachmentUploadTypes';
import {AttachmentUploadTracesByAttachment, AttachmentUploadTracesByKey} from '../../../Tables';

const GET_UPLOAD_TRACE_BY_KEY_QUERY = AttachmentUploadTracesByKey.select({
	where: AttachmentUploadTracesByKey.where.eq('upload_key'),
	limit: 1,
});
const GET_UPLOAD_TRACE_BY_ATTACHMENT_QUERY = AttachmentUploadTracesByAttachment.select({
	where: AttachmentUploadTracesByAttachment.where.eq('attachment_id'),
	limit: 1,
});

export type AttachmentUploadMode = 'form_data' | 'presigned_singlepart' | 'presigned_multipart';

interface RecordAttachmentUploadRequestInput {
	uploadKey: string;
	userId: UserID;
	channelId: ChannelID;
	filename: string;
	contentType: string;
	uploadMode: AttachmentUploadMode;
	requestIp: string;
	requestedAt?: Date;
}

interface MarkAttachmentUploadCompletedInput {
	uploadKey: string;
	completionIp: string;
	completedAt?: Date;
}

interface GetPendingAttachmentUploadInput {
	uploadKey: string;
	userId: UserID;
	channelId: ChannelID;
	uploadMode?: AttachmentUploadMode;
}

export class AttachmentUploadTraceRepository {
	async getByUploadKey(uploadKey: string): Promise<AttachmentUploadTraceByKeyRow | null> {
		return await fetchOne<AttachmentUploadTraceByKeyRow>(GET_UPLOAD_TRACE_BY_KEY_QUERY.bind({upload_key: uploadKey}));
	}

	async getByAttachmentId(attachmentId: AttachmentID): Promise<AttachmentUploadTraceByAttachmentRow | null> {
		return await fetchOne<AttachmentUploadTraceByAttachmentRow>(
			GET_UPLOAD_TRACE_BY_ATTACHMENT_QUERY.bind({attachment_id: BigInt(attachmentId)}),
		);
	}

	async getPendingUpload(input: GetPendingAttachmentUploadInput): Promise<AttachmentUploadTraceByKeyRow | null> {
		const existing = await this.getByUploadKey(input.uploadKey);
		if (
			!existing ||
			existing.user_id !== input.userId ||
			existing.channel_id !== input.channelId ||
			existing.attachment_id != null ||
			(input.uploadMode !== undefined && existing.upload_mode !== input.uploadMode)
		) {
			return null;
		}
		return existing;
	}

	async recordRequestedUpload(input: RecordAttachmentUploadRequestInput): Promise<AttachmentUploadTraceByKeyRow> {
		const now = input.requestedAt ?? new Date();
		const row: AttachmentUploadTraceByKeyRow = {
			upload_key: input.uploadKey,
			attachment_id: null,
			user_id: BigInt(input.userId),
			channel_id: BigInt(input.channelId),
			filename: input.filename,
			content_type: input.contentType,
			upload_mode: input.uploadMode,
			request_ip: input.requestIp,
			requested_at: now,
			completion_ip: null,
			completed_at: null,
			created_at: now,
			updated_at: now,
		};
		await upsertOne(AttachmentUploadTracesByKey.insert(row));
		return row;
	}

	async markUploadCompleted(input: MarkAttachmentUploadCompletedInput): Promise<AttachmentUploadTraceByKeyRow | null> {
		const existing = await this.getByUploadKey(input.uploadKey);
		if (!existing) {
			return null;
		}
		const completedAt = input.completedAt ?? new Date();
		const updated: AttachmentUploadTraceByKeyRow = {
			...existing,
			completion_ip: input.completionIp,
			completed_at: completedAt,
			updated_at: completedAt,
		};
		await upsertOne(AttachmentUploadTracesByKey.insert(updated));
		return updated;
	}

	async bindAttachment(
		uploadKey: string,
		attachmentId: AttachmentID,
	): Promise<AttachmentUploadTraceByAttachmentRow | null> {
		const existing = await this.getByUploadKey(uploadKey);
		if (!existing || existing.attachment_id != null) {
			return null;
		}
		const now = new Date();
		const updatedKeyRow: AttachmentUploadTraceByKeyRow = {
			...existing,
			attachment_id: BigInt(attachmentId),
			updated_at: now,
		};
		const attachmentRow: AttachmentUploadTraceByAttachmentRow = {
			attachment_id: BigInt(attachmentId),
			upload_key: existing.upload_key,
			user_id: existing.user_id,
			channel_id: existing.channel_id,
			filename: existing.filename,
			content_type: existing.content_type,
			upload_mode: existing.upload_mode,
			request_ip: existing.request_ip,
			requested_at: existing.requested_at,
			completion_ip: existing.completion_ip,
			completed_at: existing.completed_at,
			created_at: existing.created_at,
			updated_at: now,
		};
		await upsertOne(AttachmentUploadTracesByKey.insert(updatedKeyRow));
		await upsertOne(AttachmentUploadTracesByAttachment.insert(attachmentRow));
		return attachmentRow;
	}
}
