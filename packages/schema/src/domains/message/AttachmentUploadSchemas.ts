// SPDX-License-Identifier: AGPL-3.0-or-later

import {ATTACHMENT_UPLOAD_MAX_CHUNKS, MAX_ATTACHMENTS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {
	coerceNumberFromString,
	createStringType,
	Int32Type,
	NonNegativeSafeIntegerType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

export const PresignedAttachmentUploadRequestItem = z.object({
	id: coerceNumberFromString(Int32Type).describe('The client-side identifier for this attachment'),
	filename: FilenameType.describe('The name of the file that will be uploaded'),
	file_size: coerceNumberFromString(NonNegativeSafeIntegerType).describe('Expected file size in bytes'),
	content_type: createStringType(1, 255).describe('MIME type the client will upload'),
});

export type PresignedAttachmentUploadRequestItem = z.infer<typeof PresignedAttachmentUploadRequestItem>;

export const PresignedAttachmentUploadRequest = z.object({
	attachments: z
		.array(PresignedAttachmentUploadRequestItem)
		.min(1)
		.max(MAX_ATTACHMENTS_PER_MESSAGE)
		.describe('Attachment upload specifications'),
});

export type PresignedAttachmentUploadRequest = z.infer<typeof PresignedAttachmentUploadRequest>;

const PresignedAttachmentUploadBase = z.object({
	id: coerceNumberFromString(Int32Type).describe('The client-side identifier for this attachment'),
	filename: FilenameType.describe('The original filename for this upload'),
	upload_filename: createStringType(1, 4096).describe('Temporary upload key to reference in message send payloads'),
	file_size: coerceNumberFromString(NonNegativeSafeIntegerType).describe('Expected file size in bytes'),
	content_type: createStringType(1, 255).describe('Expected MIME type for this upload'),
});
const PresignedAttachmentUploadSinglepart = PresignedAttachmentUploadBase.extend({
	upload_mode: z.literal('singlepart'),
	upload_url: URLType.describe('Presigned URL used to upload this attachment with a single PUT'),
});

const PresignedAttachmentUploadPart = z.object({
	part_number: coerceNumberFromString(Int32Type).describe('1-indexed part number; order is required for completion'),
	upload_url: URLType.describe('Presigned URL used to upload this part'),
});

const PresignedAttachmentUploadMultipart = PresignedAttachmentUploadBase.extend({
	upload_mode: z.literal('multipart'),
	upload_id: createStringType(1, 1024).describe('S3 multipart upload identifier; required to complete the upload'),
	part_size: coerceNumberFromString(NonNegativeSafeIntegerType).describe('Size in bytes of each part except the last'),
	parts: z
		.array(PresignedAttachmentUploadPart)
		.min(1)
		.max(ATTACHMENT_UPLOAD_MAX_CHUNKS)
		.describe('Per-part presigned URLs for parallel upload'),
});

export const PresignedAttachmentUploadResponseItem = z.discriminatedUnion('upload_mode', [
	PresignedAttachmentUploadSinglepart,
	PresignedAttachmentUploadMultipart,
]);

export type PresignedAttachmentUploadResponseItem = z.infer<typeof PresignedAttachmentUploadResponseItem>;

export const PresignedAttachmentUploadResponse = z.object({
	attachments: z.array(PresignedAttachmentUploadResponseItem).describe('Presigned upload details for each attachment'),
});

export type PresignedAttachmentUploadResponse = z.infer<typeof PresignedAttachmentUploadResponse>;

export const CompleteMultipartAttachmentUploadItem = z.object({
	upload_filename: createStringType(1, 4096).describe('The upload_filename returned when the upload was planned'),
	upload_id: createStringType(1, 1024).describe('The upload_id returned when the upload was planned'),
});

export type CompleteMultipartAttachmentUploadItem = z.infer<typeof CompleteMultipartAttachmentUploadItem>;

export const CompleteMultipartAttachmentUploadRequest = z.object({
	uploads: z
		.array(CompleteMultipartAttachmentUploadItem)
		.min(1)
		.max(MAX_ATTACHMENTS_PER_MESSAGE)
		.describe('Multipart uploads to finalize'),
});

export type CompleteMultipartAttachmentUploadRequest = z.infer<typeof CompleteMultipartAttachmentUploadRequest>;

export const CompleteMultipartAttachmentUploadResult = z.object({
	upload_filename: createStringType(1, 4096).describe('Finalized upload key'),
});

export type CompleteMultipartAttachmentUploadResult = z.infer<typeof CompleteMultipartAttachmentUploadResult>;

export const CompleteMultipartAttachmentUploadResponse = z.object({
	uploads: z.array(CompleteMultipartAttachmentUploadResult).describe('Finalized upload keys'),
});

export type CompleteMultipartAttachmentUploadResponse = z.infer<typeof CompleteMultipartAttachmentUploadResponse>;
