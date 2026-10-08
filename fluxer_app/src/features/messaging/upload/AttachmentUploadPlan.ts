// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	type ChunkedUploadPart,
	type ChunkedUploadPlan,
	uploadFileInChunks,
} from '@app/features/messaging/upload/ChunkedAttachmentUploader';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {wrapDesktopLocalUploadURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import type {ApiAttachmentMetadata} from '@app/features/messaging/utils/MessageRequestUtils';
import {http} from '@app/features/platform/transport/RestTransport';

interface PresignedAttachmentUploadSinglepartResponse {
	upload_mode: 'singlepart';
	id: string | number;
	filename: string;
	upload_filename: string;
	upload_url: string;
	file_size: number;
	content_type: string;
}

interface PresignedAttachmentUploadMultipartResponse {
	upload_mode: 'multipart';
	id: string | number;
	filename: string;
	upload_filename: string;
	file_size: number;
	content_type: string;
	upload_id: string;
	part_size: number;
	parts: Array<{part_number: number; upload_url: string}>;
}

export type PresignedAttachmentUploadResponseAttachment =
	| PresignedAttachmentUploadSinglepartResponse
	| PresignedAttachmentUploadMultipartResponse;

interface PresignedAttachmentUploadRequestFile {
	id: string;
	filename: string;
	file_size: number;
	content_type: string;
}

interface PresignedAttachmentUploadRequestBody {
	attachments: Array<PresignedAttachmentUploadRequestFile>;
}

interface PresignedAttachmentUploadResponseBody {
	attachments: Array<PresignedAttachmentUploadResponseAttachment>;
}

export interface MultipartAttachmentUpload {
	upload_filename: string;
	upload_id: string;
}

interface CompleteMultipartAttachmentUploadRequestBody {
	uploads: Array<MultipartAttachmentUpload>;
}

interface CompleteMultipartAttachmentUploadResponseBody {
	uploads: Array<{upload_filename: string}>;
}

export interface TextareaAttachmentUploadResult {
	uploadFilename: string;
	fileSize: number;
	contentType: string;
}

export function canUsePresignedAttachmentUploads(files?: Array<File>): files is Array<File> {
	return RuntimeConfig.features.presigned_attachment_uploads && Boolean(files?.length);
}

export async function requestPresignedAttachmentUploads(
	channelId: string,
	attachments: Array<ApiAttachmentMetadata>,
	files: Array<File>,
	signal: AbortSignal,
): Promise<Array<PresignedAttachmentUploadResponseAttachment>> {
	const requestBody: PresignedAttachmentUploadRequestBody = {
		attachments: attachments.map((attachment, index) => ({
			id: attachment.id,
			filename: attachment.filename,
			file_size: files[index].size,
			content_type: files[index].type || 'application/octet-stream',
		})),
	};
	const response = await http.post<PresignedAttachmentUploadResponseBody>(Endpoints.CHANNEL_ATTACHMENTS(channelId), {
		body: requestBody,
		signal,
	});
	const plans = response.body?.attachments ?? [];
	for (const entry of plans) {
		if (!entry?.upload_mode || !entry.upload_filename || !entry.filename) {
			throw new Error('Invalid presigned attachment upload response');
		}
		if (entry.upload_mode === 'singlepart') {
			if (!entry.upload_url) {
				throw new Error(`Missing upload_url for singlepart attachment ${entry.id}`);
			}
		} else if (entry.upload_mode === 'multipart') {
			if (!entry.upload_id || !Array.isArray(entry.parts) || entry.parts.length === 0) {
				throw new Error(`Missing multipart metadata for attachment ${entry.id}`);
			}
		} else {
			throw new Error(`Unknown upload_mode for attachment ${(entry as {id: string}).id}`);
		}
	}
	return plans;
}

export async function uploadAttachmentsViaPlans(params: {
	nonce: string;
	attachments: Array<ApiAttachmentMetadata>;
	files: Array<File>;
	plans: Array<PresignedAttachmentUploadResponseAttachment>;
	planIndexById: Map<string, number>;
	multipartUploadsToComplete: Array<MultipartAttachmentUpload>;
	signal: AbortSignal;
}): Promise<Array<ApiAttachmentMetadata>> {
	const {nonce, attachments, files, plans, planIndexById, multipartUploadsToComplete, signal} = params;
	const totalBytes = files.reduce((sum, file) => sum + file.size, 0);
	const loadedBytesByIndex = new Array<number>(files.length).fill(0);
	let completedUploads = 0;
	const reportProgress = (): void => {
		if (totalBytes > 0) {
			const uploadedBytes = loadedBytesByIndex.reduce((sum, loaded) => sum + loaded, 0);
			CloudUpload.updateSendingProgress(nonce, (uploadedBytes / totalBytes) * 100);
			return;
		}
		if (files.length > 0) {
			CloudUpload.updateSendingProgress(nonce, (completedUploads / files.length) * 100);
		}
	};
	for (let index = 0; index < attachments.length; index += 1) {
		const attachment = attachments[index];
		const file = files[index];
		const planIndex = planIndexById.get(String(attachment.id));
		if (planIndex == null) {
			throw new Error(`Missing presigned upload metadata for attachment ${attachment.id}`);
		}
		const plan = plans[planIndex];
		if (plan.upload_mode === 'singlepart') {
			await http.put(wrapDesktopLocalUploadURL(plan.upload_url), {
				body: file,
				auth: 'none',
				headers: {
					'Content-Type': plan.content_type,
				},
				signal,
				onProgress: (event) => {
					const loaded = Math.min(file.size, event.loaded);
					if (loaded > loadedBytesByIndex[index]) {
						loadedBytesByIndex[index] = loaded;
						reportProgress();
					}
				},
			});
			loadedBytesByIndex[index] = file.size;
		} else {
			multipartUploadsToComplete.push({
				upload_filename: plan.upload_filename,
				upload_id: plan.upload_id,
			});
			const parts: Array<ChunkedUploadPart> = plan.parts.map((entry) => ({
				partNumber: entry.part_number,
				uploadUrl: entry.upload_url,
			}));
			const uploadPlan: ChunkedUploadPlan = {file, contentType: plan.content_type, partSize: plan.part_size, parts};
			await uploadFileInChunks(uploadPlan, {
				signal,
				onProgress: (uploaded) => {
					if (uploaded > loadedBytesByIndex[index]) {
						loadedBytesByIndex[index] = uploaded;
						reportProgress();
					}
				},
			});
			loadedBytesByIndex[index] = file.size;
		}
		completedUploads += 1;
		reportProgress();
	}
	return attachments.map((attachment) => {
		const planIndex = planIndexById.get(String(attachment.id));
		if (planIndex == null) {
			throw new Error(`Missing presigned upload metadata for attachment ${attachment.id}`);
		}
		const plan = plans[planIndex];
		return {
			...attachment,
			upload_filename: plan.upload_filename,
			file_size: plan.file_size,
			content_type: plan.content_type,
		};
	});
}

export async function uploadTextareaAttachmentViaPlan(params: {
	channelId: string;
	attachmentId: number;
	file: File;
	plan: PresignedAttachmentUploadResponseAttachment;
	signal: AbortSignal;
}): Promise<TextareaAttachmentUploadResult> {
	const {channelId, attachmentId, file, plan, signal} = params;
	const reportProgress = (uploadedBytes: number): void => {
		if (file.size <= 0) {
			CloudUpload.updateAttachment(channelId, attachmentId, {status: 'uploading', uploadProgress: 0});
			return;
		}
		const uploadProgress = Math.round((Math.min(file.size, uploadedBytes) / file.size) * 100);
		CloudUpload.updateAttachment(channelId, attachmentId, {status: 'uploading', uploadProgress});
	};
	if (plan.upload_mode === 'singlepart') {
		await http.put(wrapDesktopLocalUploadURL(plan.upload_url), {
			body: file,
			auth: 'none',
			headers: {
				'Content-Type': plan.content_type,
			},
			signal,
			onProgress: (event) => {
				reportProgress(event.loaded);
			},
		});
	} else {
		const parts: Array<ChunkedUploadPart> = plan.parts.map((entry) => ({
			partNumber: entry.part_number,
			uploadUrl: entry.upload_url,
		}));
		await uploadFileInChunks(
			{file, contentType: plan.content_type, partSize: plan.part_size, parts},
			{
				signal,
				onProgress: (uploadedBytes) => {
					reportProgress(uploadedBytes);
				},
			},
		);
		await completeMultipartAttachmentUploads(
			channelId,
			[{upload_filename: plan.upload_filename, upload_id: plan.upload_id}],
			signal,
		);
	}
	return {
		uploadFilename: plan.upload_filename,
		fileSize: plan.file_size,
		contentType: plan.content_type,
	};
}

export async function completeMultipartAttachmentUploads(
	channelId: string,
	uploads: Array<MultipartAttachmentUpload>,
	signal: AbortSignal,
): Promise<void> {
	const body: CompleteMultipartAttachmentUploadRequestBody = {uploads};
	await http.post<CompleteMultipartAttachmentUploadResponseBody>(Endpoints.CHANNEL_ATTACHMENTS_COMPLETE(channelId), {
		body,
		signal,
	});
}
