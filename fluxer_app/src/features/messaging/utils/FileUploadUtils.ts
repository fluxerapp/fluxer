// SPDX-License-Identifier: AGPL-3.0-or-later

import MessageQueue from '@app/features/messaging/state/MessageQueue';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {getMaxAttachmentFileSize} from '@app/features/messaging/utils/AttachmentUtils';

export interface FileUploadResult {
	success: boolean;
	error?: 'too_many_attachments' | 'no_files' | 'empty_text' | 'file_size_too_large';
	oversizedFileCount?: number;
}

export async function handleFileUpload(
	channelId: string,
	files: FileList | Array<File>,
	currentAttachmentCount: number,
	maxAttachments: number,
): Promise<FileUploadResult> {
	const fileArray = Array.from(files);
	if (fileArray.length === 0) {
		return {success: false, error: 'no_files'};
	}
	if (currentAttachmentCount + fileArray.length > maxAttachments) {
		return {success: false, error: 'too_many_attachments'};
	}
	const maxFileSize = getMaxAttachmentFileSize();
	const oversizedFileCount = fileArray.filter((file) => file.size > maxFileSize).length;
	if (oversizedFileCount > 0) {
		return {success: false, error: 'file_size_too_large', oversizedFileCount};
	}
	const attachments = await CloudUpload.addFiles(channelId, fileArray);
	MessageQueue.startTextareaAttachmentUploads(channelId, attachments);
	return {success: true};
}

export async function convertTextToFile(
	channelId: string,
	text: string,
	currentAttachmentCount: number,
	maxAttachments: number,
): Promise<FileUploadResult> {
	const trimmedText = text.trim();
	if (!trimmedText) {
		return {success: false, error: 'empty_text'};
	}
	const blob = new Blob([text], {type: 'text/plain'});
	const file = new File([blob], 'message.txt', {type: 'text/plain'});
	return handleFileUpload(channelId, [file], currentAttachmentCount, maxAttachments);
}
