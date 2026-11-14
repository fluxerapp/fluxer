// SPDX-License-Identifier: AGPL-3.0-or-later

import {type CloudAttachment, CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import type {ApiAttachmentMetadata} from '@app/features/messaging/utils/MessageRequestUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('MessageAttachmentUtils');

export interface PreparedMessageAttachments {
	attachments?: Array<ApiAttachmentMetadata>;
	files?: Array<File>;
}

export async function prepareAttachmentsForNonce(
	nonce: string,
	favoriteMemeId?: string,
): Promise<PreparedMessageAttachments> {
	logger.debug(`Preparing attachments for nonce ${nonce}`);
	const messageUpload = CloudUpload.getMessageUpload(nonce);
	if (!messageUpload) {
		throw new Error('No message upload found');
	}
	const inlineAttachments = messageUpload.attachments;
	const files = inlineAttachments.map((att) => att.file);
	const attachments = favoriteMemeId ? undefined : mapMessageUploadAttachments(messageUpload.attachments);
	return {attachments, files};
}

function mapMessageUploadAttachments(attachments: Array<CloudAttachment>): Array<ApiAttachmentMetadata> {
	return attachments.map((att, index) => ({
		id: String(index),
		filename: att.filename,
		title: att.filename,
		description: att.description,
		flags: att.flags,
		duration: att.duration != null ? Math.ceil(att.duration) : undefined,
		waveform: att.waveform ?? undefined,
	}));
}
