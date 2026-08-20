// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export const ATTACHMENT_CARD_WIDTH = 400;

const IMAGE_TYPES = ['image/png', 'image/jpeg', 'image/jpg', 'image/gif', 'image/webp', 'image/avif'];
const VIDEO_TYPES = ['video/mp4', 'video/webm', 'video/quicktime'];

function isImageType(contentType?: string): boolean {
	return contentType ? IMAGE_TYPES.includes(contentType) : false;
}

function isVideoType(contentType?: string): boolean {
	return contentType ? VIDEO_TYPES.includes(contentType) : false;
}

const hasRenderableDimensions = (attachment: MessageAttachment): boolean =>
	typeof attachment.width === 'number' &&
	attachment.width > 0 &&
	typeof attachment.height === 'number' &&
	attachment.height > 0;
export const isMediaAttachment = (attachment: MessageAttachment): boolean =>
	hasRenderableDimensions(attachment) && (isImageType(attachment.content_type) || isVideoType(attachment.content_type));

export function splitMediaAndFileAttachments(attachments: ReadonlyArray<MessageAttachment>): {
	mediaAttachments: Array<MessageAttachment>;
	fileAttachments: Array<MessageAttachment>;
} {
	const mediaAttachments: Array<MessageAttachment> = [];
	const fileAttachments: Array<MessageAttachment> = [];
	for (const attachment of attachments) {
		if (isMediaAttachment(attachment)) {
			mediaAttachments.push(attachment);
		} else {
			fileAttachments.push(attachment);
		}
	}
	return {mediaAttachments, fileAttachments};
}
