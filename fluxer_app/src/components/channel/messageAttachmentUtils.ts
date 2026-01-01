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

import type {MessageAttachment} from '~/records/MessageRecord';

export const isImageType = (contentType?: string): boolean => contentType?.startsWith('image/') ?? false;

export const isVideoType = (contentType?: string): boolean => contentType?.startsWith('video/') ?? false;

export const isAudioType = (contentType?: string): boolean => contentType?.startsWith('audio/') ?? false;

export const isGifType = (contentType?: string): boolean => contentType === 'image/gif';

const hasDimensions = (attachment: MessageAttachment): boolean =>
	typeof attachment.width === 'number' && typeof attachment.height === 'number';

export const isMediaAttachment = (attachment: MessageAttachment): boolean =>
	hasDimensions(attachment) &&
	(isImageType(attachment.content_type) ||
		isVideoType(attachment.content_type) ||
		isAudioType(attachment.content_type));

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
