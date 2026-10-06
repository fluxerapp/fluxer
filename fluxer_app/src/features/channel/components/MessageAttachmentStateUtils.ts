// SPDX-License-Identifier: AGPL-3.0-or-later

import {splitMediaAndFileAttachments} from '@app/features/channel/components/MessageAttachmentUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {mapAttachmentsWithExpiry} from '@app/features/messaging/utils/AttachmentExpiryUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

interface AttachmentRenderingState {
	enrichedAttachments: Array<MessageAttachment>;
	activeAttachments: Array<MessageAttachment>;
	mediaAttachments: Array<MessageAttachment>;
	shouldUseMosaic: boolean;
}

function freshAttachmentUrl(url: string | null): string | null {
	return url === null ? null : AttachmentUrlRefresher.fresh(url);
}

export function withFreshAttachmentUrls(attachment: MessageAttachment): MessageAttachment {
	const url = freshAttachmentUrl(attachment.url);
	const proxyUrl = freshAttachmentUrl(attachment.proxy_url);
	if (url === attachment.url && proxyUrl === attachment.proxy_url) return attachment;
	return {...attachment, url, proxy_url: proxyUrl};
}

export const getAttachmentRenderingState = (
	snapshotAttachments?: ReadonlyArray<MessageAttachment> | null,
): AttachmentRenderingState => {
	const attachments = snapshotAttachments ?? [];
	const expiryApplied = mapAttachmentsWithExpiry(attachments, DeveloperOptions.mockAttachmentStates).map((entry) => ({
		isExpired: entry.isExpired,
		attachment: withFreshAttachmentUrls(entry.attachment),
	}));
	const enrichedAttachments = expiryApplied.map((entry) => entry.attachment);
	const activeAttachments = expiryApplied.filter((entry) => !entry.isExpired).map((entry) => entry.attachment);
	const {mediaAttachments} = splitMediaAndFileAttachments(activeAttachments);
	const shouldUseMosaic = mediaAttachments.length > 0 && UserSettings.getInlineAttachmentMedia();
	return {
		enrichedAttachments,
		activeAttachments,
		mediaAttachments,
		shouldUseMosaic,
	};
};
