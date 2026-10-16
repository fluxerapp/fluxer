// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiMessageEditAttachmentMetadata} from '@app/features/messaging/utils/MessageRequestUtils';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';

interface MessageWithAttachments {
	attachments: ReadonlyArray<{id: string}>;
}

interface MessageWithContent extends MessageWithAttachments {
	content: string;
}

export function isAttachmentOnlyMessage(message: MessageWithContent): boolean {
	return message.content.length === 0 && message.attachments.length > 0;
}

export function canSubmitEmptyMessageEdit(message: MessageWithAttachments): boolean {
	return AdvancedSettings.keepAttachmentsOnEmptyMessageEdit && message.attachments.length > 0;
}

export function buildExistingAttachmentEditReferences(
	message: MessageWithAttachments,
): Array<ApiMessageEditAttachmentMetadata> {
	return message.attachments.map((attachment) => ({id: attachment.id}));
}
