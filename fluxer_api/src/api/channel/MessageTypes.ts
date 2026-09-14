// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentRequestData} from '@app/api/channel/AttachmentDTOs';
import type {
	MessageRequestSchemaType,
	MessageUpdateRequestSchemaType,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';

interface BaseMessageRequestType extends Omit<MessageRequestSchemaType, 'attachments'> {}

export interface MessageRequest extends BaseMessageRequestType {
	attachments?: Array<AttachmentRequestData>;
}

interface BaseMessageUpdateRequestType extends Omit<MessageUpdateRequestSchemaType, 'attachments'> {}

export interface MessageUpdateRequest extends BaseMessageUpdateRequestType {
	attachments?: Array<AttachmentRequestData>;
}
