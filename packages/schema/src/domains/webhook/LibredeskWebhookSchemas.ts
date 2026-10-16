// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUnboundedStringType, NonNegativeSafeIntegerType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const LibredeskPerson = z.object({
	first_name: createUnboundedStringType().nullish(),
	last_name: createUnboundedStringType().nullish(),
});

const LibredeskConversationSummary = z.object({
	uuid: createUnboundedStringType().nullish(),
	reference_number: createUnboundedStringType().nullish(),
	subject: createUnboundedStringType().nullish(),
	inbox_name: createUnboundedStringType().nullish(),
});

const LibredeskConversation = LibredeskConversationSummary.extend({
	status_category: createUnboundedStringType().nullish(),
});

const LibredeskMessageMeta = z.object({
	is_automated: z.boolean().nullish(),
	is_csat: z.boolean().nullish(),
	continuity_email: z.boolean().nullish(),
});

const LibredeskAttachment = z.object({
	name: createUnboundedStringType().nullish(),
});

const LibredeskPayload = LibredeskConversationSummary.extend({
	contact: LibredeskPerson.nullish(),
	conversation_uuid: createUnboundedStringType().nullish(),
	conversation: LibredeskConversation.nullish(),
	previous_status: createUnboundedStringType().nullish(),
	new_status: createUnboundedStringType().nullish(),
	snooze_until: createUnboundedStringType().nullish(),
	previous_tags: z.array(createUnboundedStringType()).nullish(),
	new_tags: z.array(createUnboundedStringType()).nullish(),
	assigned_to: NonNegativeSafeIntegerType.nullish(),
	type: createUnboundedStringType().nullish(),
	status: createUnboundedStringType().nullish(),
	text_content: createUnboundedStringType().nullish(),
	private: z.boolean().nullish(),
	sender_type: createUnboundedStringType().nullish(),
	meta: LibredeskMessageMeta.nullish(),
	attachments: z.array(LibredeskAttachment).nullish(),
	author: LibredeskPerson.nullish(),
	name: createUnboundedStringType().nullish(),
});

export const LibredeskWebhook = z.object({
	event: createUnboundedStringType(),
	payload: LibredeskPayload,
});

export type LibredeskWebhook = z.infer<typeof LibredeskWebhook>;
