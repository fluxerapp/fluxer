// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AllowedMentionParseTypes,
	AllowedMentionParseTypesDescriptions,
	MessageReferenceTypes,
	MessageReferenceTypesDescriptions,
	MessageTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {
	createInt32EnumType,
	createNamedStringLiteralUnion,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const MessageTypeSchema = withOpenApiType(
	createInt32EnumType(
		[
			[MessageTypes.DEFAULT, 'DEFAULT', 'A regular message'],
			[MessageTypes.POLL_RESULT, 'POLL_RESULT', "A system message indicating a poll has closed"],
			[MessageTypes.RECIPIENT_ADD, 'RECIPIENT_ADD', 'A system message indicating a user was added to the conversation'],
			[
				MessageTypes.RECIPIENT_REMOVE,
				'RECIPIENT_REMOVE',
				'A system message indicating a user was removed from the conversation',
			],
			[MessageTypes.CALL, 'CALL', 'A message representing a call'],
			[MessageTypes.CHANNEL_NAME_CHANGE, 'CHANNEL_NAME_CHANGE', 'A system message indicating the channel name changed'],
			[MessageTypes.CHANNEL_ICON_CHANGE, 'CHANNEL_ICON_CHANGE', 'A system message indicating the channel icon changed'],
			[
				MessageTypes.CHANNEL_PINNED_MESSAGE,
				'CHANNEL_PINNED_MESSAGE',
				'A system message indicating a message was pinned',
			],
			[MessageTypes.USER_JOIN, 'USER_JOIN', 'A system message indicating a user joined'],
			[MessageTypes.REPLY, 'REPLY', 'A reply message'],
		],
		'The type of message',
	),
	'MessageType',
);
export const MessageReferenceTypeSchema = createInt32EnumType(
	[
		[MessageReferenceTypes.DEFAULT, 'DEFAULT', MessageReferenceTypesDescriptions.DEFAULT],
		[MessageReferenceTypes.FORWARD, 'FORWARD', MessageReferenceTypesDescriptions.FORWARD],
	],
	'The type of message reference',
	'MessageReferenceType',
);
export const AllowedMentionParseTypeSchema = createNamedStringLiteralUnion(
	[
		[AllowedMentionParseTypes.USERS, 'USERS', AllowedMentionParseTypesDescriptions.USERS],
		[AllowedMentionParseTypes.ROLES, 'ROLES', AllowedMentionParseTypesDescriptions.ROLES],
		[AllowedMentionParseTypes.EVERYONE, 'EVERYONE', AllowedMentionParseTypesDescriptions.EVERYONE],
	],
	'Types of mentions to parse from content',
);
