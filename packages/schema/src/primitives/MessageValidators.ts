// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AllowedMentionParseTypes,
	AllowedMentionParseTypesDescriptions,
	MessageReferenceTypes,
	MessageReferenceTypesDescriptions,
	MessageTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {createInt32EnumType, createNamedStringLiteralUnion} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const MessageTypeSchema = createInt32EnumType(
	[
		[MessageTypes.DEFAULT, 'DEFAULT', 'A regular message'],
		[MessageTypes.RECIPIENT_ADD, 'RECIPIENT_ADD', 'A system message indicating a user was added to the conversation'],
		[
			MessageTypes.RECIPIENT_REMOVE,
			'RECIPIENT_REMOVE',
			'A system message indicating a user was removed from the conversation',
		],
		[MessageTypes.CALL, 'CALL', 'A message representing a call'],
		[MessageTypes.CHANNEL_NAME_CHANGE, 'CHANNEL_NAME_CHANGE', 'A system message indicating the channel name changed'],
		[MessageTypes.CHANNEL_ICON_CHANGE, 'CHANNEL_ICON_CHANGE', 'A system message indicating the channel icon changed'],
		[MessageTypes.CHANNEL_PINNED_MESSAGE, 'CHANNEL_PINNED_MESSAGE', 'A system message indicating a message was pinned'],
		[MessageTypes.USER_JOIN, 'USER_JOIN', 'A system message indicating a user joined'],
		[
			MessageTypes.CHANNEL_FOLLOW_ADD,
			'CHANNEL_FOLLOW_ADD',
			'System message posted when a channel starts following an announcement channel',
		],
		[MessageTypes.THREAD_CREATED, 'THREAD_CREATED', 'A system message indicating a thread was created'],
		[MessageTypes.REPLY, 'REPLY', 'A reply message'],
		[
			MessageTypes.THREAD_STARTER_MESSAGE,
			'THREAD_STARTER_MESSAGE',
			'The first message of a thread, pointing at its source',
		],
	],
	'The type of message',
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

function isInvisibleContentCodePoint(codePoint: number): boolean {
	return (
		codePoint === 0x0000 ||
		codePoint === 0x00ad ||
		codePoint === 0x034f ||
		codePoint === 0x061c ||
		codePoint === 0x115f ||
		codePoint === 0x1160 ||
		codePoint === 0x17b4 ||
		codePoint === 0x17b5 ||
		codePoint === 0x180e ||
		(codePoint >= 0x200b && codePoint <= 0x200f) ||
		(codePoint >= 0x202a && codePoint <= 0x202e) ||
		(codePoint >= 0x2060 && codePoint <= 0x2069) ||
		codePoint === 0x2800 ||
		codePoint === 0x3164 ||
		(codePoint >= 0xfe00 && codePoint <= 0xfe0f) ||
		codePoint === 0xfeff ||
		codePoint === 0xffa0 ||
		(codePoint >= 0xe0100 && codePoint <= 0xe01ef)
	);
}

export function hasVisibleContent(value: string): boolean {
	for (const char of value) {
		if (char.trim().length === 0) {
			continue;
		}
		const codePoint = char.codePointAt(0);
		if (codePoint == null || isInvisibleContentCodePoint(codePoint)) {
			continue;
		}
		return true;
	}
	return false;
}
