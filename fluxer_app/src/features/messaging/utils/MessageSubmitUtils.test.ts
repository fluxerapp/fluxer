// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {claimMessageAttachments, createOptimisticMessage} from '@app/features/messaging/utils/MessageSubmitUtils';
import type {User} from '@app/features/user/models/User';
import {MessageFlags, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it, vi} from 'vitest';

const claimAttachmentsForMessage = vi.hoisted(() => vi.fn(() => []));

vi.mock('@app/features/messaging/models/MessagingMessage', () => ({
	Message: class {
		constructor(data: object) {
			Object.assign(this, data);
		}
	},
}));
vi.mock('@app/features/messaging/upload/CloudUpload', () => ({CloudUpload: {claimAttachmentsForMessage}}));
vi.mock('@app/features/messaging/state/ChatInputSettings', () => ({default: {convertEmoticons: false}}));
vi.mock('@app/features/user/state/UserSettings', () => ({default: {getSanitizeUrls: () => false}}));
vi.mock('@app/features/messaging/utils/EmoticonConversionUtils', () => ({
	convertEmoticonsToEmoji: (content: string) => content,
}));

const currentUser = {toJSON: () => ({id: 'me'})} as unknown as User;
const referencedMessage = {id: 'ref', author: {toJSON: () => ({id: 'other'})}} as unknown as Message;

describe('createOptimisticMessage', () => {
	it('still strips @silent followed by a space and marks the message silent', () => {
		expect(
			createOptimisticMessage({content: '@silent hello', channelId: 'c', nonce: 'n', currentUser}, []),
		).toMatchObject({
			content: 'hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
			type: MessageTypes.DEFAULT,
			state: MessageStates.SENDING,
		});
	});

	it('strips @silent before a line break and marks the message silent', () => {
		expect(
			createOptimisticMessage({content: '@silent\nhello', channelId: 'c', nonce: 'n', currentUser}, []),
		).toMatchObject({content: 'hello', flags: MessageFlags.SUPPRESS_NOTIFICATIONS});
	});

	it('marks an attachment captioned only by @silent as silent with no content', () => {
		const attachment = {id: 'upload'} as MessageAttachment;
		expect(
			createOptimisticMessage({content: '@silent', channelId: 'c', nonce: 'n', currentUser}, [attachment]),
		).toMatchObject({content: '', flags: MessageFlags.SUPPRESS_NOTIFICATIONS, attachments: [attachment]});
	});

	it('still marks a silent reply', () => {
		expect(
			createOptimisticMessage(
				{content: '@silent hi', channelId: 'c', nonce: 'n', currentUser, referencedMessage, replyMentioning: true},
				[],
			),
		).toMatchObject({
			content: 'hi',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
			type: MessageTypes.REPLY,
			message_reference: {channel_id: 'c', message_id: 'ref', type: 0},
			_allowedMentions: {replied_user: true},
		});
	});

	it('still leaves @silent later in the message as text', () => {
		expect(
			createOptimisticMessage({content: 'hello @silent', channelId: 'c', nonce: 'n', currentUser}, []),
		).toMatchObject({content: 'hello @silent', flags: 0});
	});
});

describe('claimMessageAttachments', () => {
	it('claims attachments captioned only by @silent as silent with no content', () => {
		claimMessageAttachments('c', 'n', '@silent');
		expect(claimAttachmentsForMessage).toHaveBeenLastCalledWith('c', 'n', undefined, {
			content: '',
			messageReference: undefined,
			allowedMentions: {replied_user: true},
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it('claims attachments with the caption after @silent and a line break', () => {
		claimMessageAttachments('c', 'n', '@silent\ncaption');
		expect(claimAttachmentsForMessage).toHaveBeenLastCalledWith('c', 'n', undefined, {
			content: 'caption',
			messageReference: undefined,
			allowedMentions: {replied_user: true},
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});
});
