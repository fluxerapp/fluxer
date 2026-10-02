// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {emitMessageCreated, emitMessageUpdated} from '@app/api/channel/services/message/MessageActivity';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Channel} from '@app/api/models/Channel';
import {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, describe, expect, it, vi} from 'vitest';

const HASH = '3F'.repeat(32);

function attachment(id: bigint, hash: string | null): MessageAttachment {
	return {
		attachment_id: createAttachmentID(id),
		filename: `${id}.png`,
		size: 10n * id,
		title: null,
		description: null,
		width: 1,
		height: 1,
		content_type: 'image/png',
		content_hash: hash,
		placeholder: null,
		flags: 0,
		duration: null,
		nsfw: null,
		waveform: null,
	};
}

function message(attachments: Array<MessageAttachment>): Message {
	return new Message({
		channel_id: createChannelID(10n),
		bucket: 0,
		message_id: createMessageID(100n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: '',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		has_reaction: null,
		version: 1,
	});
}

class CapturingPublisher implements ActivityPublisher {
	readonly payloads: Array<string> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.payloads.push(payload);
	}
}

describe('message activity', () => {
	afterEach(() => {
		resetActivityEventsForTests();
	});

	function params(attachments: Array<MessageAttachment>) {
		return {
			user: {id: createUserID(3n), isBot: false} as unknown as User,
			message: message(attachments),
			channel: {id: createChannelID(10n), type: ChannelTypes.DM} as unknown as Channel,
			guildId: null,
			guildOwnerId: null,
			dmRecipientId: createUserID(4n),
			channelHadMessages: true,
			delivered: true,
			userRepository: {getRelationship: async () => null},
		};
	}

	it('serializes attachment metadata', async () => {
		const publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		emitMessageCreated(params([attachment(1n, HASH), attachment(2n, null)]));
		await vi.waitFor(() => expect(publisher.payloads).toHaveLength(1));
		const event = JSON.parse(publisher.payloads[0]!);
		expect([event.kind, event.key]).toEqual(['message_created', '3']);
		expect(event.data).toMatchObject({
			attachment_count: 2,
			attachments: [
				{size: 10, content_type: 'image/png', hash: HASH.toLowerCase()},
				{size: 20, content_type: 'image/png', hash: null},
			],
		});
	});

	it('serializes message updates', async () => {
		const publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		emitMessageCreated(params([]));
		emitMessageUpdated(params([attachment(1n, HASH)]));
		await vi.waitFor(() => expect(publisher.payloads).toHaveLength(2));
		const [created, updated] = publisher.payloads.map((payload) => JSON.parse(payload));
		expect(created.kind).toBe('message_created');
		expect(updated.kind).toBe('message_updated');
		expect(updated.data).toMatchObject({message_id: '100', attachments: [{hash: HASH.toLowerCase()}]});
		expect(updated.id).not.toBe(created.id);
	});
});
