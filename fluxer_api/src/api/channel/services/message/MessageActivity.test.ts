// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createAttachmentID, createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {emitMessageCreated} from '@app/api/channel/services/message/MessageActivity';
import {createDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import {
	idleActivityEvents,
	resetActivityEventsForTests,
	startActivityEvents,
} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Channel} from '@app/api/models/Channel';
import {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const HASH = '3F'.repeat(32);
const channelRepository = new ChannelRepository();
const userRepository = new UserRepository();

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

function message(attachments: Array<MessageAttachment>, content = ''): Message {
	return new Message({
		channel_id: createChannelID(10n),
		bucket: 0,
		message_id: createMessageID(100n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content,
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
	private readonly payloads: Array<unknown> = [];
	private readonly waiters: Array<(event: unknown) => void> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		const event: unknown = JSON.parse(payload);
		const waiter = this.waiters.shift();
		if (waiter) {
			waiter(event);
		} else {
			this.payloads.push(event);
		}
	}

	next(): Promise<unknown> {
		const event = this.payloads.shift();
		if (event !== undefined) {
			return Promise.resolve(event);
		}
		return new Promise((resolve) => this.waiters.push(resolve));
	}
}

describe('message activity', () => {
	let harness: ApiTestHarness;
	let publisher: CapturingPublisher;
	let author: TestAccount;
	let user: User;
	let channel: Channel;
	let recipient: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		author = await createTestAccount(harness);
		recipient = await createTestAccount(harness);
		const dm = await createDmChannel(harness, author.token, recipient.userId);
		const storedUser = await userRepository.findUnique(createUserID(BigInt(author.userId)));
		const storedChannel = await channelRepository.channelData.findUnique(createChannelID(BigInt(dm.id)));
		assert(storedUser !== null && storedChannel !== null);
		user = storedUser;
		channel = storedChannel;
		publisher = new CapturingPublisher();
	});

	afterEach(async () => {
		await idleActivityEvents();
		resetActivityEventsForTests();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function params(attachments: Array<MessageAttachment>, content = ''): Parameters<typeof emitMessageCreated>[0] {
		return {
			user,
			message: new Message({...message(attachments, content).toRow(), author_id: user.id, channel_id: channel.id}),
			channel,
			guildId: null,
			guildOwnerId: null,
			dmRecipientId: createUserID(BigInt(recipient.userId)),
			channelHadMessages: true,
			delivered: true,
			userRepository,
		};
	}

	it('serializes attachment metadata', async () => {
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		emitMessageCreated(params([attachment(1n, HASH), attachment(2n, null)]));
		expect(await publisher.next()).toMatchObject({
			kind: 'message_created',
			key: author.userId,
			data: {
				channel_type: 'dm',
				attachment_count: 2,
				attachments: [
					{size: 10, content_type: 'image/png', hash: '3f'.repeat(32)},
					{size: 20, content_type: 'image/png', hash: null},
				],
			},
		});
	});

	it('records the link domain of a masked markdown link without its brackets', async () => {
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		emitMessageCreated(
			params(
				[],
				'[OPEN](https://Shop.Example.com) [docs](<https://www.docs.example.org/a>) https://user@cdn.example.net:8443/x',
			),
		);
		expect(await publisher.next()).toMatchObject({
			data: {link_domains: ['shop.example.com', 'docs.example.org', 'cdn.example.net']},
		});
	});
});
