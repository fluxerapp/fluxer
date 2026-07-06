// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {describe, expect, it, vi} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import type {MessageRow} from '../../database/types/MessageTypes';
import {Message} from '../../models/Message';
import {closeExpiredPollCore} from '../tasks/CloseExpiredPoll';

const CHANNEL_ID = createChannelID(100n);
const MESSAGE_ID = createMessageID(200n);
const POLL_ID = createMessageID(300n);
const AUTHOR_ID = createUserID(400n);

type CloseExpiredPollDependencies = Parameters<typeof closeExpiredPollCore>[2];
type CloseExpiredPollOptions = NonNullable<Parameters<typeof closeExpiredPollCore>[3]>;
type DispatchMessageUpdate = NonNullable<CloseExpiredPollOptions['dispatchMessageUpdate']>;

function makeMessage(expiresAt: Date, closedAt: Date | null = null): Message {
	const row: MessageRow = {
		channel_id: CHANNEL_ID,
		bucket: 0,
		message_id: MESSAGE_ID,
		author_id: AUTHOR_ID,
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: null,
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		poll: {
			poll_id: POLL_ID,
			title: 'Deploy window',
			options: [
				{option_id: createMessageID(301n), text: 'Morning', attachment_id: null, vote_count: 0},
				{option_id: createMessageID(302n), text: 'Afternoon', attachment_id: null, vote_count: 0},
			],
			expires_at: expiresAt,
			closed_at: closedAt,
			anonymous: false,
			allow_ranked_choice: false,
			allow_custom_answers: false,
		},
		nsfw_emojis: null,
		has_reaction: null,
		version: 0,
	};
	return new Message(row);
}

function makeHarness(message: Message | null) {
	const upsertMessage = vi.fn(async (row: MessageRow) => new Message(row));
	const findUnique = vi.fn(async () => ({
		id: CHANNEL_ID,
		type: ChannelTypes.GUILD_TEXT,
		guildId: null,
	}));
	const dependencies = {
		channelRepository: {
			messages: {
				getMessage: vi.fn(async () => message),
				upsertMessage,
			},
			channelData: {
				findUnique,
			},
		},
		gatewayService: {},
	} as unknown as CloseExpiredPollDependencies;
	const addJob = vi.fn(async () => 1n);
	const helpers = {
		addJob,
	} as unknown as Pick<WorkerTaskHelpers, 'addJob'>;
	const dispatchMessageUpdate = vi.fn(async () => undefined) as DispatchMessageUpdate;
	return {dependencies, dispatchMessageUpdate, findUnique, helpers, upsertMessage, addJob};
}

describe('CloseExpiredPoll', () => {
	it('requeues the close job when the poll has not expired yet', async () => {
		const now = new Date('2026-07-05T10:00:00.000Z');
		const expiresAt = new Date('2026-07-05T10:05:00.000Z');
		const {addJob, dependencies, dispatchMessageUpdate, helpers, upsertMessage} = makeHarness(makeMessage(expiresAt));

		await closeExpiredPollCore(
			{channelId: CHANNEL_ID.toString(), messageId: MESSAGE_ID.toString(), pollId: POLL_ID.toString()},
			helpers,
			dependencies,
			{now, dispatchMessageUpdate},
		);

		expect(addJob).toHaveBeenCalledWith(
			'closeExpiredPoll',
			{channelId: CHANNEL_ID.toString(), messageId: MESSAGE_ID.toString(), pollId: POLL_ID.toString()},
			{jobKey: `close-poll:${MESSAGE_ID}:${POLL_ID}`, runAt: expiresAt},
		);
		expect(upsertMessage).not.toHaveBeenCalled();
		expect(dispatchMessageUpdate).not.toHaveBeenCalled();
	});

	it('sets closed_at and dispatches a message update after expiry', async () => {
		const now = new Date('2026-07-05T10:00:00.000Z');
		const {addJob, dependencies, dispatchMessageUpdate, helpers, upsertMessage} = makeHarness(
			makeMessage(new Date('2026-07-05T09:59:00.000Z')),
		);

		await closeExpiredPollCore(
			{channelId: CHANNEL_ID.toString(), messageId: MESSAGE_ID.toString(), pollId: POLL_ID.toString()},
			helpers,
			dependencies,
			{now, dispatchMessageUpdate},
		);

		expect(addJob).not.toHaveBeenCalled();
		expect(upsertMessage).toHaveBeenCalledOnce();
		expect(upsertMessage.mock.calls[0]![0].poll?.closed_at).toEqual(now);
		expect(dispatchMessageUpdate).toHaveBeenCalledOnce();
	});

	it('skips stale poll close jobs', async () => {
		const now = new Date('2026-07-05T10:00:00.000Z');
		const {addJob, dependencies, dispatchMessageUpdate, helpers, upsertMessage} = makeHarness(
			makeMessage(new Date('2026-07-05T09:59:00.000Z')),
		);

		await closeExpiredPollCore(
			{channelId: CHANNEL_ID.toString(), messageId: MESSAGE_ID.toString(), pollId: '999'},
			helpers,
			dependencies,
			{now, dispatchMessageUpdate},
		);

		expect(addJob).not.toHaveBeenCalled();
		expect(upsertMessage).not.toHaveBeenCalled();
		expect(dispatchMessageUpdate).not.toHaveBeenCalled();
	});
});
