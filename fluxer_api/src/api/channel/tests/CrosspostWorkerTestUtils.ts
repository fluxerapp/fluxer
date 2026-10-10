// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createWebhookID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	type AnnouncementWorld,
	announcementWorld,
	enableCrosspostWorker,
	follow,
	publish,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import type {ChannelRow, CrosspostedMessageRow} from '@app/api/database/types/ChannelTypes';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import {ensureSessionStarted, getMessages} from '@app/api/message/tests/MessageTestUtils';
import type {Message} from '@app/api/models/Message';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';

export interface FanoutWorld extends AnnouncementWorld {
	worker: SyncTaskWorkerService;
}

export async function setupFanoutWorld(harness: ApiTestHarness): Promise<FanoutWorld> {
	const world = await announcementWorld(harness);
	for (const account of [world.a.owner, world.a.member, world.a.moderator, world.b.owner, world.b.webhookManager]) {
		await ensureSessionStarted(harness, account.token);
	}
	const worker = enableCrosspostWorker();
	return {...world, worker};
}

export async function followInto(
	harness: ApiTestHarness,
	world: FanoutWorld,
	targetChannelId: string,
	token = world.b.owner.token,
	sourceChannelId = world.a.ann.id,
): Promise<string> {
	const response = await follow(harness, token, sourceChannelId, targetChannelId);
	await world.worker.drain();
	return response.webhook_id;
}

export async function sendMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	body: Record<string, unknown>,
): Promise<MessageResponse> {
	return createBuilder<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body(body)
		.expect(200)
		.execute();
}

export async function sendWithImage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	payload: Record<string, unknown>,
	filenames: Array<string> = ['yeah.png'],
): Promise<MessageResponse> {
	const {response, json, text} = await sendMessageWithAttachments(
		harness,
		token,
		channelId,
		{...payload, attachments: filenames.map((filename, index) => ({id: index, filename}))},
		filenames.map((filename, index) => ({
			index,
			filename,
			data: loadFixture(filename.endsWith('.gif') ? 'animated.gif' : 'yeah.png'),
		})),
	);
	if (response.status !== 200) {
		throw new Error(`upload failed with ${response.status}: ${text}`);
	}
	return json;
}

export async function publishAndDrain(
	harness: ApiTestHarness,
	world: FanoutWorld,
	messageId: string,
	token = world.a.owner.token,
	channelId = world.a.ann.id,
): Promise<MessageResponse> {
	const published = await publish(harness, token, channelId, messageId);
	await world.worker.drain();
	return published;
}

export async function postAndPublish(
	harness: ApiTestHarness,
	world: FanoutWorld,
	body: Record<string, unknown>,
	channelId = world.a.ann.id,
): Promise<MessageResponse> {
	const message = await sendMessage(harness, world.a.owner.token, channelId, body);
	await publishAndDrain(harness, world, message.id, world.a.owner.token, channelId);
	return message;
}

export async function listCopies(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<Array<MessageResponse>> {
	const messages = await getMessages(harness, token, channelId, {limit: '100'});
	return messages.filter(
		(message) => message.type === MessageTypes.DEFAULT && (message.flags & MessageFlags.IS_CROSSPOST) !== 0,
	);
}

export async function copiesOf(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	sourceMessageId: string,
): Promise<Array<MessageResponse>> {
	const copies = await listCopies(harness, token, channelId);
	return copies.filter((copy) => copy.message_reference?.message_id === sourceMessageId);
}

export async function readRow(channelId: string, messageId: string): Promise<Message | null> {
	return new ChannelRepository().messages.getMessage(
		createChannelID(BigInt(channelId)),
		createMessageID(BigInt(messageId)),
	);
}

export async function mappingRow(sourceMessageId: string, webhookId: string): Promise<CrosspostedMessageRow | null> {
	return new ChannelRepository().crossposts.get(
		createMessageID(BigInt(sourceMessageId)),
		createWebhookID(BigInt(webhookId)),
	);
}

export async function mappingRows(sourceMessageId: string): Promise<Array<CrosspostedMessageRow>> {
	return new ChannelRepository().crossposts.listBySourceMessage(createMessageID(BigInt(sourceMessageId)), {
		limit: 1000,
	});
}

export async function sourceIndex(channelId: string): Promise<Array<string>> {
	const ids = await new ChannelRepository().crossposts.listSourcesByChannel(createChannelID(BigInt(channelId)), {
		limit: 1000,
	});
	return ids.map((id) => id.toString());
}

export function editRequest(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	body: Record<string, unknown>,
) {
	return createBuilder<MessageResponse>(harness, token)
		.patch(`/channels/${channelId}/messages/${messageId}`)
		.body(body);
}

export async function deleteMessageRequest(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<void> {
	await createBuilder(harness, token).delete(`/channels/${channelId}/messages/${messageId}`).expect(204).execute();
}

export function workerHelpers(worker: SyncTaskWorkerService): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 0n,
		addJob: (taskType, payload, options) => worker.addJob(taskType, payload, options),
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

export async function patchChannelRow(channelId: string, patch: Partial<ChannelRow>): Promise<void> {
	const repository = new ChannelRepository();
	const channel = await repository.channelData.findUnique(createChannelID(BigInt(channelId)));
	if (!channel) {
		throw new Error(`channel ${channelId} not found`);
	}
	await repository.channelData.upsert({...channel.toRow(), ...patch});
}

export async function writeRow(message: Message, patch: Partial<MessageRow>): Promise<void> {
	await new ChannelRepository().messages.upsertMessage({...message.toRow(), ...patch}, message.toRow());
}

export function patchChannel(harness: ApiTestHarness, token: string, channelId: string, body: Record<string, unknown>) {
	return createBuilder<ChannelResponse>(harness, token).patch(`/channels/${channelId}`).body(body);
}
