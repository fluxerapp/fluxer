// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {StringCodec} from 'nats';
import type {ChannelID, GuildID, MessageID, UserID} from '../../../BrandedTypes';
import {createUserID} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import {throwForSvcErrorReply} from '../../../infrastructure/SvcErrorReply';
import {Logger} from '../../../Logger';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import {isJsonRecord, parseJsonRecord, parseJsonWithGuard} from '../../../utils/JsonBoundaryUtils';

const MESSAGE_RESPONSE_SERVICE_SUBJECT = 'svc.messages';
const MESSAGE_RESPONSE_SERVICE_TIMEOUT_MS = 6000;
export const MESSAGE_BUILD_BATCH_MAX_BYTES = 262_144;
let messageResponseDataService: MessageResponseDataService | undefined;
let injectedMessageResponseDataService: MessageResponseDataService | undefined;

export interface MessageResponseAccessContext {
	sourceGuildId: GuildID | null;
	messageHistoryCutoff: string | null;
	canReadMessageHistory: boolean;
}

interface ExtractedMentions {
	users: Array<string>;
	roles: Array<string>;
	channels: Array<string>;
	everyone: boolean;
	here: boolean;
}

type MessageServiceResponse =
	| {
			FoundApi: MessageResponse;
	  }
	| {
			FoundApiMany: Array<MessageResponse>;
	  }
	| {
			FoundMentions: Array<ExtractedMentions>;
	  }
	| 'NotFound';

function isMessageResponse(value: unknown): value is MessageResponse {
	return isJsonRecord(value) && typeof value.id === 'string';
}

function isExtractedMentions(value: unknown): value is ExtractedMentions {
	if (!isJsonRecord(value)) return false;
	return (
		Array.isArray(value.users) &&
		value.users.every((entry) => typeof entry === 'string') &&
		Array.isArray(value.roles) &&
		value.roles.every((entry) => typeof entry === 'string') &&
		Array.isArray(value.channels) &&
		value.channels.every((entry) => typeof entry === 'string') &&
		typeof value.everyone === 'boolean' &&
		typeof value.here === 'boolean'
	);
}

function isMessageServiceResponse(value: unknown): value is MessageServiceResponse {
	if (value === 'NotFound') return true;
	if (!isJsonRecord(value)) return false;
	if ('FoundApi' in value) {
		return isMessageResponse(value.FoundApi);
	}
	if ('FoundApiMany' in value) {
		return Array.isArray(value.FoundApiMany) && value.FoundApiMany.every(isMessageResponse);
	}
	if ('FoundMentions' in value) {
		return Array.isArray(value.FoundMentions) && value.FoundMentions.every(isExtractedMentions);
	}
	return false;
}

export class MessageResponseDataService {
	private readonly codec = StringCodec();

	constructor(private readonly connectionManager: INatsConnectionManager) {}

	async listMessages(params: {
		userId: UserID;
		channelId: ChannelID;
		limit: number;
		before?: MessageID;
		after?: MessageID;
		around?: MessageID;
		access: MessageResponseAccessContext;
	}): Promise<Array<MessageResponse>> {
		const response = await this.request({
			op: 'ListResponses',
			channel_id: params.channelId.toString(),
			viewer_user_id: params.userId.toString(),
			limit: params.limit,
			before_id: params.before?.toString(),
			after_id: params.after?.toString(),
			around_id: params.around?.toString(),
			source_guild_id: params.access.sourceGuildId?.toString(),
			message_history_cutoff_ms: params.access.messageHistoryCutoff
				? new Date(params.access.messageHistoryCutoff).getTime()
				: null,
			can_read_message_history: params.access.canReadMessageHistory,
			media_endpoint: Config.endpoints.media,
			media_proxy_secret_key: Config.mediaProxy.secretKey,
			include_reactions: true,
		});
		if (typeof response === 'object' && 'FoundApiMany' in response) {
			return response.FoundApiMany;
		}
		throw new Error(`[message-response-service] unexpected ListResponses response: ${JSON.stringify(response)}`);
	}

	async extractMentions(contents: Array<string>): Promise<Array<ExtractedMentions>> {
		if (contents.length === 0) return [];
		const response = await this.request({
			op: 'ExtractMentions',
			contents,
		});
		if (typeof response === 'object' && 'FoundMentions' in response) {
			return response.FoundMentions;
		}
		throw new Error(`[message-response-service] unexpected ExtractMentions response: ${JSON.stringify(response)}`);
	}

	async getMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		access: MessageResponseAccessContext;
		nonce?: string;
		tts?: boolean;
	}): Promise<MessageResponse | null> {
		const response = await this.request({
			op: 'GetResponseById',
			channel_id: params.channelId.toString(),
			message_id: params.messageId.toString(),
			viewer_user_id: params.userId.toString(),
			source_guild_id: params.access.sourceGuildId?.toString(),
			message_history_cutoff_ms: params.access.messageHistoryCutoff
				? new Date(params.access.messageHistoryCutoff).getTime()
				: null,
			can_read_message_history: params.access.canReadMessageHistory,
			media_endpoint: Config.endpoints.media,
			media_proxy_secret_key: Config.mediaProxy.secretKey,
			include_reactions: true,
			nonce: params.nonce,
			tts: params.tts,
		});
		if (response === 'NotFound') return null;
		if (typeof response === 'object' && 'FoundApi' in response) {
			return response.FoundApi;
		}
		throw new Error(`[message-response-service] unexpected GetResponseById response: ${JSON.stringify(response)}`);
	}

	async buildMessage(params: {
		userId: UserID;
		message: Message;
		access: MessageResponseAccessContext;
		nonce?: string;
		tts?: boolean;
		includeReactions?: boolean;
	}): Promise<MessageResponse> {
		const response = await this.request({
			op: 'BuildResponse',
			message: serializeMessageForService(params.message),
			viewer_user_id: params.userId.toString(),
			source_guild_id: params.access.sourceGuildId?.toString(),
			message_history_cutoff_ms: params.access.messageHistoryCutoff
				? new Date(params.access.messageHistoryCutoff).getTime()
				: null,
			can_read_message_history: params.access.canReadMessageHistory,
			media_endpoint: Config.endpoints.media,
			media_proxy_secret_key: Config.mediaProxy.secretKey,
			include_reactions: params.includeReactions ?? true,
			nonce: params.nonce,
			tts: params.tts,
		});
		if (typeof response === 'object' && 'FoundApi' in response) {
			return response.FoundApi;
		}
		throw new Error(`[message-response-service] unexpected BuildResponse response: ${JSON.stringify(response)}`);
	}

	async buildMessageForChannel(params: {
		channel: Pick<Channel, 'guildId'>;
		message: Message;
		userId?: UserID;
		nonce?: string;
		tts?: boolean;
	}): Promise<MessageResponse> {
		return this.buildMessage({
			userId: params.userId ?? messageResponseViewerId(params.message),
			message: params.message,
			access: messageResponseAccessForChannel(params.channel),
			nonce: params.nonce,
			tts: params.tts,
		});
	}

	async buildBroadcastMessage(params: {
		channel: Pick<Channel, 'guildId'>;
		message: Message;
		userId?: UserID;
		nonce?: string;
		tts?: boolean;
		sourceGuildId?: GuildID | null;
	}): Promise<MessageResponse> {
		return this.buildMessage({
			userId: messageResponseViewerId(params.message, params.userId),
			message: params.message,
			access:
				params.sourceGuildId !== undefined
					? messageResponseAccessForGuild(params.sourceGuildId)
					: messageResponseAccessForChannel(params.channel),
			nonce: params.nonce,
			tts: params.tts,
			includeReactions: false,
		});
	}

	async buildMessages(params: {
		userId: UserID;
		messages: Array<Message>;
		access: MessageResponseAccessContext;
		includeReactions?: boolean;
	}): Promise<Array<MessageResponse>> {
		if (params.messages.length === 0) return [];
		const responses: Array<MessageResponse> = [];
		for (const batch of chunkMessagesForService(params.messages)) {
			const response = await this.request({
				op: 'BuildResponses',
				messages: batch,
				viewer_user_id: params.userId.toString(),
				source_guild_id: params.access.sourceGuildId?.toString(),
				message_history_cutoff_ms: params.access.messageHistoryCutoff
					? new Date(params.access.messageHistoryCutoff).getTime()
					: null,
				can_read_message_history: params.access.canReadMessageHistory,
				media_endpoint: Config.endpoints.media,
				media_proxy_secret_key: Config.mediaProxy.secretKey,
				include_reactions: params.includeReactions ?? true,
			});
			if (typeof response !== 'object' || !('FoundApiMany' in response)) {
				throw new Error(`[message-response-service] unexpected BuildResponses response: ${JSON.stringify(response)}`);
			}
			responses.push(...response.FoundApiMany);
		}
		return responses;
	}

	async buildMessagesForChannels(params: {
		userId: UserID;
		messages: Array<Message>;
		channelById: ReadonlyMap<string, Pick<Channel, 'guildId'>>;
		includeReactions?: boolean;
	}): Promise<Array<MessageResponse>> {
		const responses = new Array<MessageResponse | undefined>(params.messages.length);
		const groups = new Map<
			string,
			{
				access: MessageResponseAccessContext;
				entries: Array<{index: number; message: Message}>;
			}
		>();
		for (const [index, message] of params.messages.entries()) {
			const channel = params.channelById.get(message.channelId.toString());
			const sourceGuildId = channel?.guildId ?? null;
			const key = sourceGuildId?.toString() ?? 'dm';
			let group = groups.get(key);
			if (!group) {
				group = {
					access: messageResponseAccessForGuild(sourceGuildId),
					entries: [],
				};
				groups.set(key, group);
			}
			group.entries.push({index, message});
		}
		await Promise.all(
			Array.from(groups.values()).map(async (group) => {
				const mapped = await this.buildMessages({
					userId: params.userId,
					messages: group.entries.map((entry) => entry.message),
					access: group.access,
					includeReactions: params.includeReactions,
				});
				const mappedByMessageId = new Map(mapped.map((response) => [response.id, response] as const));
				for (const entry of group.entries) {
					responses[entry.index] = mappedByMessageId.get(entry.message.id.toString());
				}
			}),
		);
		return responses.filter((response): response is MessageResponse => response !== undefined);
	}

	private async request(payload: Record<string, unknown>): Promise<MessageServiceResponse> {
		try {
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			const response = await connection.request(
				MESSAGE_RESPONSE_SERVICE_SUBJECT,
				this.codec.encode(JSON.stringify(payload)),
				{timeout: MESSAGE_RESPONSE_SERVICE_TIMEOUT_MS},
			);
			const decoded = this.codec.decode(response.data);
			const parsed = parseJsonWithGuard(decoded, isMessageServiceResponse);
			if (!parsed) {
				throwForSvcErrorReply('message-response-service', parseJsonRecord(decoded));
				throw new Error('[message-response-service] invalid response payload');
			}
			return parsed;
		} catch (error) {
			Logger.error({error, op: payload.op}, '[message-response-service] request failed');
			throw error;
		}
	}
}

function messageResponseViewerId(message: Message, currentUserId?: UserID): UserID {
	return currentUserId ?? message.authorId ?? createUserID(0n);
}

export function messageResponseAccessForChannel(channel: Pick<Channel, 'guildId'>): MessageResponseAccessContext {
	return messageResponseAccessForGuild(channel.guildId);
}

export function messageResponseAccessForGuild(sourceGuildId: GuildID | null): MessageResponseAccessContext {
	return {
		sourceGuildId,
		messageHistoryCutoff: null,
		canReadMessageHistory: true,
	};
}

function serializeMessageForService(message: Message): Record<string, unknown> {
	const row = serializeValue(message.toRow()) as Record<string, unknown>;
	row.pinned = message.pinnedTimestamp != null;
	return row;
}

function chunkMessagesForService(messages: Array<Message>): Array<Array<Record<string, unknown>>> {
	const batches: Array<Array<Record<string, unknown>>> = [];
	let batch: Array<Record<string, unknown>> = [];
	let batchBytes = 0;
	for (const message of messages) {
		const serialized = serializeMessageForService(message);
		const bytes = Buffer.byteLength(JSON.stringify(serialized));
		if (batch.length > 0 && batchBytes + bytes > MESSAGE_BUILD_BATCH_MAX_BYTES) {
			batches.push(batch);
			batch = [];
			batchBytes = 0;
		}
		batch.push(serialized);
		batchBytes += bytes;
	}
	if (batch.length > 0) {
		batches.push(batch);
	}
	return batches;
}

function serializeValue(value: unknown): unknown {
	if (value == null) return value;
	if (typeof value === 'bigint') return value.toString();
	if (value instanceof Date) return value.getTime();
	if (value instanceof Set) return Array.from(value, serializeValue);
	if (Array.isArray(value)) return value.map(serializeValue);
	if (typeof value === 'object') {
		return Object.fromEntries(Object.entries(value).map(([key, nestedValue]) => [key, serializeValue(nestedValue)]));
	}
	return value;
}

export function createMessageResponseDataService(): MessageResponseDataService {
	if (injectedMessageResponseDataService !== undefined) {
		return injectedMessageResponseDataService;
	}
	if (messageResponseDataService != null) return messageResponseDataService;
	const manager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: 'fluxer-api-message-responses',
	});
	void manager.connect().catch((error) => {
		Logger.error({error}, '[message-response-service] Failed to establish NATS connection');
	});
	messageResponseDataService = new MessageResponseDataService(manager);
	return messageResponseDataService;
}

export function setInjectedMessageResponseDataService(service: MessageResponseDataService | undefined): void {
	injectedMessageResponseDataService = service;
	messageResponseDataService = undefined;
}
