// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageListResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {
	HarvestCreationResponseSchema,
	HarvestDownloadUrlResponse,
	HarvestStatusResponseSchema,
} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import type {HarvestSelfDataRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {
	SavedMessageEntryListResponse,
	SavedMessageEntryResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {z} from 'zod';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {SavedMessageEntry, UserContentService} from './UserContentService';

type HarvestCreationResponse = z.infer<typeof HarvestCreationResponseSchema>;
type HarvestStatusResponse = z.infer<typeof HarvestStatusResponseSchema>;
type HarvestLatestResponse = HarvestStatusResponse | null;

interface UserMentionsParams {
	userId: UserID;
	limit: number;
	roles: boolean;
	everyone: boolean;
	guilds: boolean;
	before?: MessageID;
	requestCache: RequestCache;
}

interface UserMentionDeleteParams {
	userId: UserID;
	messageId: MessageID;
}

interface UserMentionsReadParams {
	userId: UserID;
	messageIds: Array<MessageID>;
}

interface SavedMessagesParams {
	userId: UserID;
	limit: number;
	requestCache: RequestCache;
}

interface SaveMessageParams {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
	requestCache: RequestCache;
}

interface UnsaveMessageParams {
	userId: UserID;
	messageId: MessageID;
}

interface HarvestRequestParams {
	userId: UserID;
}

interface HarvestFilteredRequestParams {
	userId: UserID;
	filter: HarvestSelfDataRequest;
}

interface HarvestStatusParams {
	userId: UserID;
	harvestId: bigint;
}

interface HarvestDownloadParams {
	userId: UserID;
	harvestId: bigint;
	storageService: IStorageService;
}

export class UserContentRequestService {
	constructor(
		private readonly userContentService: UserContentService,
		private readonly userCacheService: UserCacheService,
	) {}

	async listMentions(params: UserMentionsParams): Promise<MessageListResponse> {
		const messages = await this.userContentService.getRecentMentions({
			userId: params.userId,
			limit: params.limit,
			everyone: params.everyone,
			roles: params.roles,
			guilds: params.guilds,
			before: params.before,
		});
		return this.userContentService.buildMessageResponsesForUser(params.userId, messages);
	}

	async deleteMention(params: UserMentionDeleteParams): Promise<void> {
		await this.userContentService.deleteRecentMention({userId: params.userId, messageId: params.messageId});
	}

	async markMentionsRead(params: UserMentionsReadParams): Promise<void> {
		await this.userContentService.deleteRecentMentions({userId: params.userId, messageIds: params.messageIds});
	}

	async listSavedMessages(params: SavedMessagesParams): Promise<SavedMessageEntryListResponse> {
		const entries = await this.userContentService.getSavedMessages({userId: params.userId, limit: params.limit});
		return Promise.all(entries.map((entry) => this.mapSavedMessageEntry(params.userId, entry)));
	}

	async saveMessage(params: SaveMessageParams): Promise<void> {
		await this.userContentService.saveMessage({
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async unsaveMessage(params: UnsaveMessageParams): Promise<void> {
		await this.userContentService.unsaveMessage({userId: params.userId, messageId: params.messageId});
	}

	async requestHarvest(params: HarvestRequestParams): Promise<HarvestCreationResponse> {
		return this.userContentService.requestDataHarvest(params.userId);
	}

	async requestFilteredHarvest(params: HarvestFilteredRequestParams): Promise<HarvestCreationResponse> {
		return this.userContentService.requestFilteredDataHarvest({userId: params.userId, filter: params.filter});
	}

	async getLatestHarvest(params: HarvestRequestParams): Promise<HarvestLatestResponse> {
		return this.userContentService.getLatestHarvest(params.userId);
	}

	async getHarvestStatus(params: HarvestStatusParams): Promise<HarvestStatusResponse> {
		return this.userContentService.getHarvestStatus(params.userId, params.harvestId);
	}

	async getHarvestDownloadUrl(params: HarvestDownloadParams): Promise<HarvestDownloadUrlResponse> {
		return this.userContentService.getHarvestDownloadUrl(params.userId, params.harvestId, params.storageService);
	}

	async streamHarvestDownload(params: {
		harvestId: bigint;
		token: string;
		range?: string;
		storageService: IStorageService;
	}) {
		return this.userContentService.streamHarvestDownload(params);
	}

	private async mapSavedMessageEntry(userId: UserID, entry: SavedMessageEntry): Promise<SavedMessageEntryResponse> {
		return {
			id: entry.messageId.toString(),
			channel_id: entry.channelId.toString(),
			message_id: entry.messageId.toString(),
			status: entry.status,
			message: entry.message
				? (await this.userContentService.buildMessageResponsesForUser(userId, [entry.message]))[0]
				: null,
		};
	}
}
