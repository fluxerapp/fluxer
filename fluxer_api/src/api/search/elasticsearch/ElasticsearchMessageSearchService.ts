// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {Message} from '@app/api/models/Message';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import {
	convertMessagesToSearchableMessages,
	convertToSearchableMessage,
} from '@app/api/search/message/MessageSearchSerializer';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchMessageAdapter,
	type ElasticsearchMessageAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchMessageAdapter';

const DEFAULT_HITS_PER_PAGE = 25;

function toSearchOptions(options?: {hitsPerPage?: number; page?: number}): {
	limit?: number;
	offset?: number;
} {
	return {
		limit: options?.hitsPerPage,
		offset: options?.page ? (options.page - 1) * (options.hitsPerPage ?? DEFAULT_HITS_PER_PAGE) : 0,
	};
}

interface ElasticsearchMessageSearchServiceOptions extends ElasticsearchMessageAdapterOptions {}

export class ElasticsearchMessageSearchService
	extends SearchAdapterServiceBase<MessageSearchFilters, SearchableMessage, ElasticsearchMessageAdapter>
	implements IMessageSearchService
{
	constructor(options: ElasticsearchMessageSearchServiceOptions) {
		super(new ElasticsearchMessageAdapter({client: options.client, lock: options.lock}));
	}

	async indexMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		await this.indexDocument(convertToSearchableMessage(message, authorIsBot));
	}

	async indexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (messages.length === 0) {
			return;
		}
		await this.indexDocuments(convertMessagesToSearchableMessages(messages, authorBotMap));
	}

	async bulkIndexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (messages.length === 0) {
			return;
		}
		await this.bulkIndexDocuments(convertMessagesToSearchableMessages(messages, authorBotMap));
	}

	async updateMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		await this.updateDocument(convertToSearchableMessage(message, authorIsBot));
	}

	async deleteMessage(messageId: MessageID): Promise<void> {
		await this.deleteDocument(messageId.toString());
	}

	async deleteMessages(messageIds: Array<MessageID>): Promise<void> {
		await this.deleteDocuments(messageIds.map((id) => id.toString()));
	}

	async deleteChannelMessages(channelId: ChannelID): Promise<void> {
		await this.adapter.deleteByQuery({term: {channelId: channelId.toString()}});
	}

	async deleteGuildMessages(guildId: GuildID): Promise<void> {
		await this.adapter.deleteByQuery({term: {guildId: guildId.toString()}});
	}

	searchMessages(
		query: string,
		filters: MessageSearchFilters,
		options?: {
			hitsPerPage?: number;
			page?: number;
			cursor?: Array<string>;
		},
	): Promise<SchemaSearchResult<SearchableMessage>> {
		return this.search(query, filters, toSearchOptions(options));
	}
}
