/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildID, MessageID, UserID} from '@fluxer/api/src/BrandedTypes';
import type {Message} from '@fluxer/api/src/models/Message';
import {ElasticsearchSearchServiceBase} from '@fluxer/api/src/search/elasticsearch/ElasticsearchSearchServiceBase';
import type {IMessageSearchService} from '@fluxer/api/src/search/IMessageSearchService';
import {convertToSearchableMessage} from '@fluxer/api/src/search/message/MessageSearchSerializer';
import {
	ElasticsearchMessageAdapter,
	type ElasticsearchMessageAdapterOptions,
} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchMessageAdapter';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

const MESSAGE_DELETE_BATCH_SIZE = 1000;
const DEFAULT_HITS_PER_PAGE = 25;

function toSearchOptions(options?: {hitsPerPage?: number; page?: number}): {limit?: number; offset?: number} {
	return {
		limit: options?.hitsPerPage,
		offset: options?.page ? (options.page - 1) * (options.hitsPerPage ?? DEFAULT_HITS_PER_PAGE) : 0,
	};
}

export interface ElasticsearchMessageSearchServiceOptions extends ElasticsearchMessageAdapterOptions {}

export class ElasticsearchMessageSearchService
	extends ElasticsearchSearchServiceBase<MessageSearchFilters, SearchableMessage, ElasticsearchMessageAdapter>
	implements IMessageSearchService
{
	constructor(options: ElasticsearchMessageSearchServiceOptions) {
		super(new ElasticsearchMessageAdapter({client: options.client}));
	}

	async indexMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		await this.indexDocument(convertToSearchableMessage(message, authorIsBot));
	}

	async indexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (messages.length === 0) {
			return;
		}
		await this.indexDocuments(
			messages.map((message) => {
				const isBot = message.authorId ? (authorBotMap?.get(message.authorId) ?? false) : false;
				return convertToSearchableMessage(message, isBot);
			}),
		);
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

	async deleteGuildMessages(guildId: GuildID): Promise<void> {
		const guildIdString = guildId.toString();
		while (true) {
			const result = await this.search('', {guildId: guildIdString}, {limit: MESSAGE_DELETE_BATCH_SIZE, offset: 0});
			if (result.hits.length === 0) {
				return;
			}
			await this.deleteDocuments(result.hits.map((hit) => hit.id));
		}
	}

	searchMessages(
		query: string,
		filters: MessageSearchFilters,
		options?: {hitsPerPage?: number; page?: number},
	): Promise<SchemaSearchResult<SearchableMessage>> {
		return this.search(query, filters, toSearchOptions(options));
	}
}
