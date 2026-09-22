// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {Message} from '@app/api/models/Message';
import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IMessageSearchService extends SchemaISearchAdapter<MessageSearchFilters, SearchableMessage> {
	indexMessage(message: Message, authorIsBot?: boolean): Promise<void>;
	indexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void>;
	updateMessage(message: Message, authorIsBot?: boolean): Promise<void>;
	deleteMessage(messageId: MessageID): Promise<void>;
	deleteMessages(messageIds: Array<MessageID>): Promise<void>;
	bulkIndexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void>;
	refreshIndex(): Promise<void>;
	deleteChannelMessages(channelId: ChannelID): Promise<void>;
	deleteGuildMessages(guildId: GuildID): Promise<void>;
	searchMessages(
		query: string,
		filters: MessageSearchFilters,
		options?: {
			hitsPerPage?: number;
			page?: number;
			cursor?: Array<string>;
		},
	): Promise<SchemaSearchResult<SearchableMessage>>;
}
