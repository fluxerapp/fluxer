// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';

export function buildMessageSearchFilters(
	searchParams: MessageSearchRequest,
	channelIds: Array<string>,
): MessageSearchFilters {
	const filters: MessageSearchFilters = {
		channelIds,
	};
	if (searchParams.max_id) {
		filters.maxId = searchParams.max_id.toString();
	}
	if (searchParams.min_id) {
		filters.minId = searchParams.min_id.toString();
	}
	if (searchParams.content) {
		filters.content = searchParams.content;
	}
	if (searchParams.contents) {
		filters.contents = searchParams.contents;
	}
	if (searchParams.exact_phrases) {
		filters.exactPhrases = searchParams.exact_phrases;
	}
	if (searchParams.exclude_channel_id) {
		filters.excludeChannelIds = searchParams.exclude_channel_id.map((id) => id.toString());
	}
	if (searchParams.author_id != null) {
		filters.authorId = searchParams.author_id.map((id) => id.toString());
	}
	if (searchParams.author_type) {
		filters.authorType = searchParams.author_type;
	}
	if (searchParams.exclude_author_type) {
		filters.excludeAuthorType = searchParams.exclude_author_type;
	}
	if (searchParams.exclude_author_id != null) {
		filters.excludeAuthorIds = searchParams.exclude_author_id.map((id) => id.toString());
	}
	if (searchParams.mentions) {
		filters.mentions = searchParams.mentions.map((id: bigint) => id.toString());
	}
	if (searchParams.exclude_mentions) {
		filters.excludeMentions = searchParams.exclude_mentions.map((id: bigint) => id.toString());
	}
	if (searchParams.mention_everyone !== undefined) {
		filters.mentionEveryone = searchParams.mention_everyone;
	}
	if (searchParams.pinned !== undefined) {
		filters.pinned = searchParams.pinned;
	}
	if (searchParams.has) {
		filters.has = searchParams.has;
	}
	if (searchParams.exclude_has) {
		filters.excludeHas = searchParams.exclude_has;
	}
	if (searchParams.embed_type) {
		filters.embedType = searchParams.embed_type;
	}
	if (searchParams.exclude_embed_type) {
		filters.excludeEmbedTypes = searchParams.exclude_embed_type;
	}
	if (searchParams.embed_provider) {
		filters.embedProvider = searchParams.embed_provider;
	}
	if (searchParams.exclude_embed_provider) {
		filters.excludeEmbedProviders = searchParams.exclude_embed_provider;
	}
	if (searchParams.link_hostname) {
		filters.linkHostname = searchParams.link_hostname;
	}
	if (searchParams.exclude_link_hostname) {
		filters.excludeLinkHostnames = searchParams.exclude_link_hostname;
	}
	if (searchParams.attachment_filename) {
		filters.attachmentFilename = searchParams.attachment_filename;
	}
	if (searchParams.exclude_attachment_filename) {
		filters.excludeAttachmentFilenames = searchParams.exclude_attachment_filename;
	}
	if (searchParams.attachment_extension) {
		filters.attachmentExtension = searchParams.attachment_extension;
	}
	if (searchParams.exclude_attachment_extension) {
		filters.excludeAttachmentExtensions = searchParams.exclude_attachment_extension;
	}
	if (searchParams.sort_by) {
		filters.sortBy = searchParams.sort_by;
	}
	if (searchParams.sort_order) {
		filters.sortOrder = searchParams.sort_order;
	}
	if (searchParams.include_nsfw !== undefined) {
		filters.includeNsfw = searchParams.include_nsfw;
	}
	return filters;
}
