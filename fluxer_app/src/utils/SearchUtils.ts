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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import http from '~/lib/HttpClient';
import type {Message} from '~/records/MessageRecord';
import {MessageRecord} from '~/records/MessageRecord';
import SearchHistoryStore from '~/stores/SearchHistoryStore';
import {parseQuery} from '~/utils/SearchQueryParser';
import type {SearchSegment} from '~/utils/SearchSegmentManager';

export type MessageSearchScope = 'current' | 'open_dms' | 'all_dms' | 'all_guilds' | 'all' | 'open_dms_and_all_guilds';

export interface SearchValueOption {
	value: string;
	label: string;
	description?: string;
	isDefault?: boolean;
}

export interface MessageSearchParams {
	hitsPerPage?: number;
	page?: number;
	maxId?: string;
	minId?: string;

	content?: string;
	contents?: Array<string>;

	channelId?: Array<string>;
	excludeChannelId?: Array<string>;

	authorType?: Array<'user' | 'bot' | 'webhook'>;
	excludeAuthorType?: Array<'user' | 'bot' | 'webhook'>;
	authorId?: Array<string>;
	excludeAuthorId?: Array<string>;

	mentions?: Array<string>;
	excludeMentions?: Array<string>;
	mentionEveryone?: boolean;

	pinned?: boolean;

	has?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll' | 'snapshot'>;
	excludeHas?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll' | 'snapshot'>;

	embedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	excludeEmbedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	embedProvider?: Array<string>;
	excludeEmbedProvider?: Array<string>;

	linkHostname?: Array<string>;
	excludeLinkHostname?: Array<string>;

	attachmentFilename?: Array<string>;
	excludeAttachmentFilename?: Array<string>;

	attachmentExtension?: Array<string>;
	excludeAttachmentExtension?: Array<string>;

	sortBy?: 'timestamp' | 'relevance';
	sortOrder?: 'asc' | 'desc';

	scope?: MessageSearchScope;

	includeNsfw?: boolean;
}

interface ApiMessageSearchResponse {
	messages: Array<Message>;
	total: number;
	hits_per_page?: number;
	page?: number;
}

interface MessageSearchResponse {
	messages: Array<MessageRecord>;
	total: number;
	hitsPerPage: number;
	page: number;
}

interface IndexingResponse {
	indexing: true;
	message: string;
}

type SearchResult = MessageSearchResponse | IndexingResponse;

type MessageSearchApiParams = Record<string, string | number | boolean | Array<string> | undefined>;

export interface SearchContext {
	contextChannelId?: string;
	contextGuildId?: string | null;
}

const isHttpStatusError = (error: unknown, statusCode: number): error is {status: number} => {
	return (
		typeof error === 'object' &&
		error != null &&
		'status' in error &&
		(error as {status?: number}).status === statusCode
	);
};

export const isIndexing = (result: SearchResult): result is IndexingResponse => {
	return 'indexing' in result && result.indexing === true;
};

export const searchMessages = async (
	i18n: I18n,
	context: SearchContext,
	params: MessageSearchParams,
): Promise<SearchResult> => {
	try {
		const extraParams: MessageSearchApiParams = {};
		if (context.contextChannelId) {
			extraParams.context_channel_id = context.contextChannelId;
		}
		if (context.contextGuildId) {
			extraParams.context_guild_id = context.contextGuildId;
		}
		if (params.channelId && params.channelId.length > 0) {
			extraParams.channel_ids = params.channelId;
		}

		const response = await http.post<ApiMessageSearchResponse>('/search/messages', toApiParams(params, extraParams));

		if (response.status === 202) {
			return {indexing: true, message: i18n._(msg`One or more channels are being indexed`)};
		}

		const body = response.body;
		if (!body) {
			throw new Error(i18n._(msg`No response body received`));
		}
		return {
			messages: body.messages?.map((msg) => new MessageRecord(msg)) ?? [],
			total: body.total ?? 0,
			hitsPerPage: body.hits_per_page ?? 25,
			page: body.page ?? 1,
		};
	} catch (error: unknown) {
		if (isHttpStatusError(error, 202)) {
			return {indexing: true, message: i18n._(msg`One or more channels are being indexed`)};
		}
		throw error;
	}
};

export interface SearchFilterOption {
	key: string;
	label: string;
	description: string;
	syntax: string;
	values?: Array<SearchValueOption>;
	requiresValue?: boolean;
	requiresGuild?: boolean;
}

export const getSearchFilterOptions = (i18n: I18n): Array<SearchFilterOption> => [
	{
		key: 'from',
		label: i18n._(msg`from:`),
		description: i18n._(msg`user`),
		syntax: 'from:',
		requiresValue: true,
	},
	{
		key: '-from',
		label: i18n._(msg`-from:`),
		description: i18n._(msg`exclude user`),
		syntax: '-from:',
		requiresValue: true,
	},
	{
		key: 'mentions',
		label: i18n._(msg`mentions:`),
		description: i18n._(msg`user`),
		syntax: 'mentions:',
		requiresValue: true,
	},
	{
		key: '-mentions',
		label: i18n._(msg`-mentions:`),
		description: i18n._(msg`exclude user`),
		syntax: '-mentions:',
		requiresValue: true,
	},
	{
		key: 'has',
		label: i18n._(msg`has:`),
		description: i18n._(msg`link, embed or file`),
		syntax: 'has:',
		values: [
			{value: 'link', label: i18n._(msg`link`)},
			{value: 'embed', label: i18n._(msg`embed`)},
			{value: 'file', label: i18n._(msg`file`)},
			{value: 'image', label: i18n._(msg`image`)},
			{value: 'video', label: i18n._(msg`video`)},
			{value: 'sound', label: i18n._(msg`sound`)},
			{value: 'sticker', label: i18n._(msg`sticker`)},
			{value: 'poll', label: i18n._(msg`poll`)},
		],
	},
	{
		key: '-has',
		label: i18n._(msg`-has:`),
		description: i18n._(msg`exclude link, embed or file`),
		syntax: '-has:',
		values: [
			{value: 'link', label: i18n._(msg`link`)},
			{value: 'embed', label: i18n._(msg`embed`)},
			{value: 'file', label: i18n._(msg`file`)},
			{value: 'image', label: i18n._(msg`image`)},
			{value: 'video', label: i18n._(msg`video`)},
			{value: 'sound', label: i18n._(msg`sound`)},
			{value: 'sticker', label: i18n._(msg`sticker`)},
			{value: 'poll', label: i18n._(msg`poll`)},
		],
	},
	{
		key: 'before',
		label: i18n._(msg`before:`),
		description: i18n._(msg`specific date`),
		syntax: 'before:',
		requiresValue: true,
	},
	{
		key: 'on',
		label: i18n._(msg`on:`),
		description: i18n._(msg`specific date`),
		syntax: 'on:',
		requiresValue: true,
	},
	{
		key: 'during',
		label: i18n._(msg`during:`),
		description: i18n._(msg`specific date`),
		syntax: 'during:',
		requiresValue: true,
	},
	{
		key: 'after',
		label: i18n._(msg`after:`),
		description: i18n._(msg`specific date`),
		syntax: 'after:',
		requiresValue: true,
	},
	{
		key: 'in',
		label: i18n._(msg`in:`),
		description: i18n._(msg`channel`),
		syntax: 'in:',
		requiresValue: true,
		requiresGuild: true,
	},
	{
		key: '-in',
		label: i18n._(msg`-in:`),
		description: i18n._(msg`exclude channel`),
		syntax: '-in:',
		requiresValue: true,
		requiresGuild: true,
	},
	{
		key: 'pinned',
		label: i18n._(msg`pinned:`),
		description: i18n._(msg`true or false`),
		syntax: 'pinned:',
		values: [
			{value: 'true', label: i18n._(msg`true`)},
			{value: 'false', label: i18n._(msg`false`)},
		],
	},
	{
		key: 'authorType',
		label: i18n._(msg`author-type:`),
		description: i18n._(msg`user, bot or webhook`),
		syntax: 'author-type:',
		values: [
			{value: 'user', label: i18n._(msg`user`)},
			{value: 'bot', label: i18n._(msg`bot`)},
			{value: 'webhook', label: i18n._(msg`webhook`)},
		],
	},
	{
		key: 'link',
		label: i18n._(msg`link:`),
		description: i18n._(msg`e.g. example.com`),
		syntax: 'link:',
		requiresValue: true,
	},
	{
		key: '-link',
		label: i18n._(msg`-link:`),
		description: i18n._(msg`exclude e.g. example.com`),
		syntax: '-link:',
		requiresValue: true,
	},
	{
		key: 'filename',
		label: i18n._(msg`filename:`),
		description: i18n._(msg`attachment filename contains`),
		syntax: 'filename:',
		requiresValue: true,
	},
	{
		key: '-filename',
		label: i18n._(msg`-filename:`),
		description: i18n._(msg`exclude attachment filename contains`),
		syntax: '-filename:',
		requiresValue: true,
	},
	{
		key: 'ext',
		label: i18n._(msg`ext:`),
		description: i18n._(msg`attachment extension`),
		syntax: 'ext:',
		requiresValue: true,
	},
	{
		key: '-ext',
		label: i18n._(msg`-ext:`),
		description: i18n._(msg`exclude attachment extension`),
		syntax: '-ext:',
		requiresValue: true,
	},
];

export const toApiParams = (
	params: MessageSearchParams,
	extraParams?: MessageSearchApiParams,
): MessageSearchApiParams => {
	const hitsPerPage = params.hitsPerPage ?? 25;
	const page = params.page ?? 1;
	const apiParams: MessageSearchApiParams = {
		hits_per_page: hitsPerPage,
		page,
		max_id: params.maxId,
		min_id: params.minId,
		content: params.content,
		contents: params.contents,
		channel_id: params.channelId,
		exclude_channel_id: params.excludeChannelId,
		author_type: params.authorType,
		exclude_author_type: params.excludeAuthorType,
		author_id: params.authorId,
		exclude_author_id: params.excludeAuthorId,
		mentions: params.mentions,
		exclude_mentions: params.excludeMentions,
		mention_everyone: params.mentionEveryone,
		pinned: params.pinned,
		has: params.has,
		exclude_has: params.excludeHas,
		embed_type: params.embedType,
		exclude_embed_type: params.excludeEmbedType,
		embed_provider: params.embedProvider,
		exclude_embed_provider: params.excludeEmbedProvider,
		link_hostname: params.linkHostname,
		exclude_link_hostname: params.excludeLinkHostname,
		attachment_filename: params.attachmentFilename,
		exclude_attachment_filename: params.excludeAttachmentFilename,
		attachment_extension: params.attachmentExtension,
		exclude_attachment_extension: params.excludeAttachmentExtension,
		sort_by: params.sortBy,
		sort_order: params.sortOrder,
		scope: params.scope,
		include_nsfw: params.includeNsfw,
		...extraParams,
	};

	Object.keys(apiParams).forEach((key) => {
		if (apiParams[key] === undefined) {
			delete apiParams[key];
		}
	});

	return apiParams;
};

export const parseSearchQueryWithSegments = (query: string, _segments: Array<SearchSegment>): MessageSearchParams => {
	const entry = SearchHistoryStore.recent().find((e) => e.query === query);
	return parseQuery(query, entry?.hints);
};
