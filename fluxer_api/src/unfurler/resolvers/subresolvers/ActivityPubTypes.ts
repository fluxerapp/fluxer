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

export interface ActivityPubAuthor {
	id: string;
	type: string;
	name: string;
	preferredUsername: string;
	url: string;
	icon?: {type: string; mediaType: string; url: string};
}

export interface ActivityPubAttachment {
	type: string;
	mediaType: string;
	url: string;
	width?: number;
	height?: number;
	name?: string;
	blurhash?: string;
}

export interface ActivityPubPost {
	id: string;
	type: string;
	url: string;
	published: string;
	attributedTo: string | ActivityPubAuthor;
	content: string;
	summary?: string;
	sensitive?: boolean;
	attachment?: Array<ActivityPubAttachment>;
	tag?: Array<{type: string; name: string; href?: string}>;
	to?: Array<string>;
	cc?: Array<string>;
	inReplyTo?: string | null;
	likes?: number;
	shares?: number;
	replies?: {totalItems?: number};
}

export interface MastodonPost {
	id: string;
	created_at: string;
	in_reply_to_id: string | null;
	in_reply_to_account_id: string | null;
	sensitive: boolean;
	spoiler_text: string;
	visibility: string;
	language: string | null;
	uri: string;
	url: string;
	replies_count: number;
	reblogs_count: number;
	favourites_count: number;
	edited_at: string | null;
	content: string;
	reblog: MastodonPost | null;
	application: {name: string} | null;
	account: MastodonAccount;
	media_attachments: Array<MastodonMediaAttachment>;
	mentions: Array<MastodonMention>;
	tags: Array<MastodonTag>;
	emojis: Array<MastodonEmoji>;
	card: MastodonCard | null;
	poll: MastodonPoll | null;
}

export interface MastodonInstance {
	domain: string;
	title: string;
	version: string;
	source_url: string;
	description: string;
	usage: {users: {active_month: number}};
	thumbnail: {url: string};
	languages: Array<string>;
	configuration: {urls: {streaming: string; status: string | null}};
	registrations: {enabled: boolean; approval_required: boolean; message: string; url: string | null};
	contact: {email: string; account: MastodonAccount};
	rules: Array<{id: string; text: string; hint: string}>;
}

interface MastodonAccount {
	id: string;
	username: string;
	acct: string;
	url: string;
	display_name: string;
	note: string;
	avatar: string;
	avatar_static: string;
	header: string;
	header_static: string;
	locked: boolean;
	fields: Array<{name: string; value: string}>;
	emojis: Array<MastodonEmoji>;
	bot: boolean;
	group: boolean;
	discoverable: boolean | null;
}

export interface MastodonMediaAttachment {
	id: string;
	type: 'unknown' | 'image' | 'gifv' | 'video' | 'audio';
	url: string;
	preview_url: string;
	remote_url: string | null;
	meta: {
		original?: {width: number; height: number; size: string; aspect: number};
		small?: {width: number; height: number; size: string; aspect: number};
	};
	description: string | null;
	blurhash: string | null;
}

interface MastodonMention {
	id: string;
	username: string;
	url: string;
	acct: string;
}
interface MastodonTag {
	name: string;
	url: string;
}
interface MastodonEmoji {
	shortcode: string;
	url: string;
	static_url: string;
	visible_in_picker: boolean;
}

interface MastodonCard {
	url: string;
	title: string;
	description: string;
	type: string;
	author_name: string;
	author_url: string;
	provider_name: string;
	provider_url: string;
	html: string;
	width: number;
	height: number;
	image: string | null;
	embed_url: string;
}

interface MastodonPoll {
	id: string;
	expires_at: string | null;
	expired: boolean;
	multiple: boolean;
	votes_count: number;
	voters_count: number | null;
	voted: boolean | null;
	own_votes: Array<number> | null;
	options: Array<{title: string; votes_count: number | null}>;
	emojis: Array<MastodonEmoji>;
}

export interface ProcessedMedia {
	url: string;
	proxy_url?: string;
	width: number;
	height: number;
	placeholder?: string;
	flags: number;
	description?: string;
	content_type?: string;
	content_hash?: string;
	duration?: number;
}

export interface ActivityPubContext {
	inReplyTo?: {author: string; content: string; url: string; id?: string};
	serverName: string;
	serverTitle: string;
	serverIcon?: string;
	serverDomain: string;
}
