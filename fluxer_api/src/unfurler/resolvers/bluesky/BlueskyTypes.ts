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

type FacetFeatureType =
	| 'app.bsky.richtext.facet#link'
	| 'app.bsky.richtext.facet#mention'
	| 'app.bsky.richtext.facet#tag';

interface FacetFeature {
	$type: FacetFeatureType;
	uri?: string;
	did?: string;
	tag?: string;
}
interface FacetBytePosition {
	byteStart: number;
	byteEnd: number;
}
export interface Facet {
	index: FacetBytePosition;
	features: [FacetFeature];
}
export interface BlueskyAuthor {
	did: string;
	handle: string;
	displayName?: string;
	avatar?: string;
}
interface BlueskyImageEmbed {
	alt?: string;
	image: {ref: {$link: string}};
}
interface BlueskyVideoEmbed {
	$type: string;
	ref: {$link: string};
	mimeType: string;
	size: number;
}
export interface BlueskyAspectRatio {
	width: number;
	height: number;
}

interface BlueskyRecordEmbed {
	$type: string;
	aspectRatio?: BlueskyAspectRatio;
	video?: BlueskyVideoEmbed;
	images?: Array<BlueskyImageEmbed>;
}

export interface BlueskyPostEmbed {
	$type: 'app.bsky.embed.images#view' | 'app.bsky.embed.video#view' | 'app.bsky.embed.recordWithMedia#view';
	images?: Array<{thumb: string; fullsize: string; alt?: string; aspectRatio?: BlueskyAspectRatio}>;
	video?: {$type: string; cid?: string; aspectRatio?: BlueskyAspectRatio; thumbnail?: string; playlist?: string};
	media?: {
		$type: 'app.bsky.embed.images#view' | 'app.bsky.embed.video#view';
		images?: Array<{thumb: string; fullsize: string; alt?: string; aspectRatio?: BlueskyAspectRatio}>;
		video?: {$type: string; cid?: string; aspectRatio?: BlueskyAspectRatio; thumbnail?: string; playlist?: string};
	};
	record?: {
		$type?: 'app.bsky.embed.record#view';
		record?: {
			$type: 'app.bsky.embed.record#viewRecord';
			uri: string;
			cid: string;
			author: BlueskyAuthor;
			value: {$type: string; text: string; createdAt: string; facets?: Array<Facet>};
			labels?: Array<Record<string, unknown>>;
			indexedAt: string;
			embeds?: Array<Record<string, unknown>>;
		};
	};
}

interface BlueskyRecord {
	text: string;
	createdAt: string;
	facets?: Array<Facet>;
	embed?: BlueskyRecordEmbed;
	reply?: {parent: {cid: string; uri: string}; root: {cid: string; uri: string}};
}

export interface BlueskyPost {
	uri: string;
	author: BlueskyAuthor;
	record: BlueskyRecord;
	embed?: BlueskyPostEmbed;
	indexedAt: string;
	replyCount: number;
	repostCount: number;
	likeCount: number;
	quoteCount: number;
}

export interface BlueskyPostThread {
	thread: {post: BlueskyPost; parent?: {post: BlueskyPost}; replies?: Array<{post: BlueskyPost}>};
}

export interface BlueskyProfile {
	did: string;
	handle: string;
	displayName?: string;
	description?: string;
	avatar?: string;
	banner?: string;
	indexedAt: string;
}

export interface HandleResolution {
	did: string;
}

export interface ProcessedMedia {
	url: string;
	width: number;
	height: number;
	placeholder?: string;
	flags: number;
	description?: string;
	content_type?: string;
	content_hash?: string;
	duration?: number;
}

export interface ProcessedVideoResult {
	thumbnail?: ProcessedMedia;
	video?: ProcessedMedia;
}
export interface ReplyContext {
	authorName: string;
	authorHandle: string;
	postUrl: string;
}
