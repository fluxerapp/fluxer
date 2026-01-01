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

import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

export type FavoriteMeme = Readonly<{
	id: string;
	user_id: string;
	name: string;
	alt_text: string | null;
	tags: Array<string>;
	attachment_id: string;
	filename: string;
	content_type: string;
	content_hash: string | null;
	size: number;
	width: number | null;
	height: number | null;
	duration: number | null;
	is_gifv: boolean;
	url: string;
	tenor_id: string | null;
}>;

export class FavoriteMemeRecord {
	readonly id: string;
	readonly userId: string;
	readonly name: string;
	readonly altText: string | null;
	readonly tags: Array<string>;
	readonly attachmentId: string;
	readonly filename: string;
	readonly contentType: string;
	readonly contentHash: string | null;
	readonly size: number;
	readonly width: number | null;
	readonly height: number | null;
	readonly duration: number | null;
	readonly isGifv: boolean;
	readonly url: string;
	readonly tenorId: string | null;

	constructor(meme: FavoriteMeme) {
		this.id = meme.id;
		this.userId = meme.user_id;
		this.name = meme.name;
		this.altText = meme.alt_text;
		this.tags = meme.tags;
		this.attachmentId = meme.attachment_id;
		this.filename = meme.filename;
		this.contentType = meme.content_type;
		this.contentHash = meme.content_hash;
		this.size = meme.size;
		this.width = meme.width;
		this.height = meme.height;
		this.duration = meme.duration;
		this.isGifv = meme.is_gifv;
		this.url = meme.url;
		this.tenorId = meme.tenor_id;
	}

	get createdAtTimestamp(): number {
		return SnowflakeUtils.extractTimestamp(this.id);
	}

	get createdAt(): Date {
		return new Date(this.createdAtTimestamp);
	}

	isImage(): boolean {
		return this.contentType.startsWith('image/');
	}

	isVideo(): boolean {
		return this.contentType.startsWith('video/');
	}

	isAudio(): boolean {
		return this.contentType.startsWith('audio/');
	}

	getMediaType(): 'image' | 'gifv' | 'video' | 'audio' | 'unknown' {
		if (this.isGifv) return 'gifv';
		if (this.isImage()) return 'image';
		if (this.isVideo()) return 'video';
		if (this.isAudio()) return 'audio';
		return 'unknown';
	}

	equals(other: FavoriteMemeRecord): boolean {
		return (
			this.id === other.id &&
			this.userId === other.userId &&
			this.name === other.name &&
			this.altText === other.altText &&
			JSON.stringify(this.tags) === JSON.stringify(other.tags) &&
			this.attachmentId === other.attachmentId &&
			this.filename === other.filename &&
			this.contentType === other.contentType &&
			this.contentHash === other.contentHash &&
			this.size === other.size &&
			this.width === other.width &&
			this.height === other.height &&
			this.duration === other.duration &&
			this.isGifv === other.isGifv &&
			this.url === other.url &&
			this.tenorId === other.tenorId
		);
	}

	toJSON(): FavoriteMeme {
		return {
			id: this.id,
			user_id: this.userId,
			name: this.name,
			alt_text: this.altText,
			tags: this.tags,
			attachment_id: this.attachmentId,
			filename: this.filename,
			content_type: this.contentType,
			content_hash: this.contentHash,
			size: this.size,
			width: this.width,
			height: this.height,
			duration: this.duration,
			is_gifv: this.isGifv,
			url: this.url,
			tenor_id: this.tenorId,
		};
	}
}
