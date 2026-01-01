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

import type {FavoriteMemeRow} from '~/database/CassandraTypes';
import {extractTimestamp} from '~/utils/SnowflakeUtils';
import {type AttachmentID, type MemeID, type UserID, userIdToChannelId} from '../BrandedTypes';

export class FavoriteMeme {
	readonly id: MemeID;
	readonly userId: UserID;
	readonly name: string;
	readonly altText: string | null;
	readonly tags: Array<string>;
	readonly attachmentId: AttachmentID;
	readonly filename: string;
	readonly contentType: string;
	readonly contentHash: string | null;
	readonly size: bigint;
	readonly width: number | null;
	readonly height: number | null;
	readonly duration: number | null;
	readonly isGifv: boolean;
	readonly tenorId: string | null;
	readonly createdAt: Date;
	readonly version: number;

	constructor(row: FavoriteMemeRow) {
		this.id = row.meme_id;
		this.userId = row.user_id;
		this.name = row.name;
		this.altText = row.alt_text ?? null;
		this.tags = row.tags ?? [];
		this.attachmentId = row.attachment_id;
		this.filename = row.filename;
		this.contentType = row.content_type;
		this.contentHash = row.content_hash ?? null;
		this.size = row.size;
		this.width = row.width ?? null;
		this.height = row.height ?? null;
		this.duration = row.duration ?? null;
		this.isGifv = row.is_gifv ?? false;
		this.tenorId = row.tenor_id_str ?? null;
		this.createdAt = new Date(extractTimestamp(this.id));
		this.version = row.version;
	}

	toRow(): FavoriteMemeRow {
		return {
			user_id: this.userId,
			meme_id: this.id,
			name: this.name,
			alt_text: this.altText,
			tags: this.tags.length > 0 ? this.tags : null,
			attachment_id: this.attachmentId,
			filename: this.filename,
			content_type: this.contentType,
			content_hash: this.contentHash,
			size: this.size,
			width: this.width,
			height: this.height,
			duration: this.duration,
			is_gifv: this.isGifv,
			tenor_id_str: this.tenorId,
			version: this.version,
		};
	}

	get storageKey(): string {
		const channelId = userIdToChannelId(this.userId);
		return `attachments/${channelId}/${this.attachmentId}/${this.filename}`;
	}
}
