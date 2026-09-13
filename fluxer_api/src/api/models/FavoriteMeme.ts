// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, MemeID, UserID} from '@app/api/BrandedTypes';
import {userIdToChannelId} from '@app/api/BrandedTypes';
import type {FavoriteMemeRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import type {GifMediaFormat} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

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
	readonly gifSlug: string | null;
	readonly gifProvider: string | null;
	readonly mediaFormats: Record<string, GifMediaFormat> | null;
	readonly placeholder: string | null;
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
		if (row.klipy_slug) {
			this.gifSlug = row.klipy_slug;
			this.gifProvider = 'klipy';
		} else if (row.tenor_id_str) {
			this.gifSlug = row.tenor_id_str;
			this.gifProvider = 'tenor';
		} else {
			this.gifSlug = null;
			this.gifProvider = null;
		}
		this.mediaFormats = parseMediaFormats(row.media_formats);
		this.placeholder = row.placeholder ?? null;
		this.createdAt = snowflakeToDate(this.id);
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
			klipy_slug: this.gifProvider === 'klipy' ? this.gifSlug : null,
			tenor_id_str: this.gifProvider === 'tenor' ? this.gifSlug : null,
			media_formats: this.mediaFormats ? JSON.stringify(this.mediaFormats) : null,
			placeholder: this.placeholder,
			version: this.version,
		};
	}

	get storageKey(): string {
		const channelId = userIdToChannelId(this.userId);
		return `attachments/${channelId}/${this.attachmentId}/${this.filename}`;
	}
}

function parseMediaFormats(raw: string | null | undefined): Record<string, GifMediaFormat> | null {
	if (!raw) return null;
	const parsed = parseJsonWithGuard(raw, isGifMediaFormatRecord);
	if (parsed) {
		return parsed;
	}
	try {
		JSON.parse(raw);
		return null;
	} catch (error) {
		Logger.warn({error}, 'Failed to parse favorite_meme.media_formats; treating as null');
		return null;
	}
}

function isGifMediaFormatRecord(value: unknown): value is Record<string, GifMediaFormat> {
	return isJsonRecord(value) && Object.values(value).every(isJsonRecord);
}
