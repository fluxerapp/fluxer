// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GifMediaFormat} from '@fluxer/schema/src/domains/gif/GifSchemas';
import type {AttachmentID, MemeID, UserID} from '../BrandedTypes';
import type {FavoriteMeme} from '../models/FavoriteMeme';

export interface CreateFavoriteMemeParams {
	user_id: UserID;
	meme_id: MemeID;
	name: string;
	alt_text?: string | null;
	tags?: Array<string>;
	attachment_id: AttachmentID;
	filename: string;
	content_type: string;
	content_hash?: string | null;
	size: bigint;
	width?: number | null;
	height?: number | null;
	duration?: number | null;
	is_gifv?: boolean;
	gif_slug?: string | null;
	gif_provider?: string | null;
	media_formats?: Record<string, GifMediaFormat> | null;
	placeholder?: string | null;
}

export abstract class IFavoriteMemeRepository {
	abstract create(data: CreateFavoriteMemeParams): Promise<FavoriteMeme>;

	abstract findById(userId: UserID, memeId: MemeID): Promise<FavoriteMeme | null>;

	abstract findByUserId(userId: UserID): Promise<Array<FavoriteMeme>>;

	abstract update(userId: UserID, memeId: MemeID, data: CreateFavoriteMemeParams): Promise<FavoriteMeme>;

	abstract updatePlaceholder(userId: UserID, memeId: MemeID, placeholder: string): Promise<void>;

	abstract delete(userId: UserID, memeId: MemeID): Promise<void>;

	abstract deleteAllByUserId(userId: UserID): Promise<void>;

	abstract count(userId: UserID): Promise<number>;
}
