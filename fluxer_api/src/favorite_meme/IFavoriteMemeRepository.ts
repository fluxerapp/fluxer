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

import type {AttachmentID, MemeID, UserID} from '~/BrandedTypes';
import type {FavoriteMeme} from '~/Models';

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
	tenor_id?: string | null;
}

export abstract class IFavoriteMemeRepository {
	abstract create(data: CreateFavoriteMemeParams): Promise<FavoriteMeme>;
	abstract findById(userId: UserID, memeId: MemeID): Promise<FavoriteMeme | null>;
	abstract findByUserId(userId: UserID): Promise<Array<FavoriteMeme>>;
	abstract update(userId: UserID, memeId: MemeID, data: CreateFavoriteMemeParams): Promise<void>;
	abstract delete(userId: UserID, memeId: MemeID): Promise<void>;
	abstract deleteAllByUserId(userId: UserID): Promise<void>;
	abstract count(userId: UserID): Promise<number>;
}
