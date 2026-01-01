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

import type {MemeID, UserID} from '~/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {FavoriteMemeRow} from '~/database/CassandraTypes';
import {FavoriteMeme} from '~/Models';
import {FavoriteMemes, FavoriteMemesByMemeId} from '~/Tables';
import {type CreateFavoriteMemeParams, IFavoriteMemeRepository} from './IFavoriteMemeRepository';

const FETCH_FAVORITE_MEME_CQL = FavoriteMemes.selectCql({
	where: [FavoriteMemes.where.eq('user_id'), FavoriteMemes.where.eq('meme_id')],
	limit: 1,
});

const FETCH_FAVORITE_MEMES_BY_USER_CQL = FavoriteMemes.selectCql({
	where: FavoriteMemes.where.eq('user_id'),
});

const COUNT_FAVORITE_MEMES_CQL = FavoriteMemes.selectCountCql({
	where: FavoriteMemes.where.eq('user_id'),
});

export class FavoriteMemeRepository extends IFavoriteMemeRepository {
	async findById(userId: UserID, memeId: MemeID): Promise<FavoriteMeme | null> {
		const meme = await fetchOne<FavoriteMemeRow>(FETCH_FAVORITE_MEME_CQL, {
			user_id: userId,
			meme_id: memeId,
		});

		return meme ? new FavoriteMeme(meme) : null;
	}

	async findByUserId(userId: UserID): Promise<Array<FavoriteMeme>> {
		const memes = await fetchMany<FavoriteMemeRow>(FETCH_FAVORITE_MEMES_BY_USER_CQL, {user_id: userId});
		return memes.map((meme) => new FavoriteMeme(meme));
	}

	async count(userId: UserID): Promise<number> {
		const result = await fetchOne<{count: bigint}>(COUNT_FAVORITE_MEMES_CQL, {user_id: userId});
		return result ? Number(result.count) : 0;
	}

	async create(data: CreateFavoriteMemeParams): Promise<FavoriteMeme> {
		const memeRow: FavoriteMemeRow = {
			user_id: data.user_id,
			meme_id: data.meme_id,
			name: data.name,
			alt_text: data.alt_text ?? null,
			tags: data.tags ?? [],
			attachment_id: data.attachment_id,
			filename: data.filename,
			content_type: data.content_type,
			content_hash: data.content_hash ?? null,
			size: data.size,
			width: data.width ?? null,
			height: data.height ?? null,
			duration: data.duration ?? null,
			is_gifv: data.is_gifv ?? false,
			tenor_id_str: data.tenor_id ?? null,
			version: 1,
		};

		const batch = new BatchBuilder();
		batch.addPrepared(FavoriteMemes.upsertAll(memeRow));
		batch.addPrepared(
			FavoriteMemesByMemeId.upsertAll({
				meme_id: memeRow.meme_id,
				user_id: memeRow.user_id,
			}),
		);

		await batch.execute();
		return new FavoriteMeme(memeRow);
	}

	async update(userId: UserID, memeId: MemeID, data: CreateFavoriteMemeParams): Promise<void> {
		const memeRow: FavoriteMemeRow = {
			user_id: userId,
			meme_id: memeId,
			name: data.name,
			alt_text: data.alt_text ?? null,
			tags: data.tags ?? [],
			attachment_id: data.attachment_id,
			filename: data.filename,
			content_type: data.content_type,
			content_hash: data.content_hash ?? null,
			size: data.size,
			width: data.width ?? null,
			height: data.height ?? null,
			duration: data.duration ?? null,
			is_gifv: data.is_gifv ?? false,
			tenor_id_str: data.tenor_id ?? null,
			version: 1,
		};

		await upsertOne(FavoriteMemes.upsertAll(memeRow));
	}

	async delete(userId: UserID, memeId: MemeID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(FavoriteMemes.deleteByPk({user_id: userId, meme_id: memeId}));
		batch.addPrepared(FavoriteMemesByMemeId.deleteByPk({meme_id: memeId, user_id: userId}));
		await batch.execute();
	}

	async deleteAllByUserId(userId: UserID): Promise<void> {
		const memes = await this.findByUserId(userId);
		for (const meme of memes) {
			await this.delete(userId, meme.id);
		}
	}
}
