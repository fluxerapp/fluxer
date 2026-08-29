// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MemeID, UserID} from '../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import {Db} from '../database/CassandraTypes';
import type {FavoriteMemeRow} from '../database/types/UserTypes';
import {FavoriteMeme} from '../models/FavoriteMeme';
import {FavoriteMemes, FavoriteMemesByMemeId} from '../Tables';
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
		const result = await fetchOne<{
			count: bigint;
		}>(COUNT_FAVORITE_MEMES_CQL, {user_id: userId});
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
			klipy_slug: data.gif_provider === 'klipy' ? (data.gif_slug ?? null) : null,
			tenor_id_str: data.gif_provider === 'tenor' ? (data.gif_slug ?? null) : null,
			media_formats: data.media_formats ? JSON.stringify(data.media_formats) : null,
			placeholder: data.placeholder ?? null,
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

	async update(userId: UserID, memeId: MemeID, data: CreateFavoriteMemeParams): Promise<FavoriteMeme> {
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
			klipy_slug: data.gif_provider === 'klipy' ? (data.gif_slug ?? null) : null,
			tenor_id_str: data.gif_provider === 'tenor' ? (data.gif_slug ?? null) : null,
			media_formats: data.media_formats ? JSON.stringify(data.media_formats) : null,
			placeholder: data.placeholder ?? null,
			version: 1,
		};
		await upsertOne(FavoriteMemes.upsertAll(memeRow));
		return new FavoriteMeme(memeRow);
	}

	async updatePlaceholder(userId: UserID, memeId: MemeID, placeholder: string): Promise<void> {
		await fetchOne(FavoriteMemes.patchByPk({user_id: userId, meme_id: memeId}, {placeholder: Db.set(placeholder)}));
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
