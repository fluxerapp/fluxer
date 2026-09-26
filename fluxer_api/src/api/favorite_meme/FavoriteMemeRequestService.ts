// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MemeID, MessageID, UserID} from '@app/api/BrandedTypes';
import {mapFavoriteMemeToResponse} from '@app/api/favorite_meme/FavoriteMemeModel';
import type {FavoriteMemeService} from '@app/api/favorite_meme/FavoriteMemeService';
import type {User} from '@app/api/models/User';
import {UnknownFavoriteMemeError} from '@fluxer/errors/src/domains/core/UnknownFavoriteMemeError';
import type {
	CreateFavoriteMemeBodySchema,
	CreateFavoriteMemeFromUrlBodySchema,
	FavoriteMemeListResponse,
	FavoriteMemeResponse,
	UpdateFavoriteMemeBodySchema,
} from '@fluxer/schema/src/domains/meme/MemeSchemas';

interface FavoriteMemeListParams {
	userId: UserID;
}

interface FavoriteMemeCreateFromUrlParams {
	user: User;
	data: CreateFavoriteMemeFromUrlBodySchema;
}

interface FavoriteMemeCreateFromMessageParams {
	user: User;
	channelId: ChannelID;
	messageId: MessageID;
	data: CreateFavoriteMemeBodySchema;
}

interface FavoriteMemeGetParams {
	userId: UserID;
	memeId: MemeID;
}

interface FavoriteMemeUpdateParams {
	user: User;
	memeId: MemeID;
	data: UpdateFavoriteMemeBodySchema;
}

interface FavoriteMemeDeleteParams {
	userId: UserID;
	memeId: MemeID;
}

export class FavoriteMemeRequestService {
	constructor(private readonly favoriteMemeService: FavoriteMemeService) {}

	async listFavoriteMemes(params: FavoriteMemeListParams): Promise<FavoriteMemeListResponse> {
		const memes = await this.favoriteMemeService.listFavoriteMemes(params.userId);
		return memes.map((meme) => mapFavoriteMemeToResponse(meme));
	}

	async createFromUrl(params: FavoriteMemeCreateFromUrlParams): Promise<FavoriteMemeResponse> {
		const {user, data} = params;
		const meme = await this.favoriteMemeService.createFromUrl({
			user,
			url: data.url,
			name: data.name,
			altText: data.alt_text ?? undefined,
			tags: data.tags ?? undefined,
			gifSlug: data.gif_slug ?? undefined,
			gifProvider: data.gif_provider ?? undefined,
			media: data.media ?? undefined,
		});
		return mapFavoriteMemeToResponse(meme);
	}

	async createFromMessage(params: FavoriteMemeCreateFromMessageParams): Promise<FavoriteMemeResponse> {
		const {user, channelId, messageId, data} = params;
		const meme = await this.favoriteMemeService.createFromMessage({
			user,
			channelId,
			messageId,
			attachmentId: data.attachment_id?.toString(),
			embedIndex: data.embed_index ?? undefined,
			name: data.name,
			altText: data.alt_text ?? undefined,
			tags: data.tags ?? undefined,
		});
		return mapFavoriteMemeToResponse(meme);
	}

	async getFavoriteMeme(params: FavoriteMemeGetParams): Promise<FavoriteMemeResponse> {
		const meme = await this.favoriteMemeService.getFavoriteMeme(params.userId, params.memeId);
		if (!meme) {
			throw new UnknownFavoriteMemeError();
		}
		return mapFavoriteMemeToResponse(meme);
	}

	async updateFavoriteMeme(params: FavoriteMemeUpdateParams): Promise<FavoriteMemeResponse> {
		const {user, memeId, data} = params;
		const meme = await this.favoriteMemeService.update({
			user,
			memeId,
			name: data.name ?? undefined,
			altText: data.alt_text === undefined ? undefined : data.alt_text,
			tags: data.tags ?? undefined,
		});
		return mapFavoriteMemeToResponse(meme);
	}

	async deleteFavoriteMeme(params: FavoriteMemeDeleteParams): Promise<void> {
		await this.favoriteMemeService.delete(params.userId, params.memeId);
	}
}
