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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes, ME} from '~/Constants';
import {MaxFavoriteMemesModal} from '~/components/alerts/MaxFavoriteMemesModal';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {FavoriteMeme} from '~/records/FavoriteMemeRecord';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';

const logger = new Logger('FavoriteMemes');

const getApiErrorCode = (error: unknown): string | undefined => {
	if (typeof error === 'object' && error !== null && 'code' in error) {
		const {code} = error as {code?: unknown};
		return typeof code === 'string' ? code : undefined;
	}
	return undefined;
};

export const createFavoriteMeme = async (
	i18n: I18n,
	{
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		name,
		altText,
		tags,
	}: {
		channelId: string;
		messageId: string;
		attachmentId?: string;
		embedIndex?: number;
		name: string;
		altText?: string;
		tags?: Array<string>;
	},
): Promise<void> => {
	try {
		await http.post<FavoriteMeme>(Endpoints.CHANNEL_MESSAGE_FAVORITE_MEMES(channelId, messageId), {
			attachment_id: attachmentId,
			embed_index: embedIndex,
			name,
			alt_text: altText,
			tags,
		});

		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Added to saved media`),
		});
		logger.debug(`Successfully added favorite meme from message ${messageId}`);
	} catch (error: unknown) {
		logger.error(`Failed to add favorite meme from message ${messageId}:`, error);

		if (getApiErrorCode(error) === APIErrorCodes.MAX_FAVORITE_MEMES) {
			ModalActionCreators.push(modal(() => <MaxFavoriteMemesModal />));
			return;
		}

		throw error;
	}
};

export const createFavoriteMemeFromUrl = async (
	i18n: I18n,
	{
		url,
		name,
		altText,
		tags,
		tenorId,
	}: {
		url: string;
		name: string;
		altText?: string;
		tags?: Array<string>;
		tenorId?: string;
	},
): Promise<void> => {
	try {
		await http.post<FavoriteMeme>(Endpoints.USER_FAVORITE_MEMES(ME), {
			url,
			name,
			alt_text: altText,
			tags,
			tenor_id: tenorId,
		});

		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Added to saved media`),
		});
		logger.debug(`Successfully added favorite meme from URL ${url}`);
	} catch (error: unknown) {
		logger.error(`Failed to add favorite meme from URL ${url}:`, error);

		if (getApiErrorCode(error) === APIErrorCodes.MAX_FAVORITE_MEMES) {
			ModalActionCreators.push(modal(() => <MaxFavoriteMemesModal />));
			return;
		}

		throw error;
	}
};

export const updateFavoriteMeme = async (
	i18n: I18n,
	{
		memeId,
		name,
		altText,
		tags,
	}: {
		memeId: string;
		name?: string;
		altText?: string | null;
		tags?: Array<string>;
	},
): Promise<void> => {
	try {
		const response = await http.patch<FavoriteMeme>(Endpoints.USER_FAVORITE_MEME(ME, memeId), {
			name,
			alt_text: altText,
			tags,
		});

		FavoriteMemeStore.updateMeme(response.body);

		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Updated saved media`),
		});
		logger.debug(`Successfully updated favorite meme ${memeId}`);
	} catch (error) {
		logger.error(`Failed to update favorite meme ${memeId}:`, error);
		throw error;
	}
};

export const deleteFavoriteMeme = async (i18n: I18n, memeId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.USER_FAVORITE_MEME(ME, memeId)});

		FavoriteMemeStore.deleteMeme(memeId);

		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Removed from saved media`),
		});
		logger.debug(`Successfully deleted favorite meme ${memeId}`);
	} catch (error) {
		logger.error(`Failed to delete favorite meme ${memeId}:`, error);
		throw error;
	}
};
