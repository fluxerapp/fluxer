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

import {useLingui} from '@lingui/react/macro';
import {autorun} from 'mobx';
import {useCallback, useSyncExternalStore} from 'react';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AddFavoriteMemeModal} from '~/components/modals/AddFavoriteMemeModal';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import * as FavoriteMemeUtils from '~/utils/FavoriteMemeUtils';

interface UseMediaFavoriteParams {
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	embedIndex?: number;
	defaultName?: string;
	defaultAltText?: string;
	contentHash?: string | null;
	isGifv?: boolean;
	tenorId?: string | null;
}

interface UseMediaFavoriteReturn {
	isFavorited: boolean;
	toggleFavorite: (e: React.MouseEvent) => Promise<void>;
	canFavorite: boolean;
}

export function useMediaFavorite({
	channelId,
	messageId,
	attachmentId,
	embedIndex,
	defaultName,
	defaultAltText,
	contentHash,
	tenorId,
}: UseMediaFavoriteParams): UseMediaFavoriteReturn {
	const {i18n} = useLingui();

	const memes = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => FavoriteMemeStore.memes,
	);

	const isFavorited = FavoriteMemeUtils.isFavorited(memes, {contentHash, tenorId});

	const canFavorite = !!(channelId && messageId && (attachmentId || embedIndex !== undefined));

	const toggleFavorite = useCallback(
		async (e: React.MouseEvent) => {
			e.stopPropagation();

			if (!canFavorite) return;

			if (isFavorited) {
				const meme = FavoriteMemeUtils.findFavoritedMeme(memes, {contentHash, tenorId});
				if (!meme) return;
				await FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<AddFavoriteMemeModal
							channelId={channelId!}
							messageId={messageId!}
							attachmentId={attachmentId}
							embedIndex={embedIndex}
							defaultName={defaultName}
							defaultAltText={defaultAltText}
						/>
					)),
				);
			}
		},
		[
			canFavorite,
			isFavorited,
			contentHash,
			tenorId,
			memes,
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName,
			defaultAltText,
			i18n,
		],
	);

	return {
		isFavorited,
		toggleFavorite,
		canFavorite,
	};
}
