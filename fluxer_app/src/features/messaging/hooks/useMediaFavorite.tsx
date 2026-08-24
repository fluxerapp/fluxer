// SPDX-License-Identifier: AGPL-3.0-or-later

import {FavoriteGifFirstTimePromptModal} from '@app/features/channel/components/pickers/gif/FavoriteGifFirstTimePromptModal';
import * as FavoriteGifCommands from '@app/features/expressions/commands/FavoriteGifCommands';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import {AddFavoriteMemeModal} from '@app/features/expressions/components/modals/AddFavoriteMemeModal';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import * as FavoriteGifUtils from '@app/features/expressions/utils/FavoriteGifUtils';
import * as FavoriteMemeUtils from '@app/features/expressions/utils/FavoriteMemeUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import {autorun} from 'mobx';
import {useCallback, useSyncExternalStore} from 'react';

interface UseMediaFavoriteParams {
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	embedIndex?: number;
	defaultName?: string;
	defaultAltText?: string;
	contentHash?: string | null;
	isGifv?: boolean;
	gifSlug?: string | null;
	gifProvider?: string | null;
	embedURL?: string;
	proxyURL?: string;
	naturalWidth?: number;
	naturalHeight?: number;
}

interface UseMediaFavoriteReturn {
	isFavorited: boolean;
	toggleFavorite: (e?: React.MouseEvent) => Promise<void>;
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
	isGifv,
	gifSlug,
	gifProvider,
	embedURL,
	proxyURL,
	naturalWidth,
	naturalHeight,
}: UseMediaFavoriteParams): UseMediaFavoriteReturn {
	const {i18n} = useLingui();
	const memes = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => FavoriteMemes.memes,
	);
	const gifFavoriteState = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => {
			if (!isGifv || !embedURL) return 'none';
			const useSavedMedia = FavoriteGif.saveGifFavoritesAsSavedMedia;
			const hasUrl = FavoriteGif.hasUrl(embedURL);
			return `${useSavedMedia ? 'sm' : 'url'}:${hasUrl ? '1' : '0'}`;
		},
	);
	const useSavedMediaForGifFavorites = gifFavoriteState.startsWith('sm:');
	const hasUrlOnlyGifFavorite = gifFavoriteState.endsWith(':1');
	const useUrlOnlyGifFlow = !!isGifv && !!embedURL && !useSavedMediaForGifFavorites;
	const hasSavedMediaFavorite = FavoriteMemeUtils.isFavorited(memes, {contentHash, gifSlug, gifProvider});
	const isFavorited =
		isGifv && embedURL
			? FavoriteGifUtils.isGifFavoriteActive({
					hasUrlOnlyFavorite: hasUrlOnlyGifFavorite,
					hasSavedMediaFavorite,
					saveAsSavedMedia: useSavedMediaForGifFavorites,
				})
			: hasSavedMediaFavorite;
	const canFavorite = !!(channelId && messageId && (attachmentId || embedIndex !== undefined));
	const toggleFavorite = useCallback(
		async (e?: React.MouseEvent) => {
			e?.stopPropagation();
			if (!canFavorite) return;
			const hasUrlOnlyGifFavoriteNow = !!isGifv && !!embedURL && FavoriteGif.hasUrl(embedURL);
			if (useUrlOnlyGifFlow || hasUrlOnlyGifFavoriteNow) {
				const performToggle = () => {
					if (FavoriteGif.hasUrl(embedURL!)) {
						FavoriteGifCommands.removeFavoriteGifByUrl(i18n, embedURL!);
					} else {
						FavoriteGifCommands.addFavoriteGifFromMedia(i18n, {
							url: embedURL!,
							proxyUrl: proxyURL,
							width: naturalWidth ?? 0,
							height: naturalHeight ?? 0,
							media: {},
							placeholder: null,
						});
					}
				};
				if (!FavoriteGif.hasSeenFavoriteGifFirstTimePrompt && !isFavorited) {
					ModalCommands.push(
						modal(() => (
							<FavoriteGifFirstTimePromptModal
								onConfirm={performToggle}
								data-flx="messaging.use-media-favorite.toggle-favorite.favorite-gif-first-time-prompt-modal"
							/>
						)),
					);
					return;
				}
				performToggle();
				return;
			}
			if (isFavorited) {
				const meme = FavoriteMemeUtils.findFavoritedMeme(memes, {contentHash, gifSlug, gifProvider});
				if (!meme) return;
				await FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
			} else {
				ModalCommands.push(
					modal(() => (
						<AddFavoriteMemeModal
							channelId={channelId!}
							messageId={messageId!}
							attachmentId={attachmentId}
							embedIndex={embedIndex}
							defaultName={defaultName}
							defaultAltText={defaultAltText}
							data-flx="messaging.use-media-favorite.toggle-favorite.add-favorite-meme-modal"
						/>
					)),
				);
			}
		},
		[
			canFavorite,
			isFavorited,
			contentHash,
			gifSlug,
			gifProvider,
			memes,
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName,
			defaultAltText,
			i18n,
			isGifv,
			useUrlOnlyGifFlow,
			embedURL,
			proxyURL,
			naturalWidth,
			naturalHeight,
		],
	);
	return {
		isFavorited,
		toggleFavorite,
		canFavorite,
	};
}
