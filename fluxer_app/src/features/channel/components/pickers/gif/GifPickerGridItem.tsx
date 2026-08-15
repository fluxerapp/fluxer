// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAnimatedMediaPlaybackAllowed} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/GifPicker.module.css';
import {safePause, safePlay, useGifVideoPool} from '@app/features/channel/components/GifVideoPool';
import {FavoriteGifFirstTimePromptModal} from '@app/features/channel/components/pickers/gif/FavoriteGifFirstTimePromptModal';
import type {GifPickerGridItemData} from '@app/features/channel/components/pickers/gif/GifPickerTypes';
import {PickerThumbnail} from '@app/features/channel/components/pickers/shared/PickerThumbnail';
import {usePooledVideo} from '@app/features/channel/components/pickers/shared/usePooledVideo';
import * as FavoriteGifCommands from '@app/features/expressions/commands/FavoriteGifCommands';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import * as GifCommands from '@app/features/expressions/commands/GifCommands';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import * as FavoriteGifUtils from '@app/features/expressions/utils/FavoriteGifUtils';
import * as FavoriteMemeUtils from '@app/features/expressions/utils/FavoriteMemeUtils';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {modal, push} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {StarIcon, TrendUpIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef, useState} from 'react';

const OPEN_DESCRIPTOR = msg({
	message: 'Open {title}',
	comment:
		'Button or menu action label in the channel and chat gif picker grid item. Keep it concise. Preserve {title}; it is inserted by code.',
});
const UPDATING_FAVORITES_DESCRIPTOR = msg({
	message: 'Updating favorites…',
	comment: 'Short label in the channel and chat gif picker grid item. Keep it concise.',
});
const SEND_GIF_DESCRIPTOR = msg({
	message: 'Send GIF {title}',
	comment:
		'Button or menu action label in the channel and chat gif picker grid item. Keep it concise. Preserve {title}; it is inserted by code.',
});
const SELECT_GIF_DESCRIPTOR = msg({
	message: 'Select GIF {title}',
	comment:
		'Button label in the gif picker grid item when picking a GIF for an avatar, banner, or video background. Keep it concise. Preserve {title}; it is inserted by code.',
});
const VIDEO_FILE_EXTENSION_REGEX = /\.(mp4|webm|mov|m4v)(?:$|\?)/iu;

function isVideoSourceUrl(value: string): boolean {
	try {
		const url = new URL(value);
		return VIDEO_FILE_EXTENSION_REGEX.test(url.pathname);
	} catch {
		return VIDEO_FILE_EXTENSION_REGEX.test(value);
	}
}

export const GifPickerGridItem = observer(function GifPickerGridItem({
	item,
	coords,
	onClose,
	selectGif,
	autoSendKlipyGifs,
	gifAutoPlay,
	searchTerm,
	onShowFavorites,
	onShowTrending,
	onSearchCategory,
	isFocused = false,
	itemKey,
}: {
	item: GifPickerGridItemData;
	coords: {
		position: 'absolute' | 'sticky';
		left?: number;
		right?: number;
		width: number;
		top?: number;
		height: number;
	};
	onClose?: () => void;
	selectGif?: (gif: Gif) => void;
	autoSendKlipyGifs: boolean;
	gifAutoPlay: boolean;
	searchTerm: string;
	onShowFavorites: () => void;
	onShowTrending: () => void;
	onSearchCategory: (term: string) => void;
	isFocused?: boolean;
	itemKey?: string;
}) {
	const {i18n} = useLingui();
	const [isFavoritePending, setIsFavoritePending] = useState(false);
	const videoPool = useGifVideoPool();
	const videoContainerRef = useRef<HTMLDivElement>(null);
	const isSkeleton = item.type === 'skeleton';
	const proxySrc = (() => {
		if (item.type === 'gif') return item.gif.proxy_src;
		if (item.type === 'category') return item.previewProxySrc;
		return null;
	})();
	const mediaSourceUrl = (() => {
		if (item.type === 'gif') return item.gif.src;
		if (item.type === 'category') return item.previewUrl;
		return null;
	})();
	const thumbnailPlaceholder = (() => {
		if (item.type !== 'gif') return null;
		if (item.gif.placeholder) return item.gif.placeholder;
		const lookupUrl = item.gif.favoriteGifLookup?.url ?? item.gif.url;
		return FavoriteGif.findByUrl(lookupUrl)?.placeholder ?? null;
	})();
	const usesVideoElement = !isSkeleton && mediaSourceUrl !== null && isVideoSourceUrl(mediaSourceUrl);
	const videoRef = usePooledVideo({
		src: usesVideoElement ? proxySrc : null,
		containerRef: videoContainerRef,
		videoPool,
		autoPlay: gifAutoPlay,
		enabled: usesVideoElement,
	});
	const playOnHover = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (event.pointerType !== 'mouse') return;
			if (!getAnimatedMediaPlaybackAllowed()) return;
			const v = videoRef.current;
			if (!v) return;
			void safePlay(v);
		},
		[videoRef],
	);
	const stopOnHoverEnd = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (event.pointerType !== 'mouse') return;
			const v = videoRef.current;
			if (!v) return;
			safePause(v);
		},
		[videoRef],
	);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLDivElement> | React.KeyboardEvent<HTMLDivElement>) => {
			if (isSkeleton) return;
			if (item.type === 'category') {
				if (item.id === 'favorites') {
					onShowFavorites();
				} else if (item.id === 'trending') {
					onShowTrending();
				} else {
					onSearchCategory(item.id);
				}
				return;
			}
			const gif = item.gif;
			const provider = RuntimeConfig.gifProvider;
			const shiftKey = 'shiftKey' in event ? event.shiftKey : false;
			if (selectGif) {
				const shareId = GifSlugUtils.resolveShareId(provider, gif);
				if (shareId) {
					GifCommands.registerShare(shareId, searchTerm);
				}
				selectGif(gif);
				onClose?.();
				return;
			}
			if (gif.favoriteGifLookup) {
				ComponentDispatch.dispatch('GIF_SELECT', {
					gif,
					autoSend: autoSendKlipyGifs && !shiftKey,
				});
				if (!shiftKey) onClose?.();
				return;
			}
			const shareId = GifSlugUtils.resolveShareId(provider, gif);
			if (!shareId) return;
			GifCommands.registerShare(shareId, searchTerm);
			const shareUrl = GifSlugUtils.resolveShareUrl(provider, {url: gif.url, slug: shareId});
			ComponentDispatch.dispatch('GIF_SELECT', {
				gif: {
					...gif,
					id: shareId,
					url: shareUrl,
				},
				autoSend: autoSendKlipyGifs && !shiftKey,
			});
			if (!shiftKey) onClose?.();
		},
		[
			autoSendKlipyGifs,
			isSkeleton,
			item,
			onClose,
			onSearchCategory,
			onShowFavorites,
			onShowTrending,
			searchTerm,
			selectGif,
		],
	);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			handleClick(event);
		},
		[handleClick],
	);
	const hoverPlaybackHandlers = gifAutoPlay
		? null
		: {
				onPointerEnter: playOnHover,
				onPointerLeave: stopOnHoverEnd,
			};
	if (isSkeleton) {
		return (
			<div
				className={clsx(styles.gridItem, styles.skeletonItem, isFocused && styles.gridItemFocused)}
				style={coords}
				data-grid-item={itemKey}
				data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item"
			>
				<div
					className={styles.gifMediaContainer}
					data-flx="channel.pickers.gif.gif-picker-grid-item.gif-media-container"
				>
					<div
						className={styles.gifVideoContainer}
						data-flx="channel.pickers.gif.gif-picker-grid-item.gif-video-container"
					/>
				</div>
			</div>
		);
	}
	if (item.type === 'category') {
		let icon: React.ReactNode = null;
		let categoryClassName = styles.gridItemCategory;
		if (item.id === 'favorites') {
			icon = (
				<StarIcon className={styles.gridItemIcon} data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-icon" />
			);
			categoryClassName = clsx(styles.gridItemCategory, styles.gridItemFavorites);
		} else if (item.id === 'trending') {
			icon = (
				<TrendUpIcon
					className={styles.gridItemIcon}
					data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-icon--2"
				/>
			);
		}
		const hasPreview = Boolean(proxySrc) && Boolean(mediaSourceUrl);
		return (
			<div
				role="button"
				tabIndex={0}
				className={clsx(
					styles.gridItem,
					categoryClassName,
					!hasPreview && styles.gridItemCategoryEmpty,
					isFocused && styles.gridItemFocused,
				)}
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				style={coords}
				data-grid-item={itemKey}
				aria-label={i18n._(OPEN_DESCRIPTOR, {title: item.title})}
				data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item.click"
				{...(hoverPlaybackHandlers ?? {})}
			>
				{hasPreview && (
					<div
						className={styles.gifMediaContainer}
						data-flx="channel.pickers.gif.gif-picker-grid-item.gif-media-container--2"
					>
						{usesVideoElement ? (
							<div
								ref={videoContainerRef}
								className={styles.gifVideoContainer}
								data-flx="channel.pickers.gif.gif-picker-grid-item.gif-video-container--2"
							/>
						) : (
							<PickerThumbnail
								src={proxySrc ?? ''}
								alt=""
								className={styles.gif}
								placeholder={thumbnailPlaceholder}
								data-flx="channel.pickers.gif.gif-picker-grid-item.gif"
							/>
						)}
					</div>
				)}
				<div
					className={styles.gridItemBackdrop}
					data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-backdrop"
				/>
				<div
					className={styles.gridItemCategoryTitle}
					data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-category-title"
				>
					{icon}
					<div
						className={styles.gridItemCategoryTitleText}
						data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-category-title-text"
					>
						{item.title}
					</div>
				</div>
			</div>
		);
	}
	const gif = item.gif;
	const provider = RuntimeConfig.gifProvider;
	const useSavedMedia = FavoriteGif.saveGifFavoritesAsSavedMedia;
	const persistedGifSlug = GifSlugUtils.resolveShareId(provider, gif);
	const favoriteMemes = FavoriteMemes.memes;
	const favoriteGifUrl = gif.favoriteGifLookup?.url ?? gif.url;
	const favoriteMeme = FavoriteGifUtils.findFavoriteMemeForGif(favoriteMemes, {
		gifProvider: provider,
		primaryGifSlug: persistedGifSlug,
		fallbackGifSlug: gif.id,
	});
	const isFavorited = FavoriteGifUtils.isGifFavoriteActive({
		hasUrlOnlyFavorite: FavoriteGif.hasUrl(favoriteGifUrl),
		hasSavedMediaFavorite: favoriteMeme !== null,
		saveAsSavedMedia: useSavedMedia,
	});
	const performFavoriteToggle = async () => {
		const useSavedMediaNow = FavoriteGif.saveGifFavoritesAsSavedMedia;
		if (!useSavedMediaNow) {
			if (FavoriteGif.hasUrl(favoriteGifUrl)) {
				FavoriteGifCommands.removeFavoriteGifByUrl(i18n, favoriteGifUrl);
			} else {
				FavoriteGifCommands.addFavoriteGifFromMedia(i18n, {
					url: favoriteGifUrl,
					proxyUrl: gif.proxy_src,
					width: gif.width,
					height: gif.height,
					media: gif.media ?? {},
					placeholder: gif.placeholder ?? null,
				});
			}
			return;
		}
		setIsFavoritePending(true);
		try {
			if (FavoriteGif.hasUrl(favoriteGifUrl)) {
				FavoriteGifCommands.removeFavoriteGifByUrl(i18n, favoriteGifUrl);
			} else if (isFavorited) {
				const meme = FavoriteGifUtils.findFavoriteMemeForGif(favoriteMemes, {
					gifProvider: provider,
					primaryGifSlug: persistedGifSlug,
					fallbackGifSlug: gif.id,
				});
				if (meme) {
					await FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
				}
			} else {
				const defaultName = FavoriteMemeUtils.deriveDefaultNameFromEmbedMedia(i18n, {
					url: gif.url,
					proxy_url: gif.proxy_src,
					flags: 0,
				});
				await FavoriteMemeCommands.createFavoriteMemeFromUrl(i18n, {
					url: gif.proxy_src,
					name: defaultName || gif.title,
					gifSlug: persistedGifSlug ?? undefined,
					gifProvider: persistedGifSlug ? provider : undefined,
					media: gif.media,
				});
			}
		} finally {
			setIsFavoritePending(false);
		}
	};
	const handleFavoriteClick = (e: React.MouseEvent) => {
		e.stopPropagation();
		if (isFavoritePending) return;
		if (!FavoriteGif.hasSeenFavoriteGifFirstTimePrompt && !isFavorited) {
			push(
				modal(() => (
					<FavoriteGifFirstTimePromptModal
						onConfirm={() => void performFavoriteToggle()}
						data-flx="channel.pickers.gif.gif-picker-grid-item.handle-favorite-click.favorite-gif-first-time-prompt-modal"
					/>
				)),
			);
			return;
		}
		void performFavoriteToggle();
	};
	const favoriteTooltipText = (() => {
		if (isFavoritePending) return i18n._(UPDATING_FAVORITES_DESCRIPTOR);
		if (isFavorited) return i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR);
		return i18n._(ADD_TO_FAVORITES_DESCRIPTOR);
	})();
	return (
		<FocusRing offset={-2} data-flx="channel.pickers.gif.gif-picker-grid-item.focus-ring">
			<div
				role="button"
				tabIndex={0}
				className={clsx(
					styles.gridItem,
					styles.gridItemGif,
					styles.gridItemGifPicker,
					isFocused && styles.gridItemFocused,
					isFavoritePending && styles.gridItemFavoritePending,
				)}
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				style={coords}
				data-grid-item={itemKey}
				aria-label={
					selectGif
						? i18n._(SELECT_GIF_DESCRIPTOR, {title: gif.title})
						: i18n._(SEND_GIF_DESCRIPTOR, {title: gif.title})
				}
				data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item.click--2"
				{...(hoverPlaybackHandlers ?? {})}
			>
				<div
					className={styles.gifMediaContainer}
					data-flx="channel.pickers.gif.gif-picker-grid-item.gif-media-container--3"
				>
					{usesVideoElement ? (
						<div
							ref={videoContainerRef}
							className={styles.gifVideoContainer}
							data-flx="channel.pickers.gif.gif-picker-grid-item.gif-video-container--3"
						/>
					) : (
						<PickerThumbnail
							src={proxySrc ?? ''}
							alt={gif.title || ''}
							className={styles.gif}
							placeholder={thumbnailPlaceholder}
							data-flx="channel.pickers.gif.gif-picker-grid-item.gif--2"
						/>
					)}
				</div>
				<div
					className={styles.gridItemBackdrop}
					data-flx="channel.pickers.gif.gif-picker-grid-item.grid-item-backdrop--2"
				/>
				{selectGif == null && (
					<div
						className={styles.hoverActionButtons}
						data-flx="channel.pickers.gif.gif-picker-grid-item.hover-action-buttons"
					>
						<Tooltip
							text={favoriteTooltipText}
							position="top"
							data-flx="channel.pickers.gif.gif-picker-grid-item.tooltip"
						>
							<FocusRing offset={-2} data-flx="channel.pickers.gif.gif-picker-grid-item.focus-ring--2">
								<button
									type="button"
									onMouseDown={(e) => {
										if (e.button === 0) e.preventDefault();
									}}
									onClick={handleFavoriteClick}
									className={clsx(styles.favoriteButton, isFavorited && styles.favoriteButtonActive)}
									aria-label={
										isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)
									}
									aria-busy={isFavoritePending}
									aria-pressed={isFavorited}
									disabled={isFavoritePending}
									data-flx="channel.pickers.gif.gif-picker-grid-item.favorite-button.favorite-click"
								>
									{isFavoritePending ? (
										<span
											className={styles.favoriteButtonSpinner}
											aria-hidden="true"
											data-flx="channel.pickers.gif.gif-picker-grid-item.favorite-button-spinner"
										/>
									) : (
										<StarIcon
											size={remFromPx(18)}
											weight={isFavorited ? 'fill' : 'bold'}
											className={isFavorited ? styles.favoriteButtonActiveIcon : styles.favoriteButtonIcon}
											data-flx="channel.pickers.gif.gif-picker-grid-item.favorite-button"
										/>
									)}
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				)}
			</div>
		</FocusRing>
	);
});
