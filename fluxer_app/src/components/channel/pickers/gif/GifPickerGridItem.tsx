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
import {StarIcon, TrendUpIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React from 'react';
import * as ExpressionPickerActionCreators from '~/actions/ExpressionPickerActionCreators';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as TenorActionCreators from '~/actions/TenorActionCreators';
import styles from '~/components/channel/GifPicker.module.css';
import {useGifVideoPool} from '~/components/channel/GifVideoPool';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import messageStyles from '~/styles/Message.module.css';
import * as FavoriteMemeUtils from '~/utils/FavoriteMemeUtils';
import {usePooledVideo} from '../shared/usePooledVideo';
import type {GifGridItem} from './types';

export const GifPickerGridItem = React.memo(function GifPickerGridItem({
	item,
	coords,
	onClose,
	autoSendTenorGifs,
	gifAutoPlay,
	searchTerm,
	onShowTrending,
	onSearchCategory,
	isFocused = false,
	itemKey,
}: {
	item: GifGridItem;
	coords: {
		position: 'absolute' | 'sticky';
		left?: number;
		right?: number;
		width: number;
		top?: number;
		height: number;
	};
	onClose?: () => void;
	autoSendTenorGifs: boolean;
	gifAutoPlay: boolean;
	searchTerm: string;
	onShowTrending: () => void;
	onSearchCategory: (term: string) => void;
	isFocused?: boolean;
	itemKey?: string;
}) {
	const {t, i18n} = useLingui();
	const [isFavoritePending, setIsFavoritePending] = React.useState(false);

	const videoPool = useGifVideoPool();
	const videoContainerRef = React.useRef<HTMLDivElement>(null);

	const isSkeleton = item.type === 'skeleton';

	const proxySrc = item.type === 'gif' ? item.gif.proxy_src : item.type === 'category' ? item.previewProxySrc : null;

	const videoRef = usePooledVideo({
		src: isSkeleton ? null : proxySrc,
		containerRef: videoContainerRef,
		videoPool,
		autoPlay: gifAutoPlay,
		enabled: !isSkeleton,
	});

	const playOnHover = React.useCallback(() => {
		if (gifAutoPlay) return;
		videoRef.current?.play().catch(() => {});
	}, [gifAutoPlay, videoRef]);

	const stopOnHoverEnd = React.useCallback(() => {
		if (gifAutoPlay) return;
		const v = videoRef.current;
		if (!v) return;
		v.pause();
		try {
			v.currentTime = 0;
		} catch {}
	}, [gifAutoPlay, videoRef]);

	const handleClick = (event: React.MouseEvent<HTMLDivElement> | React.KeyboardEvent<HTMLDivElement>) => {
		if (isSkeleton) return;

		if (item.type === 'category') {
			if (item.id === 'favorites') {
				ExpressionPickerActionCreators.setTab('memes');
			} else if (item.id === 'trending') {
				onShowTrending();
			} else {
				onSearchCategory(item.id);
			}
			return;
		}

		const gif = item.gif;
		if (!gif.id) return;

		TenorActionCreators.registerShare(gif.id, searchTerm);
		const shiftKey = 'shiftKey' in event ? event.shiftKey : false;

		ComponentDispatch.dispatch('GIF_SELECT', {
			gif,
			autoSend: autoSendTenorGifs && !shiftKey,
		});

		if (!shiftKey) onClose?.();
	};

	if (isSkeleton) {
		return (
			<div
				className={clsx(styles.gridItem, styles.skeletonItem, isFocused && styles.gridItemFocused)}
				style={coords}
				data-grid-item={itemKey}
			>
				<div className={styles.gifMediaContainer}>
					<div className={styles.gifVideoContainer} />
				</div>
			</div>
		);
	}

	if (item.type === 'category') {
		let icon: React.ReactNode = null;
		let categoryClassName = styles.gridItemCategory;

		if (item.id === 'favorites') {
			icon = <StarIcon className={styles.gridItemIcon} />;
			categoryClassName = clsx(styles.gridItemCategory, styles.gridItemFavorites);
		} else if (item.id === 'trending') {
			icon = <TrendUpIcon className={styles.gridItemIcon} />;
		}

		return (
			<div
				role="button"
				tabIndex={0}
				className={clsx(styles.gridItem, categoryClassName, isFocused && styles.gridItemFocused)}
				onClick={handleClick}
				onKeyDown={(event) => event.key === 'Enter' && handleClick(event)}
				onMouseEnter={playOnHover}
				onMouseLeave={stopOnHoverEnd}
				style={coords}
				data-grid-item={itemKey}
			>
				<div className={styles.gifMediaContainer}>
					<div ref={videoContainerRef} className={styles.gifVideoContainer} />
				</div>
				<div className={styles.gridItemBackdrop} />
				<div className={styles.gridItemCategoryTitle}>
					{icon}
					<div className={styles.gridItemCategoryTitleText}>{item.title}</div>
				</div>
			</div>
		);
	}

	const gif = item.gif;
	const favoriteMemes = FavoriteMemeStore.memes;
	const isFavorited = FavoriteMemeUtils.isFavoritedByTenorId(favoriteMemes, gif.id);

	const handleFavoriteClick = async (e: React.MouseEvent) => {
		e.stopPropagation();
		if (isFavoritePending) return;

		setIsFavoritePending(true);
		try {
			if (isFavorited) {
				const meme = FavoriteMemeUtils.findFavoritedMeme(favoriteMemes, {tenorId: gif.id});
				if (meme) {
					await FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
				}
			} else {
				const defaultName = FavoriteMemeUtils.deriveDefaultNameFromEmbedMedia(i18n, {
					url: gif.url,
					proxy_url: gif.proxy_src,
					flags: 0,
				});

				await FavoriteMemeActionCreators.createFavoriteMemeFromUrl(i18n, {
					url: gif.proxy_src,
					name: defaultName || gif.title,
					tenorId: gif.id,
				});
			}
		} finally {
			setIsFavoritePending(false);
		}
	};

	return (
		<div
			role="button"
			tabIndex={0}
			className={clsx(
				styles.gridItem,
				styles.gridItemGif,
				isFocused && styles.gridItemFocused,
				isFavoritePending && styles.gridItemFavoritePending,
			)}
			onClick={handleClick}
			onKeyDown={(event) => event.key === 'Enter' && handleClick(event)}
			onMouseEnter={playOnHover}
			onMouseLeave={stopOnHoverEnd}
			style={coords}
			data-grid-item={itemKey}
		>
			<div className={styles.gifMediaContainer}>
				<div ref={videoContainerRef} className={styles.gifVideoContainer} />
			</div>

			<div className={styles.gridItemBackdrop} />

			<div className={clsx(messageStyles.hoverAction, styles.hoverActionButtons)}>
				<Tooltip
					text={
						isFavoritePending ? t`Updating favorites…` : isFavorited ? t`Remove from favorites` : t`Add to favorites`
					}
					position="top"
				>
					<FocusRing offset={-2}>
						<button
							type="button"
							onMouseDown={(e) => e.preventDefault()}
							onClick={handleFavoriteClick}
							className={clsx(styles.favoriteButton, isFavorited && styles.favoriteButtonActive)}
							aria-label={isFavorited ? t`Remove from favorites` : t`Add to favorites`}
							aria-busy={isFavoritePending}
							aria-pressed={isFavorited}
							disabled={isFavoritePending}
						>
							{isFavoritePending ? (
								<span className={styles.favoriteButtonSpinner} aria-hidden="true" />
							) : (
								<StarIcon
									size={18}
									weight={isFavorited ? 'fill' : 'bold'}
									className={isFavorited ? styles.favoriteButtonActiveIcon : styles.favoriteButtonIcon}
								/>
							)}
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		</div>
	);
});
