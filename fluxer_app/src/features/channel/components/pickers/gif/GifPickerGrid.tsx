// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/GifPicker.module.css';
import {buildGifPickerGridData} from '@app/features/channel/components/pickers/gif/GifPickerGridData';
import {GifPickerGridItem} from '@app/features/channel/components/pickers/gif/GifPickerGridItem';
import type {GifPickerState} from '@app/features/channel/components/pickers/gif/GifPickerState';
import type {GifPickerGridItemData} from '@app/features/channel/components/pickers/gif/GifPickerTypes';
import {computeMasonryColumns} from '@app/features/channel/components/pickers/shared/ComputeColumns';
import {MasonryVirtualGrid} from '@app/features/channel/components/pickers/shared/MasonryVirtualGrid';
import {MASONRY_PADDING_PX} from '@app/features/channel/components/pickers/shared/PickerConstants';
import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import * as GifCommands from '@app/features/expressions/commands/GifCommands';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';
import {FAVORITES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const TRENDING_GIFS_DESCRIPTOR = msg({
	message: 'Trending GIFs',
	comment: 'Short label in the channel and chat gif picker grid. Keep it concise.',
});

export const GifPickerGrid = observer(
	({
		store,
		onClose,
		selectGif,
		autoSendKlipyGifs,
		gifAutoPlay,
		viewportWidth,
		viewportHeight,
		scrollTop,
		onContentSizeChange,
	}: {
		store: GifPickerState;
		onClose?: () => void;
		selectGif?: (gif: Gif) => void;
		autoSendKlipyGifs: boolean;
		gifAutoPlay: boolean;
		viewportWidth: number;
		viewportHeight: number;
		scrollTop: number;
		onContentSizeChange?: (contentSize: number) => void;
	}) => {
		const {i18n} = useLingui();
		const tileSpacing = 8;
		const columns = computeMasonryColumns(viewportWidth, tileSpacing, {minColumns: 2});
		const favoriteMemes = FavoriteMemes.memes;
		const favoriteMemesVersion = favoriteMemes.length;
		const favoriteGifs = FavoriteGif.favoriteGifs;
		const favoriteGifsVersion = favoriteGifs.length;
		const useSavedMediaForGifFavorites = FavoriteGif.saveGifFavoritesAsSavedMedia;
		const data: Array<GifPickerGridItemData> = useMemo(() => {
			return buildGifPickerGridData({
				surface: store.isShowingFavorites ? 'favorites' : store.isShowingFeatured ? 'featured' : 'results',
				loading: store.loading,
				columns,
				provider: RuntimeConfig.gifProvider,
				featured: store.featured,
				gifs: store.gifsToRender,
				favoriteGifs,
				favoriteMemes,
				useSavedMediaForGifFavorites,
				includeFavoritesTile: selectGif == null,
				featuredFavoritePreviewSeed: store.featuredFavoritePreviewSeed,
				favoriteTitle: i18n._(FAVORITES_DESCRIPTOR),
				trendingTitle: i18n._(TRENDING_GIFS_DESCRIPTOR),
			});
		}, [
			store.isShowingFavorites,
			store.isShowingFeatured,
			store.loading,
			store.gifsToRender,
			store.featured,
			columns,
			favoriteMemesVersion,
			favoriteMemes,
			favoriteGifsVersion,
			favoriteGifs,
			useSavedMediaForGifFavorites,
			selectGif,
			store.featuredFavoritePreviewSeed,
			i18n.locale,
		]);
		const itemKeys = useMemo(() => data.filter((item) => item.type !== 'skeleton').map((item) => item.key), [data]);
		const itemByKey = useMemo(() => new Map(data.map((item) => [item.key, item])), [data]);
		const bottomPaddingPx = MASONRY_PADDING_PX * 2;
		const handleShowFavorites = useCallback(() => {
			if (FavoriteGif.saveGifFavoritesAsSavedMedia && FavoriteGif.favoriteGifs.length === 0) {
				ExpressionPickerCommands.setTab('memes');
			} else {
				store.goToFavorites();
			}
		}, [store]);
		const handleSelectByKey = useCallback(
			(itemKey: string) => {
				const item = itemByKey.get(itemKey);
				if (!item || item.type === 'skeleton') return;
				if (item.type === 'category') {
					if (item.id === 'favorites') {
						handleShowFavorites();
					} else if (item.id === 'trending') {
						store.goToTrending();
					} else {
						store.goToCategory(item.id);
					}
					return;
				}
				const {gif} = item;
				const shareId = GifSlugUtils.resolveShareId(RuntimeConfig.gifProvider, gif);
				if (shareId) {
					GifCommands.registerShare(shareId, store.searchTerm);
				}
				if (selectGif) {
					selectGif(gif);
					onClose?.();
				}
			},
			[itemByKey, handleShowFavorites, onClose, selectGif, store],
		);
		const suggestionsHeight = store.suggestions.length > 0 ? 60 : 0;
		return (
			<MasonryVirtualGrid
				data={data}
				itemKeys={itemKeys}
				columns={columns}
				tileSpacing={tileSpacing}
				viewportWidth={viewportWidth}
				viewportHeight={viewportHeight}
				scrollTop={scrollTop}
				onContentSizeChange={onContentSizeChange}
				bottomPaddingPx={bottomPaddingPx}
				checkSuspension={() => QuickSwitcher.isOpen}
				onSelectItemKey={(key) => {
					handleSelectByKey(key);
				}}
				tileKeyOf={(item) => item.key}
				tileHeightOf={(item, _index, laneWidth) => {
					if (item.type === 'gif') {
						const g = item.gif;
						return laneWidth * (g.height / g.width);
					}
					return laneWidth * (item.height / item.width);
				}}
				extraSections={[
					{
						sectionIndex: 1,
						height: suggestionsHeight,
						render: () =>
							store.suggestions.length > 0 ? (
								<div style={{padding: '10px'}} data-flx="channel.pickers.gif.gif-picker-grid.div">
									<div
										className={styles.suggestionsContainer}
										data-flx="channel.pickers.gif.gif-picker-grid.suggestions-container"
									>
										{store.suggestions.map((suggestion) => (
											<button
												key={suggestion}
												type="button"
												className={styles.suggestionTag}
												onClick={() => store.setSearchTerm(suggestion)}
												data-flx="channel.pickers.gif.gif-picker-grid.suggestion-tag.set-search-term.button"
											>
												{suggestion}
											</button>
										))}
									</div>
								</div>
							) : null,
					},
				]}
				renderItem={({item, itemKey, coords, isFocused}) => (
					<GifPickerGridItem
						key={itemKey}
						item={item}
						itemKey={itemKey}
						coords={coords}
						isFocused={isFocused}
						onClose={onClose}
						selectGif={selectGif}
						autoSendKlipyGifs={autoSendKlipyGifs}
						gifAutoPlay={gifAutoPlay}
						searchTerm={store.searchTerm}
						onShowFavorites={handleShowFavorites}
						onShowTrending={store.goToTrending}
						onSearchCategory={store.goToCategory}
						data-flx="channel.pickers.gif.gif-picker-grid.gif-picker-grid-item"
					/>
				)}
				data-flx="channel.pickers.gif.gif-picker-grid.masonry-virtual-grid"
			/>
		);
	},
);
