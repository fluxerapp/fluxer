// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import styles from '@app/features/channel/components/GifPicker.module.css';
import {useGifVideoPool} from '@app/features/channel/components/GifVideoPool';
import type {GifPickerProps} from '@app/features/channel/components/pickers/gif/GifPicker';
import {GifPickerGrid} from '@app/features/channel/components/pickers/gif/GifPickerGrid';
import {GifPickerHeader} from '@app/features/channel/components/pickers/gif/GifPickerHeader';
import {GifPickerLoadingSkeletonGrid} from '@app/features/channel/components/pickers/gif/GifPickerLoadingSkeletonGrid';
import {GifPickerState} from '@app/features/channel/components/pickers/gif/GifPickerState';
import {ImportFavoriteGifsModal} from '@app/features/channel/components/pickers/gif/ImportFavoriteGifsModal';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import {PickerEmptyState} from '@app/features/channel/components/shared/PickerEmptyState';
import * as FavoriteGifCommands from '@app/features/expressions/commands/FavoriteGifCommands';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import {downloadTextFile} from '@app/features/platform/utils/DownloadFile';
import {modal, push} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useWindowFocusVideoControl} from '@app/features/voice/hooks/useWindowFocusVideoControl';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SmileySadIcon, StarIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const NO_SEARCH_RESULTS_DESCRIPTOR = msg({
	message: 'No search results',
	comment: 'Empty-state text in the channel and chat gif picker view.',
});
const TRY_ANOTHER_SEARCH_TERM_DESCRIPTOR = msg({
	message: 'Try another search term',
	comment: 'Label in the channel and chat gif picker view.',
});
const GIFS_COULD_NOT_LOAD_DESCRIPTOR = msg({
	message: 'GIFs could not load',
	comment: 'Error-state text in the channel and chat gif picker view.',
});
const CHECK_CONNECTION_AND_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Check your connection and try again.',
	comment: 'Error-state description in the channel and chat gif picker view.',
});
const NO_FAVORITE_GIFS_YET_DESCRIPTOR = msg({
	message: 'No favorite GIFs yet',
	comment: 'Empty-state text in the channel and chat gif picker view.',
});
const STAR_A_GIF_OR_IMPORT_A_LIST_TO_DESCRIPTOR = msg({
	message: 'Star a GIF or import a list to see it here.',
	comment: 'Description text in the channel and chat gif picker view.',
});
export const GifPickerView = observer(({onClose, selectGif}: GifPickerProps = {}) => {
	const {i18n} = useLingui();
	const storeRef = useRef<GifPickerState | null>(null);
	if (!storeRef.current) storeRef.current = new GifPickerState();
	const store = storeRef.current;
	const headerPortalContext = useExpressionPickerHeaderPortal();
	const hasPortal = Boolean(headerPortalContext?.headerPortalElement);
	const autoSendKlipyGifs = Accessibility.autoSendKlipyGifs;
	const gifAutoPlay = true;
	const videoPool = useGifVideoPool();
	const scrollerRef = useRef<ScrollerHandle>(null);
	const searchInputRef = useRef<HTMLInputElement>(null);
	const [estimatedContentSize, setEstimatedContentSize] = useState<number | null>(null);
	useSearchInputAutofocus(searchInputRef);
	const {viewportSize, scrollTop, handleScroll, handleResize, jumpToStartEdge} = useScrollerViewport(scrollerRef);
	useWindowFocusVideoControl({scrollerRef, videoPool});
	useEffect(() => {
		store.ensureFeaturedLoaded();
		return () => store.dispose();
	}, [store]);
	useEffect(() => {
		jumpToStartEdge();
	}, [store.view, jumpToStartEdge]);
	useEffect(() => {
		if (
			(!store.loading && store.shouldRenderSearchResults) ||
			(!store.searchTerm.trim() && !store.shouldRenderSearchResults)
		) {
			jumpToStartEdge();
		}
	}, [store.loading, store.shouldRenderSearchResults, store.searchTerm, jumpToStartEdge]);
	useEffect(() => {
		if (!store.isShowingFavorites || FavoriteGif.totalCount === 0) return;
		void FavoriteGifCommands.refreshFavoriteGifPreviews();
	}, [store.isShowingFavorites, FavoriteGif.totalCount]);
	const handleOpenImport = useCallback(() => {
		push(
			modal(() => (
				<ImportFavoriteGifsModal data-flx="channel.pickers.gif.gif-picker-view.handle-open-import.import-favorite-gifs-modal" />
			)),
		);
	}, []);
	const handleExportFavorites = useCallback(() => {
		const urls = FavoriteGif.favoriteGifs.map((entry) => entry.url.trim()).filter((url) => url.length > 0);
		if (urls.length === 0) return;
		downloadTextFile(`${urls.join('\n')}\n`, 'fluxer-favorite-gifs.txt');
	}, []);
	const isSelectionMode = selectGif != null;
	const header = (
		<GifPickerHeader
			store={store}
			inputRef={searchInputRef}
			onOpenImport={isSelectionMode ? undefined : handleOpenImport}
			onExportFavorites={isSelectionMode ? undefined : handleExportFavorites}
			canExportFavorites={FavoriteGif.totalCount > 0}
			data-flx="channel.pickers.gif.gif-picker-view.gif-picker-header"
		/>
	);
	const headerElement = hasPortal ? (
		<ExpressionPickerHeaderPortal data-flx="channel.pickers.gif.gif-picker-view.expression-picker-header-portal">
			{header}
		</ExpressionPickerHeaderPortal>
	) : (
		<div className={styles.mobileHeaderWrapper} data-flx="channel.pickers.gif.gif-picker-view.mobile-header-wrapper">
			{header}
		</div>
	);
	if (store.initialFeaturedLoading && store.isLandingPage) {
		return (
			<div className={styles.gifPickerContainer} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container">
				{headerElement}
				<div
					className={styles.gifPickerMain}
					style={{display: 'grid', placeItems: 'center'}}
					data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main"
				>
					<Spinner size="large" data-flx="channel.pickers.gif.gif-picker-view.spinner" />
				</div>
			</div>
		);
	}
	if (store.shouldShowLoadingSkeleton) {
		return (
			<div
				className={styles.gifPickerContainer}
				data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container--skeleton"
			>
				{headerElement}
				<div className={styles.gifPickerMain} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main--skeleton">
					<GifPickerLoadingSkeletonGrid
						key={store.loadingSkeletonKey}
						data-flx="channel.pickers.gif.gif-picker-view.gif-picker-loading-skeleton-grid"
					/>
				</div>
			</div>
		);
	}
	if (store.shouldShowNoResults) {
		return (
			<div className={styles.gifPickerContainer} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container--2">
				{headerElement}
				<div className={styles.gifPickerMain} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main--2">
					<PickerEmptyState
						icon={SmileySadIcon}
						title={i18n._(NO_SEARCH_RESULTS_DESCRIPTOR)}
						description={i18n._(TRY_ANOTHER_SEARCH_TERM_DESCRIPTOR)}
						data-flx="channel.pickers.gif.gif-picker-view.picker-empty-state"
					/>
				</div>
			</div>
		);
	}
	if (store.shouldShowError) {
		return (
			<div
				className={styles.gifPickerContainer}
				data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container--error"
			>
				{headerElement}
				<div className={styles.gifPickerMain} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main--error">
					<PickerEmptyState
						icon={WarningCircleIcon}
						title={i18n._(GIFS_COULD_NOT_LOAD_DESCRIPTOR)}
						description={i18n._(CHECK_CONNECTION_AND_TRY_AGAIN_DESCRIPTOR)}
						data-flx="channel.pickers.gif.gif-picker-view.error-state"
					/>
				</div>
			</div>
		);
	}
	if (store.isShowingFavorites && FavoriteGif.totalCount === 0) {
		return (
			<div className={styles.gifPickerContainer} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container--3">
				{headerElement}
				<div className={styles.gifPickerMain} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main--3">
					<PickerEmptyState
						icon={StarIcon}
						title={i18n._(NO_FAVORITE_GIFS_YET_DESCRIPTOR)}
						description={i18n._(STAR_A_GIF_OR_IMPORT_A_LIST_TO_DESCRIPTOR)}
						data-flx="channel.pickers.gif.gif-picker-view.picker-empty-state--2"
					/>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.gifPickerContainer} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-container--4">
			{headerElement}
			<div className={styles.gifPickerMain} data-flx="channel.pickers.gif.gif-picker-view.gif-picker-main--4">
				<div className={styles.autoSizerWrapper} data-flx="channel.pickers.gif.gif-picker-view.auto-sizer-wrapper">
					<Scroller
						ref={scrollerRef}
						className={styles.virtualList}
						contentClassName={styles.scrollContent}
						onScroll={handleScroll}
						onResize={handleResize}
						estimatedContentSize={estimatedContentSize}
						fade={false}
						key="gif-picker-grid-scroller"
						style={{height: '100%', width: '100%'}}
						data-flx="channel.pickers.gif.gif-picker-view.virtual-list"
					>
						{viewportSize.width > 0 && viewportSize.height > 0 && (
							<GifPickerGrid
								store={store}
								onClose={onClose}
								selectGif={selectGif}
								autoSendKlipyGifs={autoSendKlipyGifs}
								gifAutoPlay={gifAutoPlay}
								viewportWidth={viewportSize.width}
								viewportHeight={viewportSize.height}
								scrollTop={scrollTop}
								onContentSizeChange={setEstimatedContentSize}
								data-flx="channel.pickers.gif.gif-picker-view.gif-picker-grid"
							/>
						)}
					</Scroller>
				</div>
			</div>
		</div>
	);
});
