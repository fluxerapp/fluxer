// SPDX-License-Identifier: AGPL-3.0-or-later

import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import gifStyles from '@app/features/channel/components/GifPicker.module.css';
import {useGifVideoPool} from '@app/features/channel/components/GifVideoPool';
import {MemesGrid} from '@app/features/channel/components/pickers/memes/MemesGrid';
import type {MemesPickerProps} from '@app/features/channel/components/pickers/memes/MemesPicker';
import {type ContentType, MemesPickerHeader} from '@app/features/channel/components/pickers/memes/MemesPickerHeader';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import {PickerEmptyState} from '@app/features/channel/components/shared/PickerEmptyState';
import MemesPicker from '@app/features/emoji/state/MemesPicker';
import {ExpressionPickerHeaderPortal} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useWindowFocusVideoControl} from '@app/features/voice/hooks/useWindowFocusVideoControl';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SmileySadIcon} from '@phosphor-icons/react';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useRef, useState} from 'react';

const NO_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'No saved media',
	comment: 'Empty-state text in the channel and chat memes picker view.',
});
const SAVE_SOME_MEDIA_FROM_MESSAGES_TO_GET_STARTED_DESCRIPTOR = msg({
	message: 'Save some media from messages to get started!',
	comment: 'Button or menu action label in the channel and chat memes picker view. Keep it concise.',
});
const NO_RESULTS_DESCRIPTOR = msg({
	message: 'No results',
	comment: 'Empty-state text in the channel and chat memes picker view.',
});
const TRY_A_DIFFERENT_SEARCH_TERM_OR_FILTER_DESCRIPTOR = msg({
	message: 'Try a different search term or filter',
	comment: 'Label in the channel and chat memes picker view.',
});

interface MemesPickerState {
	searchTerm: string;
	selectedFilter: ContentType;
}

const initialState: MemesPickerState = {
	searchTerm: '',
	selectedFilter: 'all',
};
export const MemesPickerView = observer(({onClose}: MemesPickerProps = {}) => {
	const {i18n} = useLingui();
	const [state, setState] = useState<MemesPickerState>(initialState);
	const favoriteMemes = FavoriteMemes.memes;
	const fetched = FavoriteMemes.fetched;
	const storeLoading = !fetched;
	const videoPool = useGifVideoPool();
	const scrollerRef = useRef<ScrollerHandle>(null);
	const searchInputRef = useRef<HTMLInputElement>(null);
	const [estimatedContentSize, setEstimatedContentSize] = useState<number | null>(null);
	useSearchInputAutofocus(searchInputRef);
	const {viewportSize, scrollTop, handleScroll, handleResize, jumpToStartEdge} = useScrollerViewport(scrollerRef);
	useWindowFocusVideoControl({scrollerRef, videoPool});
	useEffect(() => {
		jumpToStartEdge();
	}, [state.selectedFilter, state.searchTerm, jumpToStartEdge]);
	const filteredMemes = useMemo(() => {
		let memes = [...favoriteMemes];
		if (state.selectedFilter !== 'all') {
			memes = memes.filter((meme) => {
				const contentType = meme.contentType.toLowerCase();
				switch (state.selectedFilter) {
					case 'image':
						return contentType.startsWith('image/') && !contentType.includes('gif') && !meme.isGifv;
					case 'video':
						return contentType.startsWith('video/') && !meme.isGifv;
					case 'audio':
						return contentType.startsWith('audio/');
					case 'gif':
						return contentType.includes('gif') || meme.isGifv;
					default:
						return true;
				}
			});
		}
		if (state.searchTerm) {
			const sortedByMatch = matchSorter(memes, state.searchTerm, {
				keys: ['name', 'altText', 'filename', 'tags'],
				threshold: matchSorter.rankings.CONTAINS,
			});
			const searchIndex = new Map(sortedByMatch.map((meme, index) => [meme.id, index]));
			memes = [...sortedByMatch].sort((a, b) => {
				const frecencyDiff = MemesPicker.getFrecencyScoreForMeme(b) - MemesPicker.getFrecencyScoreForMeme(a);
				if (frecencyDiff !== 0) return frecencyDiff;
				return (searchIndex.get(a.id) ?? 0) - (searchIndex.get(b.id) ?? 0);
			});
		}
		return memes;
	}, [favoriteMemes, state.selectedFilter, state.searchTerm]);
	const header = (
		<MemesPickerHeader
			searchTerm={state.searchTerm}
			onSearchTermChange={(value) => setState((s) => ({...s, searchTerm: value}))}
			onClearSearch={() => {
				setState((s) => ({...s, searchTerm: ''}));
				searchInputRef.current?.focus();
			}}
			selectedFilter={state.selectedFilter}
			onFilterChange={(filter) => setState((s) => ({...s, selectedFilter: filter}))}
			inputRef={searchInputRef}
			data-flx="channel.pickers.memes.memes-picker-view.memes-picker-header"
		/>
	);
	if (storeLoading) {
		return (
			<div className={gifStyles.gifPickerContainer} data-flx="channel.pickers.memes.memes-picker-view.div">
				<ExpressionPickerHeaderPortal data-flx="channel.pickers.memes.memes-picker-view.expression-picker-header-portal">
					{header}
				</ExpressionPickerHeaderPortal>
				<div
					className={gifStyles.gifPickerMain}
					style={{display: 'grid', placeItems: 'center'}}
					data-flx="channel.pickers.memes.memes-picker-view.div--2"
				>
					<Spinner size="large" data-flx="channel.pickers.memes.memes-picker-view.spinner" />
				</div>
			</div>
		);
	}
	if (favoriteMemes.length === 0) {
		return (
			<div className={gifStyles.gifPickerContainer} data-flx="channel.pickers.memes.memes-picker-view.div--3">
				<ExpressionPickerHeaderPortal data-flx="channel.pickers.memes.memes-picker-view.expression-picker-header-portal--2">
					{header}
				</ExpressionPickerHeaderPortal>
				<div className={gifStyles.gifPickerMain} data-flx="channel.pickers.memes.memes-picker-view.div--4">
					<PickerEmptyState
						icon={SmileySadIcon}
						title={i18n._(NO_SAVED_MEDIA_DESCRIPTOR)}
						description={i18n._(SAVE_SOME_MEDIA_FROM_MESSAGES_TO_GET_STARTED_DESCRIPTOR)}
						data-flx="channel.pickers.memes.memes-picker-view.picker-empty-state"
					/>
				</div>
			</div>
		);
	}
	if (filteredMemes.length === 0) {
		return (
			<div className={gifStyles.gifPickerContainer} data-flx="channel.pickers.memes.memes-picker-view.div--5">
				<ExpressionPickerHeaderPortal data-flx="channel.pickers.memes.memes-picker-view.expression-picker-header-portal--3">
					{header}
				</ExpressionPickerHeaderPortal>
				<div className={gifStyles.gifPickerMain} data-flx="channel.pickers.memes.memes-picker-view.div--6">
					<PickerEmptyState
						icon={SmileySadIcon}
						title={i18n._(NO_RESULTS_DESCRIPTOR)}
						description={i18n._(TRY_A_DIFFERENT_SEARCH_TERM_OR_FILTER_DESCRIPTOR)}
						data-flx="channel.pickers.memes.memes-picker-view.picker-empty-state--2"
					/>
				</div>
			</div>
		);
	}
	return (
		<div className={gifStyles.gifPickerContainer} data-flx="channel.pickers.memes.memes-picker-view.div--7">
			<ExpressionPickerHeaderPortal data-flx="channel.pickers.memes.memes-picker-view.expression-picker-header-portal--4">
				{header}
			</ExpressionPickerHeaderPortal>
			<div className={gifStyles.gifPickerMain} data-flx="channel.pickers.memes.memes-picker-view.div--8">
				<div className={gifStyles.autoSizerWrapper} data-flx="channel.pickers.memes.memes-picker-view.div--9">
					<Scroller
						ref={scrollerRef}
						className={gifStyles.virtualList}
						onScroll={handleScroll}
						onResize={handleResize}
						estimatedContentSize={estimatedContentSize}
						fade={false}
						key="memes-picker-grid-scroller"
						style={{height: '100%', width: '100%'}}
						data-flx="channel.pickers.memes.memes-picker-view.scroller"
					>
						{viewportSize.width > 0 && viewportSize.height > 0 && (
							<MemesGrid
								memes={filteredMemes}
								onClose={onClose}
								viewportWidth={viewportSize.width}
								viewportHeight={viewportSize.height}
								scrollTop={scrollTop}
								onContentSizeChange={setEstimatedContentSize}
								data-flx="channel.pickers.memes.memes-picker-view.memes-grid"
							/>
						)}
					</Scroller>
				</div>
			</div>
		</div>
	);
});
