// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {useAnimatedMediaVideoPlayback} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/GifPicker.module.css';
import memeStyles from '@app/features/channel/components/MemesPicker.module.css';
import {PickerEmptyState} from '@app/features/channel/components/shared/PickerEmptyState';
import {PickerSearchInput} from '@app/features/channel/components/shared/PickerSearchInput';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import {EditFavoriteMemeModal} from '@app/features/expressions/components/modals/EditFavoriteMemeModal';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {AUDIO_DESCRIPTOR, GIFS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {buildStaticGifPreviewURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {DeleteIcon, EditIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {MenuBottomSheet, type MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {formatDuration as formatDurationBase} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GifIcon, ImageIcon, MusicNoteIcon, SmileySadIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence} from 'framer-motion';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Tab label in the mobile memes (saved media) picker. Shows all saved media regardless of type.',
});
const IMAGES_DESCRIPTOR = msg({
	message: 'Images',
	comment: 'Tab label in the mobile memes picker. Filters to saved images.',
});
const VIDEOS_DESCRIPTOR = msg({
	message: 'Videos',
	comment: 'Tab label in the mobile memes picker. Filters to saved videos.',
});
const SEARCH_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Search saved media',
	comment: 'Placeholder text in the mobile memes picker search input.',
});
const NO_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'No saved media',
	comment: 'Empty state title in the mobile memes picker when the user has saved nothing yet.',
});
const SAVE_SOME_MEDIA_FROM_MESSAGES_TO_GET_STARTED_DESCRIPTOR = msg({
	message: 'Save some media from messages to get started!',
	comment: 'Empty state body in the mobile memes picker explaining how to save media. Tone can be friendly.',
});
const NO_RESULTS_DESCRIPTOR = msg({
	message: 'No results',
	comment: 'Empty state title in the mobile memes picker when the search query matches nothing.',
});
const TRY_A_DIFFERENT_SEARCH_TERM_OR_FILTER_DESCRIPTOR = msg({
	message: 'Try a different search term or filter',
	comment: 'Empty state body in the mobile memes picker suggesting query changes.',
});
const SELECT_DESCRIPTOR = msg({
	message: 'Select {accessibleName}',
	comment:
		'Accessible label for a saved media tile in the mobile memes picker. accessibleName is the media filename or alt text.',
});
const DELETE_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Delete saved media',
	comment: 'Accessible label and tooltip for the delete button on a saved media tile in the mobile memes picker.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: 'Delete "{memeName}"? Can\'t be undone.',
	comment:
		'Error message in the channel and chat mobile memes picker. Preserve {memeName}; it is inserted by code. Keep the tone plain and specific.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment:
		'Button or menu action label in the channel and chat mobile memes picker. Keep it concise. Keep the tone plain and specific.',
});
const EDIT_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Edit saved media',
	comment: 'Button or menu action label in the channel and chat mobile memes picker. Keep it concise.',
});

const GRID_ITEM_PRELOAD_ROOT_MARGIN = '800px 0px';

type ContentType = 'all' | 'image' | 'video' | 'audio' | 'gif';

interface FilterOption {
	type: ContentType;
	label: string;
	icon?: React.ReactNode;
}

const formatDuration = (seconds: number | null | undefined): string => {
	if (!seconds || seconds <= 0) return '0:00';
	return formatDurationBase(seconds);
};
const getFileExtension = (filename: string, contentType: string): string => {
	const extension = filename.split('.').pop()?.toUpperCase();
	if (extension && extension.length <= 4) {
		return extension;
	}
	const typeMatch = contentType.match(/\/([^;]+)/);
	return typeMatch?.[1]?.toUpperCase() || 'FILE';
};
const GifIndicator = observer(() => (
	<div className={memeStyles.gifBadge} aria-hidden="true" data-flx="channel.mobile-memes-picker.gif-indicator.div">
		GIF
	</div>
));

interface MemesPickerState {
	searchTerm: string;
	selectedFilter: ContentType;
}

const initialState: MemesPickerState = {
	searchTerm: '',
	selectedFilter: 'all',
};

interface MobileMemesPickerProps {
	onClose?: () => void;
}

export const MobileMemesPicker = observer(({onClose}: MobileMemesPickerProps = {}) => {
	const {i18n} = useLingui();
	const headerPortalContext = useExpressionPickerHeaderPortal();
	const hasPortal = Boolean(headerPortalContext?.headerPortalElement);
	const FILTER_OPTIONS: Array<FilterOption> = [
		{type: 'all', label: i18n._(ALL_DESCRIPTOR)},
		{
			type: 'image',
			label: i18n._(IMAGES_DESCRIPTOR),
			icon: <ImageIcon className={memeStyles.filterPillIcon} data-flx="channel.mobile-memes-picker.image-icon" />,
		},
		{
			type: 'video',
			label: i18n._(VIDEOS_DESCRIPTOR),
			icon: (
				<VideoCameraIcon
					className={memeStyles.filterPillIcon}
					data-flx="channel.mobile-memes-picker.video-camera-icon"
				/>
			),
		},
		{
			type: 'audio',
			label: i18n._(AUDIO_DESCRIPTOR),
			icon: (
				<MusicNoteIcon className={memeStyles.filterPillIcon} data-flx="channel.mobile-memes-picker.music-note-icon" />
			),
		},
		{
			type: 'gif',
			label: i18n._(GIFS_DESCRIPTOR),
			icon: <GifIcon className={memeStyles.filterPillIcon} data-flx="channel.mobile-memes-picker.gif-icon" />,
		},
	];
	const [state, setState] = useState<MemesPickerState>(initialState);
	const favoriteMemes = FavoriteMemes.memes;
	const fetched = FavoriteMemes.fetched;
	const storeLoading = !fetched;
	const scrollerRef = useRef<ScrollerHandle>(null);
	const searchInputRef = useRef<HTMLInputElement>(null);
	const [selectedMeme, setSelectedMeme] = useState<FavoriteMeme | null>(null);
	const [isBottomSheetOpen, setIsBottomSheetOpen] = useState(false);
	useSearchInputAutofocus(searchInputRef);
	const getMaxWidth = useCallback(() => {
		return window.innerWidth <= 768 ? Math.floor((window.innerWidth - 32) / 2) : 227;
	}, []);
	const getColumnWidth = useCallback(() => {
		return getMaxWidth();
	}, [getMaxWidth]);
	useEffect(() => {
		scrollerRef.current?.scrollTo({to: 0, animate: false});
	}, []);
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
			memes = matchSorter(memes, state.searchTerm, {
				keys: ['name', 'altText', 'filename', 'tags'],
				threshold: matchSorter.rankings.CONTAINS,
			});
		}
		return memes;
	}, [favoriteMemes, state.selectedFilter, state.searchTerm]);
	const renderHeader = () => {
		const headerContent = (
			<div
				className={hasPortal ? memeStyles.mobileHeaderContainer : memeStyles.mobileHeaderContainerStandalone}
				data-flx="channel.mobile-memes-picker.render-header.div"
			>
				<PickerSearchInput
					value={state.searchTerm}
					onChange={(value) => setState({...state, searchTerm: value})}
					placeholder={i18n._(SEARCH_SAVED_MEDIA_DESCRIPTOR)}
					inputRef={searchInputRef}
					data-flx="channel.mobile-memes-picker.render-header.picker-search-input.set-state"
				/>
				<div className={memeStyles.filterList} data-flx="channel.mobile-memes-picker.render-header.div--2">
					{FILTER_OPTIONS.map((option) => {
						const isActive = state.selectedFilter === option.type;
						return (
							<button
								key={option.type}
								type="button"
								onClick={() => setState({...state, selectedFilter: option.type})}
								aria-pressed={isActive}
								className={clsx(memeStyles.filterPill, isActive && memeStyles.filterPillActive)}
								data-flx="channel.mobile-memes-picker.render-header.button.set-state"
							>
								{option.icon}
								{option.label}
							</button>
						);
					})}
				</div>
			</div>
		);
		if (hasPortal) {
			return (
				<ExpressionPickerHeaderPortal data-flx="channel.mobile-memes-picker.render-header.expression-picker-header-portal">
					{headerContent}
				</ExpressionPickerHeaderPortal>
			);
		}
		return headerContent;
	};
	const renderContent = () => {
		if (storeLoading) {
			return <SkeletonView data-flx="channel.mobile-memes-picker.render-content.skeleton-view" />;
		}
		const columnWidth = getColumnWidth();
		const formattedMemes = filteredMemes.map((meme) => {
			const aspectRatio = (meme.height ?? 1) / (meme.width ?? 1);
			const newWidth = columnWidth;
			const newHeight = Math.round(columnWidth * aspectRatio);
			return {
				id: meme.id,
				title: meme.name,
				memeRecord: meme,
				onClick: (event?: React.MouseEvent) => {
					const shiftKey = event?.shiftKey ?? false;
					ComponentBus.dispatch('FAVORITE_MEME_SELECT', {meme, autoSend: !shiftKey});
					if (!shiftKey) {
						onClose?.();
					}
				},
				onLongPress: () => {
					setSelectedMeme(meme);
					setIsBottomSheetOpen(true);
				},
				url: meme.url,
				width: newWidth,
				height: newHeight,
				naturalWidth: meme.width ?? 1,
				naturalHeight: meme.height ?? 1,
				contentType: meme.contentType,
				duration: meme.duration,
				filename: meme.filename,
				isGifv: meme.isGifv,
				contentHash: meme.contentHash,
			};
		});
		return (
			<>
				<MemeGridRenderer
					memes={formattedMemes}
					data-flx="channel.mobile-memes-picker.render-content.meme-grid-renderer"
				/>
				<MemeActionBottomSheet
					isOpen={isBottomSheetOpen}
					onClose={() => setIsBottomSheetOpen(false)}
					meme={selectedMeme}
					data-flx="channel.mobile-memes-picker.render-content.meme-action-bottom-sheet"
				/>
			</>
		);
	};
	if (favoriteMemes.length === 0 && !storeLoading) {
		return (
			<div className={memeStyles.fullHeightRelative} data-flx="channel.mobile-memes-picker.div">
				<div className={memeStyles.columnContainer} data-flx="channel.mobile-memes-picker.div--2">
					{renderHeader()}
					<div className={memeStyles.centeredContent} data-flx="channel.mobile-memes-picker.div--3">
						<PickerEmptyState
							icon={SmileySadIcon}
							title={i18n._(NO_SAVED_MEDIA_DESCRIPTOR)}
							description={i18n._(SAVE_SOME_MEDIA_FROM_MESSAGES_TO_GET_STARTED_DESCRIPTOR)}
							data-flx="channel.mobile-memes-picker.picker-empty-state"
						/>
					</div>
				</div>
			</div>
		);
	}
	if (filteredMemes.length === 0 && !storeLoading) {
		return (
			<div className={memeStyles.fullHeightRelative} data-flx="channel.mobile-memes-picker.div--4">
				<div className={memeStyles.columnContainer} data-flx="channel.mobile-memes-picker.div--5">
					{renderHeader()}
					<div className={memeStyles.centeredContent} data-flx="channel.mobile-memes-picker.div--6">
						<PickerEmptyState
							icon={SmileySadIcon}
							title={i18n._(NO_RESULTS_DESCRIPTOR)}
							description={i18n._(TRY_A_DIFFERENT_SEARCH_TERM_OR_FILTER_DESCRIPTOR)}
							data-flx="channel.mobile-memes-picker.picker-empty-state--2"
						/>
					</div>
				</div>
			</div>
		);
	}
	return (
		<div className={memeStyles.fullHeightRelative} data-flx="channel.mobile-memes-picker.div--7">
			<div className={memeStyles.columnContainerOverflow} data-flx="channel.mobile-memes-picker.div--8">
				{renderHeader()}
				<div className={memeStyles.bodyWrapper} data-flx="channel.mobile-memes-picker.div--9">
					<Scroller
						ref={scrollerRef}
						className={memeStyles.scrollerFull}
						key="mobile-memes-picker-scroller"
						data-flx="channel.mobile-memes-picker.scroller"
					>
						<AnimatePresence data-flx="channel.mobile-memes-picker.animate-presence">{renderContent()}</AnimatePresence>
					</Scroller>
				</div>
			</div>
		</div>
	);
});

interface GridItemProps {
	id: string;
	title: string;
	memeRecord: FavoriteMeme;
	onClick: (event?: React.MouseEvent) => void;
	onLongPress: () => void;
	url: string;
	width: number;
	height: number;
	naturalWidth: number;
	naturalHeight: number;
	contentType: string;
	duration?: number | null;
	filename: string;
	isGifv: boolean;
	contentHash: string | null;
}

const MemeGridRenderer = observer(({memes}: {memes: Array<GridItemProps>}) => {
	const [firstColumnMemes, setFirstColumnMemes] = useState<Array<GridItemProps>>([]);
	const [secondColumnMemes, setSecondColumnMemes] = useState<Array<GridItemProps>>([]);
	useEffect(() => {
		let firstColumnHeight = 0;
		let secondColumnHeight = 0;
		const firstColumn: Array<GridItemProps> = [];
		const secondColumn: Array<GridItemProps> = [];
		for (const meme of memes) {
			if (firstColumnHeight <= secondColumnHeight) {
				firstColumn.push(meme);
				firstColumnHeight += meme.height;
			} else {
				secondColumn.push(meme);
				secondColumnHeight += meme.height;
			}
		}
		setFirstColumnMemes(firstColumn);
		setSecondColumnMemes(secondColumn);
	}, [memes]);
	return (
		<div className={styles.grid} data-flx="channel.mobile-memes-picker.meme-grid-renderer.grid">
			<div className={styles.column} data-flx="channel.mobile-memes-picker.meme-grid-renderer.column">
				{firstColumnMemes.map((meme) => (
					<GridItem key={meme.id} data-flx="channel.mobile-memes-picker.meme-grid-renderer.grid-item" {...meme} />
				))}
			</div>
			<div className={styles.column} data-flx="channel.mobile-memes-picker.meme-grid-renderer.column--2">
				{secondColumnMemes.map((meme) => (
					<GridItem key={meme.id} data-flx="channel.mobile-memes-picker.meme-grid-renderer.grid-item--2" {...meme} />
				))}
			</div>
		</div>
	);
});
const GridItem = observer(
	({title, onClick, onLongPress, url, width, height, contentType, duration, filename, isGifv}: GridItemProps) => {
		const {i18n} = useLingui();
		const {ref: mediaRef, isNearViewport: isVisible} = useNearViewport<HTMLDivElement>({
			rememberKey: url,
			rootMargin: GRID_ITEM_PRELOAD_ROOT_MARGIN,
		});
		const videoRef = useRef<HTMLVideoElement>(null);
		const accessibleName = title || filename;
		const isVideo = contentType.startsWith('video/');
		const isAudio = contentType.startsWith('audio/');
		const isGifImage = !isVideo && contentType.toLowerCase().includes('gif');
		const shouldAnimateGif = useShouldAnimate({kind: 'gif', isAnimated: !isAudio && (isVideo || isGifImage)});
		const thumbnailSrc = isGifImage && !shouldAnimateGif ? buildStaticGifPreviewURL(url) : url;
		const videoPlaybackAllowed = useAnimatedMediaVideoPlayback(videoRef, {
			enabled: isVisible && !isAudio && isVideo,
			shouldPlay: shouldAnimateGif,
		});
		const handleClick = () => {
			onClick();
		};
		const handleKeyDown = (event: React.KeyboardEvent) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			handleClick();
		};
		return (
			<LongPressable
				onLongPress={onLongPress}
				onClick={handleClick}
				className={clsx(styles.gridItem, styles.gridItemGif)}
				style={{width, height}}
				role="button"
				tabIndex={0}
				aria-label={i18n._(SELECT_DESCRIPTOR, {accessibleName})}
				onKeyDown={handleKeyDown}
				data-flx="channel.mobile-memes-picker.grid-item.grid-item.click"
			>
				<div className={memeStyles.fullSize} data-flx="channel.mobile-memes-picker.grid-item.div">
					{isGifv && <GifIndicator data-flx="channel.mobile-memes-picker.grid-item.gif-indicator" />}
					<div
						ref={mediaRef}
						className={styles.gifMediaContainer}
						data-flx="channel.mobile-memes-picker.grid-item.gif-media-container"
					>
						{isVisible && !isAudio && isVideo && (
							<video
								ref={videoRef}
								className={styles.gif}
								autoPlay={shouldAnimateGif && videoPlaybackAllowed}
								loop={true}
								muted={true}
								playsInline={true}
								disablePictureInPicture={true}
								disableRemotePlayback={true}
								preload={shouldAnimateGif ? 'auto' : 'metadata'}
								src={url}
								data-flx="channel.mobile-memes-picker.grid-item.gif"
							/>
						)}
						{isVisible && !isAudio && !isVideo && (
							<img
								className={styles.gif}
								src={thumbnailSrc}
								alt={title}
								data-flx="channel.mobile-memes-picker.grid-item.gif--2"
							/>
						)}
						{isAudio && (
							<div className={memeStyles.audioCard} data-flx="channel.mobile-memes-picker.grid-item.div--2">
								<MusicNoteIcon
									className={memeStyles.audioIcon}
									data-flx="channel.mobile-memes-picker.grid-item.music-note-icon"
								/>
								<div className={memeStyles.audioMeta} data-flx="channel.mobile-memes-picker.grid-item.div--3">
									{duration && (
										<div className={memeStyles.audioDuration} data-flx="channel.mobile-memes-picker.grid-item.div--4">
											{formatDuration(duration)}
										</div>
									)}
									<Tooltip text={filename} data-flx="channel.mobile-memes-picker.grid-item.tooltip">
										<div className={memeStyles.audioFilename} data-flx="channel.mobile-memes-picker.grid-item.div--5">
											{filename}
										</div>
									</Tooltip>
									<div className={memeStyles.audioBadge} data-flx="channel.mobile-memes-picker.grid-item.div--6">
										{getFileExtension(filename, contentType)}
									</div>
								</div>
							</div>
						)}
					</div>
				</div>
			</LongPressable>
		);
	},
);
const SkeletonView = observer(() => {
	const generateSkeletonItems = useMemo(() => {
		const getMaxWidth = () => {
			return window.innerWidth <= 768 ? Math.floor((window.innerWidth - 32) / 2) : 227;
		};
		const maxWidth = getMaxWidth();
		const minHeight = 100;
		const maxHeight = 300;
		const itemCount = 12 + Math.floor(Math.random() * 5);
		const items = [];
		for (let i = 0; i < itemCount; i++) {
			const aspectRatios = [0.75, 1, 1.33, 1.5, 1.78, 0.56];
			const ratio = aspectRatios[Math.floor(Math.random() * aspectRatios.length)];
			let height = maxWidth / ratio;
			height = Math.min(maxHeight, Math.max(minHeight, height));
			height = height * (0.8 + Math.random() * 0.4);
			items.push({
				id: `skeleton-${i}`,
				width: maxWidth,
				height: Math.floor(height),
			});
		}
		return items;
	}, []);
	const [firstColumnItems, secondColumnItems] = useMemo(() => {
		let firstColumnHeight = 0;
		let secondColumnHeight = 0;
		const firstColumn = [];
		const secondColumn = [];
		for (const item of generateSkeletonItems) {
			if (firstColumnHeight <= secondColumnHeight) {
				firstColumn.push(item);
				firstColumnHeight += item.height;
			} else {
				secondColumn.push(item);
				secondColumnHeight += item.height;
			}
		}
		return [firstColumn, secondColumn];
	}, [generateSkeletonItems]);
	return (
		<div className={styles.grid} data-flx="channel.mobile-memes-picker.skeleton-view.grid">
			<div className={styles.column} data-flx="channel.mobile-memes-picker.skeleton-view.column">
				{firstColumnItems.map((item) => (
					<div
						key={item.id}
						className={styles.skeletonItem}
						style={{
							width: item.width,
							height: item.height,
							animationDelay: `${Math.random() * 0.5}s`,
						}}
						data-flx="channel.mobile-memes-picker.skeleton-view.skeleton-item"
					/>
				))}
			</div>
			<div className={styles.column} data-flx="channel.mobile-memes-picker.skeleton-view.column--2">
				{secondColumnItems.map((item) => (
					<div
						key={item.id}
						className={styles.skeletonItem}
						style={{
							width: item.width,
							height: item.height,
							animationDelay: `${Math.random() * 0.5}s`,
						}}
						data-flx="channel.mobile-memes-picker.skeleton-view.skeleton-item--2"
					/>
				))}
			</div>
		</div>
	);
});

interface MemeActionBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	meme: FavoriteMeme | null;
}

const MemeActionBottomSheet: React.FC<MemeActionBottomSheetProps> = observer(({isOpen, onClose, meme}) => {
	const {i18n} = useLingui();
	const handleEdit = useCallback(() => {
		if (!meme) return;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<EditFavoriteMemeModal
					meme={meme}
					data-flx="channel.mobile-memes-picker.handle-edit.edit-favorite-meme-modal"
				/>
			)),
		);
	}, [meme, onClose]);
	const handleDelete = useCallback(() => {
		if (!meme) return;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_SAVED_MEDIA_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {memeName: meme.name})}
					primaryText={i18n._(DELETE_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={() => {
						FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
					}}
					data-flx="channel.mobile-memes-picker.handle-delete.confirm-modal"
				/>
			)),
		);
	}, [meme, onClose, i18n]);
	const actionGroups = useMemo(() => {
		if (!meme) return [];
		const actions: Array<MenuItemType> = [
			{
				icon: <EditIcon size={20} data-flx="channel.mobile-memes-picker.action-groups.edit-icon" />,
				label: i18n._(EDIT_SAVED_MEDIA_DESCRIPTOR),
				onClick: handleEdit,
			},
			{
				icon: <DeleteIcon size={20} data-flx="channel.mobile-memes-picker.action-groups.delete-icon" />,
				label: i18n._(DELETE_SAVED_MEDIA_DESCRIPTOR),
				onClick: handleDelete,
				danger: true,
			},
		];
		return [{items: actions}];
	}, [meme, handleEdit, handleDelete, i18n.locale]);
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			groups={actionGroups}
			data-flx="channel.mobile-memes-picker.meme-action-bottom-sheet.menu-bottom-sheet"
		/>
	);
});
