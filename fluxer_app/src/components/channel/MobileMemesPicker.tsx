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
import {GifIcon, ImageIcon, MusicNoteIcon, SmileySadIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import styles from '~/components/channel/GifPicker.module.css';
import {PickerEmptyState} from '~/components/channel/shared/PickerEmptyState';
import {PickerSearchInput} from '~/components/channel/shared/PickerSearchInput';
import {LongPressable} from '~/components/LongPressable';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EditFavoriteMemeModal} from '~/components/modals/EditFavoriteMemeModal';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '~/components/popouts/ExpressionPickerPopout';
import {DeleteIcon, EditIcon} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import {MenuBottomSheet, type MenuItemType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useSearchInputAutofocus} from '~/hooks/useSearchInputAutofocus';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import memeStyles from './MemesPicker.module.css';

type ContentType = 'all' | 'image' | 'video' | 'audio' | 'gif';

interface FilterOption {
	type: ContentType;
	label: string;
	icon?: React.ReactNode;
}

const formatDuration = (seconds: number | null | undefined): string => {
	if (!seconds || seconds <= 0) return '0:00';

	const hours = Math.floor(seconds / 3600);
	const minutes = Math.floor((seconds % 3600) / 60);
	const secs = Math.floor(seconds % 60);

	if (hours > 0) {
		return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
	}
	return `${minutes}:${secs.toString().padStart(2, '0')}`;
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
	<div className={memeStyles.gifBadge} aria-hidden="true">
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
	const {t} = useLingui();
	const headerPortalContext = useExpressionPickerHeaderPortal();
	const hasPortal = Boolean(headerPortalContext?.headerPortalElement);

	const FILTER_OPTIONS: Array<FilterOption> = [
		{type: 'all', label: t`All`},
		{type: 'image', label: t`Images`, icon: <ImageIcon className={memeStyles.filterPillIcon} />},
		{type: 'video', label: t`Videos`, icon: <VideoCameraIcon className={memeStyles.filterPillIcon} />},
		{type: 'audio', label: t`Audio`, icon: <MusicNoteIcon className={memeStyles.filterPillIcon} />},
		{type: 'gif', label: t`GIFs`, icon: <GifIcon className={memeStyles.filterPillIcon} />},
	];

	const [state, setState] = useState<MemesPickerState>(initialState);
	const favoriteMemes = FavoriteMemeStore.memes;
	const fetched = FavoriteMemeStore.fetched;
	const storeLoading = !fetched;
	const scrollerRef = useRef<ScrollerHandle>(null);
	const searchInputRef = useRef<HTMLInputElement>(null);
	const [selectedMeme, setSelectedMeme] = useState<FavoriteMemeRecord | null>(null);
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
			<div className={hasPortal ? memeStyles.mobileHeaderContainer : memeStyles.mobileHeaderContainerStandalone}>
				<PickerSearchInput
					value={state.searchTerm}
					onChange={(value) => setState({...state, searchTerm: value})}
					placeholder={t`Search saved media`}
					inputRef={searchInputRef}
				/>
				<div className={memeStyles.filterList}>
					{FILTER_OPTIONS.map((option) => {
						const isActive = state.selectedFilter === option.type;
						return (
							<button
								key={option.type}
								type="button"
								onClick={() => setState({...state, selectedFilter: option.type})}
								className={clsx(memeStyles.filterPill, isActive && memeStyles.filterPillActive)}
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
			return <ExpressionPickerHeaderPortal>{headerContent}</ExpressionPickerHeaderPortal>;
		}
		return headerContent;
	};

	const renderContent = () => {
		if (storeLoading) {
			return <SkeletonView />;
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
					ComponentDispatch.dispatch('FAVORITE_MEME_SELECT', {meme, autoSend: !shiftKey});
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
				<MemeGridRenderer memes={formattedMemes} />
				<MemeActionBottomSheet
					isOpen={isBottomSheetOpen}
					onClose={() => setIsBottomSheetOpen(false)}
					meme={selectedMeme}
				/>
			</>
		);
	};

	if (favoriteMemes.length === 0 && !storeLoading) {
		return (
			<div className={memeStyles.fullHeightRelative}>
				<div className={memeStyles.columnContainer}>
					{renderHeader()}
					<div className={memeStyles.centeredContent}>
						<PickerEmptyState
							icon={SmileySadIcon}
							title={t`No Saved Media`}
							description={t`Save some media from messages to get started!`}
						/>
					</div>
				</div>
			</div>
		);
	}

	if (filteredMemes.length === 0 && !storeLoading) {
		return (
			<div className={memeStyles.fullHeightRelative}>
				<div className={memeStyles.columnContainer}>
					{renderHeader()}
					<div className={memeStyles.centeredContent}>
						<PickerEmptyState
							icon={SmileySadIcon}
							title={t`No results`}
							description={t`Try a different search term or filter`}
						/>
					</div>
				</div>
			</div>
		);
	}

	return (
		<div className={memeStyles.fullHeightRelative}>
			<div className={memeStyles.columnContainerOverflow}>
				{renderHeader()}
				<div className={memeStyles.bodyWrapper}>
					<Scroller ref={scrollerRef} className={memeStyles.scrollerFull} key="mobile-memes-picker-scroller">
						<AnimatePresence>{renderContent()}</AnimatePresence>
					</Scroller>
				</div>
			</div>
		</div>
	);
});

interface GridItemProps {
	id: string;
	title: string;
	memeRecord: FavoriteMemeRecord;
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
		<div className={styles.grid}>
			<div className={styles.column}>
				{firstColumnMemes.map((meme) => (
					<GridItem key={meme.id} {...meme} />
				))}
			</div>
			<div className={styles.column}>
				{secondColumnMemes.map((meme) => (
					<GridItem key={meme.id} {...meme} />
				))}
			</div>
		</div>
	);
});

const GridItem = observer(
	({title, onClick, onLongPress, url, width, height, contentType, duration, filename, isGifv}: GridItemProps) => {
		const [isVisible, setIsVisible] = useState(false);
		const mediaRef = useRef<HTMLDivElement>(null);

		const isVideo = contentType.startsWith('video/') || contentType.includes('gif');
		const isAudio = contentType.startsWith('audio/');

		const handleClick = () => {
			onClick();
		};

		useEffect(() => {
			const mediaElement = mediaRef.current;
			if (!mediaElement) return;

			const observer = new IntersectionObserver(
				(entries) => {
					entries.forEach((entry) => {
						if (entry.isIntersecting) {
							setIsVisible(true);
						}
					});
				},
				{
					rootMargin: '800px 0px',
					threshold: 0,
				},
			);

			observer.observe(mediaElement);

			return () => {
				observer.disconnect();
			};
		}, []);

		return (
			<LongPressable
				onLongPress={onLongPress}
				onClick={handleClick}
				className={clsx(styles.gridItem, styles.gridItemGif)}
				style={{width, height}}
				role="button"
				tabIndex={0}
			>
				<motion.div className={memeStyles.fullSize} initial={{opacity: 0}} animate={{opacity: 1}} exit={{opacity: 0}}>
					{isGifv && <GifIndicator />}
					<div ref={mediaRef} className={styles.gifMediaContainer}>
						{isVisible && !isAudio && isVideo && (
							<video
								className={styles.gif}
								autoPlay={true}
								loop={true}
								muted={true}
								playsInline={true}
								disablePictureInPicture={true}
								disableRemotePlayback={true}
								preload="auto"
								src={url}
							/>
						)}

						{isVisible && !isAudio && !isVideo && <img className={styles.gif} src={url} alt={title} loading="lazy" />}

						{isAudio && (
							<div className={memeStyles.audioCard}>
								<MusicNoteIcon className={memeStyles.audioIcon} />
								<div className={memeStyles.audioMeta}>
									{duration && <div className={memeStyles.audioDuration}>{formatDuration(duration)}</div>}
									<Tooltip text={filename}>
										<div className={memeStyles.audioFilename}>{filename}</div>
									</Tooltip>
									<div className={memeStyles.audioBadge}>{getFileExtension(filename, contentType)}</div>
								</div>
							</div>
						)}
					</div>
				</motion.div>
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
		<div className={styles.grid}>
			<div className={styles.column}>
				{firstColumnItems.map((item) => (
					<div
						key={item.id}
						className={styles.skeletonItem}
						style={{
							width: item.width,
							height: item.height,
							animationDelay: `${Math.random() * 0.5}s`,
						}}
					/>
				))}
			</div>
			<div className={styles.column}>
				{secondColumnItems.map((item) => (
					<div
						key={item.id}
						className={styles.skeletonItem}
						style={{
							width: item.width,
							height: item.height,
							animationDelay: `${Math.random() * 0.5}s`,
						}}
					/>
				))}
			</div>
		</div>
	);
});

interface MemeActionBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	meme: FavoriteMemeRecord | null;
}

const MemeActionBottomSheet: React.FC<MemeActionBottomSheetProps> = observer(({isOpen, onClose, meme}) => {
	const {t, i18n} = useLingui();
	const handleEdit = useCallback(() => {
		if (!meme) return;
		onClose();
		ModalActionCreators.push(modal(() => <EditFavoriteMemeModal meme={meme} />));
	}, [meme, onClose]);

	const handleDelete = useCallback(() => {
		if (!meme) return;
		onClose();
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete Saved Media`}
					description={t`Are you sure you want to delete "${meme.name}"? This action cannot be undone.`}
					primaryText={t`Delete`}
					primaryVariant="danger-primary"
					onPrimary={() => {
						FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
					}}
				/>
			)),
		);
	}, [meme, onClose, i18n]);

	const actionGroups = useMemo(() => {
		if (!meme) return [];

		const actions: Array<MenuItemType> = [
			{
				icon: <EditIcon size={20} />,
				label: t`Edit Saved Media`,
				onClick: handleEdit,
			},
			{
				icon: <DeleteIcon size={20} />,
				label: t`Delete Saved Media`,
				onClick: handleDelete,
				danger: true,
			},
		];

		return [{items: actions}];
	}, [meme, handleEdit, handleDelete]);

	return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={actionGroups} />;
});
