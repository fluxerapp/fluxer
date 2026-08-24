// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import gifStyles from '@app/features/channel/components/GifPicker.module.css';
import {useGifVideoPool} from '@app/features/channel/components/GifVideoPool';
import styles from '@app/features/channel/components/MemesPicker.module.css';
import {formatDuration, getFileExtension} from '@app/features/channel/components/pickers/memes/MediaFormat';
import {PickerThumbnail} from '@app/features/channel/components/pickers/shared/PickerThumbnail';
import {usePooledVideo} from '@app/features/channel/components/pickers/shared/usePooledVideo';
import MemesPicker from '@app/features/emoji/state/MemesPicker';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import {EditFavoriteMemeModal} from '@app/features/expressions/components/modals/EditFavoriteMemeModal';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {buildMediaProxyURL, buildStaticGifPreviewURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MusicNoteIcon, PencilSimpleIcon, TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const DELETE_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Delete saved media',
	comment: 'Title of the destructive delete confirmation alert in the saved media (memes) picker.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: 'Delete "{memeName}"? Can\'t be undone.',
	comment:
		'Body of the destructive delete confirmation alert in the saved media picker. memeName is the user-chosen name. Keep quotation marks.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Confirm button label on the destructive delete-saved-media alert.',
});
const SEND_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Send saved media {memeName}',
	comment:
		'Accessible label for a saved media tile in the picker. Activating sends it. memeName is the user-chosen name.',
});
const EDIT_MEDIA_DESCRIPTOR = msg({
	message: 'Edit media',
	comment: 'Tooltip on the edit button on a saved media tile in the picker.',
});
const EDIT_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Edit saved media',
	comment: 'Accessible label for the edit button on a saved media tile in the picker.',
});
const DELETE_MEDIA_DESCRIPTOR = msg({
	message: 'Delete media',
	comment: 'Accessible label for the delete button on a saved media tile in the picker.',
});
const VIDEO_HOVER_PREVIEW_DELAY_MS = 140;
const GifIndicator = observer(() => (
	<div
		className={styles.gifBadge}
		aria-hidden="true"
		data-flx="channel.pickers.memes.meme-grid-item.gif-indicator.gif-badge"
	>
		GIF
	</div>
));
const VideoPoster = ({src, placeholder}: {src: string; placeholder: string | null}) => {
	return (
		<PickerThumbnail
			src={buildMediaProxyURL(src, {format: 'webp'})}
			alt=""
			className={gifStyles.gif}
			placeholder={placeholder}
			data-flx="channel.pickers.memes.meme-grid-item.video-poster"
		/>
	);
};
export const MemeGridItem = observer(
	({
		meme,
		coords,
		onClose,
		isFocused = false,
		itemKey,
	}: {
		meme: FavoriteMeme;
		coords: {
			position: 'absolute' | 'sticky';
			left?: number;
			right?: number;
			width: number;
			top?: number;
			height: number;
		};
		onClose?: () => void;
		isFocused?: boolean;
		itemKey?: string;
	}) => {
		const {i18n} = useLingui();
		const videoContainerRef = useRef<HTMLDivElement>(null);
		const hoverPreviewTimeoutRef = useRef<number | null>(null);
		const [isVideoPreviewActive, setIsVideoPreviewActive] = useState(false);
		const videoPool = useGifVideoPool();
		const isAudio = meme.contentType.startsWith('audio/');
		const isVideo = meme.contentType.startsWith('video/');
		const isGifImage = !isVideo && meme.contentType.toLowerCase().includes('gif');
		const shouldAnimateGif = useShouldAnimate({
			kind: 'gif',
			isAnimated: !isAudio && (isVideo || isGifImage),
			isHovering: isVideoPreviewActive,
		});
		const shouldRenderVideoPreview = !isAudio && isVideo && isVideoPreviewActive && shouldAnimateGif;
		const thumbnailSrc = isGifImage && !shouldAnimateGif ? buildStaticGifPreviewURL(meme.url) : meme.url;
		const videoPreviewStartTime = meme.duration && meme.duration > 0 ? meme.duration / 2 : null;
		usePooledVideo({
			src: shouldRenderVideoPreview ? meme.url : null,
			containerRef: videoContainerRef,
			videoPool,
			autoPlay: shouldRenderVideoPreview,
			enabled: shouldRenderVideoPreview,
			preload: 'auto',
			playbackStartTime: videoPreviewStartTime ?? 0,
		});
		const clearHoverPreviewTimeout = useCallback(() => {
			if (hoverPreviewTimeoutRef.current === null) return;
			window.clearTimeout(hoverPreviewTimeoutRef.current);
			hoverPreviewTimeoutRef.current = null;
		}, []);
		const onPointerEnter = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (event.pointerType !== 'mouse' || isAudio || !isVideo) return;
				clearHoverPreviewTimeout();
				hoverPreviewTimeoutRef.current = window.setTimeout(() => {
					hoverPreviewTimeoutRef.current = null;
					setIsVideoPreviewActive(true);
				}, VIDEO_HOVER_PREVIEW_DELAY_MS);
			},
			[clearHoverPreviewTimeout, isAudio, isVideo],
		);
		const onPointerLeave = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (event.pointerType !== 'mouse') return;
				clearHoverPreviewTimeout();
				setIsVideoPreviewActive(false);
			},
			[clearHoverPreviewTimeout],
		);
		const onPointerCancel = useCallback(() => {
			clearHoverPreviewTimeout();
			setIsVideoPreviewActive(false);
		}, [clearHoverPreviewTimeout]);
		useEffect(() => {
			return () => {
				clearHoverPreviewTimeout();
			};
		}, [clearHoverPreviewTimeout]);
		const handleClick = (event: React.MouseEvent<HTMLDivElement> | React.KeyboardEvent<HTMLDivElement>) => {
			const shiftKey = 'shiftKey' in event ? event.shiftKey : false;
			MemesPicker.trackMemeUsage(meme.id);
			ComponentBus.dispatch('FAVORITE_MEME_SELECT', {meme, autoSend: !shiftKey});
			if (!shiftKey) onClose?.();
		};
		const handleEdit = (event: React.MouseEvent) => {
			event.stopPropagation();
			ModalCommands.push(
				modal(() => (
					<EditFavoriteMemeModal
						meme={meme}
						data-flx="channel.pickers.memes.meme-grid-item.handle-edit.edit-favorite-meme-modal"
					/>
				)),
			);
		};
		const handleDelete = (event: React.MouseEvent) => {
			event.stopPropagation();
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_SAVED_MEDIA_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {memeName: meme.name})}
						primaryText={i18n._(DELETE_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={() => {
							FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
						}}
						data-flx="channel.pickers.memes.meme-grid-item.handle-delete.confirm-modal"
					/>
				)),
			);
		};
		return (
			<div
				role="button"
				tabIndex={0}
				className={clsx(gifStyles.gridItem, gifStyles.gridItemGif, isFocused && gifStyles.gridItemFocused)}
				onClick={handleClick}
				onKeyDown={(event) => {
					if (!isKeyboardActivationKey(event.key)) return;
					event.preventDefault();
					handleClick(event);
				}}
				onPointerEnter={onPointerEnter}
				onPointerLeave={onPointerLeave}
				onPointerCancel={onPointerCancel}
				style={coords}
				data-grid-item={itemKey}
				aria-label={i18n._(SEND_SAVED_MEDIA_DESCRIPTOR, {memeName: meme.name})}
				data-flx="channel.pickers.memes.meme-grid-item.button.click"
			>
				{meme.isGifv && <GifIndicator data-flx="channel.pickers.memes.meme-grid-item.gif-indicator" />}
				<div className={gifStyles.gifMediaContainer} data-flx="channel.pickers.memes.meme-grid-item.div">
					{!isAudio && isVideo && (
						<div
							ref={videoContainerRef}
							className={gifStyles.gifVideoContainer}
							data-flx="channel.pickers.memes.meme-grid-item.div--2"
						>
							<VideoPoster
								src={meme.url}
								placeholder={meme.placeholder}
								data-flx="channel.pickers.memes.meme-grid-item.video-poster"
							/>
						</div>
					)}
					{!isAudio && !isVideo && (
						<PickerThumbnail
							src={thumbnailSrc}
							alt={meme.name}
							className={gifStyles.gif}
							placeholder={meme.placeholder}
							data-flx="channel.pickers.memes.meme-grid-item.picker-thumbnail"
						/>
					)}
					{isAudio && (
						<div className={styles.audioCard} data-flx="channel.pickers.memes.meme-grid-item.audio-card">
							<MusicNoteIcon className={styles.audioIcon} data-flx="channel.pickers.memes.meme-grid-item.audio-icon" />
							<div className={styles.audioMeta} data-flx="channel.pickers.memes.meme-grid-item.audio-meta">
								{meme.duration && (
									<div className={styles.audioDuration} data-flx="channel.pickers.memes.meme-grid-item.audio-duration">
										{formatDuration(meme.duration)}
									</div>
								)}
								<Tooltip text={meme.filename} data-flx="channel.pickers.memes.meme-grid-item.tooltip">
									<div className={styles.audioFilename} data-flx="channel.pickers.memes.meme-grid-item.audio-filename">
										{meme.filename}
									</div>
								</Tooltip>
								<div className={styles.audioBadge} data-flx="channel.pickers.memes.meme-grid-item.audio-badge">
									{getFileExtension(meme.filename, meme.contentType)}
								</div>
							</div>
						</div>
					)}
				</div>
				<div className={gifStyles.gridItemBackdrop} data-flx="channel.pickers.memes.meme-grid-item.div--3" />
				<div className={gifStyles.hoverActionButtons} data-flx="channel.pickers.memes.meme-grid-item.div--4">
					<Tooltip
						text={i18n._(EDIT_MEDIA_DESCRIPTOR)}
						position="bottom"
						data-flx="channel.pickers.memes.meme-grid-item.tooltip--2"
					>
						<FocusRing offset={-2} data-flx="channel.pickers.memes.meme-grid-item.focus-ring">
							<button
								type="button"
								onClick={handleEdit}
								className={gifStyles.favoriteButton}
								aria-label={i18n._(EDIT_SAVED_MEDIA_DESCRIPTOR)}
								data-flx="channel.pickers.memes.meme-grid-item.button.edit"
							>
								<PencilSimpleIcon
									className={gifStyles.favoriteButtonIcon}
									weight="fill"
									data-flx="channel.pickers.memes.meme-grid-item.pencil-simple-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip
						text={i18n._(DELETE_MEDIA_DESCRIPTOR)}
						position="bottom"
						data-flx="channel.pickers.memes.meme-grid-item.tooltip--3"
					>
						<FocusRing offset={-2} data-flx="channel.pickers.memes.meme-grid-item.focus-ring--2">
							<button
								type="button"
								onClick={handleDelete}
								className={clsx(gifStyles.favoriteButton, gifStyles.favoriteButtonDanger)}
								aria-label={i18n._(DELETE_SAVED_MEDIA_DESCRIPTOR)}
								data-flx="channel.pickers.memes.meme-grid-item.button.delete"
							>
								<TrashIcon
									className={gifStyles.favoriteButtonIcon}
									weight="fill"
									data-flx="channel.pickers.memes.meme-grid-item.trash-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
		);
	},
);
