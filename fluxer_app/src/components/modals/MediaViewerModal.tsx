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
import {observer} from 'mobx-react-lite';
import {type FC, type MouseEvent, useCallback, useEffect, useMemo, useRef} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {deriveDefaultNameFromMessage} from '~/components/channel/embeds/EmbedUtils';
import {AudioPlayer} from '~/components/media-player/components/AudioPlayer';
import {VideoPlayer} from '~/components/media-player/components/VideoPlayer';
import {AddFavoriteMemeModal} from '~/components/modals/AddFavoriteMemeModal';
import {MediaModal} from '~/components/modals/MediaModal';
import styles from '~/components/modals/MediaViewerModal.module.css';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import {Platform} from '~/lib/Platform';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import MediaViewerStore from '~/stores/MediaViewerStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {formatAttachmentDate} from '~/utils/AttachmentExpiryUtils';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';
import {openExternalUrl} from '~/utils/NativeUtils';

const MediaViewerModalComponent: FC = observer(() => {
	const {t, i18n} = useLingui();
	const {isOpen, items, currentIndex, channelId, messageId, message} = MediaViewerStore;
	const {enabled: isMobile} = MobileLayoutStore;
	const videoRef = useRef<HTMLVideoElement>(null);

	const currentItem = items[currentIndex];

	useEffect(() => {
		if (currentItem?.type !== 'gifv') return;
		const video = videoRef.current;
		if (!video) return;

		video.autoplay = true;
		video.loop = true;
		video.muted = true;
		video.playsInline = true;
		void video.play().catch(() => {});
	}, [currentItem?.src, currentItem?.type, currentIndex]);

	const memes = FavoriteMemeStore.memes;
	const isFavorited = currentItem?.contentHash
		? memes.some((meme) => meme.contentHash === currentItem.contentHash)
		: false;

	const defaultName = deriveDefaultNameFromMessage({
		message,
		attachmentId: currentItem?.attachmentId,
		url: currentItem?.originalSrc,
		proxyUrl: currentItem?.src,
		i18nInstance: i18n,
	});

	const handleFavoriteClick = useCallback(async () => {
		if (!channelId || !messageId || !currentItem) return;

		if (isFavorited && currentItem.contentHash) {
			const meme = memes.find((m) => m.contentHash === currentItem.contentHash);
			if (!meme) return;
			await FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
		} else {
			ModalActionCreators.push(
				modal(() => (
					<AddFavoriteMemeModal
						channelId={channelId}
						messageId={messageId}
						attachmentId={currentItem.attachmentId}
						embedIndex={currentItem.embedIndex}
						defaultName={defaultName}
					/>
				)),
			);
		}
	}, [channelId, messageId, currentItem, defaultName, isFavorited, memes]);

	const handleSave = useCallback(() => {
		if (!currentItem) return;
		const mediaType = currentItem.type === 'audio' ? 'audio' : currentItem.type === 'video' ? 'video' : 'image';
		createSaveHandler(currentItem.originalSrc, mediaType)();
	}, [currentItem]);

	const handleOpenInBrowser = useCallback(() => {
		if (!currentItem) return;
		void openExternalUrl(currentItem.originalSrc);
	}, [currentItem]);

	const handleDelete = useCallback(
		(bypassConfirm?: boolean) => {
			if (!message) return;

			if (bypassConfirm) {
				MessageActionCreators.remove(message.channelId, message.id);
				return;
			}

			MessageActionCreators.showDeleteConfirmation(i18n, {message});
		},
		[i18n, message],
	);

	const handlePrevious = useCallback(() => {
		if (currentIndex > 0) {
			MediaViewerActionCreators.navigateMediaViewer(currentIndex - 1);
		}
	}, [currentIndex]);

	const handleNext = useCallback(() => {
		if (currentIndex < items.length - 1) {
			MediaViewerActionCreators.navigateMediaViewer(currentIndex + 1);
		}
	}, [currentIndex, items.length]);

	const handleThumbnailSelect = useCallback(
		(index: number) => {
			if (index === currentIndex) return;
			MediaViewerActionCreators.navigateMediaViewer(index);
		},
		[currentIndex],
	);

	const handleContextMenu = useCallback(
		(event: MouseEvent<HTMLDivElement>) => {
			if (!currentItem || !message) return;

			const renderMenu = ({onClose}: {onClose: () => void}) => (
				<MediaContextMenu
					message={message}
					originalSrc={currentItem.originalSrc}
					proxyURL={currentItem.src}
					type={currentItem.type}
					contentHash={currentItem.contentHash}
					attachmentId={currentItem.attachmentId}
					embedIndex={currentItem.embedIndex}
					defaultName={defaultName}
					onClose={onClose}
					onDelete={handleDelete}
				/>
			);

			event.stopPropagation?.();

			if (Platform.isElectron) {
				event.preventDefault();
				ContextMenuActionCreators.openFromEvent(event, renderMenu);
				return;
			}

			ContextMenuActionCreators.openAtPoint(
				{
					x: event.pageX + 2,
					y: event.pageY + 2,
				},
				renderMenu,
			);
		},
		[currentItem, defaultName, handleDelete, message],
	);

	const isBlob = currentItem?.src.startsWith('blob:');

	const imageSrc = useMemo(() => {
		if (!currentItem) return '';
		if (isBlob) {
			return currentItem.src;
		}

		if (currentItem.type === 'gif') {
			return buildMediaProxyURL(currentItem.src, {
				animated: true,
			});
		}

		if (currentItem.type === 'gifv' || currentItem.type === 'video' || currentItem.type === 'audio') {
			return currentItem.src;
		}

		const maxPreviewSize = 1920;
		const aspectRatio = currentItem.naturalWidth / currentItem.naturalHeight;

		let targetWidth = currentItem.naturalWidth;
		let targetHeight = currentItem.naturalHeight;

		if (currentItem.naturalWidth > maxPreviewSize || currentItem.naturalHeight > maxPreviewSize) {
			if (aspectRatio > 1) {
				targetWidth = Math.min(currentItem.naturalWidth, maxPreviewSize);
				targetHeight = Math.round(targetWidth / aspectRatio);
			} else {
				targetHeight = Math.min(currentItem.naturalHeight, maxPreviewSize);
				targetWidth = Math.round(targetHeight * aspectRatio);
			}
		}

		return buildMediaProxyURL(currentItem.src, {
			format: 'webp',
			width: targetWidth,
			height: targetHeight,
		});
	}, [currentItem, isBlob]);

	const thumbnails = useMemo(
		() =>
			items.map((item, index) => {
				const name = item.filename || item.originalSrc.split('/').pop()?.split('?')[0] || t`Attachment ${index + 1}`;
				if ((item.type === 'image' || item.type === 'gif') && !item.src.startsWith('blob:')) {
					return {
						src: buildMediaProxyURL(item.src, {
							format: 'webp',
							width: 320,
							height: 320,
						}),
						alt: name,
						type: item.type,
					};
				}

				return {
					src: item.src,
					alt: name,
					type: item.type,
				};
			}),
		[items],
	);

	if (!isOpen || !currentItem) {
		return null;
	}

	const dimensions =
		currentItem.naturalWidth && currentItem.naturalHeight
			? `${currentItem.naturalWidth}×${currentItem.naturalHeight}`
			: undefined;
	const fileName = currentItem.filename || currentItem.originalSrc.split('/').pop()?.split('?')[0] || 'media';
	const expiryInfo =
		currentItem.expiresAt && currentItem.expiresAt.length > 0
			? {
					expiresAt: new Date(currentItem.expiresAt),
					isExpired: currentItem.expired ?? false,
					label: formatAttachmentDate(new Date(currentItem.expiresAt)),
				}
			: undefined;

	const getTitle = () => {
		switch (currentItem.type) {
			case 'image':
				return t`Image preview`;
			case 'gif':
				return t`GIF preview`;
			case 'gifv':
				return t`GIF preview`;
			case 'video':
				return t`Video preview`;
			case 'audio':
				return t`Audio preview`;
			default:
				return t`Media preview`;
		}
	};

	const modalTitle = getTitle();

	const renderMedia = () => {
		if (currentItem.type === 'gifv') {
			const isActualGif = currentItem.src.endsWith('.gif') || currentItem.originalSrc.endsWith('.gif');

			if (isActualGif) {
				return (
					<img
						src={imageSrc}
						alt={t`Animated GIF`}
						className={styles.gifvImage}
						style={{
							objectFit: 'contain',
						}}
						draggable={false}
					/>
				);
			}

			return (
				<video
					key={currentItem.src}
					ref={videoRef}
					src={currentItem.src}
					className={styles.gifvVideo}
					style={{
						objectFit: 'contain',
					}}
					autoPlay
					loop
					muted
					playsInline
					controls={false}
					aria-label={t`Animated video`}
				>
					<track kind="captions" />
				</video>
			);
		}

		if (currentItem.type === 'video') {
			return (
				<div className={styles.videoPlayerContainer}>
					<VideoPlayer
						src={currentItem.src}
						width={currentItem.naturalWidth}
						height={currentItem.naturalHeight}
						duration={currentItem.duration}
						autoPlay
						isMobile={isMobile}
						fillContainer={isMobile}
						className={styles.videoPlayer}
					/>
				</div>
			);
		}

		if (currentItem.type === 'audio') {
			return (
				<div className={styles.mediaContainer}>
					<div className={styles.audioPlayerContainer}>
						<AudioPlayer
							src={currentItem.src}
							title={fileName}
							duration={currentItem.duration}
							autoPlay
							isMobile={isMobile}
							className={styles.audioPlayer}
						/>
					</div>
				</div>
			);
		}

		return (
			<img
				src={imageSrc}
				alt={currentItem.type === 'gif' ? t`Animated GIF` : t`Image`}
				className={styles.image}
				style={{
					objectFit: 'contain',
				}}
				draggable={false}
			/>
		);
	};

	return (
		<MediaModal
			title={modalTitle}
			fileName={fileName}
			expiryInfo={
				expiryInfo
					? {
							expiresAt: expiryInfo.expiresAt,
							isExpired: expiryInfo.isExpired,
						}
					: undefined
			}
			dimensions={dimensions}
			isFavorited={
				channelId &&
				messageId &&
				(currentItem.type === 'image' || currentItem.type === 'gif' || currentItem.type === 'gifv')
					? isFavorited
					: undefined
			}
			onFavorite={
				channelId &&
				messageId &&
				(currentItem.type === 'image' || currentItem.type === 'gif' || currentItem.type === 'gifv')
					? handleFavoriteClick
					: undefined
			}
			onSave={currentItem.type !== 'gifv' ? handleSave : undefined}
			onOpenInBrowser={handleOpenInBrowser}
			enablePanZoom={currentItem.type === 'image' || currentItem.type === 'gif' || currentItem.type === 'gifv'}
			currentIndex={currentIndex}
			totalAttachments={items.length}
			onPrevious={currentIndex > 0 ? handlePrevious : undefined}
			onNext={currentIndex < items.length - 1 ? handleNext : undefined}
			thumbnails={thumbnails}
			onSelectThumbnail={handleThumbnailSelect}
		>
			<div
				className={styles.mediaContextMenuWrapper}
				onContextMenu={handleContextMenu}
				role="region"
				aria-label={modalTitle}
			>
				{renderMedia()}
			</div>
		</MediaModal>
	);
});

export const MediaViewerModal: FC = MediaViewerModalComponent;
