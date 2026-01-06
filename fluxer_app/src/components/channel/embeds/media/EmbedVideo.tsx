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
import {PlayIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import {useCallback, useEffect, useState} from 'react';
import {thumbHashToDataURL} from 'thumbhash';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import {deriveDefaultNameFromMessage} from '~/components/channel/embeds/EmbedUtils';
import {OverlayPlayButton} from '~/components/channel/embeds/media/MediaButtons';
import {getMediaButtonVisibility} from '~/components/channel/embeds/media/MediaButtonUtils';
import {MediaContainer} from '~/components/channel/embeds/media/MediaContainer';
import type {BaseMediaProps} from '~/components/channel/embeds/media/MediaTypes';
import {NSFWBlurOverlay} from '~/components/channel/embeds/NSFWBlurOverlay';
import {VideoPlayer} from '~/components/media-player/components/VideoPlayer';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import {useDeleteAttachment} from '~/hooks/useDeleteAttachment';
import {useMediaFavorite} from '~/hooks/useMediaFavorite';
import {useNSFWMedia} from '~/hooks/useNSFWMedia';
import type {MessageAttachment} from '~/records/MessageRecord';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {createCalculator} from '~/utils/DimensionUtils';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import * as ImageCacheUtils from '~/utils/ImageCacheUtils';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';
import styles from './EmbedVideo.module.css';

const VIDEO_CONFIG = {
	MAX_WIDTH: 400,
} as const;

const videoCalculator = createCalculator({
	maxWidth: VIDEO_CONFIG.MAX_WIDTH,
	responsive: true,
});

type EmbedVideoProps = BaseMediaProps & {
	src: string;
	width: number;
	height: number;
	placeholder?: string;
	title?: string;
	duration?: number;
	embedUrl?: string;
	fillContainer?: boolean;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
};

const MobileVideoOverlay: FC<{
	thumbHashURL?: string;
	posterSrc: string | null;
	posterLoaded: boolean;
	onTap: () => void;
	title?: string;
}> = observer(({thumbHashURL, posterSrc, posterLoaded, onTap, title}) => {
	const {t} = useLingui();
	return (
		<button type="button" className={styles.videoOverlay} onClick={onTap} aria-label={t`Play video`}>
			<AnimatePresence>
				{thumbHashURL && !posterLoaded && (
					<motion.img
						key="placeholder"
						initial={{opacity: 1}}
						exit={{opacity: 0}}
						transition={{duration: 0.2}}
						src={thumbHashURL}
						alt={title ? t`Thumbnail for ${title}` : t`Video thumbnail`}
						className={styles.thumbnailPlaceholder}
					/>
				)}
			</AnimatePresence>

			{posterSrc && posterLoaded && (
				<img
					src={posterSrc}
					alt={title ? t`Thumbnail for ${title}` : t`Video thumbnail`}
					className={styles.thumbnailPlaceholder}
				/>
			)}

			<div className={styles.playButtonWrapper}>
				<OverlayPlayButton onClick={onTap} icon={<PlayIcon size={28} aria-hidden="true" />} ariaLabel={t`Play video`} />
			</div>
		</button>
	);
});

const EmbedVideo: FC<EmbedVideoProps> = observer(
	({
		src,
		width,
		height,
		placeholder,
		title,
		duration,
		nsfw,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		embedUrl,
		message,
		contentHash,
		onDelete,
		fillContainer = false,
		mediaAttachments = [],
		isPreview,
	}) => {
		const {enabled: isMobile} = MobileLayoutStore;
		const effectiveSrc = buildMediaProxyURL(src);
		const isBlob = src.startsWith('blob:');
		const posterSrc = isBlob ? null : buildMediaProxyURL(src, {format: 'webp'});
		const [posterLoaded, setPosterLoaded] = useState(posterSrc ? ImageCacheUtils.hasImage(posterSrc) : false);

		const {shouldBlur, gateReason} = useNSFWMedia(nsfw, channelId);

		const defaultName =
			title || deriveDefaultNameFromMessage({message, attachmentId, embedIndex, url: embedUrl || src, proxyUrl: src});

		const {
			isFavorited,
			toggleFavorite: handleFavoriteClick,
			canFavorite,
		} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName,
			contentHash,
		});

		const handleDownloadClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createSaveHandler(src, 'video')();
			},
			[src],
		);

		const handleDeleteClick = useDeleteAttachment(message, attachmentId);

		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message || isPreview) return;

				e.preventDefault();
				e.stopPropagation();

				ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						originalSrc={src}
						type="video"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
					/>
				));
			},
			[message, src, contentHash, attachmentId, defaultName, onDelete, isPreview],
		);

		const thumbHashUrl = placeholder
			? thumbHashToDataURL(Uint8Array.from(atob(placeholder), (c) => c.charCodeAt(0)))
			: undefined;

		const {dimensions} = useCallback(() => {
			return videoCalculator.calculate({width, height}, {responsive: true});
		}, [width, height])();

		const aspectRatio = `${dimensions.width} / ${dimensions.height}`;

		useEffect(() => {
			if (!posterSrc) return;
			if (DeveloperOptionsStore.forceRenderPlaceholders || DeveloperOptionsStore.forceMediaLoading) {
				return;
			}

			ImageCacheUtils.loadImage(
				posterSrc,
				() => setPosterLoaded(true),
				() => setPosterLoaded(false),
			);
		}, [posterSrc]);

		const handleMobileTap = useCallback(() => {
			const currentIndex = mediaAttachments.findIndex((a) => a.id === attachmentId);

			const videoItems = mediaAttachments
				.filter((att) => att.content_type?.startsWith('video/'))
				.map((att) => ({
					src: buildMediaProxyURL(att.proxy_url ?? att.url ?? ''),
					originalSrc: att.url ?? '',
					naturalWidth: att.width || 0,
					naturalHeight: att.height || 0,
					type: 'video' as const,
					contentHash: att.content_hash,
					attachmentId: att.id,
					embedIndex: undefined,
					filename: att.filename,
					fileSize: att.size,
					duration: att.duration,
					expiresAt: att.expires_at ?? null,
					expired: att.expired ?? false,
				}));

			MediaViewerActionCreators.openMediaViewer(videoItems, currentIndex, {
				channelId,
				messageId,
				message,
			});
		}, [channelId, messageId, message, mediaAttachments, attachmentId]);

		const containerStyles: React.CSSProperties = isMobile
			? {
					aspectRatio,
					width: '100%',
					maxWidth: '100%',
				}
			: fillContainer
				? {
						width: '100%',
						height: '100%',
					}
				: {
						width: dimensions.width,
						maxWidth: '100%',
						aspectRatio,
					};

		if (shouldBlur) {
			return (
				<div className={styles.blurContainer}>
					<div className={styles.blurContent} style={containerStyles}>
						<div className={styles.blurInner}>
							{thumbHashUrl && (
								<img src={thumbHashUrl} alt="" className={styles.blurThumbnail} style={{filter: 'blur(40px)'}} />
							)}
						</div>
					</div>
					<NSFWBlurOverlay reason={gateReason} />
				</div>
			);
		}

		const {showFavoriteButton, showDownloadButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview},
		);

		if (isMobile) {
			return (
				<MediaContainer
					className={styles.mediaContainer}
					style={containerStyles}
					showFavoriteButton={showFavoriteButton}
					isFavorited={isFavorited}
					onFavoriteClick={handleFavoriteClick}
					showDownloadButton={showDownloadButton}
					onDownloadClick={handleDownloadClick}
					showDeleteButton={showDeleteButton}
					onDeleteClick={handleDeleteClick}
					onContextMenu={handleContextMenu}
					renderedWidth={dimensions.width}
					renderedHeight={dimensions.height}
				>
					<div className={styles.mobileContainer}>
						<MobileVideoOverlay
							thumbHashURL={thumbHashUrl}
							posterSrc={posterSrc}
							posterLoaded={posterLoaded}
							onTap={handleMobileTap}
							title={title}
						/>
					</div>
				</MediaContainer>
			);
		}

		return (
			<MediaContainer
				className={styles.mediaContainer}
				style={containerStyles}
				showFavoriteButton={showFavoriteButton}
				isFavorited={isFavorited}
				onFavoriteClick={handleFavoriteClick}
				showDownloadButton={showDownloadButton}
				onDownloadClick={handleDownloadClick}
				showDeleteButton={showDeleteButton}
				onDeleteClick={handleDeleteClick}
				onContextMenu={handleContextMenu}
				renderedWidth={dimensions.width}
				renderedHeight={dimensions.height}
			>
				<VideoPlayer
					src={effectiveSrc}
					poster={posterSrc || undefined}
					placeholder={placeholder}
					duration={duration}
					width={dimensions.width}
					height={dimensions.height}
					fillContainer={fillContainer}
					className={fillContainer ? styles.videoPlayerFill : styles.videoPlayerBlock}
				/>
			</MediaContainer>
		);
	},
);

export default EmbedVideo;
