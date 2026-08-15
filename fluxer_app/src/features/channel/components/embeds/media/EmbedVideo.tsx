// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {AltTextBadge} from '@app/features/channel/components/embeds/AltTextBadge';
import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import {MatureMediaBlurOverlay} from '@app/features/channel/components/embeds/MatureMediaBlurOverlay';
import styles from '@app/features/channel/components/embeds/media/EmbedVideo.module.css';
import {OverlayPlayButton} from '@app/features/channel/components/embeds/media/MediaButtons';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import {MediaContainer} from '@app/features/channel/components/embeds/media/MediaContainer';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import {
	getEffectiveVideoLayoutDimensions,
	hasDifferentAspectRatio,
	normalizeVideoDimensions,
	resolveVideoLayout,
} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {attachmentsToViewerItems, findViewerItemIndex} from '@app/features/messaging/utils/MediaViewerItemUtils';
import {decodeThumbHashDataURL} from '@app/features/messaging/utils/ThumbHashUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {
	VideoPlayer,
	type VideoPlayerMetadata,
} from '@app/features/voice/components/media_player/components/VideoPlayer';
import {useInAppMediaSoundCapture} from '@app/features/voice/hooks/useInAppMediaSoundCapture';
import VideoVolume from '@app/features/voice/state/VideoVolume';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlayIcon, SpeakerHighIcon, SpeakerXIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const OPEN_VIDEO_DESCRIPTOR = msg({
	message: 'Open video',
	comment: 'Button or menu action label in the channel and chat embed video. Keep it concise.',
});
const THUMBNAIL_FOR_DESCRIPTOR = msg({
	message: 'Thumbnail for {title}',
	comment:
		'Short label in the channel and chat embed video. Keep it concise. Preserve {title}; it is inserted by code.',
});
const VIDEO_THUMBNAIL_DESCRIPTOR = msg({
	message: 'Video thumbnail',
	comment: 'Short label in the channel and chat embed video. Keep it concise.',
});
const PLAY_VIDEO_DESCRIPTOR = msg({
	message: 'Play video',
	comment: 'Short label in the channel and chat embed video. Keep it concise.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	comment: 'Button or menu action label in the channel and chat embed video. Keep it concise.',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment: 'Button or menu action label in the channel and chat embed video. Keep it concise.',
});

type EmbedVideoProps = BaseMediaProps & {
	src: string;
	width: number;
	height: number;
	maxWidth?: number;
	maxHeight?: number;
	placeholder?: string;
	title?: string;
	alt?: string;
	duration?: number;
	embedUrl?: string;
	fillContainer?: boolean;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
	snapshotIndex?: number;
};

interface DecodedVideoDimensions {
	src: string;
	width: number;
	height: number;
}

interface PosterCacheAtMount {
	src: string | null;
	cached: boolean;
}

const MobileVideoOverlay: FC<{
	thumbHashURL?: string;
	posterSrc: string | null;
	posterLoaded: boolean;
	posterCachedOnMount: boolean;
	onTap: () => void;
	onPlayInline: () => void;
	title?: string;
	alt?: string;
	onPopoutToggle?: (open: boolean) => void;
}> = observer(
	({thumbHashURL, posterSrc, posterLoaded, posterCachedOnMount, onTap, onPlayInline, title, alt, onPopoutToggle}) => {
		const {i18n} = useLingui();
		const handleKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			onTap();
		};
		return (
			<div
				className={styles.videoOverlay}
				onClick={onTap}
				onKeyDown={handleKeyDown}
				role="button"
				tabIndex={0}
				aria-label={i18n._(OPEN_VIDEO_DESCRIPTOR)}
				data-flx="channel.embeds.media.embed-video.mobile-video-overlay.video-overlay.tap"
			>
				<AnimatePresence data-flx="channel.embeds.media.embed-video.mobile-video-overlay.animate-presence">
					{thumbHashURL && !posterLoaded && (
						<motion.img
							key="placeholder"
							initial={{opacity: 1}}
							exit={{opacity: 0}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
							src={thumbHashURL}
							alt={title ? i18n._(THUMBNAIL_FOR_DESCRIPTOR, {title}) : i18n._(VIDEO_THUMBNAIL_DESCRIPTOR)}
							className={styles.thumbHashPlaceholder}
							data-flx="channel.embeds.media.embed-video.mobile-video-overlay.thumbnail-placeholder"
						/>
					)}
				</AnimatePresence>
				{posterSrc && (
					<motion.img
						src={posterSrc}
						alt={title ? i18n._(THUMBNAIL_FOR_DESCRIPTOR, {title}) : i18n._(VIDEO_THUMBNAIL_DESCRIPTOR)}
						className={styles.thumbnailPlaceholder}
						initial={{opacity: posterCachedOnMount ? 1 : 0}}
						animate={{opacity: posterLoaded ? 1 : 0}}
						transition={{duration: posterCachedOnMount || Accessibility.useReducedMotion ? 0 : 0.2}}
						data-flx="channel.embeds.media.embed-video.mobile-video-overlay.thumbnail-placeholder--2"
					/>
				)}
				<div
					className={styles.playButtonWrapper}
					data-flx="channel.embeds.media.embed-video.mobile-video-overlay.play-button-wrapper"
				>
					<OverlayPlayButton
						onClick={(event) => {
							event.stopPropagation();
							onPlayInline();
						}}
						icon={
							<PlayIcon
								size={remFromPx(28)}
								aria-hidden="true"
								data-flx="channel.embeds.media.embed-video.mobile-video-overlay.play-icon"
							/>
						}
						ariaLabel={i18n._(PLAY_VIDEO_DESCRIPTOR)}
						data-flx="channel.embeds.media.embed-video.mobile-video-overlay.overlay-play-button.stop-propagation"
					/>
				</div>
				<AltTextBadge
					altText={alt}
					onPopoutToggle={onPopoutToggle}
					data-flx="channel.embeds.media.embed-video.mobile-video-overlay.alt-text-badge"
				/>
			</div>
		);
	},
);
const EmbedVideo: FC<EmbedVideoProps> = observer(
	({
		src,
		width,
		height,
		maxWidth,
		maxHeight,
		placeholder,
		title,
		alt,
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
		snapshotIndex,
	}) => {
		const {i18n} = useLingui();
		const {enabled: isMobile} = MobileLayout;
		const messageViewContext = useMaybeMessageViewContext();
		const effectiveSrc = buildMediaProxyURL(src);
		const isBlob = src.startsWith('blob:');
		const posterSrc = isBlob ? null : buildMediaProxyURL(src, {format: 'webp'});
		const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({
			rememberKey: posterSrc ?? effectiveSrc,
		});
		const [posterCacheAtMount] = useState<PosterCacheAtMount>(() => ({
			src: posterSrc,
			cached: posterSrc ? ImageCacheUtils.hasImage(posterSrc) : false,
		}));
		const posterCachedOnMount = posterCacheAtMount.src === posterSrc && posterCacheAtMount.cached;
		const [loadedPosterSrc, setLoadedPosterSrc] = useState<string | null>(() => {
			if (posterSrc && ImageCacheUtils.hasImage(posterSrc)) return posterSrc;
			return null;
		});
		const posterLoaded = posterSrc !== null && loadedPosterSrc === posterSrc;
		const [hasPlayed, setHasPlayed] = useState(false);
		const [isPlayingInline, setIsPlayingInline] = useState(false);
		const [decodedVideoDimensions, setDecodedVideoDimensions] = useState<DecodedVideoDimensions | null>(null);
		const [posterNaturalDimensions, setPosterNaturalDimensions] = useState<DecodedVideoDimensions | null>(null);
		const inlineVideoRef = useRef<HTMLVideoElement>(null);
		useInAppMediaSoundCapture(inlineVideoRef);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const shouldLoadMedia = isNearViewport && !shouldBlur;
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
				createDownloadHandler(src, 'video')();
			},
			[src],
		);
		const handleDeleteClick = useDeleteAttachment(message, attachmentId);
		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message) return;
				if (isPreview && snapshotIndex === undefined) return;
				e.preventDefault();
				e.stopPropagation();
				ContextMenuCommands.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						sourceChannel={messageViewContext?.channel}
						originalSrc={src}
						type="video"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						defaultAltText={alt}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.embed-video.handle-context-menu.media-context-menu.video"
					/>
				));
			},
			[
				message,
				messageViewContext?.channel,
				src,
				contentHash,
				attachmentId,
				defaultName,
				alt,
				onDelete,
				isPreview,
				snapshotIndex,
			],
		);
		const thumbHashUrl = useMemo(() => decodeThumbHashDataURL(placeholder), [placeholder]);
		const decodedDimensionsForSrc =
			decodedVideoDimensions?.src === effectiveSrc
				? {width: decodedVideoDimensions.width, height: decodedVideoDimensions.height}
				: null;
		const layoutSourceDimensions = getEffectiveVideoLayoutDimensions({width, height}, decodedDimensionsForSrc);
		const viewerVideoDimensions = normalizeVideoDimensions(decodedDimensionsForSrc) ?? layoutSourceDimensions;
		const {renderDimensions: dimensions, aspectRatio} = resolveVideoLayout(layoutSourceDimensions, {
			maxWidth,
			maxHeight,
		});
		const posterDimensionsForSrc =
			posterNaturalDimensions?.src === posterSrc
				? {width: posterNaturalDimensions.width, height: posterNaturalDimensions.height}
				: null;
		const posterMatchesVideo =
			posterDimensionsForSrc && decodedDimensionsForSrc
				? !hasDifferentAspectRatio(decodedDimensionsForSrc, posterDimensionsForSrc, 0.05)
				: true;
		const effectivePosterSrc = posterMatchesVideo ? posterSrc : null;
		const updateDecodedVideoDimensions = useCallback(
			(nextWidth: number, nextHeight: number) => {
				const nextDimensions = normalizeVideoDimensions({width: nextWidth, height: nextHeight});
				if (!nextDimensions) return;
				setDecodedVideoDimensions((previous) => {
					if (
						previous?.src === effectiveSrc &&
						previous.width === nextDimensions.width &&
						previous.height === nextDimensions.height
					) {
						return previous;
					}
					return {src: effectiveSrc, width: nextDimensions.width, height: nextDimensions.height};
				});
			},
			[effectiveSrc],
		);
		const handleMetadataProbeLoaded = useCallback(
			(event: React.SyntheticEvent<HTMLVideoElement>) => {
				const video = event.currentTarget;
				updateDecodedVideoDimensions(video.videoWidth, video.videoHeight);
			},
			[updateDecodedVideoDimensions],
		);
		const handleVideoPlayerLoadedMetadata = useCallback(
			(metadata: VideoPlayerMetadata) => {
				updateDecodedVideoDimensions(metadata.width, metadata.height);
			},
			[updateDecodedVideoDimensions],
		);
		useEffect(() => {
			if (!shouldLoadMedia) return;
			if (!posterSrc) return;
			if (DeveloperOptions.forceRenderPlaceholders || DeveloperOptions.forceMediaLoading) {
				return;
			}
			if (loadedPosterSrc === posterSrc) return;
			let active = true;
			const cleanup = ImageCacheUtils.loadImage(
				posterSrc,
				() => {
					if (!active) return;
					setLoadedPosterSrc(posterSrc);
					const image = ImageCacheUtils.getImage(posterSrc);
					if (active && image && image.naturalWidth > 0 && image.naturalHeight > 0) {
						setPosterNaturalDimensions({src: posterSrc, width: image.naturalWidth, height: image.naturalHeight});
					}
				},
				() => {
					if (active) setLoadedPosterSrc((currentSource) => (currentSource === posterSrc ? null : currentSource));
				},
			);
			return () => {
				active = false;
				cleanup();
			};
		}, [loadedPosterSrc, posterSrc, shouldLoadMedia]);
		const handleMobileTap = useCallback(() => {
			const videoItems = attachmentsToViewerItems(mediaAttachments, {filterType: 'video'});
			if (videoItems.length > 0) {
				const currentIndex = findViewerItemIndex(videoItems, attachmentId);
				MediaViewerCommands.openMediaViewer(videoItems, currentIndex, {
					channelId,
					messageId,
					message,
					sourceChannel: messageViewContext?.channel,
				});
			} else {
				MediaViewerCommands.openMediaViewer(
					[
						{
							src: effectiveSrc,
							originalSrc: embedUrl || src,
							naturalWidth: viewerVideoDimensions.width,
							naturalHeight: viewerVideoDimensions.height,
							type: 'video' as const,
							contentHash,
							embedIndex,
							duration,
						},
					],
					0,
					{channelId, messageId, message, sourceChannel: messageViewContext?.channel},
				);
			}
		}, [
			channelId,
			messageId,
			message,
			messageViewContext?.channel,
			mediaAttachments,
			attachmentId,
			effectiveSrc,
			embedUrl,
			src,
			viewerVideoDimensions.width,
			viewerVideoDimensions.height,
			contentHash,
			embedIndex,
			duration,
		]);
		const handlePlayInline = useCallback(() => {
			setIsPlayingInline(true);
		}, []);
		const handleInlineVideoTap = useCallback(() => {
			const video = inlineVideoRef.current;
			const currentTime = video?.currentTime ?? 0;
			if (video) {
				video.pause();
			}
			const videoItems = attachmentsToViewerItems(mediaAttachments, {
				filterType: 'video',
				initialTimeForId: attachmentId ? {attachmentId, time: currentTime} : undefined,
			});
			if (videoItems.length > 0) {
				const currentIndex = findViewerItemIndex(videoItems, attachmentId);
				MediaViewerCommands.openMediaViewer(videoItems, currentIndex, {
					channelId,
					messageId,
					message,
					sourceChannel: messageViewContext?.channel,
				});
			} else {
				MediaViewerCommands.openMediaViewer(
					[
						{
							src: effectiveSrc,
							originalSrc: embedUrl || src,
							naturalWidth: viewerVideoDimensions.width,
							naturalHeight: viewerVideoDimensions.height,
							type: 'video' as const,
							contentHash,
							embedIndex,
							duration,
							initialTime: currentTime,
						},
					],
					0,
					{channelId, messageId, message, sourceChannel: messageViewContext?.channel},
				);
			}
			setIsPlayingInline(false);
		}, [
			channelId,
			messageId,
			message,
			messageViewContext?.channel,
			mediaAttachments,
			attachmentId,
			effectiveSrc,
			embedUrl,
			src,
			viewerVideoDimensions.width,
			viewerVideoDimensions.height,
			contentHash,
			embedIndex,
			duration,
		]);
		const handleInlineVideoEnded = useCallback(() => {
			setIsPlayingInline(false);
		}, []);
		const handleToggleMute = useCallback((e: React.MouseEvent) => {
			e.stopPropagation();
			VideoVolume.toggleMute();
		}, []);
		const handleInitialPlay = useCallback(() => {
			setHasPlayed(true);
		}, []);
		const containerStyles: React.CSSProperties = isMobile
			? {
					aspectRatio,
					width: remFromPx(dimensions.width),
					maxWidth: '100%',
				}
			: fillContainer
				? {
						width: '100%',
						height: '100%',
					}
				: {
						width: remFromPx(dimensions.width),
						maxWidth: '100%',
						aspectRatio,
					};
		if (shouldBlur) {
			return (
				<div
					ref={visibilityRef}
					className={styles.blurContainer}
					data-flx="channel.embeds.media.embed-video.blur-container"
				>
					<div
						className={styles.blurContent}
						style={containerStyles}
						data-flx="channel.embeds.media.embed-video.blur-content"
					>
						<div className={styles.blurInner} data-flx="channel.embeds.media.embed-video.blur-inner">
							{thumbHashUrl && (
								<img
									src={thumbHashUrl}
									alt=""
									className={styles.blurThumbnail}
									style={{filter: 'blur(40px)'}}
									data-flx="channel.embeds.media.embed-video.blur-thumbnail"
								/>
							)}
						</div>
						<MatureMediaBlurOverlay
							reason={gateReason}
							canReveal={canReveal}
							onReveal={revealSensitiveMedia}
							data-flx="channel.embeds.media.embed-video.mature-media-blur-overlay"
						/>
					</div>
				</div>
			);
		}
		const {showFavoriteButton, showDownloadButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview || snapshotIndex !== undefined},
		);
		const metadataProbe =
			shouldLoadMedia && !fillContainer && !isBlob && !hasPlayed && !isPlayingInline ? (
				<video
					aria-hidden="true"
					className={styles.metadataProbe}
					data-embed-media="true"
					muted
					playsInline
					preload="metadata"
					src={effectiveSrc}
					tabIndex={-1}
					onLoadedMetadata={handleMetadataProbeLoaded}
					data-flx="channel.embeds.media.embed-video.metadata-probe"
				>
					<track kind="captions" data-flx="channel.embeds.media.embed-video.track" />
				</video>
			) : null;
		if (isMobile) {
			return (
				<MediaContainer
					ref={visibilityRef}
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
					data-flx="channel.embeds.media.embed-video.media-container.context-menu"
				>
					{metadataProbe}
					<div className={styles.mobileContainer} data-flx="channel.embeds.media.embed-video.mobile-container">
						{isPlayingInline ? (
							<>
								<video
									ref={inlineVideoRef}
									className={styles.inlineVideo}
									src={effectiveSrc}
									autoPlay
									playsInline
									muted={VideoVolume.isMuted}
									data-embed-media="true"
									onClick={handleInlineVideoTap}
									onEnded={handleInlineVideoEnded}
									onLoadedMetadata={handleMetadataProbeLoaded}
									data-flx="channel.embeds.media.embed-video.inline-video"
								/>
								<button
									type="button"
									className={styles.inlineMuteButton}
									onClick={handleToggleMute}
									aria-label={VideoVolume.isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR)}
									data-flx="channel.embeds.media.embed-video.inline-mute-button.toggle-mute"
								>
									{VideoVolume.isMuted ? (
										<SpeakerXIcon
											size={remFromPx(16)}
											weight="fill"
											data-flx="channel.embeds.media.embed-video.speaker-x-icon"
										/>
									) : (
										<SpeakerHighIcon
											size={remFromPx(16)}
											weight="fill"
											data-flx="channel.embeds.media.embed-video.speaker-high-icon"
										/>
									)}
								</button>
							</>
						) : (
							<MobileVideoOverlay
								thumbHashURL={thumbHashUrl}
								posterSrc={shouldLoadMedia ? effectivePosterSrc : null}
								posterLoaded={shouldLoadMedia && posterLoaded && posterMatchesVideo}
								posterCachedOnMount={shouldLoadMedia && posterCachedOnMount && posterMatchesVideo}
								onTap={handleMobileTap}
								onPlayInline={handlePlayInline}
								title={title}
								alt={alt}
								onPopoutToggle={messageViewContext?.onPopoutToggle}
								data-flx="channel.embeds.media.embed-video.mobile-video-overlay"
							/>
						)}
					</div>
				</MediaContainer>
			);
		}
		return (
			<MediaContainer
				ref={visibilityRef}
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
				data-flx="channel.embeds.media.embed-video.media-container.context-menu--2"
			>
				{metadataProbe}
				<div className={styles.videoPlayerWrapper} data-flx="channel.embeds.media.embed-video.video-player-wrapper">
					<VideoPlayer
						src={effectiveSrc}
						poster={shouldLoadMedia ? (effectivePosterSrc ?? undefined) : undefined}
						placeholder={placeholder}
						duration={duration}
						width={dimensions.width}
						height={dimensions.height}
						fillContainer={fillContainer}
						className={fillContainer ? styles.videoPlayerFill : styles.videoPlayerBlock}
						onInitialPlay={handleInitialPlay}
						onLoadedMetadata={handleVideoPlayerLoadedMetadata}
						data-flx="channel.embeds.media.embed-video.video-player"
					/>
					{!hasPlayed && (
						<AltTextBadge
							altText={alt}
							onPopoutToggle={messageViewContext?.onPopoutToggle}
							data-flx="channel.embeds.media.embed-video.alt-text-badge"
						/>
					)}
				</div>
			</MediaContainer>
		);
	},
);

export default EmbedVideo;
