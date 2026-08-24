// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useAnimatedMediaPlaybackAllowed} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {AltTextBadge} from '@app/features/channel/components/embeds/AltTextBadge';
import embedStyles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import {MatureMediaBlurOverlay} from '@app/features/channel/components/embeds/MatureMediaBlurOverlay';
import styles from '@app/features/channel/components/embeds/media/EmbedGifv.module.css';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import {useGifViewportGate} from '@app/features/channel/components/embeds/media/GifViewportGate';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import {MediaContainer} from '@app/features/channel/components/embeds/media/MediaContainer';
import {shouldShowOverlays} from '@app/features/channel/components/embeds/media/MediaOverlayFit';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import {isInlinePlayableVideoSize} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import {safePause, safePlay} from '@app/features/channel/components/GifVideoPool';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import type {Channel} from '@app/features/channel/models/Channel';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import {useMediaLoading} from '@app/features/messaging/hooks/useMediaLoading';
import {useMediaViewerHoverWarm} from '@app/features/messaging/hooks/useMediaViewerHoverWarm';
import {useOpenInBrowserOnMiddleClick} from '@app/features/messaging/hooks/useOpenInBrowserOnMiddleClick';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {resolveProxyRequestSize} from '@app/features/messaging/utils/MediaProxyRequestSize';
import {
	buildFittedAnimatedImageProxyURL,
	buildFittedStaticGifPreviewURL,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {decodeThumbHashDataURL} from '@app/features/messaging/utils/ThumbHashUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import MediaViewer, {type MediaViewerItem} from '@app/features/ui/state/MediaViewer';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const OPEN_ANIMATED_GIF_VIDEO_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open animated GIF video in full view',
	comment: 'Button or menu action label in the channel and chat embed gifv. Keep it concise.',
});
const OPEN_ANIMATED_GIF_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open animated GIF in full view',
	comment: 'Button or menu action label in the channel and chat embed gifv. Keep it concise.',
});
const OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open image in full view',
	comment: 'Button or menu action label in the channel and chat embed gifv. Keep it concise.',
});
const ANIMATED_GIF_VIDEO_DESCRIPTOR = msg({
	message: 'Animated GIF video',
	comment: 'Short label in the channel and chat embed gifv. Keep it concise.',
});
const ANIMATED_GIF_DESCRIPTOR = msg({
	message: 'Animated GIF',
	comment: 'Short label in the channel and chat embed gifv. Keep it concise.',
});
const PLACEHOLDER_FAST_FADE_WINDOW_MS = 200;

type GifvEmbedProps = BaseMediaProps & {
	embedURL: string;
	naturalWidth: number;
	naturalHeight: number;
	placeholder?: string;
	thumbnailProxyURL?: string;
	alt?: string | null;
};

interface VideoConfig {
	autoplay?: boolean;
	loop?: boolean;
	muted?: boolean;
	playsInline?: boolean;
	controls?: boolean;
	preload?: 'none' | 'metadata' | 'auto';
}

function useEmbedMediaCalculator(constraints?: {maxWidth: number; maxHeight: number}) {
	const embedDimensions = getEmbedMediaDimensions();
	const maxWidth = constraints?.maxWidth ?? embedDimensions.maxWidth;
	const maxHeight = constraints?.maxHeight ?? embedDimensions.maxHeight;
	return useMemo(
		() =>
			createCalculator({
				maxWidth,
				maxHeight,
				responsive: true,
			}),
		[maxWidth, maxHeight],
	);
}

function useAspectRatioStyle(width: number, height: number): React.CSSProperties | undefined {
	return useMemo(() => {
		if (width <= 0 || height <= 0) return undefined;
		return {aspectRatio: `${width} / ${height}`};
	}, [height, width]);
}

function useMediaSurfaceInteraction(container: HTMLDivElement | null): {
	isPointerInside: boolean;
	hasFocusInside: boolean;
} {
	const [isPointerInside, setIsPointerInside] = useState(false);
	const [hasFocusInside, setHasFocusInside] = useState(false);
	useEffect(() => {
		if (container == null) return;
		const handleMouseEnter = () => setIsPointerInside(true);
		const handleMouseLeave = () => setIsPointerInside(false);
		const handleFocusIn = () => setHasFocusInside(true);
		const handleFocusOut = (event: FocusEvent) => {
			const nextTarget = event.relatedTarget;
			if (nextTarget != null && container.contains(nextTarget as Node)) return;
			setHasFocusInside(false);
		};
		container.addEventListener('mouseenter', handleMouseEnter);
		container.addEventListener('mouseleave', handleMouseLeave);
		container.addEventListener('focusin', handleFocusIn);
		container.addEventListener('focusout', handleFocusOut);
		return () => {
			container.removeEventListener('mouseenter', handleMouseEnter);
			container.removeEventListener('mouseleave', handleMouseLeave);
			container.removeEventListener('focusin', handleFocusIn);
			container.removeEventListener('focusout', handleFocusOut);
		};
	}, [container]);
	return {isPointerInside, hasFocusInside};
}

interface ThumbHashCurtainState {
	shouldMount: boolean;
	curtainLifted: boolean;
	fastFade: boolean;
	posterRevealed: boolean;
}

function useThumbHashCurtain(source: string, enabled: boolean): ThumbHashCurtainState {
	const [mountedAt] = useState(() => Date.now());
	const [cachedAtMount] = useState(() => ImageCacheUtils.hasImage(source));
	const [curtainLifted, setCurtainLifted] = useState(false);
	const [fastFade, setFastFade] = useState(false);
	useLayoutEffect(() => {
		if (!enabled || source.length === 0) return;
		return ImageCacheUtils.loadImage(source, () => {
			setFastFade(Date.now() - mountedAt < PLACEHOLDER_FAST_FADE_WINDOW_MS);
			setCurtainLifted(true);
		});
	}, [enabled, mountedAt, source]);
	return {
		shouldMount: !cachedAtMount,
		curtainLifted,
		fastFade,
		posterRevealed: source.length === 0 || cachedAtMount || curtainLifted,
	};
}

const useImagePreview = ({
	proxyUrl,
	embedUrl,
	naturalWidth,
	naturalHeight,
	type,
	channelId,
	messageId,
	attachmentId,
	embedIndex,
	contentHash,
	message,
	sourceChannel,
	providerName,
}: {
	proxyUrl: string;
	embedUrl: string;
	naturalWidth: number;
	naturalHeight: number;
	type: 'gifv' | 'gif' | 'image';
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	embedIndex?: number;
	contentHash?: string | null;
	message?: Message;
	sourceChannel?: Channel | null;
	providerName?: string;
}): {viewerItem: MediaViewerItem; openPreview: (event: React.MouseEvent | React.KeyboardEvent) => void} => {
	const viewerItem = useMemo<MediaViewerItem>(
		() => ({
			src: proxyUrl,
			originalSrc: embedUrl,
			naturalWidth,
			naturalHeight,
			type,
			contentHash,
			attachmentId,
			embedIndex,
			animated: true,
			providerName,
		}),
		[proxyUrl, embedUrl, naturalWidth, naturalHeight, type, contentHash, attachmentId, embedIndex, providerName],
	);
	const openPreview = useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			if (event.type === 'click' && (event as React.MouseEvent).button !== 0) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			MediaViewerCommands.openMediaViewer([viewerItem], 0, {
				channelId,
				messageId,
				message,
				sourceChannel,
			});
		},
		[viewerItem, channelId, messageId, message, sourceChannel],
	);
	return {viewerItem, openPreview};
};

interface ImagePreviewHandlerProps {
	src: string;
	originalSrc: string;
	naturalWidth: number;
	naturalHeight: number;
	type: 'gifv' | 'gif' | 'image';
	handlePress?: (event: React.MouseEvent | React.KeyboardEvent) => void;
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	embedIndex?: number;
	contentHash?: string | null;
	message?: Message;
	sourceChannel?: Channel | null;
	onViewerWarmEnter?: () => void;
	onViewerWarmLeave?: () => void;
	children: React.ReactNode;
}

const ImagePreviewHandler: FC<ImagePreviewHandlerProps> = observer(
	({
		src,
		originalSrc,
		naturalWidth,
		naturalHeight,
		type,
		handlePress,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		contentHash,
		message,
		sourceChannel,
		onViewerWarmEnter,
		onViewerWarmLeave,
		children,
	}) => {
		const {i18n} = useLingui();
		const openImagePreview = useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				if (event.type === 'click' && (event as React.MouseEvent).button !== 0) {
					return;
				}
				if (event.type === 'keydown') {
					const keyEvent = event as React.KeyboardEvent;
					if (!isKeyboardActivationKey(keyEvent.key)) {
						return;
					}
				}
				if (handlePress) {
					event.preventDefault();
					event.stopPropagation();
					handlePress(event);
					return;
				}
				event.preventDefault();
				event.stopPropagation();
				MediaViewerCommands.openMediaViewer(
					[
						{
							src,
							originalSrc,
							naturalWidth,
							naturalHeight,
							type,
							contentHash,
							attachmentId,
							embedIndex,
							animated: true,
						},
					],
					0,
					{
						channelId,
						messageId,
						message,
						sourceChannel,
					},
				);
			},
			[
				src,
				originalSrc,
				naturalWidth,
				naturalHeight,
				handlePress,
				type,
				channelId,
				messageId,
				attachmentId,
				embedIndex,
				contentHash,
				message,
				sourceChannel,
			],
		);
		const openInBrowser = useOpenInBrowserOnMiddleClick(originalSrc || src);
		const ariaLabel = (() => {
			if (type === 'gifv') return i18n._(OPEN_ANIMATED_GIF_VIDEO_IN_FULL_VIEW_DESCRIPTOR);
			if (type === 'gif') return i18n._(OPEN_ANIMATED_GIF_IN_FULL_VIEW_DESCRIPTOR);
			return i18n._(OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR);
		})();
		return (
			<button
				type="button"
				className={styles.imagePreviewHandler}
				aria-label={ariaLabel}
				onClick={openImagePreview}
				onMouseDown={openInBrowser.onMouseDown}
				onAuxClick={openInBrowser.onAuxClick}
				onKeyDown={openImagePreview}
				onMouseEnter={onViewerWarmEnter}
				onMouseLeave={onViewerWarmLeave}
				data-flx="channel.embeds.media.embed-gifv.image-preview-handler.image-preview-handler.open-image-preview.button"
			>
				{children}
			</button>
		);
	},
);
export const EmbedGifv: FC<
	GifvEmbedProps & {
		videoProxyURL: string;
		videoURL: string;
		videoConfig?: VideoConfig;
		isPreview?: boolean;
		snapshotIndex?: number;
		providerName?: string;
	}
> = observer(
	({
		embedURL,
		videoProxyURL,
		thumbnailProxyURL,
		alt,
		naturalWidth,
		naturalHeight,
		placeholder,
		videoConfig,
		nsfw,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		message,
		contentHash,
		onDelete,
		isPreview,
		snapshotIndex,
		providerName,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const mediaCalculator = useEmbedMediaCalculator();
		const videoRef = useRef<HTMLVideoElement>(null);
		const [containerElement, setContainerElement] = useState<HTMLDivElement | null>(null);
		const {isPointerInside, hasFocusInside} = useMediaSurfaceInteraction(containerElement);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const {
			ref: visibilityRef,
			loadMedia: shouldLoadMedia,
			animate: shouldAnimate,
		} = useGifViewportGate<HTMLDivElement>({
			element: containerElement,
			rememberKey: videoProxyURL,
			shouldBlur,
		});
		const setContainerRef = useCallback(
			(node: HTMLDivElement | null) => {
				setContainerElement(node);
				visibilityRef(node);
			},
			[visibilityRef],
		);
		const {dimensions, style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight});
		const canPlayInline = isInlinePlayableVideoSize({width: naturalWidth, height: naturalHeight});
		const posterSource = thumbnailProxyURL && thumbnailProxyURL.length > 0 ? thumbnailProxyURL : videoProxyURL;
		const posterURL = useMemo(() => {
			const requestedSize = resolveProxyRequestSize(dimensions.width, dimensions.height, naturalWidth, naturalHeight);
			return buildFittedStaticGifPreviewURL(
				stripMediaProxyParams(posterSource),
				requestedSize?.width,
				requestedSize?.height,
			);
		}, [dimensions.width, naturalHeight, naturalWidth, posterSource]);
		const thumbHashURL = useMemo(() => decodeThumbHashDataURL(placeholder), [placeholder]);
		const thumbHashCurtain = useThumbHashCurtain(posterURL, shouldLoadMedia);
		const aspectRatioStyle = useAspectRatioStyle(dimensions.width, dimensions.height);
		const defaultName = deriveDefaultNameFromMessage({
			message,
			attachmentId,
			embedIndex,
			url: embedURL,
			proxyUrl: videoProxyURL,
		});
		const effectiveDefaultName = alt?.trim() ? alt.trim() : defaultName || 'GIF';
		const {toggleFavorite, isFavorited, canFavorite} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName: effectiveDefaultName,
			contentHash,
			isGifv: true,
			embedURL,
			proxyURL: videoProxyURL,
			naturalWidth,
			naturalHeight,
		});
		const animationPolicyAllowed = useShouldAnimate({
			kind: 'gif',
			isHovering: isPointerInside,
			isFocused: hasFocusInside,
		});
		const animatedMediaPlaybackAllowed = useAnimatedMediaPlaybackAllowed();
		const isMediaViewerOpen = MediaViewer.isOpen;
		const {openPreview: openImagePreview} = useImagePreview({
			proxyUrl: videoProxyURL,
			embedUrl: embedURL,
			naturalWidth,
			naturalHeight,
			type: 'gifv',
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			contentHash,
			message,
			sourceChannel: messageViewContext?.channel,
			providerName,
		});
		const handleDeleteClick = useDeleteAttachment(message, attachmentId);
		const handleDownloadClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createDownloadHandler(videoProxyURL, 'video')();
			},
			[videoProxyURL],
		);
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
						originalSrc={embedURL}
						proxyURL={videoProxyURL}
						type="gifv"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={effectiveDefaultName}
						defaultAltText={alt ?? undefined}
						naturalWidth={naturalWidth}
						naturalHeight={naturalHeight}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.embed-gifv.handle-context-menu.media-context-menu.gifv"
					/>
				));
			},
			[
				message,
				messageViewContext?.channel,
				embedURL,
				videoProxyURL,
				contentHash,
				attachmentId,
				effectiveDefaultName,
				alt,
				naturalWidth,
				naturalHeight,
				onDelete,
				isPreview,
				snapshotIndex,
			],
		);
		const shouldPlay =
			shouldAnimate &&
			animationPolicyAllowed &&
			animatedMediaPlaybackAllowed &&
			!isMediaViewerOpen &&
			thumbHashCurtain.posterRevealed;
		useEffect(() => {
			const video = videoRef.current;
			if (video == null) return;
			if (shouldPlay) {
				video.autoplay = true;
				void safePlay(video);
				return;
			}
			video.autoplay = false;
			safePause(video);
		}, [shouldPlay]);
		if (shouldBlur) {
			const {width: _width, height: _height, ...styleWithoutDimensions} = style;
			const blurContainerStyle = {...styleWithoutDimensions, maxWidth: '100%', width: '100%'};
			return (
				<div
					ref={visibilityRef}
					className={styles.blurContainer}
					data-flx="channel.embeds.media.embed-gifv.blur-container"
				>
					<div
						className={styles.blurContent}
						style={blurContainerStyle}
						data-flx="channel.embeds.media.embed-gifv.blur-content"
					>
						<div className={styles.blurInnerContainer} data-flx="channel.embeds.media.embed-gifv.blur-inner-container">
							{thumbHashURL && (
								<img
									src={thumbHashURL}
									className={styles.thumbHashPlaceholder}
									alt=""
									aria-hidden={true}
									style={{filter: 'blur(40px)'}}
									data-flx="channel.embeds.media.embed-gifv.thumb-hash-placeholder"
								/>
							)}
						</div>
						<MatureMediaBlurOverlay
							reason={gateReason}
							canReveal={canReveal}
							onReveal={revealSensitiveMedia}
							data-flx="channel.embeds.media.embed-gifv.mature-media-blur-overlay"
						/>
					</div>
				</div>
			);
		}
		const {showFavoriteButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview || snapshotIndex !== undefined},
		);
		const showGifIndicator = Accessibility.showGifIndicator && shouldShowOverlays(dimensions.width, dimensions.height);
		const containerStyle: React.CSSProperties = {
			maxWidth: '100%',
			width: remFromPx(dimensions.width),
			...aspectRatioStyle,
		};
		return (
			<MediaContainer
				ref={setContainerRef}
				className={clsx(embedStyles.embedGifvContainer, styles.mediaContainer)}
				style={containerStyle}
				showFavoriteButton={showFavoriteButton}
				isFavorited={isFavorited}
				onFavoriteClick={toggleFavorite}
				showDownloadButton={false}
				onDownloadClick={handleDownloadClick}
				showDeleteButton={showDeleteButton}
				onDeleteClick={handleDeleteClick}
				onContextMenu={handleContextMenu}
				renderedWidth={dimensions.width}
				renderedHeight={dimensions.height}
				forceShowFavoriteButton={true}
				data-flx="channel.embeds.media.embed-gifv.media-container.context-menu"
			>
				{showGifIndicator && <GifIndicator data-flx="channel.embeds.media.embed-gifv.gif-indicator" />}
				<ImagePreviewHandler
					src={videoProxyURL}
					originalSrc={embedURL}
					naturalWidth={naturalWidth}
					naturalHeight={naturalHeight}
					type="gifv"
					handlePress={openImagePreview}
					data-flx="channel.embeds.media.embed-gifv.image-preview-handler.gifv"
				>
					<div
						className={styles.videoWrapper}
						style={aspectRatioStyle}
						data-flx="channel.embeds.media.embed-gifv.video-wrapper"
					>
						{canPlayInline ? (
							<video
								ref={videoRef}
								className={styles.videoElement}
								src={shouldLoadMedia ? videoProxyURL : undefined}
								poster={shouldLoadMedia ? posterURL : thumbHashURL}
								preload={videoConfig?.preload ?? 'none'}
								loop={videoConfig?.loop ?? true}
								muted={videoConfig?.muted ?? true}
								playsInline={videoConfig?.playsInline ?? true}
								controls={videoConfig?.controls ?? false}
								width={dimensions.width}
								height={dimensions.height}
								tabIndex={-1}
								aria-label={i18n._(ANIMATED_GIF_VIDEO_DESCRIPTOR)}
								data-embed-media="gifv"
								data-flx="channel.embeds.media.embed-gifv.video-element"
							/>
						) : (
							<img
								className={styles.videoElement}
								src={shouldLoadMedia ? posterURL : thumbHashURL}
								alt=""
								width={dimensions.width}
								height={dimensions.height}
								loading="eager"
								tabIndex={-1}
								data-embed-media="gifv-still"
								data-flx="channel.embeds.media.embed-gifv.still-element"
							/>
						)}
						{thumbHashCurtain.shouldMount && thumbHashURL && (
							<img
								src={thumbHashURL}
								className={clsx(
									styles.placeholder,
									thumbHashCurtain.curtainLifted ? styles.placeholderHidden : styles.placeholderVisible,
									thumbHashCurtain.fastFade && styles.placeholderFastFade,
								)}
								alt=""
								aria-hidden="true"
								data-flx="channel.embeds.media.embed-gifv.image-placeholder"
							/>
						)}
					</div>
				</ImagePreviewHandler>
				<AltTextBadge
					altText={alt}
					onPopoutToggle={messageViewContext?.onPopoutToggle}
					data-flx="channel.embeds.media.embed-gifv.alt-text-badge"
				/>
			</MediaContainer>
		);
	},
);
export const EmbedGif: FC<
	GifvEmbedProps & {
		proxyURL: string;
		includeButton?: boolean;
		isPreview?: boolean;
		snapshotIndex?: number;
		layoutConstraints?: {maxWidth: number; maxHeight: number};
	}
> = observer(
	({
		embedURL,
		proxyURL,
		alt,
		naturalWidth,
		naturalHeight,
		placeholder,
		nsfw,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		message,
		contentHash,
		onDelete,
		isPreview,
		snapshotIndex,
		layoutConstraints,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const mediaCalculator = useEmbedMediaCalculator(layoutConstraints);
		const [containerElement, setContainerElement] = useState<HTMLDivElement | null>(null);
		const {isPointerInside, hasFocusInside} = useMediaSurfaceInteraction(containerElement);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const {
			ref: visibilityRef,
			loadMedia: shouldLoadMedia,
			animate: shouldAnimate,
		} = useGifViewportGate<HTMLDivElement>({
			element: containerElement,
			rememberKey: proxyURL,
			shouldBlur,
		});
		const setContainerRef = useCallback(
			(node: HTMLDivElement | null) => {
				setContainerElement(node);
				visibilityRef(node);
			},
			[visibilityRef],
		);
		const {dimensions, style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight});
		const {width: displayWidth, height: displayHeight} = dimensions;
		const gifAutoPlay = useShouldAnimate({kind: 'gif'});
		const animationPolicyAllowed = useShouldAnimate({
			kind: 'gif',
			isHovering: isPointerInside,
			isFocused: hasFocusInside,
		});
		const baseProxyURL = stripMediaProxyParams(proxyURL);
		const requestedSize = resolveProxyRequestSize(displayWidth, displayHeight, naturalWidth, naturalHeight);
		const optimizedAnimatedURL = buildFittedAnimatedImageProxyURL(
			baseProxyURL,
			requestedSize?.width,
			requestedSize?.height,
		);
		const optimizedStaticURL = buildFittedStaticGifPreviewURL(
			baseProxyURL,
			requestedSize?.width,
			requestedSize?.height,
		);
		const shouldShowAnimated = shouldAnimate && animationPolicyAllowed;
		const activeSource = shouldShowAnimated ? optimizedAnimatedURL : optimizedStaticURL;
		const {
			loaded,
			cachedOnMount,
			thumbHashURL,
			ref: mediaRef,
			onLoad: handleImageLoad,
			onError: handleImageError,
		} = useMediaLoading(activeSource, placeholder, {enabled: shouldLoadMedia});
		const [mountedAt] = useState(() => Date.now());
		const [placeholderMounted] = useState(!cachedOnMount);
		const [placeholderHidden, setPlaceholderHidden] = useState(loaded);
		const [placeholderFastFade, setPlaceholderFastFade] = useState(false);
		const aspectRatioStyle = useAspectRatioStyle(dimensions.width, dimensions.height);
		useEffect(() => {
			if (!loaded) return;
			setPlaceholderFastFade(Date.now() - mountedAt < PLACEHOLDER_FAST_FADE_WINDOW_MS);
			setPlaceholderHidden(true);
		}, [loaded, mountedAt]);
		useEffect(() => {
			if (!shouldLoadMedia) return;
			if (!animationPolicyAllowed) return;
			if (activeSource === optimizedAnimatedURL) return;
			return ImageCacheUtils.pinImage(optimizedAnimatedURL);
		}, [activeSource, animationPolicyAllowed, optimizedAnimatedURL, shouldLoadMedia]);
		const defaultName = deriveDefaultNameFromMessage({
			message,
			attachmentId,
			embedIndex,
			url: embedURL,
			proxyUrl: proxyURL,
		});
		const effectiveDefaultName = alt?.trim() ? alt.trim() : defaultName || 'GIF';
		const {toggleFavorite, isFavorited, canFavorite} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName: effectiveDefaultName,
			contentHash,
			isGifv: true,
			embedURL,
			proxyURL,
			naturalWidth,
			naturalHeight,
		});
		const {viewerItem, openPreview: openImagePreview} = useImagePreview({
			proxyUrl: optimizedAnimatedURL,
			embedUrl: embedURL,
			naturalWidth,
			naturalHeight,
			type: 'gif',
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			contentHash,
			message,
			sourceChannel: messageViewContext?.channel,
		});
		const {scheduleViewerWarm, cancelViewerWarm} = useMediaViewerHoverWarm(viewerItem, {
			allowAnimated: gifAutoPlay,
			enabled: !shouldBlur,
		});
		const handleDeleteClick = useDeleteAttachment(message, attachmentId);
		const handleDownloadClickGif = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createDownloadHandler(baseProxyURL, 'gif')();
			},
			[baseProxyURL],
		);
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
						originalSrc={embedURL}
						proxyURL={proxyURL}
						type="gif"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={effectiveDefaultName}
						defaultAltText={alt ?? undefined}
						naturalWidth={naturalWidth}
						naturalHeight={naturalHeight}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.embed-gifv.handle-context-menu.media-context-menu.gif"
					/>
				));
			},
			[
				message,
				messageViewContext?.channel,
				embedURL,
				proxyURL,
				contentHash,
				attachmentId,
				effectiveDefaultName,
				alt,
				naturalWidth,
				naturalHeight,
				onDelete,
				isPreview,
				snapshotIndex,
			],
		);
		if (shouldBlur) {
			const {width: _width, height: _height, ...styleWithoutDimensions} = style;
			const blurContainerStyle = {...styleWithoutDimensions, maxWidth: '100%', width: '100%'};
			return (
				<div
					ref={visibilityRef}
					className={styles.blurContainer}
					data-flx="channel.embeds.media.embed-gifv.embed-gif.blur-container"
				>
					<div
						className={styles.blurContent}
						style={blurContainerStyle}
						data-flx="channel.embeds.media.embed-gifv.embed-gif.blur-content"
					>
						<div
							className={styles.blurInnerContainer}
							data-flx="channel.embeds.media.embed-gifv.embed-gif.blur-inner-container"
						>
							{thumbHashURL && (
								<img
									src={thumbHashURL}
									className={styles.thumbHashPlaceholder}
									alt=""
									aria-hidden={true}
									style={{filter: 'blur(40px)'}}
									data-flx="channel.embeds.media.embed-gifv.embed-gif.thumb-hash-placeholder"
								/>
							)}
						</div>
						<MatureMediaBlurOverlay
							reason={gateReason}
							canReveal={canReveal}
							onReveal={revealSensitiveMedia}
							data-flx="channel.embeds.media.embed-gifv.embed-gif.mature-media-blur-overlay"
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
		const showGifIndicator = Accessibility.showGifIndicator && shouldShowOverlays(dimensions.width, dimensions.height);
		const containerStyle: React.CSSProperties = {
			maxWidth: '100%',
			width: remFromPx(dimensions.width),
			...aspectRatioStyle,
		};
		return (
			<MediaContainer
				ref={setContainerRef}
				className={clsx(embedStyles.embedGifvContainer, styles.mediaContainer)}
				style={containerStyle}
				showFavoriteButton={showFavoriteButton}
				isFavorited={isFavorited}
				onFavoriteClick={toggleFavorite}
				showDownloadButton={showDownloadButton}
				onDownloadClick={handleDownloadClickGif}
				showDeleteButton={showDeleteButton}
				onDeleteClick={handleDeleteClick}
				onContextMenu={handleContextMenu}
				renderedWidth={dimensions.width}
				renderedHeight={dimensions.height}
				forceShowFavoriteButton={true}
				data-flx="channel.embeds.media.embed-gifv.embed-gif.media-container.context-menu"
			>
				{showGifIndicator && <GifIndicator data-flx="channel.embeds.media.embed-gifv.embed-gif.gif-indicator" />}
				<ImagePreviewHandler
					src={optimizedAnimatedURL}
					originalSrc={embedURL}
					naturalWidth={naturalWidth}
					naturalHeight={naturalHeight}
					type="gif"
					handlePress={openImagePreview}
					channelId={channelId}
					messageId={messageId}
					attachmentId={attachmentId}
					embedIndex={embedIndex}
					contentHash={contentHash}
					message={message}
					sourceChannel={messageViewContext?.channel}
					onViewerWarmEnter={scheduleViewerWarm}
					onViewerWarmLeave={cancelViewerWarm}
					data-flx="channel.embeds.media.embed-gifv.embed-gif.image-preview-handler.gif"
				>
					<div
						className={styles.videoWrapper}
						style={aspectRatioStyle}
						data-flx="channel.embeds.media.embed-gifv.embed-gif.video-wrapper"
					>
						<img
							ref={mediaRef}
							alt={i18n._(ANIMATED_GIF_DESCRIPTOR)}
							src={shouldLoadMedia ? activeSource : undefined}
							className={styles.videoElement}
							data-embed-media="gif"
							loading="eager"
							tabIndex={-1}
							width={dimensions.width}
							height={dimensions.height}
							onLoad={handleImageLoad}
							onError={handleImageError}
							data-flx="channel.embeds.media.embed-gifv.embed-gif.video-element"
						/>
						{placeholderMounted && thumbHashURL && (
							<img
								src={thumbHashURL}
								className={clsx(
									styles.placeholder,
									placeholderHidden ? styles.placeholderHidden : styles.placeholderVisible,
									placeholderFastFade && styles.placeholderFastFade,
								)}
								alt=""
								aria-hidden="true"
								data-flx="channel.embeds.media.embed-gifv.embed-gif.image-placeholder"
							/>
						)}
					</div>
				</ImagePreviewHandler>
				<AltTextBadge
					altText={alt}
					onPopoutToggle={messageViewContext?.onPopoutToggle}
					data-flx="channel.embeds.media.embed-gifv.embed-gif.alt-text-badge"
				/>
			</MediaContainer>
		);
	},
);
