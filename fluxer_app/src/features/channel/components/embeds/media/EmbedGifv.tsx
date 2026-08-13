// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useAnimatedImageDecoder} from '@app/features/app/hooks/useAnimatedImageDecoder';
import {
	getAnimatedMediaPlaybackAllowed,
	useAnimatedMediaPlaybackAllowed,
} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {AltTextBadge} from '@app/features/channel/components/embeds/AltTextBadge';
import embedStyles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import {MatureMediaBlurOverlay} from '@app/features/channel/components/embeds/MatureMediaBlurOverlay';
import styles from '@app/features/channel/components/embeds/media/EmbedGifv.module.css';
import {
	createInitialGIFVRenderState,
	GIFVPlaybackState,
	GIFVRenderEventType,
	GIFVVideoState,
	getGIFVRenderLayers,
	reduceGIFVRenderState,
} from '@app/features/channel/components/embeds/media/GIFVRenderStateMachine';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import {MediaContainer, shouldShowOverlays} from '@app/features/channel/components/embeds/media/MediaContainer';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import {safePause, safePlay} from '@app/features/channel/components/GifVideoPool';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import type {Channel} from '@app/features/channel/models/Channel';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import {useMediaLoading} from '@app/features/messaging/hooks/useMediaLoading';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {useOpenInBrowserOnMiddleClick} from '@app/features/messaging/hooks/useOpenInBrowserOnMiddleClick';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {
	buildFittedAnimatedImageProxyURL,
	buildFittedStaticGifPreviewURL,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import MediaViewer from '@app/features/ui/state/MediaViewer';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useLayoutEffect, useMemo, useReducer, useRef, useState} from 'react';

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
const LOADING_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Loading placeholder',
	comment: 'Placeholder text in the channel and chat embed gifv. Keep it concise.',
});
const ANIMATED_GIF_VIDEO_DESCRIPTOR = msg({
	message: 'Animated GIF video',
	comment: 'Short label in the channel and chat embed gifv. Keep it concise.',
});
const ANIMATED_GIF_DESCRIPTOR = msg({
	message: 'Animated GIF',
	comment: 'Short label in the channel and chat embed gifv. Keep it concise.',
});
const EMBED_MEDIA_FADE_DURATION_SECONDS = 0.08;
const DECODER_MAX_CACHED_FRAMES = 24;

type AnimatedImagePreloadStatus = 'idle' | 'loading' | 'ready' | 'failed';

interface AnimatedImagePreloadState {
	url: string;
	status: AnimatedImagePreloadStatus;
}

function watchPresentedVideoFrame(video: HTMLVideoElement, onPresented: () => void): () => void {
	let cancelled = false;
	const requestVideoFrameCallback = video.requestVideoFrameCallback;
	if (typeof requestVideoFrameCallback === 'function') {
		const callbackHandle = requestVideoFrameCallback.call(video, () => {
			if (!cancelled) onPresented();
		});
		return () => {
			cancelled = true;
			const cancelVideoFrameCallback = video.cancelVideoFrameCallback;
			if (typeof cancelVideoFrameCallback === 'function') {
				cancelVideoFrameCallback.call(video, callbackHandle);
			}
		};
	}
	const ownerWindow = video.ownerDocument.defaultView;
	if (ownerWindow == null) return () => {};
	let secondFrameHandle: number | null = null;
	const firstFrameHandle = ownerWindow.requestAnimationFrame(() => {
		secondFrameHandle = ownerWindow.requestAnimationFrame(() => {
			if (!cancelled) onPresented();
		});
	});
	return () => {
		cancelled = true;
		ownerWindow.cancelAnimationFrame(firstFrameHandle);
		if (secondFrameHandle != null) ownerWindow.cancelAnimationFrame(secondFrameHandle);
	};
}

function mediaElementHasSource(element: HTMLImageElement | HTMLVideoElement, source: string): boolean {
	if (element.currentSrc.length > 0) {
		try {
			return element.currentSrc === new URL(source, element.ownerDocument.baseURI).href;
		} catch {
			return element.currentSrc === source;
		}
	}
	const attributeSource = element.getAttribute('src');
	if (attributeSource === source) return true;
	return element.src === source;
}

function completeImageHasSource(image: HTMLImageElement | null, source: string): image is HTMLImageElement {
	if (image == null) return false;
	if (!mediaElementHasSource(image, source)) return false;
	return image.complete && image.naturalWidth > 0 && image.naturalHeight > 0;
}

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
}) => {
	return useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			if (event.type === 'click' && (event as React.MouseEvent).button !== 0) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			MediaViewerCommands.openMediaViewer(
				[
					{
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
		],
	);
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
		const posterElementRef = useRef<HTMLImageElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const savedTimeRef = useRef(0);
		const frameCleanupRef = useRef<(() => void) | null>(null);
		const playbackRequestIdRef = useRef(0);
		const [containerElement, setContainerElement] = useState<HTMLDivElement | null>(null);
		const [isHoveredState, setIsHoveredState] = useState(false);
		const [isFocusWithin, setIsFocusWithin] = useState(false);
		const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({rememberKey: videoProxyURL});
		const setContainerRef = useCallback(
			(node: HTMLDivElement | null) => {
				containerRef.current = node;
				setContainerElement(node);
				visibilityRef(node);
			},
			[visibilityRef],
		);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const shouldLoadVideo = isNearViewport && !shouldBlur;
		const {dimensions} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
		const posterSource = thumbnailProxyURL && thumbnailProxyURL.length > 0 ? thumbnailProxyURL : videoProxyURL;
		const posterURL = useMemo(
			() =>
				buildFittedStaticGifPreviewURL(
					stripMediaProxyParams(posterSource),
					Math.max(1, Math.round(dimensions.width * 2)),
					Math.max(1, Math.round(dimensions.height * 2)),
				),
			[dimensions.height, dimensions.width, posterSource],
		);
		const posterCachedForPaint = ImageCacheUtils.hasImage(posterURL);
		const shouldLoadPoster = (isNearViewport || posterCachedForPaint) && !shouldBlur;
		const {
			loaded,
			error,
			cached,
			cachedOnMount,
			thumbHashURL,
			ref: mediaRef,
			onLoad: handleMediaLoad,
			onError: handleMediaError,
		} = useMediaLoading(posterURL, placeholder, {enabled: shouldLoadPoster});
		const renderSourceKey = `${videoProxyURL}\n${posterURL}\n${shouldLoadVideo ? 'active' : 'inactive'}`;
		const [renderState, dispatchRenderEvent] = useReducer(
			reduceGIFVRenderState,
			{
				sourceKey: renderSourceKey,
				posterCached: ImageCacheUtils.hasImage(posterURL),
			},
			createInitialGIFVRenderState,
		);
		const currentRenderSourceKeyRef = useRef(renderSourceKey);
		const setVideoRef = useCallback(
			(node: HTMLVideoElement | null) => {
				if (videoRef.current !== node && frameCleanupRef.current != null) {
					frameCleanupRef.current();
					frameCleanupRef.current = null;
				}
				videoRef.current = node;
				if (node == null) return;
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (!mediaElementHasSource(node, videoProxyURL)) return;
				if (node.readyState < 2) return;
				dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_READY});
			},
			[renderSourceKey, videoProxyURL],
		);
		const setPosterRef = useCallback(
			(node: HTMLImageElement | null) => {
				posterElementRef.current = node;
				mediaRef(node);
			},
			[mediaRef],
		);
		useLayoutEffect(() => {
			currentRenderSourceKeyRef.current = renderSourceKey;
			if (frameCleanupRef.current != null) {
				frameCleanupRef.current();
				frameCleanupRef.current = null;
			}
			dispatchRenderEvent({
				type: GIFVRenderEventType.SOURCE_CHANGED,
				sourceKey: renderSourceKey,
				posterCached: ImageCacheUtils.hasImage(posterURL),
			});
			if (!shouldLoadVideo) return;
			const video = videoRef.current;
			if (video == null) return;
			if (!mediaElementHasSource(video, videoProxyURL)) return;
			if (video.readyState < 2) return;
			dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_READY});
		}, [posterURL, renderSourceKey, shouldLoadVideo, videoProxyURL]);
		const renderStateIsCurrent = renderState.sourceKey === renderSourceKey;
		const currentRenderState = renderStateIsCurrent
			? renderState
			: createInitialGIFVRenderState({
					sourceKey: renderSourceKey,
					posterCached: ImageCacheUtils.hasImage(posterURL),
				});
		useEffect(() => {
			if (!shouldLoadPoster) return;
			if (loaded) {
				dispatchRenderEvent({type: GIFVRenderEventType.POSTER_LOADED});
				return;
			}
			if (error) {
				dispatchRenderEvent({type: GIFVRenderEventType.POSTER_FAILED});
				return;
			}
			dispatchRenderEvent({type: GIFVRenderEventType.POSTER_LOAD_STARTED});
		}, [error, loaded, shouldLoadPoster]);
		useEffect(() => {
			if (shouldLoadVideo) dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_LOAD_STARTED});
		}, [renderSourceKey, shouldLoadVideo]);
		useEffect(() => {
			if (frameCleanupRef.current != null) {
				frameCleanupRef.current();
				frameCleanupRef.current = null;
			}
			if (currentRenderState.video === GIFVVideoState.PRESENTED) return;
			if (currentRenderState.playback !== GIFVPlaybackState.PLAYING) return;
			const video = videoRef.current;
			if (video == null || video.readyState < 2) return;
			const sourceKey = renderSourceKey;
			const cancelFrameWatch = watchPresentedVideoFrame(video, () => {
				if (currentRenderSourceKeyRef.current !== sourceKey) return;
				if (videoRef.current !== video) return;
				if (!mediaElementHasSource(video, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_FRAME_PRESENTED});
			});
			frameCleanupRef.current = cancelFrameWatch;
			return () => {
				cancelFrameWatch();
				if (frameCleanupRef.current === cancelFrameWatch) frameCleanupRef.current = null;
			};
		}, [currentRenderState.playback, currentRenderState.video, renderSourceKey, videoProxyURL]);
		useEffect(() => {
			return () => {
				if (frameCleanupRef.current != null) frameCleanupRef.current();
			};
		}, []);
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
		const gifAutoPlay = useShouldAnimate({kind: 'gif', respectPlaybackAllowed: false});
		const animatedMediaPlaybackAllowed = useAnimatedMediaPlaybackAllowed();
		const isMediaViewerOpen = MediaViewer.isOpen;
		const openImagePreview = useImagePreview({
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
		const handlePosterLoad = useCallback(
			(event: React.SyntheticEvent<HTMLImageElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (posterElementRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, posterURL)) return;
				handleMediaLoad(event);
				dispatchRenderEvent({type: GIFVRenderEventType.POSTER_LOADED});
			},
			[handleMediaLoad, posterURL, renderSourceKey],
		);
		const handlePosterError = useCallback(
			(event: React.SyntheticEvent<HTMLImageElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (posterElementRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, posterURL)) return;
				handleMediaError(event);
				dispatchRenderEvent({type: GIFVRenderEventType.POSTER_FAILED});
			},
			[handleMediaError, posterURL, renderSourceKey],
		);
		const handleVideoReady = useCallback(
			(event: React.SyntheticEvent<HTMLVideoElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (videoRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_READY});
			},
			[renderSourceKey, videoProxyURL],
		);
		const handleVideoPlaying = useCallback(
			(event: React.SyntheticEvent<HTMLVideoElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (videoRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.PLAYING});
			},
			[renderSourceKey, videoProxyURL],
		);
		const handleVideoPause = useCallback(
			(event: React.SyntheticEvent<HTMLVideoElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (videoRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.PLAYBACK_PAUSED});
			},
			[renderSourceKey, videoProxyURL],
		);
		const handleVideoError = useCallback(
			(event: React.SyntheticEvent<HTMLVideoElement>) => {
				if (currentRenderSourceKeyRef.current !== renderSourceKey) return;
				if (videoRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.VIDEO_FAILED});
			},
			[renderSourceKey, videoProxyURL],
		);
		const requestVideoPlayback = useCallback(
			(video: HTMLVideoElement) => {
				const sourceKey = renderSourceKey;
				const requestId = playbackRequestIdRef.current;
				if (currentRenderSourceKeyRef.current !== sourceKey) return;
				if (videoRef.current !== video) return;
				if (!mediaElementHasSource(video, videoProxyURL)) return;
				dispatchRenderEvent({type: GIFVRenderEventType.PLAY_REQUESTED});
				if (!getAnimatedMediaPlaybackAllowed()) {
					if (currentRenderSourceKeyRef.current === sourceKey) {
						dispatchRenderEvent({type: GIFVRenderEventType.PLAYBACK_BLOCKED});
					}
					return;
				}
				const playPromise = safePlay(video);
				void playPromise.then(() => {
					if (playbackRequestIdRef.current !== requestId) return;
					if (currentRenderSourceKeyRef.current !== sourceKey) return;
					if (videoRef.current !== video) return;
					if (!mediaElementHasSource(video, videoProxyURL)) return;
					if (video.paused) dispatchRenderEvent({type: GIFVRenderEventType.PLAYBACK_BLOCKED});
				});
			},
			[renderSourceKey, videoProxyURL],
		);
		useEffect(() => {
			const container = containerElement;
			if (container == null) return;
			const saveCurrentTime = () => {
				const video = videoRef.current;
				if (video != null && Number.isFinite(video.currentTime)) savedTimeRef.current = video.currentTime;
			};
			const handleMouseEnter = () => {
				setIsHoveredState(true);
			};
			const handleMouseLeave = () => {
				setIsHoveredState(false);
				saveCurrentTime();
			};
			const handleFocusIn = () => {
				setIsFocusWithin(true);
			};
			const handleFocusOut = (event: FocusEvent) => {
				const nextTarget = event.relatedTarget;
				if (nextTarget != null && container.contains(nextTarget as Node)) return;
				setIsFocusWithin(false);
				saveCurrentTime();
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
		}, [containerElement]);
		useEffect(() => {
			const video = videoRef.current;
			if (video == null) return;
			playbackRequestIdRef.current += 1;
			const playbackWanted = shouldLoadVideo && !isMediaViewerOpen && (gifAutoPlay || isHoveredState || isFocusWithin);
			if (!playbackWanted || !animatedMediaPlaybackAllowed) {
				video.autoplay = false;
				if (playbackWanted && currentRenderSourceKeyRef.current === renderSourceKey) {
					dispatchRenderEvent({type: GIFVRenderEventType.PLAYBACK_BLOCKED});
				}
				safePause(video);
				return;
			}
			video.autoplay = true;
			const target = savedTimeRef.current;
			if (Number.isFinite(target) && Math.abs(video.currentTime - target) > 0.01) {
				try {
					video.currentTime = target;
				} catch {}
			}
			requestVideoPlayback(video);
		}, [
			animatedMediaPlaybackAllowed,
			gifAutoPlay,
			isFocusWithin,
			isHoveredState,
			isMediaViewerOpen,
			renderSourceKey,
			requestVideoPlayback,
			shouldLoadVideo,
		]);
		if (shouldBlur) {
			const {style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
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
		const {style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
		const {
			showFavoriteButton,
			showDownloadButton: _showDownloadButton,
			showDeleteButton,
		} = getMediaButtonVisibility(canFavorite, isPreview ? undefined : message, attachmentId, {
			disableDelete: !!isPreview || snapshotIndex !== undefined,
		});
		const showDownloadButton = false;
		const showGifIndicator = Accessibility.showGifIndicator && shouldShowOverlays(dimensions.width, dimensions.height);
		const {width} = style;
		const aspectRatio =
			dimensions.width > 0 && dimensions.height > 0 ? `${dimensions.width} / ${dimensions.height}` : '';
		const containerStyle = {
			'--embed-aspect-ratio': aspectRatio || 'auto',
			'--embed-height': remFromPx(dimensions.height),
			'--embed-width': typeof width === 'number' ? remFromPx(width) : remFromPx(dimensions.width),
			maxWidth: '100%',
			width: remFromPx(dimensions.width),
			...(aspectRatio ? {aspectRatio} : {}),
		} as React.CSSProperties;
		let effectivePreload: 'none' | 'metadata' | 'auto' = 'none';
		if (shouldLoadVideo) {
			if (videoConfig && videoConfig.preload !== undefined) {
				effectivePreload = videoConfig.preload;
			} else {
				effectivePreload = gifAutoPlay ? 'auto' : 'metadata';
			}
		}
		const renderLayers = getGIFVRenderLayers(currentRenderState);
		const posterReady = loaded && !error;
		const showPoster = shouldLoadPoster && posterReady && renderLayers.showPoster;
		const showVideo = shouldLoadVideo && renderLayers.showVideo;
		let videoControls = false;
		let videoPlaysInline = true;
		let videoLoop = true;
		let videoMuted = true;
		if (videoConfig) {
			if (videoConfig.controls !== undefined) videoControls = videoConfig.controls;
			if (videoConfig.playsInline !== undefined) videoPlaysInline = videoConfig.playsInline;
			if (videoConfig.loop !== undefined) videoLoop = videoConfig.loop;
			if (videoConfig.muted !== undefined) videoMuted = videoConfig.muted;
		}
		return (
			<MediaContainer
				ref={setContainerRef}
				className={clsx(embedStyles.embedGifvContainer, styles.mediaContainer)}
				style={containerStyle}
				showFavoriteButton={showFavoriteButton}
				isFavorited={isFavorited}
				onFavoriteClick={toggleFavorite}
				showDownloadButton={showDownloadButton}
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
					<div className={styles.videoWrapper} data-flx="channel.embeds.media.embed-gifv.video-wrapper">
						{(renderLayers.showPlaceholder || !loaded || error) && thumbHashURL && (
							<img
								src={thumbHashURL}
								className={styles.thumbHashPlaceholder}
								alt={i18n._(LOADING_PLACEHOLDER_DESCRIPTOR)}
								data-flx="channel.embeds.media.embed-gifv.thumb-hash-placeholder--2"
							/>
						)}
						<motion.img
							ref={setPosterRef}
							className={styles.posterElement}
							alt={i18n._(LOADING_PLACEHOLDER_DESCRIPTOR)}
							src={shouldLoadPoster ? posterURL : undefined}
							loading="lazy"
							width={dimensions.width}
							height={dimensions.height}
							tabIndex={-1}
							onLoad={handlePosterLoad}
							onError={handlePosterError}
							initial={{opacity: cached || cachedOnMount ? 1 : 0}}
							animate={{opacity: showPoster ? 1 : 0}}
							transition={{
								duration:
									cached || cachedOnMount || Accessibility.useReducedMotion ? 0 : EMBED_MEDIA_FADE_DURATION_SECONDS,
							}}
							data-flx="channel.embeds.media.embed-gifv.poster-element"
						/>
						<motion.video
							className={styles.videoElement}
							controls={videoControls}
							playsInline={videoPlaysInline}
							loop={videoLoop}
							muted={videoMuted}
							poster={shouldLoadPoster ? posterURL : thumbHashURL}
							preload={effectivePreload}
							src={shouldLoadVideo ? videoProxyURL : undefined}
							ref={setVideoRef}
							aria-label={i18n._(ANIMATED_GIF_VIDEO_DESCRIPTOR)}
							data-embed-media="gifv"
							tabIndex={-1}
							width={dimensions.width}
							height={dimensions.height}
							onLoadedData={handleVideoReady}
							onCanPlay={handleVideoReady}
							onPlaying={handleVideoPlaying}
							onPause={handleVideoPause}
							onError={handleVideoError}
							initial={{opacity: 0}}
							animate={{opacity: showVideo ? 1 : 0}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : EMBED_MEDIA_FADE_DURATION_SECONDS}}
							data-flx="channel.embeds.media.embed-gifv.video-element"
						/>
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
		const isMobile = MobileLayout.enabled;
		const mediaCalculator = useEmbedMediaCalculator(layoutConstraints);
		const containerRef = useRef<HTMLDivElement>(null);
		const [containerElement, setContainerElement] = useState<HTMLDivElement | null>(null);
		const imgRef = useRef<HTMLImageElement>(null);
		const animatedImageRef = useRef<HTMLImageElement>(null);
		const freezeCanvasRef = useRef<HTMLCanvasElement>(null);
		const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({
			disabled: !isMobile,
			rememberKey: proxyURL,
		});
		const setContainerRef = useCallback(
			(node: HTMLDivElement | null) => {
				containerRef.current = node;
				setContainerElement(node);
				visibilityRef(node);
			},
			[visibilityRef],
		);
		const {dimensions} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
		const {width: displayWidth, height: displayHeight} = dimensions;
		const gifAutoPlay = useShouldAnimate({kind: 'gif', respectPlaybackAllowed: false});
		const animatedMediaPlaybackAllowed = useAnimatedMediaPlaybackAllowed();
		const baseProxyURL = stripMediaProxyParams(proxyURL);
		const animatedTargetWidth = Math.min(naturalWidth, Math.round(displayWidth * 2));
		const animatedTargetHeight = Math.min(naturalHeight, Math.round(displayHeight * 2));
		const naturalSizeIsKnown = naturalWidth > 0 && naturalHeight > 0;
		const shouldResizeAnimated =
			naturalSizeIsKnown && (animatedTargetWidth < naturalWidth || animatedTargetHeight < naturalHeight);
		const optimizedAnimatedURL = buildFittedAnimatedImageProxyURL(
			baseProxyURL,
			shouldResizeAnimated ? animatedTargetWidth : undefined,
			shouldResizeAnimated ? animatedTargetHeight : undefined,
		);
		const optimizedStaticURL = buildFittedStaticGifPreviewURL(
			baseProxyURL,
			Math.round(displayWidth * 2),
			Math.round(displayHeight * 2),
		);
		const animatedImageSourceRef = useRef(optimizedAnimatedURL);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const shouldLoadMedia = isNearViewport && !shouldBlur;
		const {
			loaded,
			error,
			cached,
			cachedOnMount,
			thumbHashURL,
			ref: mediaRef,
			onLoad: handleImageLoad,
			onError: handleImageError,
		} = useMediaLoading(optimizedStaticURL, placeholder, {
			enabled: shouldLoadMedia,
		});
		const setImgRef = useCallback(
			(node: HTMLImageElement | null) => {
				imgRef.current = node;
				mediaRef(node);
			},
			[mediaRef],
		);
		const [decoderCanvas, setDecoderCanvas] = useState<HTMLCanvasElement | null>(null);
		const isHoveredRef = useRef(false);
		const [hasStartedAnimating, setHasStartedAnimating] = useState(gifAutoPlay);
		const [isHoveredState, setIsHoveredState] = useState(false);
		const [renderedAnimatedURL, setRenderedAnimatedURL] = useState('');
		const animatedPreloadStateRef = useRef<AnimatedImagePreloadState>({
			url: optimizedAnimatedURL,
			status: ImageCacheUtils.hasImage(optimizedAnimatedURL) ? 'ready' : 'idle',
		});
		const [decoderRequested, setDecoderRequested] = useState(() => getAnimatedMediaPlaybackAllowed());
		const shouldUseDecoder = shouldLoadMedia && hasStartedAnimating;
		const decoderPlaying =
			shouldUseDecoder && decoderRequested && animatedMediaPlaybackAllowed && (gifAutoPlay || isHoveredState);
		const decoderState = useAnimatedImageDecoder({
			src: shouldUseDecoder && decoderRequested ? optimizedAnimatedURL : null,
			playing: decoderPlaying,
			canvas: decoderCanvas,
			maxCachedFrames: DECODER_MAX_CACHED_FRAMES,
		});
		const useDecoder =
			shouldUseDecoder && decoderRequested && decoderState.supported && hasStartedAnimating && !decoderState.error;
		const decoderCanvasVisible = useDecoder && decoderState.loaded;
		const shouldRenderFreezeFrame = !decoderCanvasVisible && (gifAutoPlay || hasStartedAnimating);
		const setAnimatedImageRef = useCallback(
			(node: HTMLImageElement | null) => {
				animatedImageRef.current = node;
				if (animatedImageSourceRef.current !== optimizedAnimatedURL) return;
				if (completeImageHasSource(node, optimizedAnimatedURL)) setRenderedAnimatedURL(optimizedAnimatedURL);
			},
			[optimizedAnimatedURL],
		);
		useLayoutEffect(() => {
			animatedImageSourceRef.current = optimizedAnimatedURL;
			setRenderedAnimatedURL('');
			if (completeImageHasSource(animatedImageRef.current, optimizedAnimatedURL)) {
				setRenderedAnimatedURL(optimizedAnimatedURL);
			}
		}, [optimizedAnimatedURL]);
		useLayoutEffect(() => {
			const nextState: AnimatedImagePreloadState = {
				url: optimizedAnimatedURL,
				status: ImageCacheUtils.hasImage(optimizedAnimatedURL) ? 'ready' : 'idle',
			};
			animatedPreloadStateRef.current = nextState;
		}, [optimizedAnimatedURL]);
		const handleAnimatedImageLoad = useCallback(
			(event: React.SyntheticEvent<HTMLImageElement>) => {
				const element = event.currentTarget;
				if (animatedImageSourceRef.current !== optimizedAnimatedURL) return;
				if (animatedImageRef.current !== element) return;
				if (!completeImageHasSource(element, optimizedAnimatedURL)) return;
				ImageCacheUtils.rememberImage(optimizedAnimatedURL, element);
				animatedPreloadStateRef.current = {url: optimizedAnimatedURL, status: 'ready'};
				setRenderedAnimatedURL(optimizedAnimatedURL);
			},
			[optimizedAnimatedURL],
		);
		const handleAnimatedImageError = useCallback(
			(event: React.SyntheticEvent<HTMLImageElement>) => {
				if (animatedImageSourceRef.current !== optimizedAnimatedURL) return;
				if (animatedImageRef.current !== event.currentTarget) return;
				if (!mediaElementHasSource(event.currentTarget, optimizedAnimatedURL)) return;
				setRenderedAnimatedURL('');
				ImageCacheUtils.forgetImage(optimizedAnimatedURL);
				animatedPreloadStateRef.current = {url: optimizedAnimatedURL, status: 'failed'};
			},
			[optimizedAnimatedURL],
		);
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
		const openImagePreview = useImagePreview({
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
		useEffect(() => {
			if (gifAutoPlay) setHasStartedAnimating(true);
		}, [gifAutoPlay]);
		useEffect(() => {
			if (!shouldUseDecoder) {
				setDecoderRequested(false);
				return;
			}
			if (animatedMediaPlaybackAllowed) {
				setDecoderRequested(true);
			}
		}, [animatedMediaPlaybackAllowed, shouldUseDecoder]);
		useEffect(() => {
			const shouldPreloadAnimated = shouldLoadMedia && animatedMediaPlaybackAllowed && optimizedAnimatedURL.length > 0;
			if (!shouldPreloadAnimated) return;
			const preloadState = animatedPreloadStateRef.current;
			if (preloadState.url !== optimizedAnimatedURL || preloadState.status !== 'idle') return;
			const loadingState: AnimatedImagePreloadState = {url: optimizedAnimatedURL, status: 'loading'};
			animatedPreloadStateRef.current = loadingState;
			let active = true;
			const cleanup = ImageCacheUtils.loadImage(
				optimizedAnimatedURL,
				() => {
					if (!active) return;
					const readyState: AnimatedImagePreloadState = {url: optimizedAnimatedURL, status: 'ready'};
					animatedPreloadStateRef.current = readyState;
				},
				() => {
					if (!active) return;
					const failedState: AnimatedImagePreloadState = {url: optimizedAnimatedURL, status: 'failed'};
					animatedPreloadStateRef.current = failedState;
				},
			);
			return () => {
				active = false;
				cleanup();
				if (animatedPreloadStateRef.current !== loadingState) return;
				const idleState: AnimatedImagePreloadState = {url: optimizedAnimatedURL, status: 'idle'};
				animatedPreloadStateRef.current = idleState;
			};
		}, [animatedMediaPlaybackAllowed, optimizedAnimatedURL, shouldLoadMedia]);
		const showFreezeFrame = useCallback(() => {
			const canvas = freezeCanvasRef.current;
			if (canvas == null) return;
			canvas.removeAttribute('data-frozen');
			const animatedImage = animatedImageRef.current;
			const staticImage = imgRef.current;
			let image: HTMLImageElement;
			if (completeImageHasSource(animatedImage, optimizedAnimatedURL)) {
				image = animatedImage;
			} else if (completeImageHasSource(staticImage, optimizedStaticURL)) {
				image = staticImage;
			} else {
				return;
			}
			const sourceWidth = image.naturalWidth;
			const sourceHeight = image.naturalHeight;
			if (canvas.width !== sourceWidth) canvas.width = sourceWidth;
			if (canvas.height !== sourceHeight) canvas.height = sourceHeight;
			const ctx = canvas.getContext('2d');
			if (ctx == null) return;
			try {
				ctx.clearRect(0, 0, sourceWidth, sourceHeight);
				ctx.drawImage(image, 0, 0, sourceWidth, sourceHeight);
			} catch {
				return;
			}
			canvas.dataset.frozen = 'true';
		}, [optimizedAnimatedURL, optimizedStaticURL]);
		const hideFreezeFrame = useCallback(() => {
			const canvas = freezeCanvasRef.current;
			if (canvas == null) return;
			canvas.removeAttribute('data-frozen');
		}, []);
		const resetFreezeFrame = useCallback(() => {
			const canvas = freezeCanvasRef.current;
			if (canvas == null) return;
			canvas.removeAttribute('data-frozen');
			canvas.width = 0;
			canvas.height = 0;
		}, []);
		useLayoutEffect(() => {
			resetFreezeFrame();
		}, [optimizedAnimatedURL, optimizedStaticURL, resetFreezeFrame]);
		useEffect(() => {
			if (!shouldLoadMedia || gifAutoPlay) return;
			const container = containerElement;
			if (container == null) return;
			const handleMouseEnter = () => {
				isHoveredRef.current = true;
				setIsHoveredState(true);
				if (!getAnimatedMediaPlaybackAllowed()) return;
				if (!hasStartedAnimating) {
					setHasStartedAnimating(true);
				}
				hideFreezeFrame();
			};
			const handleMouseLeave = () => {
				isHoveredRef.current = false;
				setIsHoveredState(false);
				if (!hasStartedAnimating) return;
				showFreezeFrame();
			};
			container.addEventListener('mouseenter', handleMouseEnter);
			container.addEventListener('mouseleave', handleMouseLeave);
			return () => {
				container.removeEventListener('mouseenter', handleMouseEnter);
				container.removeEventListener('mouseleave', handleMouseLeave);
			};
		}, [containerElement, gifAutoPlay, hasStartedAnimating, hideFreezeFrame, shouldLoadMedia, showFreezeFrame]);
		useEffect(() => {
			if (!shouldLoadMedia || gifAutoPlay) return;
			if (!animatedMediaPlaybackAllowed) {
				if (isHoveredRef.current && hasStartedAnimating) {
					showFreezeFrame();
				}
				return;
			}
			if (isHoveredRef.current) {
				if (!hasStartedAnimating) {
					setHasStartedAnimating(true);
				}
				hideFreezeFrame();
			}
		}, [
			animatedMediaPlaybackAllowed,
			gifAutoPlay,
			hasStartedAnimating,
			hideFreezeFrame,
			shouldLoadMedia,
			showFreezeFrame,
		]);
		useEffect(() => {
			if (!shouldLoadMedia || !shouldRenderFreezeFrame) return;
			if (!animatedMediaPlaybackAllowed) {
				showFreezeFrame();
				return;
			}
			if (gifAutoPlay || isHoveredRef.current) {
				hideFreezeFrame();
			}
		}, [
			animatedMediaPlaybackAllowed,
			gifAutoPlay,
			hideFreezeFrame,
			shouldLoadMedia,
			shouldRenderFreezeFrame,
			showFreezeFrame,
		]);
		if (shouldBlur) {
			const {style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
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
		const {style, dimensions: renderedDimensions} = mediaCalculator.calculate(
			{width: naturalWidth, height: naturalHeight},
			{forceScale: true},
		);
		const {showFavoriteButton, showDownloadButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview || snapshotIndex !== undefined},
		);
		const showGifIndicator =
			Accessibility.showGifIndicator && shouldShowOverlays(renderedDimensions.width, renderedDimensions.height);
		const {width} = style;
		const aspectRatio =
			renderedDimensions.width > 0 && renderedDimensions.height > 0
				? `${renderedDimensions.width} / ${renderedDimensions.height}`
				: '';
		const containerStyle = {
			'--embed-aspect-ratio': aspectRatio || 'auto',
			'--embed-height': remFromPx(renderedDimensions.height),
			'--embed-width': typeof width === 'number' ? remFromPx(width) : remFromPx(renderedDimensions.width),
			maxWidth: '100%',
			width: remFromPx(renderedDimensions.width),
			...(aspectRatio ? {aspectRatio} : {}),
		} as React.CSSProperties;
		const shouldUseAnimatedImage =
			shouldLoadMedia &&
			animatedMediaPlaybackAllowed &&
			!decoderCanvasVisible &&
			(gifAutoPlay || (hasStartedAnimating && isHoveredState));
		const animatedImageVisible = shouldUseAnimatedImage && renderedAnimatedURL === optimizedAnimatedURL;
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
				renderedWidth={renderedDimensions.width}
				renderedHeight={renderedDimensions.height}
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
					data-flx="channel.embeds.media.embed-gifv.embed-gif.image-preview-handler.gif"
				>
					<div className={styles.videoWrapper} data-flx="channel.embeds.media.embed-gifv.embed-gif.video-wrapper">
						{(!loaded || error) && thumbHashURL && (
							<img
								src={thumbHashURL}
								className={styles.thumbHashPlaceholder}
								alt={i18n._(LOADING_PLACEHOLDER_DESCRIPTOR)}
								data-flx="channel.embeds.media.embed-gifv.embed-gif.thumb-hash-placeholder--2"
							/>
						)}
						<motion.img
							ref={setImgRef}
							alt={i18n._(ANIMATED_GIF_DESCRIPTOR)}
							src={shouldLoadMedia ? optimizedStaticURL : undefined}
							className={styles.videoElement}
							data-embed-media="gif"
							loading={isMobile ? 'lazy' : 'eager'}
							tabIndex={-1}
							width={renderedDimensions.width}
							height={renderedDimensions.height}
							onLoad={handleImageLoad}
							onError={handleImageError}
							initial={{opacity: cached || cachedOnMount ? 1 : 0}}
							animate={{opacity: loaded && !decoderCanvasVisible ? 1 : 0}}
							transition={{
								duration:
									cached || cachedOnMount || Accessibility.useReducedMotion ? 0 : EMBED_MEDIA_FADE_DURATION_SECONDS,
							}}
							data-flx="channel.embeds.media.embed-gifv.embed-gif.video-element"
						/>
						{shouldUseAnimatedImage && (
							<motion.img
								ref={setAnimatedImageRef}
								alt={i18n._(ANIMATED_GIF_DESCRIPTOR)}
								src={optimizedAnimatedURL}
								className={styles.posterElement}
								data-embed-media="gif-animated"
								loading={isMobile ? 'lazy' : 'eager'}
								tabIndex={-1}
								width={renderedDimensions.width}
								height={renderedDimensions.height}
								onLoad={handleAnimatedImageLoad}
								onError={handleAnimatedImageError}
								initial={{opacity: 0}}
								animate={{opacity: animatedImageVisible ? 1 : 0}}
								transition={{duration: Accessibility.useReducedMotion ? 0 : EMBED_MEDIA_FADE_DURATION_SECONDS}}
								data-flx="channel.embeds.media.embed-gifv.embed-gif.animated-image"
							/>
						)}
						<canvas
							ref={setDecoderCanvas}
							className={clsx(
								styles.videoElement,
								decoderCanvasVisible ? styles.videoOpacityVisible : styles.videoOpacityHidden,
							)}
							style={{position: 'absolute', inset: 0, pointerEvents: 'none', zIndex: decoderCanvasVisible ? 3 : 0}}
							tabIndex={-1}
							aria-hidden={decoderCanvasVisible ? undefined : true}
							width={renderedDimensions.width}
							height={renderedDimensions.height}
							data-flx="channel.embeds.media.embed-gifv.embed-gif.video-element--2"
						/>
						{shouldRenderFreezeFrame && (
							<canvas
								ref={freezeCanvasRef}
								className={styles.gifFreezeFrame}
								data-frozen="false"
								tabIndex={-1}
								aria-hidden="true"
								data-flx="channel.embeds.media.embed-gifv.embed-gif.gif-freeze-frame"
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
