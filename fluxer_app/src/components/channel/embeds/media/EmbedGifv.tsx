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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useRef} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import embedStyles from '~/components/channel/embeds/Embed.module.css';
import {deriveDefaultNameFromMessage} from '~/components/channel/embeds/EmbedUtils';
import {GifIndicator} from '~/components/channel/embeds/media/GifIndicator';
import {getMediaButtonVisibility} from '~/components/channel/embeds/media/MediaButtonUtils';
import {MediaContainer, shouldShowOverlays} from '~/components/channel/embeds/media/MediaContainer';
import type {BaseMediaProps} from '~/components/channel/embeds/media/MediaTypes';
import {NSFWBlurOverlay} from '~/components/channel/embeds/NSFWBlurOverlay';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import {useDeleteAttachment} from '~/hooks/useDeleteAttachment';
import {useMediaFavorite} from '~/hooks/useMediaFavorite';
import {useMediaLoading} from '~/hooks/useMediaLoading';
import {useNSFWMedia} from '~/hooks/useNSFWMedia';
import FocusManager from '~/lib/FocusManager';
import type {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import MediaViewerStore from '~/stores/MediaViewerStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {createCalculator} from '~/utils/DimensionUtils';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import {buildMediaProxyURL, stripMediaProxyParams} from '~/utils/MediaProxyUtils';
import styles from './EmbedGifv.module.css';

const MEDIA_CONFIG = {
	MAX_WIDTH: 400,
	MAX_HEIGHT: 300,
} as const;

const mediaCalculator = createCalculator({
	maxWidth: MEDIA_CONFIG.MAX_WIDTH,
	maxHeight: MEDIA_CONFIG.MAX_HEIGHT,
	responsive: true,
});

type GifvEmbedProps = BaseMediaProps & {
	embedURL: string;
	naturalWidth: number;
	naturalHeight: number;
	placeholder?: string;
};

interface VideoConfig {
	autoplay?: boolean;
	loop?: boolean;
	muted?: boolean;
	playsInline?: boolean;
	controls?: boolean;
	preload?: 'none' | 'metadata' | 'auto';
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
	message?: MessageRecord;
}) => {
	return useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			if (event.type === 'click' && (event as React.MouseEvent).button !== 0) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();

			MediaViewerActionCreators.openMediaViewer(
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
					},
				],
				0,
				{
					channelId,
					messageId,
					message,
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
	message?: MessageRecord;
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
		children,
	}) => {
		const {t} = useLingui();
		const openImagePreview = useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				if (event.type === 'click' && (event as React.MouseEvent).button !== 0) {
					return;
				}

				if (event.type === 'keydown') {
					const keyEvent = event as React.KeyboardEvent;
					if (keyEvent.key !== 'Enter' && keyEvent.key !== ' ') {
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
				MediaViewerActionCreators.openMediaViewer(
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
						},
					],
					0,
					{
						channelId,
						messageId,
						message,
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
			],
		);

		const altText = type === 'gifv' ? t`Animated GIF Video` : type === 'gif' ? t`Animated GIF` : t`Image`;

		return (
			<button
				type="button"
				className={styles.imagePreviewHandler}
				aria-label={t`Open ${altText} in full view`}
				onClick={openImagePreview}
				onKeyDown={openImagePreview}
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
	}
> = observer(
	({
		embedURL,
		videoProxyURL,
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
	}) => {
		const {t} = useLingui();
		const {loaded, error, thumbHashURL} = useMediaLoading(
			buildMediaProxyURL(videoProxyURL, {format: 'webp'}),
			placeholder,
		);
		const videoRef = useRef<HTMLVideoElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const {shouldBlur, gateReason} = useNSFWMedia(nsfw, channelId);

		const defaultName = deriveDefaultNameFromMessage({
			message,
			attachmentId,
			embedIndex,
			url: embedURL,
			proxyUrl: videoProxyURL,
		});

		const {toggleFavorite, isFavorited, canFavorite} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName: defaultName || 'GIF',
			contentHash,
			isGifv: true,
		});

		const {gifAutoPlay} = UserSettingsStore;
		const isMediaViewerOpen = MediaViewerStore.isOpen;

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
		});

		const handleDeleteClick = useDeleteAttachment(message, attachmentId);

		const handleDownloadClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createSaveHandler(videoProxyURL, 'video')();
			},
			[videoProxyURL],
		);

		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message || isPreview) return;

				e.preventDefault();
				e.stopPropagation();

				ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						originalSrc={embedURL}
						proxyURL={videoProxyURL}
						type="gifv"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
					/>
				));
			},
			[message, embedURL, videoProxyURL, contentHash, attachmentId, defaultName, onDelete, isPreview],
		);

		useEffect(() => {
			const video = videoRef.current;
			if (!video) return;

			video.loop = videoConfig?.loop ?? true;
			video.muted = videoConfig?.muted ?? true;
			video.playsInline = videoConfig?.playsInline ?? true;
			video.preload = videoConfig?.preload ?? 'auto';

			if (isMediaViewerOpen) {
				video.autoplay = false;
				video.pause();
				return;
			}

			if (gifAutoPlay && FocusManager.isFocused()) {
				video.autoplay = true;
				video.play().catch(() => {});
			} else {
				video.autoplay = false;
			}
		}, [videoConfig, gifAutoPlay, isMediaViewerOpen]);

		useEffect(() => {
			if (gifAutoPlay || isMediaViewerOpen) return;

			const video = videoRef.current;
			const container = containerRef.current;
			if (!video || !container) return;

			const handleMouseEnter = () => {
				if (FocusManager.isFocused()) {
					video.play().catch(() => {});
				}
			};
			const handleMouseLeave = () => {
				video.pause();
				video.currentTime = 0;
			};

			container.addEventListener('mouseenter', handleMouseEnter);
			container.addEventListener('mouseleave', handleMouseLeave);

			return () => {
				container.removeEventListener('mouseenter', handleMouseEnter);
				container.removeEventListener('mouseleave', handleMouseLeave);
			};
		}, [gifAutoPlay, isMediaViewerOpen]);

		useEffect(() => {
			const video = videoRef.current;
			if (!video) return;

			if (isMediaViewerOpen) {
				video.pause();
				return;
			}

			const unsubscribe = FocusManager.subscribe((focused) => {
				if (isMediaViewerOpen) {
					return;
				}
				if (!focused) {
					video.pause();
				} else if (gifAutoPlay) {
					video.play().catch(() => {});
				}
			});

			return unsubscribe;
		}, [gifAutoPlay, isMediaViewerOpen]);

		if (shouldBlur) {
			const {style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
			const {width: _width, height: _height, ...styleWithoutDimensions} = style;
			const blurContainerStyle = {...styleWithoutDimensions, maxWidth: '100%', width: '100%'};
			return (
				<div className={styles.blurContainer}>
					<div className={styles.blurContent} style={blurContainerStyle}>
						<div className={styles.blurInnerContainer}>
							{thumbHashURL && (
								<img src={thumbHashURL} className={styles.thumbHashPlaceholder} alt="" style={{filter: 'blur(40px)'}} />
							)}
						</div>
					</div>
					<NSFWBlurOverlay reason={gateReason} />
				</div>
			);
		}

		const {style, dimensions} = mediaCalculator.calculate(
			{width: naturalWidth, height: naturalHeight},
			{forceScale: true},
		);
		const {
			showFavoriteButton,
			showDownloadButton: _showDownloadButton,
			showDeleteButton,
		} = getMediaButtonVisibility(canFavorite, isPreview ? undefined : message, attachmentId, {
			disableDelete: !!isPreview,
		});
		const showDownloadButton = false;
		const showGifIndicator =
			AccessibilityStore.showGifIndicator && shouldShowOverlays(dimensions.width, dimensions.height);

		const {width, aspectRatio} = style;
		const containerStyle = {
			'--embed-width': `${width}px`,
			maxWidth: '100%',
			width,
			aspectRatio,
		} as React.CSSProperties;

		return (
			<MediaContainer
				ref={containerRef}
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
			>
				{showGifIndicator && <GifIndicator />}
				<ImagePreviewHandler
					src={videoProxyURL}
					originalSrc={embedURL}
					naturalWidth={naturalWidth}
					naturalHeight={naturalHeight}
					type="gifv"
					handlePress={openImagePreview}
				>
					<div className={styles.videoWrapper} style={aspectRatio ? {aspectRatio} : undefined}>
						{(!loaded || error) && thumbHashURL && (
							<img src={thumbHashURL} className={styles.thumbHashPlaceholder} alt={t`Loading placeholder`} />
						)}
						<video
							className={clsx(
								styles.videoElement,
								!loaded || error ? styles.videoOpacityHidden : styles.videoOpacityVisible,
							)}
							controls={videoConfig?.controls ?? false}
							playsInline={videoConfig?.playsInline ?? true}
							loop={videoConfig?.loop ?? true}
							muted={videoConfig?.muted ?? true}
							poster={buildMediaProxyURL(videoProxyURL, {format: 'webp'})}
							preload={videoConfig?.preload ?? 'auto'}
							src={videoProxyURL}
							ref={videoRef}
							aria-label={t`Animated GIF video`}
							tabIndex={-1}
						/>
					</div>
				</ImagePreviewHandler>
			</MediaContainer>
		);
	},
);

export const EmbedGif: FC<GifvEmbedProps & {proxyURL: string; includeButton?: boolean; isPreview?: boolean}> = observer(
	({
		embedURL,
		proxyURL,
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
	}) => {
		const {t} = useLingui();
		const {dimensions} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
		const {width: displayWidth, height: displayHeight} = dimensions;

		const baseProxyURL = stripMediaProxyParams(proxyURL);

		const optimizedAnimatedURL = buildMediaProxyURL(baseProxyURL, {
			width: Math.round(displayWidth * 2),
			height: Math.round(displayHeight * 2),
			animated: true,
		});

		const optimizedStaticURL = buildMediaProxyURL(baseProxyURL, {
			format: 'webp',
			width: Math.round(displayWidth * 2),
			height: Math.round(displayHeight * 2),
			animated: false,
		});

		const {loaded, error, thumbHashURL} = useMediaLoading(optimizedAnimatedURL, placeholder);
		const {shouldBlur, gateReason} = useNSFWMedia(nsfw, channelId);
		const containerRef = useRef<HTMLDivElement>(null);
		const imgRef = useRef<HTMLImageElement>(null);
		const isHoveredRef = useRef(false);

		const defaultName = deriveDefaultNameFromMessage({
			message,
			attachmentId,
			embedIndex,
			url: embedURL,
			proxyUrl: proxyURL,
		});

		const {toggleFavorite, isFavorited, canFavorite} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName: defaultName || 'GIF',
			contentHash,
		});

		const {gifAutoPlay} = UserSettingsStore;

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
		});

		const handleDeleteClick = useDeleteAttachment(message, attachmentId);

		const handleDownloadClickGif = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createSaveHandler(proxyURL, 'image')();
			},
			[proxyURL],
		);

		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message || isPreview) return;

				e.preventDefault();
				e.stopPropagation();

				ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						originalSrc={embedURL}
						proxyURL={proxyURL}
						type="gif"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
					/>
				));
			},
			[message, embedURL, proxyURL, contentHash, attachmentId, defaultName, onDelete, isPreview],
		);

		useEffect(() => {
			if (gifAutoPlay) return;

			const container = containerRef.current;
			if (!container) return;

			const handleMouseEnter = () => {
				isHoveredRef.current = true;
				if (FocusManager.isFocused()) {
					const img = imgRef.current;
					if (img) {
						img.src = optimizedAnimatedURL;
					}
				}
			};
			const handleMouseLeave = () => {
				isHoveredRef.current = false;
				const img = imgRef.current;
				if (img) {
					img.src = optimizedStaticURL;
				}
			};

			container.addEventListener('mouseenter', handleMouseEnter);
			container.addEventListener('mouseleave', handleMouseLeave);

			return () => {
				container.removeEventListener('mouseenter', handleMouseEnter);
				container.removeEventListener('mouseleave', handleMouseLeave);
			};
		}, [gifAutoPlay, optimizedAnimatedURL, optimizedStaticURL]);

		useEffect(() => {
			if (gifAutoPlay) return;

			const unsubscribe = FocusManager.subscribe((focused) => {
				const img = imgRef.current;
				if (!img) return;
				if (!focused) {
					img.src = optimizedStaticURL;
					return;
				}
				if (isHoveredRef.current && focused) {
					img.src = optimizedAnimatedURL;
				}
			});

			return unsubscribe;
		}, [gifAutoPlay, optimizedAnimatedURL, optimizedStaticURL]);

		if (shouldBlur) {
			const {style} = mediaCalculator.calculate({width: naturalWidth, height: naturalHeight}, {forceScale: true});
			const {width: _width, height: _height, ...styleWithoutDimensions} = style;
			const blurContainerStyle = {...styleWithoutDimensions, maxWidth: '100%', width: '100%'};
			return (
				<div className={styles.blurContainer}>
					<div className={styles.blurContent} style={blurContainerStyle}>
						<div className={styles.blurInnerContainer}>
							{thumbHashURL && (
								<img src={thumbHashURL} className={styles.thumbHashPlaceholder} alt="" style={{filter: 'blur(40px)'}} />
							)}
						</div>
					</div>
					<NSFWBlurOverlay reason={gateReason} />
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
			{disableDelete: !!isPreview},
		);
		const showGifIndicator =
			AccessibilityStore.showGifIndicator && shouldShowOverlays(renderedDimensions.width, renderedDimensions.height);

		const {width, aspectRatio} = style;
		const containerStyle = {
			'--embed-width': `${width}px`,
			maxWidth: '100%',
			width,
			aspectRatio,
		} as React.CSSProperties;

		return (
			<MediaContainer
				ref={containerRef}
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
			>
				{showGifIndicator && <GifIndicator />}
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
				>
					<div className={styles.videoWrapper} style={aspectRatio ? {aspectRatio} : undefined}>
						{(!loaded || error) && thumbHashURL && (
							<img src={thumbHashURL} className={styles.thumbHashPlaceholder} alt={t`Loading placeholder`} />
						)}
						<img
							ref={imgRef}
							alt={t`Animated GIF`}
							src={gifAutoPlay ? optimizedAnimatedURL : optimizedStaticURL}
							className={clsx(
								styles.videoElement,
								!loaded || error ? styles.videoOpacityHidden : styles.videoOpacityVisible,
							)}
							loading="lazy"
							tabIndex={-1}
						/>
					</div>
				</ImagePreviewHandler>
			</MediaContainer>
		);
	},
);
