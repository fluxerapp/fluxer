// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {AltTextBadge} from '@app/features/channel/components/embeds/AltTextBadge';
import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import {MatureMediaBlurOverlay} from '@app/features/channel/components/embeds/MatureMediaBlurOverlay';
import styles from '@app/features/channel/components/embeds/media/EmbedImage.module.css';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import {MediaContainer} from '@app/features/channel/components/embeds/media/MediaContainer';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import {MediaActionBottomSheet} from '@app/features/channel/components/MediaActionBottomSheet';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import {useMediaLoading} from '@app/features/messaging/hooks/useMediaLoading';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {useOpenInBrowserOnMiddleClick} from '@app/features/messaging/hooks/useOpenInBrowserOnMiddleClick';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import {buildStaticGifPreviewURL, stripMediaProxyParams} from '@app/features/messaging/utils/MediaProxyUtils';
import {attachmentsToViewerItems, findViewerItemIndex} from '@app/features/messaging/utils/MediaViewerItemUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useMemo, useState} from 'react';

const OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open image in full view',
	comment: 'Button or menu action label in the channel and chat embed image. Keep it concise.',
});
const LOADING_DESCRIPTOR = msg({
	message: 'Loading: {alt}',
	comment: 'Short label in the channel and chat embed image. Keep it concise. Preserve {alt}; it is inserted by code.',
});
const LOADING_IMAGE_DESCRIPTOR = msg({
	message: 'Loading image',
	comment: 'Short label in the channel and chat embed image. Keep it concise.',
});
const IMAGE_DESCRIPTOR = msg({
	message: 'Image',
	comment: 'Short label in the channel and chat embed image. Keep it concise.',
});
const IMAGE_CONFIG = {
	MAX_WIDTH: 400,
} as const;
const imageCalculator = createCalculator({
	maxWidth: IMAGE_CONFIG.MAX_WIDTH,
	responsive: true,
});

interface ImagePreviewHandlerProps {
	src: string;
	originalSrc: string;
	naturalWidth: number;
	naturalHeight: number;
	contentHash?: string | null;
	embedIndex?: number;
	handlePress?: (event: React.MouseEvent | React.KeyboardEvent) => void;
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	message?: Message;
	animated?: boolean;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	children: React.ReactNode;
}

type EmbedImageProps = React.ImgHTMLAttributes<HTMLImageElement> &
	BaseMediaProps & {
		src: string;
		originalSrc: string;
		naturalWidth: number;
		naturalHeight: number;
		width: number;
		height: number;
		placeholder?: string;
		constrain?: boolean;
		isInline?: boolean;
		handlePress?: (event: React.MouseEvent | React.KeyboardEvent) => void;
		alt?: string;
		mediaAttachments?: ReadonlyArray<MessageAttachment>;
		isPreview?: boolean;
		snapshotIndex?: number;
		animated?: boolean;
	};

const ImagePreviewHandler: FC<ImagePreviewHandlerProps> = observer(
	({
		src,
		originalSrc,
		naturalWidth,
		naturalHeight,
		contentHash,
		embedIndex,
		handlePress,
		channelId,
		messageId,
		attachmentId,
		message,
		animated,
		mediaAttachments = [],
		children,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
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
				if (mediaAttachments.length > 0) {
					const items = attachmentsToViewerItems(mediaAttachments);
					const currentIndex = findViewerItemIndex(items, attachmentId);
					MediaViewerCommands.openMediaViewer(items, currentIndex, {
						channelId,
						messageId,
						message,
						sourceChannel: messageViewContext?.channel,
					});
				} else {
					MediaViewerCommands.openMediaViewer(
						[
							{
								src,
								originalSrc: originalSrc ?? '',
								naturalWidth,
								naturalHeight,
								type: 'image' as const,
								contentHash,
								embedIndex,
								expiresAt: undefined,
								expired: undefined,
								animated,
							},
						],
						0,
						{
							channelId,
							messageId,
							message,
							sourceChannel: messageViewContext?.channel,
						},
					);
				}
			},
			[
				src,
				originalSrc,
				naturalWidth,
				naturalHeight,
				contentHash,
				embedIndex,
				handlePress,
				channelId,
				messageId,
				attachmentId,
				message,
				messageViewContext?.channel,
				mediaAttachments,
			],
		);
		const openInBrowser = useOpenInBrowserOnMiddleClick(originalSrc || src);
		return (
			<FocusRing offset={-2} data-flx="channel.embeds.media.embed-image.image-preview-handler.focus-ring">
				<button
					type="button"
					className={styles.imagePreviewHandler}
					aria-label={i18n._(OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR)}
					onClick={openImagePreview}
					onMouseDown={openInBrowser.onMouseDown}
					onAuxClick={openInBrowser.onAuxClick}
					onKeyDown={openImagePreview}
					data-flx="channel.embeds.media.embed-image.image-preview-handler.image-preview-handler.open-image-preview.button"
				>
					{children}
				</button>
			</FocusRing>
		);
	},
);
export const EmbedImage: FC<EmbedImageProps> = observer(
	({
		src,
		originalSrc,
		naturalWidth,
		naturalHeight,
		width,
		height,
		placeholder,
		constrain,
		className,
		isInline,
		handlePress,
		alt = '',
		nsfw,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		message,
		contentHash,
		onDelete,
		mediaAttachments = [],
		isPreview,
		snapshotIndex,
		animated = false,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const isMobile = MobileLayout.enabled;
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, channelId);
		const shouldAnimateImage = useShouldAnimate({kind: 'gif'});
		const {style: containerStyle, dimensions} = imageCalculator.calculate(
			{width, height},
			{
				preserve: constrain,
				responsive: !isInline,
				aspectRatio: true,
			},
		);
		const effectiveSrc = useMemo(() => {
			if (!animated || shouldAnimateImage || src.startsWith('blob:')) return src;
			return buildStaticGifPreviewURL(src, Math.round(dimensions.width * 2), Math.round(dimensions.height * 2));
		}, [animated, dimensions.height, dimensions.width, shouldAnimateImage, src]);
		const staticPreviewSrc = useMemo(() => {
			if (!animated || src.startsWith('blob:')) return src;
			return buildStaticGifPreviewURL(src, Math.round(dimensions.width * 2), Math.round(dimensions.height * 2));
		}, [animated, dimensions.height, dimensions.width, src]);
		const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({
			disabled: !isMobile,
			rememberKey: effectiveSrc,
		});
		const shouldLoadMedia = isNearViewport && !shouldBlur;
		const staticImageSrc = animated && !src.startsWith('blob:') ? staticPreviewSrc : effectiveSrc;
		const animatedImageSource =
			animated && !src.startsWith('blob:') && shouldAnimateImage && shouldLoadMedia && effectiveSrc === src ? src : '';
		const {
			loaded,
			error,
			cachedOnMount,
			thumbHashURL,
			ref: mediaRef,
			onLoad: handleImageLoad,
			onError: handleImageError,
		} = useMediaLoading(staticImageSrc, placeholder, {enabled: shouldLoadMedia});
		const {
			loaded: animatedLoaded,
			error: animatedError,
			cachedOnMount: animatedCachedOnMount,
			ref: animatedMediaRef,
			onLoad: handleAnimatedImageLoad,
			onError: handleAnimatedImageError,
		} = useMediaLoading(animatedImageSource, placeholder, {enabled: animatedImageSource.length > 0});
		const defaultName = deriveDefaultNameFromMessage({
			message,
			attachmentId,
			embedIndex,
			url: originalSrc,
			proxyUrl: src,
		});
		const {
			isFavorited,
			toggleFavorite: handleFavoriteClick,
			canFavorite,
		} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName: alt || defaultName,
			defaultAltText: alt,
			contentHash,
			isGifv: animated,
			embedURL: animated ? originalSrc : undefined,
			proxyURL: animated ? src : undefined,
			naturalWidth: animated ? naturalWidth : undefined,
			naturalHeight: animated ? naturalHeight : undefined,
		});
		const resolvedContainerStyle: React.CSSProperties = {
			...containerStyle,
			width: remFromPx(dimensions.width),
			maxWidth: '100%',
		};
		const shouldRenderPlaceholder = error || !loaded;
		const handleDownloadClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				const downloadSrc = src !== originalSrc ? stripMediaProxyParams(src) : originalSrc;
				createDownloadHandler(downloadSrc, 'image')();
			},
			[originalSrc, src],
		);
		const handleDeleteClick = useDeleteAttachment(message, attachmentId);
		const [mediaSheetOpen, setMediaSheetOpen] = useState(false);
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
						originalSrc={originalSrc}
						proxyURL={src}
						type="image"
						contentHash={contentHash}
						attachmentId={attachmentId}
						embedIndex={embedIndex}
						defaultName={alt || defaultName}
						defaultAltText={alt}
						naturalWidth={naturalWidth}
						naturalHeight={naturalHeight}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.embed-image.handle-context-menu.media-context-menu.image"
					/>
				));
			},
			[
				message,
				messageViewContext?.channel,
				src,
				originalSrc,
				contentHash,
				attachmentId,
				embedIndex,
				alt,
				defaultName,
				naturalWidth,
				naturalHeight,
				onDelete,
				isPreview,
				snapshotIndex,
			],
		);
		const handleLongPress = useCallback(() => {
			if (!message) return;
			if (isPreview && snapshotIndex === undefined) return;
			setMediaSheetOpen(true);
		}, [message, isPreview, snapshotIndex]);
		const handleCloseMediaSheet = useCallback(() => {
			setMediaSheetOpen(false);
		}, []);
		if (shouldBlur) {
			return (
				<div
					ref={visibilityRef}
					className={styles.blurContainer}
					data-flx="channel.embeds.media.embed-image.blur-container"
				>
					<div
						className={clsx(styles.rowContainer, isInline && styles.justifyEnd)}
						data-flx="channel.embeds.media.embed-image.row-container"
					>
						<div className={styles.innerContainer} data-flx="channel.embeds.media.embed-image.inner-container">
							<div
								className={styles.imageWrapper}
								style={resolvedContainerStyle}
								data-flx="channel.embeds.media.embed-image.image-wrapper"
							>
								<div className={styles.imageContainer} data-flx="channel.embeds.media.embed-image.image-container">
									{thumbHashURL && (
										<div
											className={styles.thumbHashContainer}
											data-flx="channel.embeds.media.embed-image.thumb-hash-container"
										>
											<img
												src={thumbHashURL}
												className={styles.thumbHashImage}
												alt=""
												loading={isMobile ? 'lazy' : 'eager'}
												style={{filter: 'blur(40px)'}}
												data-flx="channel.embeds.media.embed-image.thumb-hash-image"
											/>
										</div>
									)}
								</div>
								<MatureMediaBlurOverlay
									reason={gateReason}
									canReveal={canReveal}
									onReveal={revealSensitiveMedia}
									data-flx="channel.embeds.media.embed-image.mature-media-blur-overlay"
								/>
							</div>
						</div>
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
		return (
			<>
				<div className={styles.container} data-flx="channel.embeds.media.embed-image.container">
					<div
						className={clsx(styles.rowContainer, isInline && styles.justifyEnd)}
						data-flx="channel.embeds.media.embed-image.row-container--2"
					>
						<MediaContainer
							ref={visibilityRef}
							className={clsx(styles.mediaContainer, styles.cursorPointer)}
							style={resolvedContainerStyle}
							showFavoriteButton={showFavoriteButton}
							isFavorited={isFavorited}
							onFavoriteClick={handleFavoriteClick}
							showDownloadButton={showDownloadButton}
							onDownloadClick={handleDownloadClick}
							showDeleteButton={showDeleteButton}
							onDeleteClick={handleDeleteClick}
							onContextMenu={handleContextMenu}
							onLongPress={handleLongPress}
							renderedWidth={dimensions.width}
							renderedHeight={dimensions.height}
							data-flx="channel.embeds.media.embed-image.media-container.context-menu"
						>
							<ImagePreviewHandler
								src={src}
								originalSrc={originalSrc}
								naturalWidth={naturalWidth}
								naturalHeight={naturalHeight}
								contentHash={contentHash}
								embedIndex={embedIndex}
								handlePress={handlePress}
								channelId={channelId}
								messageId={messageId}
								attachmentId={attachmentId}
								message={message}
								mediaAttachments={mediaAttachments}
								animated={animated}
								data-flx="channel.embeds.media.embed-image.image-preview-handler"
							>
								<div
									className={styles.imageInnerContainer}
									data-flx="channel.embeds.media.embed-image.image-inner-container"
								>
									{shouldRenderPlaceholder && thumbHashURL && (
										<div
											className={styles.thumbHashContainer}
											data-flx="channel.embeds.media.embed-image.thumb-hash-container--2"
										>
											<img
												src={thumbHashURL}
												className={styles.thumbHashImage}
												alt={alt ? i18n._(LOADING_DESCRIPTOR, {alt}) : i18n._(LOADING_IMAGE_DESCRIPTOR)}
												loading={isMobile ? 'lazy' : 'eager'}
												data-flx="channel.embeds.media.embed-image.thumb-hash-image--2"
											/>
										</div>
									)}
									<motion.img
										alt={alt || i18n._(IMAGE_DESCRIPTOR)}
										src={shouldLoadMedia ? staticImageSrc : undefined}
										ref={mediaRef}
										width={naturalWidth}
										height={naturalHeight}
										className={clsx(styles.imageElement, className)}
										loading={isMobile ? 'lazy' : 'eager'}
										tabIndex={-1}
										onLoad={handleImageLoad}
										onError={handleImageError}
										initial={{opacity: cachedOnMount ? 1 : 0}}
										animate={{opacity: shouldRenderPlaceholder ? 0 : 1}}
										transition={{duration: cachedOnMount || Accessibility.useReducedMotion ? 0 : 0.2}}
										data-flx="channel.embeds.media.embed-image.image-element"
									/>
									{animatedImageSource.length > 0 && (
										<motion.img
											alt=""
											aria-hidden="true"
											src={animatedImageSource}
											ref={animatedMediaRef}
											width={naturalWidth}
											height={naturalHeight}
											className={clsx(styles.imageElement, styles.animatedImageElement)}
											loading={isMobile ? 'lazy' : 'eager'}
											tabIndex={-1}
											onLoad={handleAnimatedImageLoad}
											onError={handleAnimatedImageError}
											initial={{opacity: animatedCachedOnMount ? 1 : 0}}
											animate={{opacity: animatedLoaded && !animatedError ? 1 : 0}}
											transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
											data-flx="channel.embeds.media.embed-image.animated-image-element"
										/>
									)}
								</div>
							</ImagePreviewHandler>
							<AltTextBadge
								altText={alt}
								onPopoutToggle={messageViewContext?.onPopoutToggle}
								data-flx="channel.embeds.media.embed-image.alt-text-badge"
							/>
						</MediaContainer>
					</div>
				</div>
				{message && (
					<MediaActionBottomSheet
						isOpen={mediaSheetOpen}
						onClose={handleCloseMediaSheet}
						message={message}
						originalSrc={originalSrc}
						proxyURL={src}
						type={animated ? 'gif' : 'image'}
						contentHash={contentHash}
						attachmentId={attachmentId}
						embedIndex={embedIndex}
						defaultName={alt || defaultName}
						defaultAltText={alt}
						naturalWidth={naturalWidth}
						naturalHeight={naturalHeight}
						handleDelete={onDelete}
						sourceChannel={messageViewContext?.channel}
						data-flx="channel.embeds.media.embed-image.media-action-bottom-sheet"
					/>
				)}
			</>
		);
	},
);
