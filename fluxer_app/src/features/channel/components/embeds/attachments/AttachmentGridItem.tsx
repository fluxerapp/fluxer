// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SpoilerOverlay} from '@app/features/app/components/shared/SpoilerOverlay';
import spoilerStyles from '@app/features/app/components/shared/SpoilerOverlay.module.css';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {AltTextBadge} from '@app/features/channel/components/embeds/AltTextBadge';
import {MatureMediaBlurOverlay} from '@app/features/channel/components/embeds/MatureMediaBlurOverlay';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import {MediaContainer} from '@app/features/channel/components/embeds/media/MediaContainer';
import {MediaActionBottomSheet} from '@app/features/channel/components/MediaActionBottomSheet';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import {AddFavoriteMemeModal} from '@app/features/expressions/components/modals/AddFavoriteMemeModal';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import * as FavoriteMemeUtils from '@app/features/expressions/utils/FavoriteMemeUtils';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMediaLoading} from '@app/features/messaging/hooks/useMediaLoading';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {useOpenInBrowserOnMiddleClick} from '@app/features/messaging/hooks/useOpenInBrowserOnMiddleClick';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import {getMosaicMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {
	buildAnimatedImageProxyURL,
	buildMediaProxyURL,
	buildStaticGifPreviewURL,
	resolvePreferredImageFormat,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {
	attachmentsToViewerItems,
	determineMediaType,
	findViewerItemIndex,
} from '@app/features/messaging/utils/MediaViewerItemUtils';
import {useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/AttachmentGridItem.module.css';
import type {MediaType} from '@app/features/ui/action_menu/items/MediaMenuData';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlayIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, FC, KeyboardEvent, MouseEvent, ReactElement} from 'react';
import {useCallback, useMemo, useState} from 'react';

const OPEN_AUDIO_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open audio in full view',
	comment: 'Button or menu action label in the channel and chat attachment grid item. Keep it concise.',
});
const OPEN_VIDEO_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open video in full view',
	comment: 'Button or menu action label in the channel and chat attachment grid item. Keep it concise.',
});
const OPEN_ANIMATED_GIF_VIDEO_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open animated GIF video in full view',
	comment: 'Button or menu action label in the channel and chat attachment grid item. Keep it concise.',
});
const OPEN_ANIMATED_GIF_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open animated GIF in full view',
	comment: 'Button or menu action label in the channel and chat attachment grid item. Keep it concise.',
});
const OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open image in full view',
	comment: 'Button or menu action label in the channel and chat attachment grid item. Keep it concise.',
});

export type LayoutType =
	| 'single'
	| 'grid'
	| 'mosaic'
	| 'two'
	| 'three'
	| 'four'
	| 'five'
	| 'six'
	| 'seven'
	| 'eight'
	| 'nine'
	| 'ten';

export interface AttachmentGridItemProps {
	attachment: MessageAttachment;
	style?: CSSProperties;
	message?: Message;
	mediaAttachments: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
	snapshotIndex?: number;
	targetAspectRatio?: string;
}

export const AttachmentGridItem: FC<AttachmentGridItemProps> = observer(
	({attachment, style, message, mediaAttachments, isPreview, snapshotIndex, targetAspectRatio}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const attachmentMediaType = determineMediaType(attachment);
		const isVideo = attachmentMediaType === 'video';
		const isAudio = attachmentMediaType === 'audio';
		const isGifv = attachmentMediaType === 'gifv';
		const isAnimatedGif = attachmentMediaType === 'gif' || isGifv;
		const isMobile = MobileLayout.enabled;
		const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		const shouldAnimateGif = useShouldAnimate({kind: 'gif'});
		const {hidden: spoilerHidden, reveal: revealSpoiler} = useSpoilerState(isSpoiler, message?.channelId);
		const {shouldBlur, gateReason, canReveal, reveal: revealSensitiveMedia} = useMatureMedia(nsfw, message?.channelId);
		const wrapSpoiler = (node: ReactElement) =>
			isSpoiler ? (
				<SpoilerOverlay
					hidden={spoilerHidden}
					onReveal={revealSpoiler}
					className={spoilerStyles.gridMedia}
					data-flx="channel.embeds.attachments.attachment-grid-item.wrap-spoiler.spoiler-overlay"
				>
					{node}
				</SpoilerOverlay>
			) : (
				node
			);
		const mosaicDimensions = getMosaicMediaDimensions(message);
		const maxMosaicWidth = mosaicDimensions.maxWidth;
		let targetWidth = maxMosaicWidth;
		let targetHeight = maxMosaicWidth;
		if (
			typeof attachment.width === 'number' &&
			attachment.width > 0 &&
			typeof attachment.height === 'number' &&
			attachment.height > 0
		) {
			targetWidth = Math.min(attachment.width, maxMosaicWidth * 2);
			targetHeight = Math.max(1, Math.round((targetWidth / attachment.width) * attachment.height));
		}
		const proxyUrl = attachment.proxy_url ?? attachment.url ?? '';
		const isBlob = proxyUrl.startsWith('blob:');
		const isPlainGif = attachmentMediaType === 'gif';
		const thumbnailSrc =
			proxyUrl.length === 0
				? ''
				: isBlob
					? proxyUrl
					: isPlainGif
						? shouldAnimateGif
							? buildAnimatedImageProxyURL(proxyUrl, targetWidth, targetHeight)
							: buildStaticGifPreviewURL(proxyUrl, targetWidth, targetHeight)
						: buildMediaProxyURL(proxyUrl, {
								format: resolvePreferredImageFormat(attachment.content_type),
								width: targetWidth,
								height: targetHeight,
								animated: isAnimatedGif && shouldAnimateGif,
							});
		const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({
			disabled: !isMobile,
			rememberKey: thumbnailSrc,
		});
		const shouldLoadMedia = isNearViewport && !shouldBlur;
		const {
			loaded,
			error,
			cachedOnMount,
			thumbHashURL,
			ref: mediaRef,
			onLoad: handleImageLoad,
			onError: handleImageError,
		} = useMediaLoading(thumbnailSrc, attachment.placeholder, {
			enabled: shouldLoadMedia && !isAudio,
		});
		const memes = FavoriteMemes.memes;
		const isFavorited = attachment.content_hash
			? memes.some((meme) => meme.contentHash === attachment.content_hash)
			: false;
		const handleClick = useCallback(
			(event: MouseEvent | KeyboardEvent) => {
				if (shouldBlur) {
					event.preventDefault();
					event.stopPropagation();
					return;
				}
				if (event.type === 'keydown') {
					const keyEvent = event as KeyboardEvent;
					if (!isKeyboardActivationKey(keyEvent.key)) {
						return;
					}
					event.preventDefault();
				}
				const items = attachmentsToViewerItems(mediaAttachments);
				const currentIndex = findViewerItemIndex(items, attachment.id);
				MediaViewerCommands.openMediaViewer(items, currentIndex, {
					channelId: message?.channelId,
					messageId: message?.id,
					message,
					sourceChannel: messageViewContext?.channel,
				});
			},
			[attachment, message, messageViewContext?.channel, mediaAttachments, shouldBlur],
		);
		const openInBrowser = useOpenInBrowserOnMiddleClick(attachment.url ?? attachment.proxy_url ?? '', !shouldBlur);
		const handleFavoriteClick = useCallback(
			async (e: MouseEvent) => {
				e.stopPropagation();
				if (!message?.channelId || !message?.id) return;
				if (isFavorited && attachment.content_hash) {
					const meme = memes.find((m) => m.contentHash === attachment.content_hash);
					if (!meme) return;
					await FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
				} else {
					const defaultName = FavoriteMemeUtils.deriveDefaultNameFromAttachment(i18n, attachment);
					ModalCommands.push(
						modal(() => (
							<AddFavoriteMemeModal
								channelId={message.channelId}
								messageId={message.id}
								attachmentId={attachment.id}
								defaultName={defaultName}
								defaultAltText={attachment.filename}
								data-flx="channel.embeds.attachments.attachment-grid-item.handle-favorite-click.add-favorite-meme-modal"
							/>
						)),
					);
				}
			},
			[message, attachment, isFavorited, memes, i18n],
		);
		const handleDownloadClick = useCallback(
			(e: MouseEvent) => {
				e.stopPropagation();
				const type = (() => {
					if (isAudio) return 'audio';
					if (isVideo || isGifv) return 'video';
					if (attachmentMediaType === 'gif') return 'gif';
					return 'image';
				})();
				const downloadUrl = attachment.proxy_url ?? attachment.url ?? '';
				createDownloadHandler(downloadUrl, type)();
			},
			[attachment.proxy_url, attachment.url, attachmentMediaType, isAudio, isVideo, isGifv],
		);
		const isRealAttachment = !message?.attachments ? false : message.attachments.some((a) => a.id === attachment.id);
		const handleDeleteClick = useDeleteAttachment(message, isRealAttachment ? attachment.id : undefined);
		const [mediaSheetOpen, setMediaSheetOpen] = useState(false);
		const handleContextMenu = useCallback(
			(e: MouseEvent) => {
				if (!message) return;
				if (isPreview && snapshotIndex === undefined) return;
				e.preventDefault();
				e.stopPropagation();
				const mediaType = (() => {
					if (isAudio) return 'audio';
					if (isGifv) return 'gifv';
					if (isVideo) return 'video';
					if (attachmentMediaType === 'gif') return 'gif';
					return 'image';
				})();
				const defaultName = FavoriteMemeUtils.deriveDefaultNameFromAttachment(i18n, attachment);
				ContextMenuCommands.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						sourceChannel={messageViewContext?.channel}
						originalSrc={attachment.url ?? ''}
						proxyURL={attachment.proxy_url ?? attachment.url ?? ''}
						type={mediaType}
						contentHash={attachment.content_hash}
						attachmentId={attachment.id}
						defaultName={defaultName}
						defaultAltText={attachment.filename}
						naturalWidth={attachment.width}
						naturalHeight={attachment.height}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={isPreview ? () => {} : (messageViewContext?.handleDelete ?? (() => {}))}
						data-flx="channel.embeds.attachments.attachment-grid-item.handle-context-menu.media-context-menu"
					/>
				));
			},
			[
				message,
				attachment,
				attachmentMediaType,
				isAudio,
				isVideo,
				isGifv,
				isPreview,
				snapshotIndex,
				i18n,
				messageViewContext,
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
		const mediaType: MediaType = useMemo(() => {
			if (isAudio) return 'audio';
			if (isGifv) return 'gifv';
			if (isVideo) return 'video';
			if (isAnimatedGif) return 'gif';
			return 'image';
		}, [isAudio, isGifv, isVideo, isAnimatedGif]);
		const ariaLabel = useMemo(() => {
			if (isAudio) return i18n._(OPEN_AUDIO_IN_FULL_VIEW_DESCRIPTOR);
			if (isVideo) return i18n._(OPEN_VIDEO_IN_FULL_VIEW_DESCRIPTOR);
			if (isGifv) return i18n._(OPEN_ANIMATED_GIF_VIDEO_IN_FULL_VIEW_DESCRIPTOR);
			if (isAnimatedGif) return i18n._(OPEN_ANIMATED_GIF_IN_FULL_VIEW_DESCRIPTOR);
			return i18n._(OPEN_IMAGE_IN_FULL_VIEW_DESCRIPTOR);
		}, [isAudio, isVideo, isGifv, isAnimatedGif, i18n.locale]);
		const shouldRenderPlaceholder = !loaded || error;
		const canFavorite = !!(message?.channelId && message?.id);
		const {showFavoriteButton, showDownloadButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			isRealAttachment ? attachment.id : undefined,
			{disableDelete: !!isPreview || snapshotIndex !== undefined},
		);
		const gridItemStyle: CSSProperties = {
			...style,
			...(targetAspectRatio ? {aspectRatio: targetAspectRatio} : {}),
		};
		const defaultName = useMemo(
			() => FavoriteMemeUtils.deriveDefaultNameFromAttachment(i18n, attachment),
			[i18n.locale, attachment],
		);
		return wrapSpoiler(
			<>
				<MediaContainer
					ref={visibilityRef}
					className={styles.gridItem}
					style={gridItemStyle}
					showFavoriteButton={showFavoriteButton}
					isFavorited={isFavorited}
					onFavoriteClick={handleFavoriteClick}
					showDownloadButton={showDownloadButton}
					onDownloadClick={handleDownloadClick}
					showDeleteButton={showDeleteButton}
					onDeleteClick={handleDeleteClick}
					onContextMenu={handleContextMenu}
					onLongPress={handleLongPress}
					data-flx="channel.embeds.attachments.attachment-grid-item.grid-item.context-menu"
				>
					<div
						role="button"
						tabIndex={0}
						className={styles.clickableButton}
						onClick={handleClick}
						onMouseDown={openInBrowser.onMouseDown}
						onAuxClick={openInBrowser.onAuxClick}
						onKeyDown={handleClick}
						aria-label={ariaLabel}
						data-flx="channel.embeds.attachments.attachment-grid-item.clickable-button"
					>
						{isAudio ? (
							<div
								className={styles.audioPlaceholder}
								data-flx="channel.embeds.attachments.attachment-grid-item.audio-placeholder"
							>
								<SpeakerHighIcon
									weight="fill"
									data-flx="channel.embeds.attachments.attachment-grid-item.speaker-high-icon"
								/>
							</div>
						) : (
							<div
								className={styles.mediaContainer}
								data-flx="channel.embeds.attachments.attachment-grid-item.media-container"
							>
								<div
									className={styles.loadingOverlay}
									data-flx="channel.embeds.attachments.attachment-grid-item.loading-overlay"
								>
									{isAnimatedGif && (
										<div
											className={styles.gifIndicator}
											data-flx="channel.embeds.attachments.attachment-grid-item.gif-indicator"
										>
											GIF
										</div>
									)}
									<AnimatePresence data-flx="channel.embeds.attachments.attachment-grid-item.animate-presence">
										{shouldRenderPlaceholder && thumbHashURL && (
											<motion.img
												key="placeholder"
												initial={{opacity: 1}}
												exit={{opacity: 0}}
												transition={{duration: Accessibility.useReducedMotion ? 0 : 0.3}}
												src={thumbHashURL}
												alt=""
												className={styles.placeholderImage}
												data-flx="channel.embeds.attachments.attachment-grid-item.placeholder-image"
											/>
										)}
									</AnimatePresence>
									<motion.img
										src={shouldLoadMedia ? thumbnailSrc : undefined}
										ref={mediaRef}
										alt={attachment.filename}
										loading={isMobile ? 'lazy' : 'eager'}
										draggable={false}
										className={clsx(styles.mediaImage, shouldBlur && styles.mediaBlurred)}
										aria-hidden={shouldBlur}
										onLoad={handleImageLoad}
										onError={handleImageError}
										initial={{opacity: cachedOnMount ? 1 : 0}}
										animate={{opacity: shouldRenderPlaceholder ? 0 : 1}}
										transition={{duration: cachedOnMount || Accessibility.useReducedMotion ? 0 : 0.3}}
										data-flx="channel.embeds.attachments.attachment-grid-item.media-image"
									/>
									<AltTextBadge
										altText={attachment.description}
										onPopoutToggle={messageViewContext?.onPopoutToggle}
										data-flx="channel.embeds.attachments.attachment-grid-item.alt-text-badge"
									/>
								</div>
								{shouldBlur && (
									<div
										className={styles.matureOverlay}
										data-flx="channel.embeds.attachments.attachment-grid-item.mature-overlay"
									>
										<MatureMediaBlurOverlay
											reason={gateReason}
											canReveal={canReveal}
											onReveal={revealSensitiveMedia}
											data-flx="channel.embeds.attachments.attachment-grid-item.mature-media-blur-overlay"
										/>
									</div>
								)}
							</div>
						)}
						{(isVideo || isAudio) && (
							<div
								className={styles.playButtonOverlay}
								data-flx="channel.embeds.attachments.attachment-grid-item.play-button-overlay"
							>
								<div
									className={styles.playButton}
									data-flx="channel.embeds.attachments.attachment-grid-item.play-button"
								>
									<PlayIcon
										size={remFromPx(28)}
										weight="fill"
										aria-hidden="true"
										data-flx="channel.embeds.attachments.attachment-grid-item.play-icon"
									/>
								</div>
							</div>
						)}
					</div>
				</MediaContainer>
				{message && (
					<MediaActionBottomSheet
						isOpen={mediaSheetOpen}
						onClose={handleCloseMediaSheet}
						message={message}
						originalSrc={attachment.url ?? ''}
						proxyURL={attachment.proxy_url ?? attachment.url ?? ''}
						type={mediaType}
						contentHash={attachment.content_hash}
						attachmentId={attachment.id}
						defaultName={defaultName}
						defaultAltText={attachment.filename}
						naturalWidth={attachment.width}
						naturalHeight={attachment.height}
						handleDelete={messageViewContext?.handleDelete}
						sourceChannel={messageViewContext?.channel}
						data-flx="channel.embeds.attachments.attachment-grid-item.media-action-bottom-sheet"
					/>
				)}
			</>,
		);
	},
);
