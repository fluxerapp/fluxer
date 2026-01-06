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
import {type FC, useCallback} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import {deriveDefaultNameFromMessage} from '~/components/channel/embeds/EmbedUtils';
import {getMediaButtonVisibility} from '~/components/channel/embeds/media/MediaButtonUtils';
import {MediaContainer} from '~/components/channel/embeds/media/MediaContainer';
import type {BaseMediaProps} from '~/components/channel/embeds/media/MediaTypes';
import {NSFWBlurOverlay} from '~/components/channel/embeds/NSFWBlurOverlay';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import {useDeleteAttachment} from '~/hooks/useDeleteAttachment';
import {useMediaFavorite} from '~/hooks/useMediaFavorite';
import {useMediaLoading} from '~/hooks/useMediaLoading';
import {useNSFWMedia} from '~/hooks/useNSFWMedia';
import type {MessageAttachment, MessageRecord} from '~/records/MessageRecord';
import {createCalculator} from '~/utils/DimensionUtils';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import styles from './EmbedImage.module.css';

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
	message?: MessageRecord;
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
		mediaAttachments = [],
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

				if (mediaAttachments.length > 0) {
					const currentIndex = mediaAttachments.findIndex((a) => a.id === attachmentId);

					const items = mediaAttachments.map((att) => ({
						src: att.proxy_url ?? att.url ?? '',
						originalSrc: att.url ?? '',
						naturalWidth: att.width!,
						naturalHeight: att.height!,
						type: 'image' as const,
						contentHash: att.content_hash,
						attachmentId: att.id,
						expiresAt: att.expires_at ?? null,
						expired: att.expired ?? false,
					}));

					MediaViewerActionCreators.openMediaViewer(items, currentIndex, {
						channelId,
						messageId,
						message,
					});
				} else {
					MediaViewerActionCreators.openMediaViewer(
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
							},
						],
						0,
						{
							channelId,
							messageId,
							message,
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
				mediaAttachments,
			],
		);

		return (
			<button
				type="button"
				className={styles.imagePreviewHandler}
				aria-label={t`Open image in full view`}
				onClick={openImagePreview}
				onKeyDown={openImagePreview}
			>
				{children}
			</button>
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
	}) => {
		const {t} = useLingui();
		const {loaded, error, thumbHashURL} = useMediaLoading(src, placeholder);
		const {shouldBlur, gateReason} = useNSFWMedia(nsfw, channelId);

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
		});

		const {style: containerStyle, dimensions} = imageCalculator.calculate(
			{width, height},
			{
				preserve: constrain,
				responsive: !isInline,
				aspectRatio: true,
			},
		);
		const resolvedContainerStyle: React.CSSProperties = {
			...containerStyle,
			width: dimensions.width,
			maxWidth: '100%',
		};

		const shouldRenderPlaceholder = error || !loaded;

		const handleDownloadClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				createSaveHandler(originalSrc, 'image')();
			},
			[originalSrc],
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
						originalSrc={originalSrc}
						proxyURL={src}
						type="image"
						contentHash={contentHash}
						attachmentId={attachmentId}
						embedIndex={embedIndex}
						defaultName={alt || defaultName}
						defaultAltText={alt}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
					/>
				));
			},
			[message, src, originalSrc, contentHash, attachmentId, embedIndex, alt, defaultName, onDelete, isPreview],
		);

		if (shouldBlur) {
			return (
				<div className={styles.blurContainer}>
					<div className={clsx(styles.rowContainer, isInline && styles.justifyEnd)}>
						<div className={styles.innerContainer}>
							<div className={styles.imageWrapper} style={resolvedContainerStyle}>
								<div className={styles.imageContainer}>
									{thumbHashURL && (
										<div className={styles.thumbHashContainer}>
											<img
												src={thumbHashURL}
												className={styles.thumbHashImage}
												alt=""
												loading="lazy"
												style={{filter: 'blur(40px)'}}
											/>
										</div>
									)}
								</div>
							</div>
							<NSFWBlurOverlay reason={gateReason} />
						</div>
					</div>
				</div>
			);
		}

		const {showFavoriteButton, showDownloadButton, showDeleteButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview},
		);

		return (
			<div className={styles.container}>
				<div className={clsx(styles.rowContainer, isInline && styles.justifyEnd)}>
					<MediaContainer
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
						renderedWidth={dimensions.width}
						renderedHeight={dimensions.height}
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
						>
							<div className={styles.imageInnerContainer}>
								{shouldRenderPlaceholder && thumbHashURL && (
									<div className={styles.thumbHashContainer}>
										<img
											src={thumbHashURL}
											className={styles.thumbHashImage}
											alt={alt ? t`Loading: ${alt}` : t`Loading image`}
											loading="lazy"
										/>
									</div>
								)}
								<img
									alt={alt || t`Image`}
									src={src}
									className={clsx(
										styles.imageElement,
										shouldRenderPlaceholder ? styles.opacityHidden : styles.opacityVisible,
										className,
									)}
									loading="lazy"
									tabIndex={-1}
								/>
							</div>
						</ImagePreviewHandler>
					</MediaContainer>
				</div>
			</div>
		);
	},
);
