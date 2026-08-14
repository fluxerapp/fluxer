// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import mediaStyles from '@app/features/channel/components/embeds/media/MediaContainer.module.css';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	DELETE_ATTACHMENT_DESCRIPTOR,
	DOWNLOAD_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon, StarIcon, TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {forwardRef, type ReactNode} from 'react';

const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment:
		'Button or menu action label in the channel and chat media container. Keep it concise. Keep the tone plain and specific.',
});
const DOWNLOAD_MEDIA_DESCRIPTOR = msg({
	message: 'Download media',
	comment: 'Button or menu action label in the channel and chat media container. Keep it concise.',
});
const MIN_SIZE_FOR_OVERLAYS = 120;
export const shouldShowOverlays = (renderedWidth?: number, renderedHeight?: number): boolean => {
	if (renderedWidth === undefined || renderedHeight === undefined) {
		return true;
	}
	return renderedWidth >= MIN_SIZE_FOR_OVERLAYS && renderedHeight >= MIN_SIZE_FOR_OVERLAYS;
};

type LongPressEvent = React.PointerEvent<HTMLDivElement> | React.TouchEvent<HTMLDivElement>;

const clearTextSelection = () => {
	window.getSelection()?.removeAllRanges();
};

interface MediaContainerProps {
	children: ReactNode;
	className?: string;
	style?: React.CSSProperties;
	showFavoriteButton?: boolean;
	isFavorited?: boolean;
	onFavoriteClick?: (e: React.MouseEvent) => void;
	showDownloadButton?: boolean;
	onDownloadClick?: (e: React.MouseEvent) => void;
	showDeleteButton?: boolean;
	onDeleteClick?: (e: React.MouseEvent) => void;
	onContextMenu?: (e: React.MouseEvent) => void;
	onMouseEnter?: (e: React.MouseEvent) => void;
	onMouseLeave?: (e: React.MouseEvent) => void;
	onLongPress?: (e: LongPressEvent) => void;
	renderedWidth?: number;
	renderedHeight?: number;
	forceShowFavoriteButton?: boolean;
}

export const MediaContainer = observer(
	forwardRef<HTMLDivElement, MediaContainerProps>(
		(
			{
				children,
				className,
				style,
				showFavoriteButton = false,
				isFavorited = false,
				onFavoriteClick,
				showDownloadButton = false,
				onDownloadClick,
				showDeleteButton = false,
				onDeleteClick,
				onContextMenu,
				onMouseEnter,
				onMouseLeave,
				onLongPress,
				renderedWidth,
				renderedHeight,
				forceShowFavoriteButton = false,
			},
			ref,
		) => {
			const {i18n} = useLingui();
			const isMobileLayout = MobileLayout.isMobileLayout();
			const handleDownloadClick = (e: React.MouseEvent) => {
				e.stopPropagation();
				onDownloadClick?.(e);
			};
			const handleDeleteClick = (e: React.MouseEvent) => {
				e.stopPropagation();
				onDeleteClick?.(e);
			};
			const isMediaTooSmall =
				renderedWidth !== undefined &&
				renderedHeight !== undefined &&
				(renderedWidth < MIN_SIZE_FOR_OVERLAYS || renderedHeight < MIN_SIZE_FOR_OVERLAYS);
			const shouldShowFavorite = showFavoriteButton && (forceShowFavoriteButton || !isMediaTooSmall);
			const shouldShowDownload = showDownloadButton && !isMediaTooSmall;
			const shouldShowDelete = showDeleteButton && !isMediaTooSmall;
			const hasAnyButton = shouldShowFavorite || shouldShowDownload || shouldShowDelete;
			const useLongPress = isMobileLayout && onLongPress;
			const content = (
				<>
					{hasAnyButton && (
						<div className={mediaStyles.mediaHoverAction} data-flx="channel.embeds.media.media-container.div">
							{shouldShowDelete && onDeleteClick && (
								<Tooltip
									text={i18n._(DELETE_DESCRIPTOR)}
									position="top"
									data-flx="channel.embeds.media.media-container.tooltip"
								>
									<FocusRing offset={-2} data-flx="channel.embeds.media.media-container.focus-ring">
										<button
											type="button"
											onClick={handleDeleteClick}
											className={`${mediaStyles.actionButton} ${mediaStyles.deleteButton}`}
											aria-label={i18n._(DELETE_ATTACHMENT_DESCRIPTOR)}
											data-flx="channel.embeds.media.media-container.button.delete-click"
										>
											<TrashIcon
												size={remFromPx(18)}
												weight="bold"
												className={mediaStyles.actionIcon}
												data-flx="channel.embeds.media.media-container.trash-icon"
											/>
										</button>
									</FocusRing>
								</Tooltip>
							)}
							{shouldShowDownload && onDownloadClick && (
								<Tooltip
									text={i18n._(DOWNLOAD_DESCRIPTOR)}
									position="top"
									data-flx="channel.embeds.media.media-container.tooltip--2"
								>
									<FocusRing offset={-2} data-flx="channel.embeds.media.media-container.focus-ring--2">
										<button
											type="button"
											onClick={handleDownloadClick}
											className={mediaStyles.actionButton}
											aria-label={i18n._(DOWNLOAD_MEDIA_DESCRIPTOR)}
											data-flx="channel.embeds.media.media-container.button.download-click"
										>
											<DownloadSimpleIcon
												size={remFromPx(18)}
												weight="bold"
												className={mediaStyles.actionIcon}
												data-flx="channel.embeds.media.media-container.download-simple-icon"
											/>
										</button>
									</FocusRing>
								</Tooltip>
							)}
							{shouldShowFavorite && onFavoriteClick && (
								<Tooltip
									text={isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)}
									position="top"
									data-flx="channel.embeds.media.media-container.tooltip--3"
								>
									<FocusRing offset={-2} data-flx="channel.embeds.media.media-container.focus-ring--3">
										<button
											type="button"
											onClick={onFavoriteClick}
											className={clsx(mediaStyles.actionButton, isFavorited && mediaStyles.favoriteButtonActive)}
											aria-label={
												isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)
											}
											aria-pressed={isFavorited}
											data-flx="channel.embeds.media.media-container.button.favorite-click"
										>
											<StarIcon
												size={remFromPx(18)}
												weight={isFavorited ? 'fill' : 'bold'}
												className={mediaStyles.actionIcon}
												data-flx="channel.embeds.media.media-container.star-icon"
											/>
										</button>
									</FocusRing>
								</Tooltip>
							)}
						</div>
					)}
					{children}
				</>
			);
			if (useLongPress) {
				return (
					<LongPressable
						ref={ref}
						className={clsx(mediaStyles.mediaContainer, className)}
						style={style}
						onContextMenu={onContextMenu}
						onMouseDown={clearTextSelection}
						onMouseEnter={onMouseEnter}
						onMouseLeave={onMouseLeave}
						onLongPress={onLongPress}
						data-flx="channel.embeds.media.media-container.long-pressable.clear-text-selection"
					>
						{content}
					</LongPressable>
				);
			}
			return (
				<div
					ref={ref}
					role="group"
					className={clsx(mediaStyles.mediaContainer, className)}
					style={style}
					onContextMenu={onContextMenu}
					onMouseDown={clearTextSelection}
					onMouseEnter={onMouseEnter}
					onMouseLeave={onMouseLeave}
					data-flx="channel.embeds.media.media-container.group.clear-text-selection"
				>
					{content}
				</div>
			);
		},
	),
);
