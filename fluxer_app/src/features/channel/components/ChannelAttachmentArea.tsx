// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {computeHorizontalDropPosition} from '@app/features/app/components/layout/dnd/DndDropPosition';
import {
	type AttachmentDragItem,
	type AttachmentDropResult,
	DND_TYPES,
} from '@app/features/app/components/layout/types/DndTypes';
import styles from '@app/features/channel/components/ChannelAttachmentArea.module.css';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {AttachmentEditModal} from '@app/features/messaging/components/modals/AttachmentEditModal';
import {useTextareaAttachments} from '@app/features/messaging/hooks/useCloudUpload';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {type CloudAttachment, CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {isEmbeddableImageFile} from '@app/features/messaging/utils/EmbeddableImageTypes';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	EyeIcon,
	EyeSlashIcon,
	FileAudioIcon,
	FileCodeIcon,
	FileIcon,
	FilePdfIcon,
	FileTextIcon,
	FileZipIcon,
	type Icon,
	PencilIcon,
	TrashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Short label in the channel attachment area. Keep it concise.',
});
const SPOILER_DESCRIPTOR = msg({
	message: 'Spoiler',
	comment: 'Short label in the channel attachment area. Keep it concise.',
});
const REMOVE_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Remove attachment',
	comment:
		'Button or menu action label in the channel attachment area. Keep it concise. Keep the tone plain and specific.',
});
const ATTACHMENT_ACTIONS_DESCRIPTOR = msg({
	message: 'Attachment actions',
	comment: 'Short label in the channel attachment area. Keep it concise.',
});
const REMOVE_SPOILER_DESCRIPTOR = msg({
	message: 'Remove spoiler',
	comment:
		'Button or menu action label in the channel attachment area. Keep it concise. Keep the tone plain and specific.',
});
const SPOILER_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Spoiler attachment',
	comment: 'Short label in the channel attachment area. Keep it concise.',
});
const EDIT_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Edit attachment',
	comment: 'Button or menu action label in the channel attachment area. Keep it concise.',
});
const MESSAGE_SCROLLER_SELECTOR = '[data-flx="channel.messages.scroller"][data-fluxer-scroll-container="true"]';
const MESSAGE_SCROLLER_BOTTOM_THRESHOLD = 16;
const getActiveMessageScroller = (): HTMLElement | null =>
	document.querySelector<HTMLElement>(MESSAGE_SCROLLER_SELECTOR);
const isMessageScrollerNearBottom = (scrollerElement: HTMLElement): boolean =>
	scrollerElement.scrollHeight <=
	scrollerElement.scrollTop + scrollerElement.clientHeight + MESSAGE_SCROLLER_BOTTOM_THRESHOLD;
const getFileExtension = (filename: string): string => {
	const ext = filename.split('.').pop()?.toLowerCase() || '';
	return ext.length > 0 && ext.length <= 4 ? ext : '';
};
const getFileIcon = (file: File): Icon => {
	const mimeType = file.type.toLowerCase();
	const extension = file.name.split('.').pop()?.toLowerCase() || '';
	if (mimeType.startsWith('audio/')) {
		return FileAudioIcon;
	}
	if (mimeType === 'application/pdf') {
		return FilePdfIcon;
	}
	if (mimeType.startsWith('text/') || ['txt', 'md', 'markdown', 'rtf'].includes(extension)) {
		return FileTextIcon;
	}
	if (
		[
			'application/zip',
			'application/x-zip-compressed',
			'application/x-rar-compressed',
			'application/x-7z-compressed',
		].includes(mimeType) ||
		['zip', 'rar', '7z', 'tar', 'gz'].includes(extension)
	) {
		return FileZipIcon;
	}
	if (
		mimeType.startsWith('application/') &&
		[
			'js',
			'ts',
			'jsx',
			'tsx',
			'html',
			'css',
			'json',
			'xml',
			'py',
			'java',
			'cpp',
			'c',
			'cs',
			'php',
			'rb',
			'go',
			'rs',
			'swift',
		].includes(extension)
	) {
		return FileCodeIcon;
	}
	return FileIcon;
};
const isAttachmentMedia = (attachment: CloudAttachment): boolean => {
	if (attachment.file.type.startsWith('video/')) {
		return attachment.previewURL !== null || attachment.thumbnailURL !== null;
	}
	if (isEmbeddableImageFile(attachment.file)) {
		return attachment.previewURL !== null;
	}
	return false;
};
const VideoPreviewModal = observer(({file, width, height}: {file: File; width: number; height: number}) => {
	const {i18n} = useLingui();
	const [blobUrl, setBlobUrl] = useState<string | null>(null);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	useEffect(() => {
		const url = URL.createObjectURL(file);
		setBlobUrl(url);
		return () => URL.revokeObjectURL(url);
	}, [file]);
	if (!blobUrl) return null;
	return (
		<Modal.Root
			className={styles.videoModal}
			onClose={handleClose}
			data-flx="channel.channel-attachment-area.video-preview-modal.video-modal"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(VIDEO_DESCRIPTOR)}
				data-flx="channel.channel-attachment-area.video-preview-modal.modal-screen-reader-label"
			/>
			<Modal.InsetCloseButton
				onClick={handleClose}
				data-flx="channel.channel-attachment-area.video-preview-modal.modal-inset-close-button.close"
			/>
			<div
				className={styles.videoContainer}
				data-flx="channel.channel-attachment-area.video-preview-modal.video-container"
			>
				<EmbedVideo
					src={blobUrl}
					width={width}
					height={height}
					data-flx="channel.channel-attachment-area.video-preview-modal.embed-video"
				/>
			</div>
		</Modal.Root>
	);
});
const SortableAttachmentItem = observer(
	({
		attachment,
		channelId,
		isSortingList = false,
		onAttachmentDrop,
		onDragStateChange,
	}: {
		attachment: CloudAttachment;
		channelId: string;
		isSortingList?: boolean;
		onAttachmentDrop?: (item: AttachmentDragItem, result: AttachmentDropResult) => void;
		onDragStateChange?: (item: AttachmentDragItem | null) => void;
	}) => {
		const {i18n} = useLingui();
		const itemRef = useRef<HTMLLIElement | null>(null);
		const cachedItemRectRef = useRef<DOMRect | null>(null);
		const clearCachedItemRectFrameRef = useRef<number | null>(null);
		const dropIndicatorRef = useRef<'left' | 'right' | null>(null);
		const mobileLayout = MobileLayout;
		const [spoilerHidden, setSpoilerHidden] = useState(true);
		const [dropIndicator, setDropIndicator] = useState<'left' | 'right' | null>(null);
		const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;
		const dragItemData = useMemo<AttachmentDragItem>(
			() => ({
				type: DND_TYPES.ATTACHMENT,
				id: attachment.id,
				channelId,
			}),
			[attachment.id, channelId],
		);
		const setDropIndicatorIfChanged = useCallback((nextDropIndicator: 'left' | 'right' | null) => {
			if (dropIndicatorRef.current === nextDropIndicator) {
				return;
			}
			dropIndicatorRef.current = nextDropIndicator;
			setDropIndicator(nextDropIndicator);
		}, []);
		const getCachedItemRect = useCallback((): DOMRect | null => {
			const node = itemRef.current;
			if (!node) return null;
			if (cachedItemRectRef.current) {
				return cachedItemRectRef.current;
			}
			const rect = node.getBoundingClientRect();
			cachedItemRectRef.current = rect;
			if (clearCachedItemRectFrameRef.current == null) {
				clearCachedItemRectFrameRef.current = requestAnimationFrame(() => {
					clearCachedItemRectFrameRef.current = null;
					cachedItemRectRef.current = null;
				});
			}
			return rect;
		}, []);
		const [{isDragging}, dragRef, preview] = useDrag(
			() => ({
				type: DND_TYPES.ATTACHMENT,
				item: () => {
					onDragStateChange?.(dragItemData);
					return dragItemData;
				},
				canDrag: !mobileLayout.enabled,
				collect: (monitor) => ({isDragging: monitor.isDragging()}),
				end: () => {
					onDragStateChange?.(null);
					setDropIndicatorIfChanged(null);
				},
			}),
			[dragItemData, mobileLayout.enabled, onDragStateChange, setDropIndicatorIfChanged],
		);
		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: DND_TYPES.ATTACHMENT,
				canDrop: (item: AttachmentDragItem) => item.id !== attachment.id,
				hover: (item: AttachmentDragItem, monitor) => {
					if (item.id === attachment.id) {
						setDropIndicatorIfChanged(null);
						return;
					}
					const hoverBoundingRect = getCachedItemRect();
					if (!hoverBoundingRect) return;
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const dropPos = computeHorizontalDropPosition(clientOffset, hoverBoundingRect);
					setDropIndicatorIfChanged(dropPos === 'before' ? 'left' : 'right');
				},
				drop: (item: AttachmentDragItem, monitor): AttachmentDropResult | undefined => {
					if (!monitor.canDrop()) {
						setDropIndicatorIfChanged(null);
						return;
					}
					const node = itemRef.current;
					if (!node) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const result: AttachmentDropResult = {
						targetId: attachment.id,
						position: computeHorizontalDropPosition(clientOffset, hoverBoundingRect),
					};
					onAttachmentDrop?.(item, result);
					setDropIndicatorIfChanged(null);
					return result;
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
				}),
			}),
			[attachment.id, getCachedItemRect, onAttachmentDrop, setDropIndicatorIfChanged],
		);
		useEffect(() => {
			if (!isOver) setDropIndicatorIfChanged(null);
		}, [isOver, setDropIndicatorIfChanged]);
		useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);
		useEffect(() => {
			return () => {
				if (clearCachedItemRectFrameRef.current != null) {
					cancelAnimationFrame(clearCachedItemRectFrameRef.current);
					clearCachedItemRectFrameRef.current = null;
				}
			};
		}, []);
		const dragConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dragRef(node);
			},
			[dragRef],
		);
		const dropConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dropRef(node);
			},
			[dropRef],
		);
		const setRefs = useCallback(
			(node: HTMLLIElement | null) => {
				itemRef.current = node;
				dragConnectorRef(node);
				dropConnectorRef(node);
			},
			[dragConnectorRef, dropConnectorRef],
		);
		useEffect(() => {
			if (isSpoiler) {
				setSpoilerHidden(true);
			}
		}, [isSpoiler]);
		const handleClick = () => {
			if (isSpoiler && spoilerHidden) {
				setSpoilerHidden(false);
				return;
			}
			if (isEmbeddableImageFile(attachment.file)) {
				if (!attachment.previewURL) return;
				MediaViewerCommands.openMediaViewer(
					[
						{
							src: attachment.previewURL,
							originalSrc: attachment.previewURL,
							naturalWidth: attachment.width,
							naturalHeight: attachment.height,
							type: 'image' as const,
							filename: attachment.file.name,
						},
					],
					0,
				);
			} else if (attachment.file.type.startsWith('video/')) {
				ModalCommands.push(
					modal(() => (
						<VideoPreviewModal
							file={attachment.file}
							width={attachment.width}
							height={attachment.height}
							data-flx="channel.channel-attachment-area.handle-click.video-preview-modal"
						/>
					)),
				);
			}
		};
		const containerStyle: React.CSSProperties = {
			width: '200px',
			height: '200px',
			position: 'relative',
			opacity: isDragging ? 0.5 : 1,
			cursor: isDragging ? 'grabbing' : 'default',
		};
		const isMedia = isAttachmentMedia(attachment);
		const isHiddenSpoiler = isSpoiler && spoilerHidden;
		const IconComponent = getFileIcon(attachment.file);
		return (
			<li
				ref={setRefs}
				style={containerStyle}
				className={clsx(
					styles.upload,
					dropIndicator === 'left' && styles.dropIndicatorLeft,
					dropIndicator === 'right' && styles.dropIndicatorRight,
				)}
				tabIndex={-1}
				data-flx="channel.channel-attachment-area.sortable-attachment-item.upload"
			>
				<div
					className={styles.uploadContainer}
					data-flx="channel.channel-attachment-area.sortable-attachment-item.upload-container"
				>
					{isMedia ? (
						<div
							className={styles.mediaContainer}
							data-flx="channel.channel-attachment-area.sortable-attachment-item.media-container"
						>
							<button
								type="button"
								className={styles.clickableMedia}
								onClick={handleClick}
								data-flx="channel.channel-attachment-area.sortable-attachment-item.clickable-media.button"
							>
								<div
									className={clsx(
										styles.spoilerContainer,
										isHiddenSpoiler && styles.hidden,
										isHiddenSpoiler && styles.hiddenSpoiler,
									)}
									data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-container"
								>
									{isHiddenSpoiler && (
										<div
											className={clsx(styles.spoilerWarning, styles.obscureWarning)}
											data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-warning"
										>
											{i18n._(SPOILER_DESCRIPTOR)}
										</div>
									)}
									<div
										className={styles.spoilerInnerContainer}
										aria-hidden={isHiddenSpoiler}
										data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-inner-container"
									>
										<div
											className={styles.spoilerWrapper}
											data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-wrapper"
										>
											{isEmbeddableImageFile(attachment.file) ? (
												<ImageThumbnail
													attachment={attachment}
													spoiler={isHiddenSpoiler}
													data-flx="channel.channel-attachment-area.sortable-attachment-item.image-thumbnail"
												/>
											) : attachment.file.type.startsWith('video/') ? (
												<VideoThumbnail
													attachment={attachment}
													spoiler={isHiddenSpoiler}
													data-flx="channel.channel-attachment-area.sortable-attachment-item.video-thumbnail"
												/>
											) : null}
											<div
												className={styles.tags}
												data-flx="channel.channel-attachment-area.sortable-attachment-item.tags"
											>
												{isSpoiler && !spoilerHidden && (
													<span
														className={styles.altTag}
														data-flx="channel.channel-attachment-area.sortable-attachment-item.alt-tag"
													>
														{i18n._(SPOILER_DESCRIPTOR)}
													</span>
												)}
											</div>
										</div>
									</div>
								</div>
							</button>
						</div>
					) : (
						<div className={styles.icon} data-flx="channel.channel-attachment-area.sortable-attachment-item.icon">
							<button
								type="button"
								className={styles.clickableMedia}
								onClick={handleClick}
								data-flx="channel.channel-attachment-area.sortable-attachment-item.clickable-media.button--2"
							>
								<div
									className={clsx(
										styles.spoilerContainer,
										isHiddenSpoiler && styles.hidden,
										isHiddenSpoiler && styles.hiddenSpoiler,
									)}
									data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-container--2"
								>
									{isHiddenSpoiler && (
										<div
											className={clsx(styles.spoilerWarning, styles.obscureWarning)}
											data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-warning--2"
										>
											{i18n._(SPOILER_DESCRIPTOR)}
										</div>
									)}
									<div
										className={styles.spoilerInnerContainer}
										aria-hidden={isHiddenSpoiler}
										data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-inner-container--2"
									>
										<div
											className={styles.spoilerWrapper}
											data-flx="channel.channel-attachment-area.sortable-attachment-item.spoiler-wrapper--2"
										>
											<IconComponent
												className={clsx(styles.iconImage, isHiddenSpoiler && styles.spoiler)}
												weight="fill"
												aria-label={attachment.filename}
												data-flx="channel.channel-attachment-area.sortable-attachment-item.icon-image"
											/>
											<div
												className={styles.tags}
												data-flx="channel.channel-attachment-area.sortable-attachment-item.tags--2"
											>
												{isSpoiler && !spoilerHidden && (
													<span
														className={styles.altTag}
														data-flx="channel.channel-attachment-area.sortable-attachment-item.alt-tag--2"
													>
														{i18n._(SPOILER_DESCRIPTOR)}
													</span>
												)}
											</div>
										</div>
									</div>
								</div>
							</button>
						</div>
					)}
					<div
						className={styles.filenameContainer}
						data-flx="channel.channel-attachment-area.sortable-attachment-item.filename-container"
					>
						<Tooltip
							text={attachment.filename}
							data-flx="channel.channel-attachment-area.sortable-attachment-item.tooltip"
						>
							<div
								className={styles.filename}
								data-flx="channel.channel-attachment-area.sortable-attachment-item.filename"
							>
								{attachment.filename}
							</div>
						</Tooltip>
						<div
							className={styles.fileDetails}
							data-flx="channel.channel-attachment-area.sortable-attachment-item.file-details"
						>
							<span
								className={styles.fileSize}
								data-flx="channel.channel-attachment-area.sortable-attachment-item.file-size"
							>
								{formatFileSize(attachment.file.size)}
							</span>
							<span
								className={styles.fileExtension}
								data-flx="channel.channel-attachment-area.sortable-attachment-item.file-extension"
							>
								{getFileExtension(attachment.filename)}
							</span>
						</div>
					</div>
					{!isSortingList && (
						<div
							className={styles.actionBarContainer}
							data-flx="channel.channel-attachment-area.sortable-attachment-item.action-bar-container"
						>
							{attachment.status === 'failed' ? (
								<div
									className={styles.actionBar}
									data-flx="channel.channel-attachment-area.sortable-attachment-item.action-bar"
								>
									<AttachmentActionBarButton
										icon={TrashIcon}
										label={i18n._(REMOVE_ATTACHMENT_DESCRIPTOR)}
										danger={true}
										onClick={() => CloudUpload.removeAttachment(channelId, attachment.id)}
										data-flx="channel.channel-attachment-area.sortable-attachment-item.attachment-action-bar-button.remove-attachment"
									/>
								</div>
							) : (
								<AttachmentActionBar
									channelId={channelId}
									attachment={attachment}
									data-flx="channel.channel-attachment-area.sortable-attachment-item.attachment-action-bar"
								/>
							)}
						</div>
					)}
				</div>
			</li>
		);
	},
);
export const ChannelAttachmentArea = observer(({channelId}: {channelId: string}) => {
	const attachments = useTextareaAttachments(channelId);
	const prevAttachmentsLength = useRef<number | null>(null);
	const wasAtBottomBeforeChange = useRef<boolean>(true);
	const forceJumpFrameRef = useRef<number | null>(null);
	const channelIdRef = useRef(channelId);
	const [isDragging, setIsDragging] = useState(false);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const getScrollElement = useCallback(() => {
		const scroller = scrollerRef.current;
		if (scroller == null) return null;
		return scroller.getScrollerNode();
	}, []);
	useDragAutoScroll({active: isDragging, axis: 'horizontal', getScrollElement});
	channelIdRef.current = channelId;
	const handleAttachmentDrop = useCallback(
		(item: AttachmentDragItem, result: AttachmentDropResult) => {
			const sourceId = item.id;
			const targetId = result.targetId;
			if (sourceId === targetId) return;
			const oldIndex = attachments.findIndex((attachment: CloudAttachment) => attachment.id === sourceId);
			const targetIndex = attachments.findIndex((attachment: CloudAttachment) => attachment.id === targetId);
			if (oldIndex === -1 || targetIndex === -1) return;
			let newIndex = result.position === 'after' ? targetIndex + 1 : targetIndex;
			if (oldIndex < targetIndex && result.position === 'after') newIndex--;
			const newArray = [...attachments];
			const [movedItem] = newArray.splice(oldIndex, 1);
			newArray.splice(newIndex, 0, movedItem);
			CloudUpload.reorderAttachments(channelId, newArray);
		},
		[attachments, channelId],
	);
	const handleDragStateChange = useCallback((item: AttachmentDragItem | null) => {
		setIsDragging(item !== null);
	}, []);
	const scheduleForceJumpToPresent = useCallback(() => {
		if (forceJumpFrameRef.current != null) {
			return;
		}
		forceJumpFrameRef.current = requestAnimationFrame(() => {
			forceJumpFrameRef.current = null;
			const messages = Messages.getMessages(channelIdRef.current);
			if (messages.hasMoreAfter) {
				ComponentDispatch.dispatch('FORCE_JUMP_TO_PRESENT');
			}
		});
	}, []);
	useEffect(() => {
		const updateWasAtBottom = () => {
			const scrollerElement = getActiveMessageScroller();
			if (scrollerElement) {
				wasAtBottomBeforeChange.current = isMessageScrollerNearBottom(scrollerElement);
			}
		};
		let updateFrame: number | null = null;
		const scheduleUpdate = () => {
			if (updateFrame != null) return;
			updateFrame = requestAnimationFrame(() => {
				updateFrame = null;
				updateWasAtBottom();
			});
		};
		const scrollerElement = getActiveMessageScroller();
		if (!scrollerElement) {
			return undefined;
		}
		updateWasAtBottom();
		scrollerElement.addEventListener('scroll', scheduleUpdate, {passive: true});
		return () => {
			scrollerElement.removeEventListener('scroll', scheduleUpdate);
			if (updateFrame != null) {
				cancelAnimationFrame(updateFrame);
			}
		};
	}, [channelId]);
	useEffect(() => {
		const currentLength = attachments.length;
		const previousLength = prevAttachmentsLength.current;
		if (previousLength !== null && previousLength !== currentLength) {
			const crossedEmptyBoundary =
				(previousLength === 0 && currentLength > 0) || (previousLength > 0 && currentLength === 0);
			if (crossedEmptyBoundary && wasAtBottomBeforeChange.current) {
				scheduleForceJumpToPresent();
			}
		}
		prevAttachmentsLength.current = currentLength;
		const scrollerElement = getActiveMessageScroller();
		if (scrollerElement) {
			wasAtBottomBeforeChange.current = isMessageScrollerNearBottom(scrollerElement);
		}
	}, [attachments.length, scheduleForceJumpToPresent]);
	useEffect(() => {
		return () => {
			if (forceJumpFrameRef.current != null) {
				cancelAnimationFrame(forceJumpFrameRef.current);
				forceJumpFrameRef.current = null;
			}
		};
	}, []);
	if (attachments.length === 0) {
		return null;
	}
	return (
		<>
			<Scroller
				ref={scrollerRef}
				key="channel-attachment-scroller"
				orientation="horizontal"
				fade={false}
				className={styles.scroller}
				data-flx="channel.channel-attachment-area.scroller"
			>
				<ul className={styles.channelAttachmentArea} data-flx="channel.channel-attachment-area.channel-attachment-area">
					{attachments.map((attachment: CloudAttachment) => (
						<SortableAttachmentItem
							key={attachment.id}
							attachment={attachment}
							channelId={channelId}
							isSortingList={isDragging}
							onAttachmentDrop={handleAttachmentDrop}
							onDragStateChange={handleDragStateChange}
							data-flx="channel.channel-attachment-area.sortable-attachment-item"
						/>
					))}
				</ul>
			</Scroller>
			<div className={styles.divider} data-flx="channel.channel-attachment-area.divider" />
		</>
	);
});
const ImageThumbnail = observer(({attachment, spoiler}: {attachment: CloudAttachment; spoiler: boolean}) => {
	const [hasError, setHasError] = useState(false);
	const src = attachment.previewURL;
	if (hasError || !src) return null;
	return (
		<img
			src={src}
			className={clsx(styles.media, spoiler && styles.spoiler)}
			aria-hidden={true}
			alt={attachment.filename}
			onError={() => setHasError(true)}
			data-flx="channel.channel-attachment-area.image-thumbnail.media"
		/>
	);
});
const VideoThumbnail = observer(({attachment, spoiler}: {attachment: CloudAttachment; spoiler: boolean}) => {
	const [hasError, setHasError] = useState(false);
	const src = attachment.thumbnailURL || attachment.previewURL;
	if (hasError || !src) return null;
	return (
		<img
			src={src}
			alt={attachment.filename}
			className={clsx(styles.media, spoiler && styles.spoiler)}
			onError={() => setHasError(true)}
			data-flx="channel.channel-attachment-area.video-thumbnail.media"
		/>
	);
});
const AttachmentActionBarButton = observer(
	({
		label,
		icon: Icon,
		onClick,
		danger = false,
	}: {
		label: string;
		icon: Icon;
		onClick: (event: React.MouseEvent | React.KeyboardEvent) => void;
		danger?: boolean;
	}) => {
		const handleClick = (event: React.MouseEvent | React.KeyboardEvent) => {
			event.preventDefault();
			event.stopPropagation();
			onClick(event);
		};
		return (
			<Tooltip text={label} data-flx="channel.channel-attachment-area.attachment-action-bar-button.tooltip">
				<FocusRing offset={-2} data-flx="channel.channel-attachment-area.attachment-action-bar-button.focus-ring">
					<button
						type="button"
						aria-label={label}
						onClick={handleClick}
						className={clsx(styles.button, danger && styles.danger)}
						data-flx="channel.channel-attachment-area.attachment-action-bar-button.button.click"
					>
						<Icon
							className={styles.actionBarIcon}
							data-flx="channel.channel-attachment-area.attachment-action-bar-button.action-bar-icon"
						/>
					</button>
				</FocusRing>
			</Tooltip>
		);
	},
);
const AttachmentActionBar = observer(({channelId, attachment}: {channelId: string; attachment: CloudAttachment}) => {
	const {i18n} = useLingui();
	const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;
	const toggleSpoiler = () => {
		const nextFlags = isSpoiler
			? attachment.flags & ~MessageAttachmentFlags.IS_SPOILER
			: attachment.flags | MessageAttachmentFlags.IS_SPOILER;
		CloudUpload.updateAttachment(channelId, attachment.id, {
			flags: nextFlags,
			spoiler: !isSpoiler,
		});
	};
	const editAttachment = () => {
		ModalCommands.push(
			modal(() => (
				<AttachmentEditModal
					channelId={channelId}
					attachment={attachment}
					data-flx="channel.channel-attachment-area.edit-attachment.attachment-edit-modal"
				/>
			)),
		);
	};
	const removeAttachment = () => {
		CloudUpload.removeAttachment(channelId, attachment.id);
	};
	return (
		<div
			className={styles.actionBarContainer}
			data-flx="channel.channel-attachment-area.attachment-action-bar.action-bar-container"
		>
			<div
				className={styles.actionBar}
				role="toolbar"
				aria-label={i18n._(ATTACHMENT_ACTIONS_DESCRIPTOR)}
				data-flx="channel.channel-attachment-area.attachment-action-bar.action-bar"
			>
				<AttachmentActionBarButton
					icon={isSpoiler ? EyeSlashIcon : EyeIcon}
					label={isSpoiler ? i18n._(REMOVE_SPOILER_DESCRIPTOR) : i18n._(SPOILER_ATTACHMENT_DESCRIPTOR)}
					onClick={toggleSpoiler}
					data-flx="channel.channel-attachment-area.attachment-action-bar.attachment-action-bar-button.toggle-spoiler"
				/>
				<AttachmentActionBarButton
					icon={PencilIcon}
					label={i18n._(EDIT_ATTACHMENT_DESCRIPTOR)}
					onClick={editAttachment}
					data-flx="channel.channel-attachment-area.attachment-action-bar.attachment-action-bar-button.edit-attachment"
				/>
				<AttachmentActionBarButton
					icon={TrashIcon}
					label={i18n._(REMOVE_ATTACHMENT_DESCRIPTOR)}
					danger={true}
					onClick={removeAttachment}
					data-flx="channel.channel-attachment-area.attachment-action-bar.attachment-action-bar-button.remove-attachment"
				/>
			</div>
		</div>
	);
});
