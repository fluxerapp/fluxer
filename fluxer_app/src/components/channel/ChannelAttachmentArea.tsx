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

import {DndContext, type DragEndEvent, PointerSensor, useSensor, useSensors} from '@dnd-kit/core';
import {restrictToHorizontalAxis} from '@dnd-kit/modifiers';
import {arrayMove, horizontalListSortingStrategy, SortableContext, useSortable} from '@dnd-kit/sortable';
import {CSS} from '@dnd-kit/utilities';
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
import React from 'react';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {MessageAttachmentFlags} from '~/Constants';
import styles from '~/components/channel/ChannelAttachmentArea.module.css';
import EmbedVideo from '~/components/channel/embeds/media/EmbedVideo';
import {AttachmentEditModal} from '~/components/modals/AttachmentEditModal';
import * as Modal from '~/components/modals/Modal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useTextareaAttachments} from '~/hooks/useCloudUpload';
import {type CloudAttachment, CloudUpload} from '~/lib/CloudUpload';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import MessageStore from '~/stores/MessageStore';
import {formatFileSize} from '~/utils/FileUtils';

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

const VideoPreviewModal = observer(({file, width, height}: {file: File; width: number; height: number}) => {
	const {t} = useLingui();

	const [blobUrl, setBlobUrl] = React.useState<string | null>(null);

	React.useEffect(() => {
		const url = URL.createObjectURL(file);
		setBlobUrl(url);
		return () => URL.revokeObjectURL(url);
	}, [file]);

	if (!blobUrl) return null;

	return (
		<Modal.Root className={styles.videoModal}>
			<Modal.ScreenReaderLabel text={t`Video`} />
			<div className={styles.videoContainer}>
				<EmbedVideo src={blobUrl} width={width} height={height} />
			</div>
		</Modal.Root>
	);
});

const SortableAttachmentItem = observer(
	({
		attachment,
		channelId,
		isSortingList = false,
	}: {
		attachment: CloudAttachment;
		channelId: string;
		isSortingList?: boolean;
	}) => {
		const {t} = useLingui();

		const [spoilerHidden, setSpoilerHidden] = React.useState(true);
		const {attributes, listeners, setNodeRef, transform, transition, isDragging} = useSortable({
			id: attachment.id,
		});
		const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;

		React.useEffect(() => {
			if (isSpoiler) {
				setSpoilerHidden(true);
			}
		}, [isSpoiler]);

		const handleClick = () => {
			if (isSpoiler && spoilerHidden) {
				setSpoilerHidden(false);
				return;
			}

			if (attachment.file.type.startsWith('image/')) {
				if (!attachment.previewURL) return;

				MediaViewerActionCreators.openMediaViewer(
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
				ModalActionCreators.push(
					modal(() => <VideoPreviewModal file={attachment.file} width={attachment.width} height={attachment.height} />),
				);
			}
		};

		const containerStyle: React.CSSProperties = {
			width: '200px',
			height: '200px',
			position: 'relative',
			transform: CSS.Transform.toString(transform),
			transition,
			opacity: isDragging ? 0.5 : 1,
			cursor: isDragging ? 'grabbing' : 'default',
		};

		const isMedia =
			attachment.previewURL && (attachment.file.type.startsWith('image/') || attachment.file.type.startsWith('video/'));

		return (
			<li
				{...attributes}
				{...listeners}
				ref={setNodeRef}
				style={containerStyle}
				className={styles.upload}
				tabIndex={-1}
			>
				<div className={styles.uploadContainer}>
					{isMedia ? (
						<div className={styles.mediaContainer}>
							<button type="button" className={styles.clickableMedia} onClick={handleClick}>
								<div
									className={clsx(
										styles.spoilerContainer,
										isSpoiler && spoilerHidden && styles.hidden,
										isSpoiler && spoilerHidden && styles.hiddenSpoiler,
									)}
								>
									{isSpoiler && spoilerHidden && (
										<div className={clsx(styles.spoilerWarning, styles.obscureWarning)}>{t`Spoiler`}</div>
									)}

									<div className={styles.spoilerInnerContainer} aria-hidden={spoilerHidden}>
										<div className={styles.spoilerWrapper}>
											{attachment.file.type.startsWith('image/') ? (
												<ImageThumbnail attachment={attachment} spoiler={isSpoiler && spoilerHidden} />
											) : attachment.file.type.startsWith('video/') ? (
												<VideoThumbnail attachment={attachment} spoiler={isSpoiler && spoilerHidden} />
											) : null}
											<div className={styles.tags}>
												{isSpoiler && !spoilerHidden && <span className={styles.altTag}>{t`Spoiler`}</span>}
											</div>
										</div>
									</div>
								</div>
							</button>
						</div>
					) : (
						<div className={styles.icon}>
							{(() => {
								const IconComponent = getFileIcon(attachment.file);
								return <IconComponent className={styles.iconImage} weight="fill" aria-label={attachment.filename} />;
							})()}
						</div>
					)}

					<div className={styles.filenameContainer}>
						<Tooltip text={attachment.filename}>
							<div className={styles.filename}>{attachment.filename}</div>
						</Tooltip>
						<div className={styles.fileDetails}>
							<span className={styles.fileSize}>{formatFileSize(attachment.file.size)}</span>
							<span className={styles.fileExtension}>{getFileExtension(attachment.filename)}</span>
						</div>
					</div>

					{!isSortingList && (
						<div className={styles.actionBarContainer}>
							{attachment.status === 'failed' ? (
								<div className={styles.actionBar}>
									<AttachmentActionBarButton
										icon={TrashIcon}
										label={t`Remove Attachment`}
										danger={true}
										onClick={() => CloudUpload.removeAttachment(channelId, attachment.id)}
									/>
								</div>
							) : (
								<AttachmentActionBar channelId={channelId} attachment={attachment} />
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
	const prevAttachmentsLength = React.useRef<number | null>(null);
	const wasAtBottomBeforeChange = React.useRef<boolean>(true);
	const [isDragging, setIsDragging] = React.useState(false);
	const sensors = useSensors(useSensor(PointerSensor, {activationConstraint: {distance: 8}}));

	const handleDragEnd = (event: DragEndEvent) => {
		const {active, over} = event;
		if (over && active.id !== over.id) {
			const oldIndex = attachments.findIndex((attachment) => attachment.id === active.id);
			const newIndex = attachments.findIndex((attachment) => attachment.id === over.id);
			if (oldIndex !== -1 && newIndex !== -1) {
				const newArray = arrayMove(attachments.slice(), oldIndex, newIndex);
				CloudUpload.reorderAttachments(channelId, newArray);
			}
		}
		setIsDragging(false);
	};

	if (attachments.length !== prevAttachmentsLength.current) {
		const scrollerElement = document.querySelector('.scroller-base') as HTMLElement | null;
		if (scrollerElement) {
			const isNearBottom =
				scrollerElement.scrollHeight <= scrollerElement.scrollTop + scrollerElement.offsetHeight + 16;
			wasAtBottomBeforeChange.current = isNearBottom;
		}
	}

	React.useLayoutEffect(() => {
		const currentLength = attachments.length;
		const previousLength = prevAttachmentsLength.current;

		if (previousLength !== null && previousLength !== currentLength) {
			if ((previousLength === 0 && currentLength > 0) || (previousLength > 0 && currentLength === 0)) {
				if (wasAtBottomBeforeChange.current) {
					const messages = MessageStore.getMessages(channelId);
					if (messages.hasMoreAfter) {
						ComponentDispatch.dispatch('FORCE_JUMP_TO_PRESENT');
					}
				}
			}

			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
		prevAttachmentsLength.current = currentLength;
	}, [attachments, channelId]);

	if (attachments.length === 0) {
		return null;
	}

	return (
		<>
			<DndContext
				sensors={sensors}
				modifiers={[restrictToHorizontalAxis]}
				onDragEnd={handleDragEnd}
				onDragStart={() => setIsDragging(true)}
			>
				<SortableContext
					items={attachments.map((attachment) => attachment.id)}
					strategy={horizontalListSortingStrategy}
				>
					<Scroller orientation="horizontal" fade={false} className={styles.scroller}>
						<ul className={styles.channelAttachmentArea}>
							{attachments.map((attachment) => (
								<SortableAttachmentItem
									key={attachment.id}
									attachment={attachment}
									channelId={channelId}
									isSortingList={isDragging}
								/>
							))}
						</ul>
					</Scroller>
				</SortableContext>
			</DndContext>
			<div className={styles.divider} />
		</>
	);
});

const ImageThumbnail = observer(({attachment, spoiler}: {attachment: CloudAttachment; spoiler: boolean}) => {
	const [hasError, setHasError] = React.useState(false);
	const src = attachment.previewURL;

	if (hasError || !src) return null;

	return (
		<img
			src={src}
			className={clsx(styles.media, spoiler && styles.spoiler)}
			aria-hidden={true}
			alt={attachment.filename}
			onError={() => setHasError(true)}
		/>
	);
});

const VideoThumbnail = observer(({attachment, spoiler}: {attachment: CloudAttachment; spoiler: boolean}) => {
	const [hasError, setHasError] = React.useState(false);
	const src = attachment.thumbnailURL || attachment.previewURL;

	if (hasError || !src) return null;

	return (
		<img
			src={src}
			alt={attachment.filename}
			className={clsx(styles.media, spoiler && styles.spoiler)}
			onError={() => setHasError(true)}
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
			<Tooltip text={label}>
				<FocusRing offset={-2}>
					<button
						type="button"
						aria-label={label}
						onClick={handleClick}
						className={clsx(styles.button, danger && styles.danger)}
					>
						<Icon className={styles.actionBarIcon} />
					</button>
				</FocusRing>
			</Tooltip>
		);
	},
);

const AttachmentActionBar = observer(({channelId, attachment}: {channelId: string; attachment: CloudAttachment}) => {
	const {t} = useLingui();

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
		ModalActionCreators.push(modal(() => <AttachmentEditModal channelId={channelId} attachment={attachment} />));
	};

	const removeAttachment = () => {
		CloudUpload.removeAttachment(channelId, attachment.id);
	};

	return (
		<div className={styles.actionBarContainer}>
			<div className={styles.actionBar} role="toolbar" aria-label={t`Attachment Actions`}>
				<AttachmentActionBarButton
					icon={isSpoiler ? EyeSlashIcon : EyeIcon}
					label={isSpoiler ? t`Remove Spoiler` : t`Spoiler Attachment`}
					onClick={toggleSpoiler}
				/>
				<AttachmentActionBarButton icon={PencilIcon} label={t`Edit Attachment`} onClick={editAttachment} />
				<AttachmentActionBarButton
					icon={TrashIcon}
					label={t`Remove Attachment`}
					danger={true}
					onClick={removeAttachment}
				/>
			</div>
		</div>
	);
});
