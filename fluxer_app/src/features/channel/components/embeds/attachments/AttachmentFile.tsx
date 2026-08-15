// SPDX-License-Identifier: AGPL-3.0-or-later

import {TextualAttachmentPreview} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreview';
import {splitFilename} from '@app/features/channel/components/embeds/EmbedUtils';
import {canDeleteAttachmentUtil} from '@app/features/channel/components/MessageActionUtils';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {DELETE_ATTACHMENT_DESCRIPTOR, DOWNLOAD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {shouldPreviewAttachment} from '@app/features/messaging/utils/AttachmentPreviewUtils';
import {downloadFile} from '@app/features/messaging/utils/FileDownloadUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import attachmentFileStyles from '@app/features/theme/styles/AttachmentFile.module.css';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	DownloadSimpleIcon,
	FileArchiveIcon,
	FileAudioIcon,
	FileCodeIcon,
	FileIcon,
	FileImageIcon,
	FilePdfIcon,
	FilePptIcon,
	FileTextIcon,
	FileVideoIcon,
	FileXlsIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const ATTACHMENT_EXPIRED_DESCRIPTOR = msg({
	message: 'Attachment expired',
	comment: 'Error message in the channel and chat attachment file.',
});

interface AttachmentFileProps {
	attachment: MessageAttachment;
	isPreview?: boolean;
	message?: Message;
}

export const AttachmentFile = observer(({attachment, message, isPreview}: AttachmentFileProps) => {
	const {i18n} = useLingui();
	const {enabled: isMobile} = MobileLayout;
	const isExpired = Boolean(attachment.expired);
	const fileName = attachment.title || attachment.filename;
	const fileSize = formatFileSize(attachment.size);
	const fileExtension = fileName.split('.').pop()?.toLowerCase() || '';
	const {name: fileNameWithoutExt, extension: fileExt} = splitFilename(fileName);
	const showTextPreview = !isPreview && shouldPreviewAttachment(attachment);
	const getFileTypeIcon = () => {
		const size = remFromPx(32);
		const imageTypes = ['jpg', 'jpeg', 'png', 'gif', 'webp', 'svg', 'bmp', 'ico'];
		const textTypes = ['txt', 'rtf', 'md', 'log'];
		const archiveTypes = ['zip', 'rar', '7z', 'tar', 'gz', 'bz2', 'xz'];
		const audioTypes = ['mp3', 'wav', 'ogg', 'flac', 'm4a', 'aac', 'wma'];
		const videoTypes = ['mp4', 'webm', 'mov', 'avi', 'mkv', 'flv', 'wmv'];
		const codeTypes = [
			'js',
			'jsx',
			'ts',
			'tsx',
			'py',
			'java',
			'c',
			'cpp',
			'h',
			'css',
			'html',
			'json',
			'xml',
			'yml',
			'yaml',
			'sh',
			'go',
			'rs',
			'rb',
			'php',
		];
		const excelTypes = ['xls', 'xlsx', 'csv'];
		const presentationTypes = ['ppt', 'pptx'];
		const documentTypes = ['doc', 'docx'];
		if (imageTypes.includes(fileExtension))
			return (
				<FileImageIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-image-icon"
				/>
			);
		if (fileExtension === 'pdf')
			return (
				<FilePdfIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-pdf-icon"
				/>
			);
		if (textTypes.includes(fileExtension))
			return (
				<FileTextIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-text-icon"
				/>
			);
		if (documentTypes.includes(fileExtension))
			return (
				<FileTextIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-text-icon--2"
				/>
			);
		if (archiveTypes.includes(fileExtension))
			return (
				<FileArchiveIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-archive-icon"
				/>
			);
		if (audioTypes.includes(fileExtension))
			return (
				<FileAudioIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-audio-icon"
				/>
			);
		if (videoTypes.includes(fileExtension))
			return (
				<FileVideoIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-video-icon"
				/>
			);
		if (codeTypes.includes(fileExtension))
			return (
				<FileCodeIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-code-icon"
				/>
			);
		if (excelTypes.includes(fileExtension))
			return (
				<FileXlsIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-xls-icon"
				/>
			);
		if (presentationTypes.includes(fileExtension))
			return (
				<FilePptIcon
					size={size}
					data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-ppt-icon"
				/>
			);
		return <FileIcon size={size} data-flx="channel.embeds.attachments.attachment-file.get-file-type-icon.file-icon" />;
	};
	let containerStyles: React.CSSProperties;
	if (isMobile) {
		containerStyles = {
			display: 'grid',
			width: '100%',
			maxWidth: '100%',
			minWidth: 0,
		};
	} else if (showTextPreview) {
		containerStyles = {
			display: 'grid',
			width: '100%',
			maxWidth: '50vw',
			minWidth: 0,
		};
	} else {
		containerStyles = {
			display: 'grid',
			width: '100%',
			maxWidth: '400px',
			minWidth: 'min(400px, 100%)',
		};
	}
	const handleDownload = async (e: React.MouseEvent) => {
		e.preventDefault();
		const downloadUrl = attachment.proxy_url ?? attachment.url;
		if (!downloadUrl || isExpired) return;
		await downloadFile(downloadUrl, 'file', fileName);
	};
	const handleDelete = useDeleteAttachment(message, attachment.id);
	const canDelete = canDeleteAttachmentUtil(message) && !isMobile;
	const showDeleteButton = canDelete && !isPreview;
	const messageViewContext = useMaybeMessageViewContext();
	const handleMouseDown = () => {
		window.getSelection()?.removeAllRanges();
	};
	const handleContextMenu = (e: React.MouseEvent) => {
		if (!message || isPreview) return;
		e.preventDefault();
		e.stopPropagation();
		ContextMenuCommands.openFromEvent(e, ({onClose}) => (
			<MediaContextMenu
				message={message}
				sourceChannel={messageViewContext?.channel}
				originalSrc={attachment.url ?? ''}
				proxyURL={attachment.proxy_url ?? undefined}
				type="file"
				contentHash={attachment.content_hash}
				attachmentId={attachment.id}
				defaultName={attachment.filename}
				defaultAltText={attachment.filename}
				onClose={onClose}
				onDelete={isPreview ? () => {} : (messageViewContext?.handleDelete ?? (() => {}))}
				data-flx="channel.embeds.attachments.attachment-file.handle-context-menu.media-context-menu.file"
			/>
		));
	};
	return (
		<div
			role="group"
			style={containerStyles}
			className={attachmentFileStyles.container}
			onContextMenu={handleContextMenu}
			onMouseDown={handleMouseDown}
			data-flx="channel.embeds.attachments.attachment-file.group.mouse-down"
		>
			{showDeleteButton && (
				<button
					type="button"
					onClick={handleDelete}
					className={clsx(messageStyles.hoverAction, attachmentFileStyles.deleteButton)}
					aria-label={i18n._(DELETE_ATTACHMENT_DESCRIPTOR)}
					data-flx="channel.embeds.attachments.attachment-file.button.delete"
				>
					<TrashIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="channel.embeds.attachments.attachment-file.trash-icon"
					/>
				</button>
			)}
			{showTextPreview ? (
				<TextualAttachmentPreview
					attachment={attachment}
					data-flx="channel.embeds.attachments.attachment-file.textual-attachment-preview"
				/>
			) : (
				<div
					className={attachmentFileStyles.attachmentContainer}
					data-flx="channel.embeds.attachments.attachment-file.div"
				>
					<div
						className={attachmentFileStyles.iconContainer}
						data-flx="channel.embeds.attachments.attachment-file.div--2"
					>
						{getFileTypeIcon()}
					</div>
					<div
						className={attachmentFileStyles.fileInfoContainer}
						data-flx="channel.embeds.attachments.attachment-file.div--3"
					>
						<p className={attachmentFileStyles.fileName} data-flx="channel.embeds.attachments.attachment-file.p">
							<span
								className={attachmentFileStyles.fileNameTruncate}
								data-flx="channel.embeds.attachments.attachment-file.span"
							>
								{fileNameWithoutExt}
							</span>
							<span
								className={attachmentFileStyles.fileExtension}
								data-flx="channel.embeds.attachments.attachment-file.span--2"
							>
								{fileExt}
							</span>
						</p>
						<p className={attachmentFileStyles.fileSize} data-flx="channel.embeds.attachments.attachment-file.p--2">
							{fileSize}
						</p>
					</div>
					{isExpired ? (
						<Tooltip
							text={i18n._(ATTACHMENT_EXPIRED_DESCRIPTOR)}
							data-flx="channel.embeds.attachments.attachment-file.tooltip"
						>
							<div
								className={clsx(attachmentFileStyles.downloadButton, attachmentFileStyles.downloadButtonDisabled)}
								data-flx="channel.embeds.attachments.attachment-file.div--4"
							>
								<WarningCircleIcon
									size={remFromPx(20)}
									weight="bold"
									data-flx="channel.embeds.attachments.attachment-file.warning-circle-icon"
								/>
							</div>
						</Tooltip>
					) : (
						<button
							type="button"
							onClick={handleDownload}
							className={attachmentFileStyles.downloadButton}
							aria-label={i18n._(DOWNLOAD_DESCRIPTOR)}
							disabled={!attachment.url}
							data-flx="channel.embeds.attachments.attachment-file.button.download"
						>
							<DownloadSimpleIcon
								size={remFromPx(20)}
								weight="bold"
								data-flx="channel.embeds.attachments.attachment-file.download-simple-icon"
							/>
						</button>
					)}
				</div>
			)}
		</div>
	);
});
