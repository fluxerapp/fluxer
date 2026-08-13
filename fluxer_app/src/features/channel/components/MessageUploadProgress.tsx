// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/MessageUploadProgress.module.css';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {useMessageUpload} from '@app/features/messaging/hooks/useCloudUpload';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageQueue from '@app/features/messaging/state/MessageQueue';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FileIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const CANCEL_UPLOAD_DESCRIPTOR = msg({
	message: 'Cancel upload',
	comment: 'Button or menu action label in the channel and chat message upload progress. Keep it concise.',
});
const logger = new Logger('MessageUploadProgress');

interface MessageUploadProgressProps {
	attachment: MessageAttachment;
	message: Message;
}

export const MessageUploadProgress = observer(({attachment, message}: MessageUploadProgressProps) => {
	const {i18n} = useLingui();
	const {enabled: isMobile} = MobileLayout;
	const messageUpload = useMessageUpload(message.nonce || '');
	const resolveProgress = (): number | null => {
		if (!messageUpload) return null;
		if (typeof messageUpload.sendingProgress === 'number') {
			return Math.round(messageUpload.sendingProgress);
		}
		if (!messageUpload.attachments.length) return null;
		const withProgress = messageUpload.attachments.filter(
			(att) => att.uploadProgress !== undefined && att.status !== 'failed',
		);
		if (!withProgress.length) {
			return null;
		}
		const total = withProgress.reduce((sum, att) => sum + (att.uploadProgress ?? 0), 0);
		return Math.round(total / withProgress.length);
	};
	const hasFailedUploads = (): boolean => {
		if (!messageUpload) return false;
		return messageUpload.attachments.some((att) => att.status === 'failed');
	};
	const handleCancel = async () => {
		if (!message.nonce || !messageUpload) return;
		MessageQueue.cancelRequest(message.nonce);
		try {
			await Promise.all(messageUpload.attachments.map((att) => CloudUpload.cancelUpload(att.id)));
		} catch (error) {
			logger.error('Failed to cancel some uploads:', error);
		}
		CloudUpload.removeMessageUpload(message.nonce);
		MessageCommands.deleteOptimistic(message.channelId, message.id);
	};
	const progress = resolveProgress();
	const failed = hasFailedUploads();
	const fileName = attachment.filename;
	const fileSize = formatFileSize(attachment.size);
	const isIndeterminate = progress === null;
	const progressValue = progress ?? 0;
	const containerStyles: React.CSSProperties = isMobile
		? {
				display: 'grid',
				width: '100%',
				maxWidth: '100%',
				minWidth: 0,
			}
		: {
				display: 'grid',
				width: '400px',
				maxWidth: '400px',
			};
	return (
		<div style={containerStyles} data-flx="channel.message-upload-progress.div">
			<div className={styles.container} data-flx="channel.message-upload-progress.container">
				<div className={styles.iconContainer} data-flx="channel.message-upload-progress.icon-container">
					<FileIcon size={remFromPx(32)} data-flx="channel.message-upload-progress.file-icon" />
				</div>
				<div className={styles.content} data-flx="channel.message-upload-progress.content">
					<p className={styles.fileName} data-flx="channel.message-upload-progress.file-name">
						{fileName}
					</p>
					<p className={styles.fileSize} data-flx="channel.message-upload-progress.file-size">
						{fileSize}
					</p>
					<div className={styles.progressContainer} data-flx="channel.message-upload-progress.progress-container">
						{isIndeterminate ? (
							<div
								className={styles.progressBarIndeterminate}
								data-flx="channel.message-upload-progress.progress-bar-indeterminate"
							/>
						) : (
							<div
								className={`${styles.progressBar} ${failed ? styles.progressBarFailed : styles.progressBarNormal}`}
								style={{width: `${progressValue}%`}}
								data-flx="channel.message-upload-progress.progress-bar"
							/>
						)}
					</div>
				</div>
				<FocusRing offset={-2} data-flx="channel.message-upload-progress.focus-ring">
					<button
						type="button"
						onClick={handleCancel}
						className={styles.cancelButton}
						aria-label={i18n._(CANCEL_UPLOAD_DESCRIPTOR)}
						data-flx="channel.message-upload-progress.cancel-button"
					>
						<XIcon size={remFromPx(20)} weight="bold" data-flx="channel.message-upload-progress.x-icon" />
					</button>
				</FocusRing>
			</div>
		</div>
	);
});
