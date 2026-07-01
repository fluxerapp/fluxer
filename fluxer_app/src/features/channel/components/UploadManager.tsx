// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {AttachmentPermissionDeniedModal} from '@app/features/messaging/components/alerts/AttachmentPermissionDeniedModal';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {TooManyAttachmentsModal} from '@app/features/messaging/components/alerts/TooManyAttachmentsModal';
import {UploadDropModal} from '@app/features/messaging/components/modals/UploadDropModal';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import MessageQueue from '@app/features/messaging/state/MessageQueue';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {isDialogPasteTarget} from '@app/features/messaging/utils/TextInputEditUtils';
import {formatUploadingAttachmentSummary} from '@app/features/messaging/utils/UploadingAttachmentLabelUtils';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {useSlowmode} from '@app/features/slowmode/hooks/useSlowmode';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Modal from '@app/features/ui/state/Modal';
import Users from '@app/features/user/state/Users';
import {MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_ATTACHMENTS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const UPLOAD_DROP_MODAL_KEY = 'upload-drop-modal';
const hasFileTransfer = (event: DragEvent): boolean => event.dataTransfer?.types?.includes('Files') ?? false;

interface UploadManagerProps {
	channel: Channel;
	canAttachFiles: boolean;
	canSendMessages: boolean;
}

export const UploadManager = observer(({channel, canAttachFiles, canSendMessages}: UploadManagerProps) => {
	const {i18n} = useLingui();
	const [isDragging, setIsDragging] = useState(false);
	const [dragCounter, setDragCounter] = useState(0);
	const [isShiftHeld, setIsShiftHeld] = useState(false);
	const {isSlowmodeActive} = useSlowmode(channel);
	const pendingFileCountRef = useRef(0);
	const wasDraggingRef = useRef(false);
	const resetDragState = useCallback(() => {
		setIsDragging(false);
		setDragCounter(0);
		setIsShiftHeld(false);
		ModalCommands.popWithKey(UPLOAD_DROP_MODAL_KEY);
	}, []);
	const focusTextarea = useCallback(() => {
		setTimeout(() => {
			ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId: channel.id});
		}, 0);
	}, [channel.id]);
	const onDrop = useCallback(
		async (files: Array<File>, directUpload = false) => {
			if (!canSendMessages) {
				return;
			}
			if (!canAttachFiles) {
				const hasOnlyImages = files.length > 0 && files.every((file) => file.type.startsWith('image/'));
				ModalCommands.push(
					modal(() => (
						<AttachmentPermissionDeniedModal
							hasOnlyImages={hasOnlyImages}
							canSendMessages={canSendMessages}
							data-flx="channel.upload-manager.on-drop.attachment-permission-denied-modal"
						/>
					)),
				);
				return;
			}
			const maxAttachments = Users.getCurrentUser()?.maxAttachmentsPerMessage ?? MAX_ATTACHMENTS_PER_MESSAGE;
			if (directUpload && isSlowmodeActive) {
				directUpload = false;
			}
			if (directUpload) {
				if (files.length > maxAttachments) {
					ModalCommands.push(
						modal(() => (
							<TooManyAttachmentsModal data-flx="channel.upload-manager.on-drop.too-many-attachments-modal" />
						)),
					);
					return;
				}
				const maxFileSize = Users.getCurrentUser()?.maxAttachmentFileSize ?? 25 * 1024 * 1024;
				const oversizedFileCount = files.filter((file) => file.size > maxFileSize).length;
				if (oversizedFileCount > 0) {
					ModalCommands.push(
						modal(() => (
							<FileSizeTooLargeModal
								oversizedFileCount={oversizedFileCount}
								data-flx="channel.upload-manager.on-drop.file-size-too-large-modal"
							/>
						)),
					);
					return;
				}
				const pendingAttachments = await CloudUpload.createAndStartUploads(channel.id, files);
				const nonce = SnowflakeUtils.fromTimestamp(Date.now());
				const currentUser = Users.getCurrentUser();
				if (!currentUser) return;
				CloudUpload.claimAttachmentsForMessage(channel.id, nonce, pendingAttachments, {
					content: '',
				});
				const uploadingAttachment = UploadingAttachment.fromFiles(files, {
					formatMultipleFileLabel: (count) => formatUploadingAttachmentSummary(i18n, count),
				})?.toJSON();
				if (!uploadingAttachment) return;
				const message = new Message({
					id: nonce,
					channel_id: channel.id,
					author: currentUser.toJSON(),
					type: MessageTypes.DEFAULT,
					flags: 0,
					pinned: false,
					mention_everyone: false,
					content: '',
					timestamp: new Date().toISOString(),
					mentions: [],
					state: MessageStates.SENDING,
					nonce,
					attachments: [uploadingAttachment],
				});
				MessageCommands.createOptimistic(channel.id, message.toJSON());
				MessageCommands.send(channel.id, {
					content: '',
					nonce,
					hasAttachments: true,
				});
			} else {
				const existingAttachments = CloudUpload.getTextareaAttachments(channel.id);
				const totalCount = existingAttachments.length + pendingFileCountRef.current + files.length;
				if (totalCount > maxAttachments) {
					ModalCommands.push(
						modal(() => (
							<TooManyAttachmentsModal data-flx="channel.upload-manager.on-drop.too-many-attachments-modal--2" />
						)),
					);
					return;
				}
				const maxFileSize = Users.getCurrentUser()?.maxAttachmentFileSize ?? 25 * 1024 * 1024;
				const oversizedFileCount = files.filter((file) => file.size > maxFileSize).length;
				if (oversizedFileCount > 0) {
					ModalCommands.push(
						modal(() => (
							<FileSizeTooLargeModal
								oversizedFileCount={oversizedFileCount}
								data-flx="channel.upload-manager.on-drop.file-size-too-large-modal--2"
							/>
						)),
					);
					return;
				}
				focusTextarea();
				pendingFileCountRef.current += files.length;
				try {
					const attachments = await CloudUpload.addFiles(channel.id, files);
					MessageQueue.startTextareaAttachmentUploads(channel.id, attachments);
				} finally {
					pendingFileCountRef.current -= files.length;
				}
			}
		},
		[channel.id, focusTextarea, i18n, isSlowmodeActive, canAttachFiles, canSendMessages],
	);
	const handlePaste = useCallback(
		(event: ClipboardEvent) => {
			if (event.defaultPrevented || Modal.hasModalOpen() || isDialogPasteTarget(event.target)) {
				return;
			}
			const items = event.clipboardData?.items;
			if (!items) {
				return;
			}
			const files: Array<File> = [];
			for (const item of items) {
				if (item.kind === 'file') {
					const file = item.getAsFile();
					if (file) {
						files.push(file);
					}
				}
			}
			if (files.length > 0) {
				event.preventDefault();
				if (!canSendMessages) {
					return;
				}
				onDrop(files);
			}
		},
		[canSendMessages, onDrop],
	);
	useEffect(() => {
		const handleDragEnter = (event: DragEvent) => {
			if (!hasFileTransfer(event)) {
				return;
			}
			if (!canSendMessages) {
				event.preventDefault();
				resetDragState();
				return;
			}
			if (!canAttachFiles) {
				event.preventDefault();
				return;
			}
			event.preventDefault();
			if (Modal.hasModalOpen() && !Modal.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			setDragCounter((prev) => prev + 1);
			setIsDragging(true);
			setIsShiftHeld(event.shiftKey);
		};
		const handleDragOver = (event: DragEvent) => {
			if (!hasFileTransfer(event)) {
				return;
			}
			if (!canSendMessages) {
				event.preventDefault();
				resetDragState();
				return;
			}
			if (!canAttachFiles) {
				event.preventDefault();
				return;
			}
			event.preventDefault();
			setIsShiftHeld(isSlowmodeActive ? false : event.shiftKey);
		};
		const handleDragLeave = (event: DragEvent) => {
			if (!hasFileTransfer(event)) {
				return;
			}
			if (!canSendMessages) {
				event.preventDefault();
				resetDragState();
				return;
			}
			if (!canAttachFiles) {
				event.preventDefault();
				return;
			}
			event.preventDefault();
			if (Modal.hasModalOpen() && !Modal.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			setDragCounter((prev) => Math.max(prev - 1, 0));
		};
		const handleDrop = (event: DragEvent) => {
			if (!hasFileTransfer(event)) {
				return;
			}
			event.preventDefault();
			if (!canSendMessages) {
				resetDragState();
				return;
			}
			if (Modal.hasModalOpen() && !Modal.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			const directUpload = isSlowmodeActive ? false : event.shiftKey;
			resetDragState();
			const files = Array.from(event.dataTransfer?.files ?? []);
			if (files.length > 0) {
				onDrop(files, directUpload);
			}
		};
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Escape' && isDragging) {
				event.preventDefault();
				resetDragState();
			}
		};
		window.addEventListener('dragenter', handleDragEnter);
		window.addEventListener('dragover', handleDragOver);
		window.addEventListener('dragleave', handleDragLeave);
		window.addEventListener('drop', handleDrop);
		window.addEventListener('paste', handlePaste);
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('dragenter', handleDragEnter);
			window.removeEventListener('dragover', handleDragOver);
			window.removeEventListener('dragleave', handleDragLeave);
			window.removeEventListener('drop', handleDrop);
			window.removeEventListener('paste', handlePaste);
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [onDrop, handlePaste, isDragging, isSlowmodeActive, resetDragState, canAttachFiles, canSendMessages]);
	useEffect(() => {
		if (isDragging && (!canSendMessages || !canAttachFiles)) {
			resetDragState();
		}
	}, [canAttachFiles, canSendMessages, isDragging, resetDragState]);
	useEffect(() => {
		if (dragCounter === 0) {
			setIsDragging(false);
		}
	}, [dragCounter]);
	useEffect(() => {
		if (!isDragging) {
			if (!wasDraggingRef.current) return;
			wasDraggingRef.current = false;
			ModalCommands.popWithKey(UPLOAD_DROP_MODAL_KEY);
			return;
		}
		wasDraggingRef.current = true;
		ModalCommands.pushWithKey(
			modal(() => (
				<UploadDropModal
					channel={channel}
					isShiftHeld={isShiftHeld}
					isSlowmodeActive={isSlowmodeActive}
					data-flx="channel.upload-manager.upload-drop-modal"
				/>
			)),
			UPLOAD_DROP_MODAL_KEY,
		);
	}, [isDragging, channel, isShiftHeld, isSlowmodeActive]);
	useEffect(
		() => () => {
			wasDraggingRef.current = false;
			ModalCommands.popWithKey(UPLOAD_DROP_MODAL_KEY);
		},
		[],
	);
	return null;
});
