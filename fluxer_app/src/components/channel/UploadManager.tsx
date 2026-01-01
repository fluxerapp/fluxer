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

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {MAX_ATTACHMENTS_PER_MESSAGE, MessageStates, MessageTypes} from '~/Constants';
import {TooManyAttachmentsModal} from '~/components/alerts/TooManyAttachmentsModal';
import {UploadDropModal} from '~/components/modals/UploadDropModal';
import {useSlowmode} from '~/hooks/useSlowmode';
import {CloudUpload} from '~/lib/CloudUpload';
import type {ChannelRecord} from '~/records/ChannelRecord';
import {MessageRecord} from '~/records/MessageRecord';
import ModalStore from '~/stores/ModalStore';
import UserStore from '~/stores/UserStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const UPLOAD_DROP_MODAL_KEY = 'upload-drop-modal';

export const UploadManager = observer(({channel}: {channel: ChannelRecord}) => {
	const [isDragging, setIsDragging] = React.useState(false);
	const [dragCounter, setDragCounter] = React.useState(0);
	const [isShiftHeld, setIsShiftHeld] = React.useState(false);
	const {isSlowmodeActive} = useSlowmode(channel);
	const pendingFileCountRef = React.useRef(0);
	const resetDragState = React.useCallback(() => {
		setIsDragging(false);
		setDragCounter(0);
		setIsShiftHeld(false);
		ModalActionCreators.popWithKey(UPLOAD_DROP_MODAL_KEY);
	}, []);

	const onDrop = React.useCallback(
		async (files: Array<File>, directUpload = false) => {
			if (directUpload && isSlowmodeActive) {
				directUpload = false;
			}

			if (directUpload) {
				if (files.length > MAX_ATTACHMENTS_PER_MESSAGE) {
					ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
					return;
				}

				const pendingAttachments = await CloudUpload.createAndStartUploads(channel.id, files);

				const nonce = SnowflakeUtils.fromTimestamp(Date.now());
				const currentUser = UserStore.getCurrentUser();
				if (!currentUser) return;

				CloudUpload.claimAttachmentsForMessage(channel.id, nonce, pendingAttachments, {
					content: '',
				});

				const uploadingAttachment = {
					id: 'uploading',
					filename: files.length === 1 ? files[0].name : `Uploading ${files.length} Files`,
					title: files.length === 1 ? files[0].name : undefined,
					size: files.reduce((total, f) => total + f.size, 0),
					url: '',
					proxy_url: '',
					content_type: 'application/octet-stream',
					flags: 0x1000,
				};

				const message = new MessageRecord({
					id: nonce,
					channel_id: channel.id,
					author: currentUser,
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

				MessageActionCreators.createOptimistic(channel.id, message.toJSON());

				MessageActionCreators.send(channel.id, {
					content: '',
					nonce,
					hasAttachments: true,
				});
			} else {
				const existingAttachments = CloudUpload.getTextareaAttachments(channel.id);
				const totalCount = existingAttachments.length + pendingFileCountRef.current + files.length;
				if (totalCount > MAX_ATTACHMENTS_PER_MESSAGE) {
					ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
					return;
				}

				pendingFileCountRef.current += files.length;
				try {
					await CloudUpload.addFiles(channel.id, files);
				} finally {
					pendingFileCountRef.current -= files.length;
				}
			}
		},
		[channel.id, isSlowmodeActive],
	);

	const handlePaste = React.useCallback(
		(event: ClipboardEvent) => {
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
				onDrop(files);
			}
		},
		[onDrop],
	);

	React.useEffect(() => {
		const handleDragEnter = (event: DragEvent) => {
			event.preventDefault();
			if (ModalStore.hasModalOpen() && !ModalStore.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			if (event.dataTransfer?.types?.includes('Files')) {
				setDragCounter((prev) => prev + 1);
				setIsDragging(true);
				setIsShiftHeld(event.shiftKey);
			}
		};

		const handleDragOver = (event: DragEvent) => {
			event.preventDefault();
			setIsShiftHeld(isSlowmodeActive ? false : event.shiftKey);
		};

		const handleDragLeave = (event: DragEvent) => {
			event.preventDefault();
			if (ModalStore.hasModalOpen() && !ModalStore.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			if (event.dataTransfer?.types?.includes('Files')) {
				setDragCounter((prev) => Math.max(prev - 1, 0));
			}
		};

		const handleDrop = (event: DragEvent) => {
			event.preventDefault();
			if (ModalStore.hasModalOpen() && !ModalStore.hasModal(UPLOAD_DROP_MODAL_KEY)) {
				return;
			}
			if (event.dataTransfer?.types?.includes('Files')) {
				const directUpload = isSlowmodeActive ? false : event.shiftKey;
				resetDragState();
				if (event.dataTransfer?.files) {
					onDrop(Array.from(event.dataTransfer.files), directUpload);
				}
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
	}, [onDrop, handlePaste, isDragging, isSlowmodeActive, resetDragState]);

	React.useEffect(() => {
		if (dragCounter === 0) {
			setIsDragging(false);
		}
	}, [dragCounter]);

	React.useEffect(() => {
		if (isDragging) {
			ModalActionCreators.pushWithKey(
				modal(() => (
					<UploadDropModal channel={channel} isShiftHeld={isShiftHeld} isSlowmodeActive={isSlowmodeActive} />
				)),
				UPLOAD_DROP_MODAL_KEY,
			);
		} else {
			ModalActionCreators.popWithKey(UPLOAD_DROP_MODAL_KEY);
		}
	}, [isDragging, channel, isShiftHeld, isSlowmodeActive]);

	return null;
});
