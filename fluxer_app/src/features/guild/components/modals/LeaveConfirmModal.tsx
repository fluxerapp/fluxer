// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {DELETE_MY_MESSAGES_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type ReactNode, useCallback, useRef, useState} from 'react';

const DELETE_MY_MESSAGES_DESCRIPTION_DESCRIPTOR = msg({
	message: "Wipe every message you've sent here on the way out. Can't be undone.",
	comment:
		"Switch description in the leave-community or leave-group confirmation. Warns that deleting the caller's messages is destructive.",
});
const logger = new Logger('LeaveConfirmModal');
interface LeaveConfirmModalProps {
	title: string;
	body: ReactNode;
	extraSwitches?: ReactNode;
	onLeave: (deleteMessages: boolean) => Promise<void>;
	errorModal: () => React.ReactElement;
	toastText: string;
	dataFlx: string;
}
export function LeaveConfirmModal({
	title,
	body,
	extraSwitches,
	onLeave,
	errorModal,
	toastText,
	dataFlx,
}: LeaveConfirmModalProps) {
	const {i18n} = useLingui();
	const [deleteMessages, setDeleteMessages] = useState(false);
	const [submitting, setSubmitting] = useState(false);
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const handleConfirm = useCallback(async () => {
		setSubmitting(true);
		try {
			await onLeave(deleteMessages);
			ToastCommands.createToast({type: 'success', children: toastText});
			ModalCommands.pop();
		} catch (error) {
			logger.error('Failed to leave conversation', error);
			ModalCommands.pop();
			window.setTimeout(() => ModalCommands.push(ModalCommands.modal(errorModal)), 0);
		} finally {
			setSubmitting(false);
		}
	}, [deleteMessages, onLeave, toastText, errorModal]);
	return (
		<Modal.Root size="small" initialFocusRef={initialFocusRef} centered data-flx={dataFlx}>
			<Modal.Header title={title} data-flx="guild.leave-confirm-modal.modal-header" />
			<Modal.Content data-flx="guild.leave-confirm-modal.modal-content">
				<Modal.ContentLayout data-flx="guild.leave-confirm-modal.modal-content-layout">
					<Modal.Description data-flx="guild.leave-confirm-modal.modal-description">{body}</Modal.Description>
					{extraSwitches}
					<Switch
						label={i18n._(DELETE_MY_MESSAGES_DESCRIPTOR)}
						description={i18n._(DELETE_MY_MESSAGES_DESCRIPTION_DESCRIPTOR)}
						value={deleteMessages}
						onChange={setDeleteMessages}
						data-flx="guild.leave-confirm-modal.switch.set-delete-messages"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="guild.leave-confirm-modal.modal-footer">
				<Button onClick={() => ModalCommands.pop()} variant="secondary" data-flx="guild.leave-confirm-modal.button.pop">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				<Button
					onClick={handleConfirm}
					submitting={submitting}
					variant="danger"
					ref={initialFocusRef}
					data-flx="guild.leave-confirm-modal.button.confirm"
				>
					{title}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
}
