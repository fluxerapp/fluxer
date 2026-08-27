// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {DELETE_ATTACHMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Trans, useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

interface DeleteAttachmentOptions {
	onDeleted?: () => void;
}

export function useDeleteAttachment(
	message: Message | null | undefined,
	attachmentId: string | null | undefined,
	options?: DeleteAttachmentOptions,
) {
	const {i18n} = useLingui();
	const onDeleted = options?.onDeleted;
	return useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!message || !attachmentId) return;
			const deleteAttachment = async () => {
				await MessageCommands.deleteAttachment(message.channelId, message.id, attachmentId);
				onDeleted?.();
			};
			if (e.shiftKey) {
				void deleteAttachment();
				return;
			}
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_ATTACHMENT_DESCRIPTOR)}
						description={
							<Trans>
								Are you sure you want to delete this attachment? This action cannot be undone and will remove the
								attachment from this message.
							</Trans>
						}
						primaryText={i18n._(DELETE_ATTACHMENT_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={deleteAttachment}
						data-flx="messaging.use-delete-attachment.confirm-modal"
					/>
				)),
			);
		},
		[message, attachmentId, i18n, onDeleted],
	);
}
