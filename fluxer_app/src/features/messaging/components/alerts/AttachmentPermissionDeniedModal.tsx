// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const CAN_T_PASTE_IMAGE_HERE_DESCRIPTOR = msg({
	message: "Can't paste image here",
	comment: 'Error message in the attachment permission denied modal. Keep the tone plain and specific.',
});
const CAN_T_UPLOAD_FILES_HERE_DESCRIPTOR = msg({
	message: "Can't upload files here",
	comment: 'Error message in the attachment permission denied modal. Keep the tone plain and specific.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_ATTACH_FILES_DESCRIPTOR = msg({
	message: "You can't attach files in this channel.",
	comment: 'Description text in the attachment permission denied modal. Keep the tone plain and specific.',
});

interface AttachmentPermissionDeniedModalProps {
	hasOnlyImages?: boolean;
	canSendMessages?: boolean;
}

export const AttachmentPermissionDeniedModal = observer(
	({hasOnlyImages = false, canSendMessages = true}: AttachmentPermissionDeniedModalProps) => {
		const {i18n} = useLingui();
		const title = hasOnlyImages
			? i18n._(CAN_T_PASTE_IMAGE_HERE_DESCRIPTOR)
			: i18n._(CAN_T_UPLOAD_FILES_HERE_DESCRIPTOR);
		const description = canSendMessages ? (
			hasOnlyImages ? (
				<Trans>
					Images pasted from your clipboard are sent as attachments, and you don't have permission to attach files in
					this channel.
				</Trans>
			) : (
				i18n._(YOU_DON_T_HAVE_PERMISSION_TO_ATTACH_FILES_DESCRIPTOR)
			)
		) : (
			i18n._(CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR)
		);
		return (
			<GenericErrorModal
				title={title}
				message={description}
				data-flx="messaging.attachment-permission-denied-modal.confirm-modal"
			/>
		);
	},
);
