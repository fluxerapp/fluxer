// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const PUBLISH_MESSAGE_TITLE_DESCRIPTOR = msg({
	message: 'Publish this message?',
	comment: 'Title of the confirmation shown before publishing a message from an announcement channel.',
});
const PUBLISH_MESSAGE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Every community that follows this channel gets a copy. If you edit or delete it later, the copies change too.',
	comment:
		'Body of the confirmation shown before publishing a message from an announcement channel to the communities that follow it.',
});
const PUBLISH_DESCRIPTOR = msg({
	message: 'Publish',
	comment: 'Confirm button on the alert shown before publishing a message from an announcement channel.',
});

interface MessageCrosspostConfirmModalProps {
	message: Message;
}

export const MessageCrosspostConfirmModal = observer(({message}: MessageCrosspostConfirmModalProps) => {
	const {i18n} = useLingui();
	return (
		<ConfirmModal
			title={i18n._(PUBLISH_MESSAGE_TITLE_DESCRIPTOR)}
			description={i18n._(PUBLISH_MESSAGE_DESCRIPTION_DESCRIPTOR)}
			message={message}
			primaryText={i18n._(PUBLISH_DESCRIPTOR)}
			primaryVariant="primary"
			onPrimary={async () => {
				await MessageCommands.crosspost(i18n, message.channelId, message.id);
			}}
			showShiftBypassConfirmationTip={true}
			data-flx="messaging.message-crosspost-confirm-modal.confirm-modal"
		/>
	);
});
