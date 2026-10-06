// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const THAT_MESSAGE_DIDN_T_DELETE_DESCRIPTOR = msg({
	message: "That message couldn't be deleted",
	comment: 'Label in the message delete failed modal. Keep the tone plain and specific.',
});
const TRY_DELETING_THAT_MESSAGE_AGAIN_DESCRIPTOR = msg({
	message: 'Try deleting that message again.',
	comment: 'Description text in the message delete failed modal. Keep the tone plain and specific.',
});
export const MessageDeleteFailedModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(THAT_MESSAGE_DIDN_T_DELETE_DESCRIPTOR)}
			message={i18n._(TRY_DELETING_THAT_MESSAGE_AGAIN_DESCRIPTOR)}
			data-flx="messaging.message-delete-failed-modal.generic-error-modal"
		/>
	);
});
