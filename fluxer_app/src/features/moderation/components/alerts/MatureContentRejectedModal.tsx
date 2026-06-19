// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const MATURE_CONTENT_NOT_ALLOWED_DESCRIPTOR = msg({
	message: 'Mature content not allowed',
	comment: 'Error message in the mature content rejected modal.',
});
const THIS_CHANNEL_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR = msg({
	message:
		'This channel is not marked for mature content. Mature content can only be sent in channels marked for mature content. Ask a moderator to update this channel if appropriate.',
	comment: 'Label in the mature content rejected modal.',
});
export const MatureContentRejectedModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(MATURE_CONTENT_NOT_ALLOWED_DESCRIPTOR)}
			message={i18n._(THIS_CHANNEL_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR)}
			data-flx="moderation.mature-content-rejected-modal.confirm-modal"
		/>
	);
});
