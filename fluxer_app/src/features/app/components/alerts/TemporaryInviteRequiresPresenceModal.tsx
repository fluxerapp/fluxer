// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {GenericErrorModal} from './GenericErrorModal';

const GATEWAY_CONNECTION_REQUIRED_DESCRIPTOR = msg({
	message: 'Gateway connection required',
	comment: 'Short label in the temporary invite requires presence modal.',
});
const YOU_MUST_BE_CONNECTED_TO_THE_GATEWAY_TO_DESCRIPTOR = msg({
	message: 'Temporary invites need a live connection. Check yours and try again.',
	comment:
		'Modal body shown when the user tries to accept a temporary invite while the realtime connection is offline.',
});
export const TemporaryInviteRequiresPresenceModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(GATEWAY_CONNECTION_REQUIRED_DESCRIPTOR)}
			message={i18n._(YOU_MUST_BE_CONNECTED_TO_THE_GATEWAY_TO_DESCRIPTOR)}
			data-flx="app.temporary-invite-requires-presence-modal.confirm-modal"
		/>
	);
});
