// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const YOUR_IP_IS_BANNED_DESCRIPTOR = msg({
	message: 'Your IP is banned',
	comment: 'Label in the user ip banned from guild modal. Keep the tone plain and specific.',
});
const YOUR_IP_ADDRESS_IS_BANNED_FROM_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Your IP address is banned from this community and you cannot join.',
	comment: 'Error message in the user ip banned from guild modal. Keep the tone plain and specific.',
});
export const UserIpBannedFromGuildModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(YOUR_IP_IS_BANNED_DESCRIPTOR)}
			message={i18n._(YOUR_IP_ADDRESS_IS_BANNED_FROM_THIS_COMMUNITY_DESCRIPTOR)}
			data-flx="moderation.user-ip-banned-from-guild-modal.confirm-modal"
		/>
	);
});
