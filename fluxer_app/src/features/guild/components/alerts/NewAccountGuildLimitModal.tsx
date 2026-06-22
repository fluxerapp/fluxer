// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SLOW_DOWN_DESCRIPTOR = msg({
	message: 'Slow down',
	comment: 'Short label in the new account guild limit modal. Keep it concise.',
});
const NEW_ACCOUNTS_ARE_LIMITED_TO_JOINING_10_COMMUNITIES_DESCRIPTOR = msg({
	message: 'New accounts can join up to 10 communities in the first 24 hours. Try again later.',
	comment: 'Description text in the new account guild limit modal.',
});
export const NewAccountGuildLimitModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(SLOW_DOWN_DESCRIPTOR)}
			message={i18n._(NEW_ACCOUNTS_ARE_LIMITED_TO_JOINING_10_COMMUNITIES_DESCRIPTOR)}
			data-flx="guild.new-account-guild-limit-modal.confirm-modal"
		/>
	);
});
