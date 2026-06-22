// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const COMMUNITY_AT_CAPACITY_DESCRIPTOR = msg({
	message: 'Community at capacity',
	comment: 'Short label in the guild at capacity modal. Keep it concise.',
});
const THIS_COMMUNITY_HAS_REACHED_ITS_MAXIMUM_MEMBER_LIMIT_DESCRIPTOR = msg({
	message: 'This community has reached its maximum member limit and is not accepting new members at this time.',
	comment: 'Description text in the guild at capacity modal.',
});
export const GuildAtCapacityModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(COMMUNITY_AT_CAPACITY_DESCRIPTOR)}
			message={i18n._(THIS_COMMUNITY_HAS_REACHED_ITS_MAXIMUM_MEMBER_LIMIT_DESCRIPTOR)}
			data-flx="guild.guild-at-capacity-modal.confirm-modal"
		/>
	);
});
