// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {GenericErrorModal} from './GenericErrorModal';

const MAX_POLL_VOTE_COUNT_REACHED = msg({
	message: 'Max poll vote count reached',
	comment: 'Short label in the max poll vote count reached modal.',
});
const YOU_REACHED_THE_MAX_VOTE_PER_ANSWER = msg({
	message: "You've reached the maximum number of votes per answer.",
	comment: 'Modal body shown when the maximum vote count per answer has been reached on a poll. Keep plain.',
});
export const MaxPollVoteCountReachedModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(MAX_POLL_VOTE_COUNT_REACHED)}
			message={i18n._(YOU_REACHED_THE_MAX_VOTE_PER_ANSWER)}
			data-flx="app.max-poll-vote-count-reached-modal.confirm-modal"
		/>
	);
});
