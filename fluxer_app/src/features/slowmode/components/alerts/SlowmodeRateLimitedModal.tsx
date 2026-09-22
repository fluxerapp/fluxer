// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SLOWMODE_ACTIVE_DESCRIPTOR = msg({
	message: 'Slowmode active',
	comment: 'Short label in the slowmode rate limited modal. Keep it concise.',
});
const SLOWMODE_WAIT_SECONDS_DESCRIPTOR = msg({
	message: 'Slowmode is on — wait {seconds, plural, one {# second} other {# seconds}} before sending another.',
	comment: 'Modal body shown when slowmode blocks sending a message. {seconds} is the remaining wait, under a minute.',
});
const SLOWMODE_WAIT_MINUTES_DESCRIPTOR = msg({
	message: 'Slowmode is on — wait {minutes, plural, one {# minute} other {# minutes}} before sending another.',
	comment:
		'Modal body shown when slowmode blocks sending a message. {minutes} is the remaining wait, a whole number of minutes.',
});
const SLOWMODE_WAIT_MINUTES_AND_SECONDS_DESCRIPTOR = msg({
	message:
		'Slowmode is on — wait {minutes, plural, one {# minute} other {# minutes}} and {seconds, plural, one {# second} other {# seconds}} before sending another.',
	comment:
		'Modal body shown when slowmode blocks sending a message. {minutes} and {seconds} are the two parts of the remaining wait.',
});

interface SlowmodeRateLimitedModalProps {
	retryAfter: number;
}

export const SlowmodeRateLimitedModal = observer(({retryAfter}: SlowmodeRateLimitedModalProps) => {
	const {i18n} = useLingui();
	const formatWaitMessage = (seconds: number): string => {
		if (seconds < 60) {
			return i18n._(SLOWMODE_WAIT_SECONDS_DESCRIPTOR, {seconds});
		}
		const minutes = Math.floor(seconds / 60);
		const remainingSeconds = seconds % 60;
		if (remainingSeconds === 0) {
			return i18n._(SLOWMODE_WAIT_MINUTES_DESCRIPTOR, {minutes});
		}
		return i18n._(SLOWMODE_WAIT_MINUTES_AND_SECONDS_DESCRIPTOR, {minutes, seconds: remainingSeconds});
	};
	return (
		<GenericErrorModal
			title={i18n._(SLOWMODE_ACTIVE_DESCRIPTOR)}
			message={formatWaitMessage(retryAfter)}
			data-flx="slowmode.slowmode-rate-limited-modal.confirm-modal"
		/>
	);
});
