// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {
	MINUTES_AND_SECONDS_DURATION_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SLOWMODE_ACTIVE_DESCRIPTOR = msg({
	message: 'Slowmode active',
	comment: 'Short label in the slowmode rate limited modal. Keep it concise.',
});
const SLOWMODE_WAIT_DURATION_DESCRIPTOR = msg({
	message: 'Slowmode is on — wait {duration} before sending another.',
	comment:
		'Modal body shown when slowmode blocks sending a message. {duration} is a localized short duration such as "2 minutes".',
});

interface SlowmodeRateLimitedModalProps {
	retryAfter: number;
}

export const SlowmodeRateLimitedModal = observer(({retryAfter}: SlowmodeRateLimitedModalProps) => {
	const {i18n} = useLingui();
	const formatTime = (seconds: number): string => {
		if (seconds < 60) {
			return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds});
		}
		const minutes = Math.floor(seconds / 60);
		const remainingSeconds = seconds % 60;
		if (remainingSeconds === 0) {
			return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes});
		}
		return i18n._(MINUTES_AND_SECONDS_DURATION_DESCRIPTOR, {minutes, seconds: remainingSeconds});
	};
	return (
		<GenericErrorModal
			title={i18n._(SLOWMODE_ACTIVE_DESCRIPTOR)}
			message={i18n._(SLOWMODE_WAIT_DURATION_DESCRIPTOR, {duration: formatTime(retryAfter)})}
			data-flx="slowmode.slowmode-rate-limited-modal.confirm-modal"
		/>
	);
});
