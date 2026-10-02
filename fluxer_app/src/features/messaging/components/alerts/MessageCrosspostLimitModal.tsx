// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitedConfirmModal} from '@app/features/app/components/alerts/RateLimitedConfirmModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const PUBLISHING_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Publishing limit reached',
	comment: 'Title of the alert shown when publishing a message from an announcement channel is rate limited.',
});
const CHANNEL_LIMIT_WITH_DURATION_DESCRIPTOR = msg({
	message: 'This channel can publish 10 messages in a row, then one more every 6 minutes. Try again in {duration}.',
	comment:
		'Body of the alert shown when an announcement channel hit its publishing limit. {duration} is a localized short duration such as "4 minutes".',
});
const CHANNEL_LIMIT_DESCRIPTOR = msg({
	message: 'This channel can publish 10 messages in a row, then one more every 6 minutes. Try again in a few minutes.',
	comment: 'Body of the alert shown when an announcement channel hit its publishing limit and no wait time is known.',
});

interface MessageCrosspostLimitModalProps {
	retryAfter?: number;
}

export const MessageCrosspostLimitModal = observer(({retryAfter}: MessageCrosspostLimitModalProps) => {
	const {i18n} = useLingui();
	const describe = (duration: string | undefined): string =>
		duration ? i18n._(CHANNEL_LIMIT_WITH_DURATION_DESCRIPTOR, {duration}) : i18n._(CHANNEL_LIMIT_DESCRIPTOR);
	return (
		<RateLimitedConfirmModal
			title={i18n._(PUBLISHING_LIMIT_REACHED_DESCRIPTOR)}
			retryAfter={retryAfter}
			describe={describe}
			data-flx="messaging.message-crosspost-limit-modal.rate-limited-confirm-modal"
		/>
	);
});
