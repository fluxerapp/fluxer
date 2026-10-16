// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitedConfirmModal} from '@app/features/app/components/alerts/RateLimitedConfirmModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const YOU_RE_SENDING_MESSAGES_TOO_QUICKLY_DESCRIPTOR = msg({
	message: "You're sending messages too quickly",
	comment: 'Label in the message send too quick modal.',
});

interface MessageSendTooQuickModalProps {
	retryAfter?: number;
	onRetry?: () => void;
}

export const MessageSendTooQuickModal = observer(({retryAfter, onRetry}: MessageSendTooQuickModalProps) => {
	const {i18n} = useLingui();
	return (
		<RateLimitedConfirmModal
			title={i18n._(YOU_RE_SENDING_MESSAGES_TOO_QUICKLY_DESCRIPTOR)}
			retryAfter={retryAfter}
			onRetry={onRetry}
			data-flx="messaging.message-send-too-quick-modal.rate-limited-confirm-modal"
		/>
	);
});
