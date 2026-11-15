// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitedConfirmModal} from '@app/features/app/components/alerts/RateLimitedConfirmModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const YOU_RE_EDITING_MESSAGES_TOO_QUICKLY_DESCRIPTOR = msg({
	message: "You're editing messages too quickly",
	comment: 'Label in the message edit too quick modal.',
});

interface MessageEditTooQuickModalProps {
	retryAfter?: number;
}

export const MessageEditTooQuickModal = observer(({retryAfter}: MessageEditTooQuickModalProps) => {
	const {i18n} = useLingui();
	return (
		<RateLimitedConfirmModal
			title={i18n._(YOU_RE_EDITING_MESSAGES_TOO_QUICKLY_DESCRIPTOR)}
			retryAfter={retryAfter}
			data-flx="messaging.message-edit-too-quick-modal.rate-limited-confirm-modal"
		/>
	);
});
