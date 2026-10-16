// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitedConfirmModal} from '@app/features/app/components/alerts/RateLimitedConfirmModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const EDITING_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Editing limit reached',
	comment: 'Title of the alert shown when edits to a published announcement message are rate limited.',
});
const EDIT_LIMIT_WITH_DURATION_DESCRIPTOR = msg({
	message: 'Each published message allows 3 quick edits, then 1 every 20 minutes. Try again in {duration}.',
	comment:
		'Body of the alert shown when a published announcement message hit its editing limit. {duration} is a localized short duration such as "12 minutes".',
});
const EDIT_LIMIT_DESCRIPTOR = msg({
	message: 'Each published message allows 3 quick edits, then 1 every 20 minutes. Try again later.',
	comment:
		'Body of the alert shown when a published announcement message hit its editing limit and no wait time is known.',
});

interface PublishedMessageEditLimitModalProps {
	retryAfter?: number;
}

export const PublishedMessageEditLimitModal = observer(({retryAfter}: PublishedMessageEditLimitModalProps) => {
	const {i18n} = useLingui();
	return (
		<RateLimitedConfirmModal
			title={i18n._(EDITING_LIMIT_REACHED_DESCRIPTOR)}
			retryAfter={retryAfter}
			describe={(duration) =>
				duration ? i18n._(EDIT_LIMIT_WITH_DURATION_DESCRIPTOR, {duration}) : i18n._(EDIT_LIMIT_DESCRIPTOR)
			}
			data-flx="messaging.published-message-edit-limit-modal.rate-limited-confirm-modal"
		/>
	);
});
