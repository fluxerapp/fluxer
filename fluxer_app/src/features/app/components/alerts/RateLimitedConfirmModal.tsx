// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CLOSE_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	formatRateLimitDuration,
	RATE_LIMITED_DURATION_BODY_DESCRIPTOR,
} from '@app/features/i18n/utils/RateLimitDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const RATE_LIMITED_FALLBACK_BODY_DESCRIPTOR = msg({
	message: 'Slow down and wait a moment before trying again.',
	comment: 'Modal body shown when an action is blocked by rate limiting. Keep plain.',
});

interface RateLimitedConfirmModalProps {
	title: string;
	retryAfter?: number;
	onRetry?: () => void;
	describe?: (duration: string | undefined) => string;
}

export const RateLimitedConfirmModal = observer(
	({title, retryAfter, onRetry, describe}: RateLimitedConfirmModalProps) => {
		const {i18n} = useLingui();
		const hasRetryAfter = retryAfter != null;
		let description: string;
		if (describe) {
			const duration = hasRetryAfter ? formatRateLimitDuration(i18n, retryAfter) : undefined;
			description = describe(duration);
		} else if (hasRetryAfter) {
			description = i18n._(RATE_LIMITED_DURATION_BODY_DESCRIPTOR, {
				duration: formatRateLimitDuration(i18n, retryAfter),
			});
		} else {
			description = i18n._(RATE_LIMITED_FALLBACK_BODY_DESCRIPTOR);
		}
		if (!onRetry) {
			return (
				<ConfirmModal title={title} description={description} data-flx="app.rate-limited-confirm-modal.confirm-modal" />
			);
		}
		return (
			<ConfirmModal
				title={title}
				description={description}
				secondaryText={hasRetryAfter ? i18n._(TRY_AGAIN_DESCRIPTOR) : i18n._(CLOSE_DESCRIPTOR)}
				onSecondary={onRetry}
				data-flx="app.rate-limited-confirm-modal.confirm-modal"
			/>
		);
	},
);
