// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	CLOSE_DESCRIPTOR,
	MINUTES_AND_SECONDS_DURATION_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const RATE_LIMITED_FALLBACK_BODY_DESCRIPTOR = msg({
	message: 'Slow down — wait a moment before trying again.',
	comment: 'Modal body shown when an action is blocked by rate limiting. Keep plain.',
});
const RATE_LIMITED_DURATION_BODY_DESCRIPTOR = msg({
	message: "You're being rate limited. Wait {duration} before trying again.",
	comment:
		'Modal body shown when an action is blocked by rate limiting. {duration} is a localized short duration such as "2 minutes".',
});

interface RateLimitedConfirmModalProps {
	title: string;
	retryAfter?: number;
	onRetry?: () => void;
	hideCloseButton?: boolean;
	describe?: (duration: string | undefined) => string;
}

export const RateLimitedConfirmModal = observer(
	({title, retryAfter, onRetry, hideCloseButton, describe}: RateLimitedConfirmModalProps) => {
		const {i18n} = useLingui();
		const hasRetryAfter = retryAfter != null;
		const formatRateLimitTime = (totalSeconds: number): string => {
			if (totalSeconds < 60) {
				return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds: totalSeconds});
			}
			const minutes = Math.floor(totalSeconds / 60);
			const seconds = totalSeconds % 60;
			if (seconds === 0) {
				return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes});
			}
			return i18n._(MINUTES_AND_SECONDS_DURATION_DESCRIPTOR, {minutes, seconds});
		};
		return (
			<ConfirmModal
				title={title}
				description={
					describe
						? describe(hasRetryAfter ? formatRateLimitTime(retryAfter) : undefined)
						: hasRetryAfter
							? i18n._(RATE_LIMITED_DURATION_BODY_DESCRIPTOR, {
									duration: formatRateLimitTime(retryAfter),
								})
							: i18n._(RATE_LIMITED_FALLBACK_BODY_DESCRIPTOR)
				}
				secondaryText={hasRetryAfter ? i18n._(TRY_AGAIN_DESCRIPTOR) : i18n._(CLOSE_DESCRIPTOR)}
				onSecondary={onRetry}
				hideCloseButton={hideCloseButton}
				data-flx="app.rate-limited-confirm-modal.confirm-modal"
			/>
		);
	},
);
