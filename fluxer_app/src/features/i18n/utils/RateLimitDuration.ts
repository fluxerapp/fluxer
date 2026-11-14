// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MINUTES_AND_SECONDS_DURATION_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const RATE_LIMITED_DURATION_BODY_DESCRIPTOR = msg({
	message: "You're being rate limited. Wait {duration} before trying again.",
	comment:
		'Shown when an action is blocked by rate limiting. {duration} is a localized short duration such as "2 minutes".',
});

export function formatRateLimitDuration(i18n: I18n, retryAfterSeconds: number): string {
	const totalSeconds = Math.max(1, Math.ceil(retryAfterSeconds));
	if (totalSeconds < 60) {
		return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds: totalSeconds});
	}
	const minutes = Math.floor(totalSeconds / 60);
	const seconds = totalSeconds % 60;
	if (seconds === 0) {
		return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes});
	}
	return i18n._(MINUTES_AND_SECONDS_DURATION_DESCRIPTOR, {minutes, seconds});
}
