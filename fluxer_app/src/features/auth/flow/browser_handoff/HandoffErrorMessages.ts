// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpError} from '@app/features/platform/types/EndpointError';
import {failureCode, failureRetryAfterSeconds} from '@app/features/platform/utils/ResponseInspection';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const HANDOFF_LOCKOUT_FALLBACK_SECONDS = 900;
const DESKTOP_HANDOFF_HTTP_STATUS_PATTERN = /failed with HTTP \d{3}/u;

const HANDOFF_ATTEMPTS_EXCEEDED_DESCRIPTOR = msg({
	message: 'Too many attempts from your network. Try again in {minutes, plural, one {# minute} other {# minutes}}.',
	comment:
		'Shown on the browser sign-in page and in the desktop app after too many wrong or reused sign-in codes from the same network. minutes is how long until signing in works again.',
});
const HANDOFF_CODE_ALREADY_USED_DESCRIPTOR = msg({
	message: 'This code was already used. Start a new sign-in from the app.',
	comment:
		'Shown on the browser sign-in page when the code the user typed or opened was already used to approve a sign-in.',
});

export function resolveHandoffErrorMessage(i18n: I18n, error: unknown): string | null {
	switch (failureCode(error)) {
		case APIErrorCodes.HANDOFF_ATTEMPTS_EXCEEDED: {
			const seconds = failureRetryAfterSeconds(error) ?? HANDOFF_LOCKOUT_FALLBACK_SECONDS;
			return i18n._(HANDOFF_ATTEMPTS_EXCEEDED_DESCRIPTOR, {minutes: Math.max(1, Math.ceil(seconds / 60))});
		}
		case APIErrorCodes.HANDOFF_CODE_ALREADY_USED:
			return i18n._(HANDOFF_CODE_ALREADY_USED_DESCRIPTOR);
		default:
			return null;
	}
}

export function handoffReachedServer(error: unknown): boolean {
	return (
		error instanceof HttpError || (error instanceof Error && DESKTOP_HANDOFF_HTTP_STATUS_PATTERN.test(error.message))
	);
}
