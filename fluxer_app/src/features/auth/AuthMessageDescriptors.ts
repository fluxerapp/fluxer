// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const FORGOT_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot your password?',
	comment: 'Action label that starts the password reset flow from a sign-in surface.',
});
export const INSTANCE_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Instance unavailable',
	comment: 'Status label for a saved account whose instance cannot currently be reached.',
});
export const SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR = msg({
	message: 'Sign in with a passkey',
	comment: 'Action label that starts passkey authentication.',
});
export const SIGN_IN_WITH_BROWSER_DESCRIPTOR = msg({
	message: 'Sign in with your browser',
	comment: 'Action label that hands sign-in off to the system web browser.',
});
