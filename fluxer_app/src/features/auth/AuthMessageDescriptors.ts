// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const FORGOT_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot your password?',
	comment: 'Action label that starts the password reset flow from a sign-in surface.',
});
export const NEW_CODE_SENT_DESCRIPTOR = msg({
	message: 'We sent a new code.',
	comment: 'Short status line after a fresh email code was sent.',
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
export const NETWORK_BLOCKED_DESCRIPTOR = msg({
	message: 'Network blocked',
	comment: 'Title of the screen and dialog shown when the instance blocks the network a request came from.',
});
export const NETWORK_BLOCK_LEAD_WITH_REASON_DESCRIPTOR = msg({
	message: '{productName} blocked access from this network because of activity that broke its rules.',
	comment:
		'First sentence of the network blocked screen when a rule is shown. productName is the app name. Must read the same as the error message for this block.',
});
export const NETWORK_BLOCK_LEAD_DESCRIPTOR = msg({
	message: '{productName} blocked access from this network.',
	comment:
		'First sentence of the network blocked screen when no rule is shown. productName is the app name. Must read the same as the error message for this block.',
});
export const SHARED_NETWORK_DESCRIPTOR = msg({
	message:
		'If you share this network, for example at school, at work, on public Wi-Fi, through your mobile carrier or a VPN, this block may have nothing to do with you. The quickest fix is to switch networks, for example to mobile data instead of Wi-Fi, or to turn your VPN on or off.',
	comment:
		'Paragraph on the network blocked screen for people who share a network with others. Must read the same as the error message for this block.',
});

export const USE_A_DIFFERENT_EMAIL_DESCRIPTOR = msg({
	message: 'Use a different email',
	comment: 'Quiet text button that goes back to the email field of the sign-in screen.',
});
