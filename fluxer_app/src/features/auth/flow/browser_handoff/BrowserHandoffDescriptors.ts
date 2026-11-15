// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Browser sign-in is unavailable here.',
	comment: 'Error shown when the browser sign-in handoff cannot run in the current environment.',
});
export const DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR = msg({
	message: 'The sign-in request expired before your browser approved it.',
	comment:
		'Error shown in the desktop app when a browser sign-in request expires before it is approved in the browser.',
});
export const DESKTOP_HANDOFF_DENIED_DESCRIPTOR = msg({
	message: 'The sign-in was cancelled in your browser.',
	comment: 'Error shown in the desktop app when the user pressed Cancel on the browser sign-in page.',
});
