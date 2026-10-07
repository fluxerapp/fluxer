// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Browser sign-in is unavailable here.',
	comment: 'Error shown when the browser sign-in handoff cannot run in the current environment.',
});
export const DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR = msg({
	message: 'That sign-in code expired. Try again.',
	comment: 'Error shown when a browser sign-in handoff code expires before it is approved.',
});
