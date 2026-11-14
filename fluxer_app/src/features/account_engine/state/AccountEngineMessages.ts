// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const SENSITIVE_MESSAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'This message may contain sensitive content.',
	comment:
		'Shown instead of the message text in a desktop notification or inbox preview for another signed-in account, when the channel is marked as mature or has a content warning.',
});
