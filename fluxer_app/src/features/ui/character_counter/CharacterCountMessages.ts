// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const CHARACTERS_LEFT_DESCRIPTOR = msg({
	message: '{remaining, plural, one {# character left} other {# characters left}}',
	comment: 'Character counter status showing the number of characters remaining.',
});

export const CHARACTERS_LEFT_GET_TO_WRITE_UP_TO_CHARACTERS_DESCRIPTOR = msg({
	message:
		'{remaining, plural, one {# character left} other {# characters left}}. Get {premiumProductName} to write up to {premiumMaxLength, plural, one {# character} other {# characters}}.',
	comment: 'Character counter status with an upsell to the premium tier.',
});

export const MESSAGE_IS_TOO_LONG_DESCRIPTOR = msg({
	message: 'Message is too long',
	comment: 'Form validation error indicating the message exceeds the allowed length.',
});

export const CHARACTER_LIMIT_EXCEEDED_BY_DESCRIPTOR = msg({
	message: 'Character limit exceeded by {remaining}',
	comment: 'Character counter status indicating the input exceeds the limit by a number of characters.',
});
