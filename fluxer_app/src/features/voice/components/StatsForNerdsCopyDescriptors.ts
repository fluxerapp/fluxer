// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const COPY_STATS_JSON_DESCRIPTOR = msg({
	message: 'Copy stats JSON',
	comment: 'Menu item label in the screen share menu. Copies stats for nerds data as JSON to the clipboard.',
});
export const COPIED_STATS_JSON_DESCRIPTOR = msg({
	message: 'Copied stats JSON to clipboard',
	comment: 'Toast shown after copying stats for nerds JSON to clipboard.',
});
