// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const BAN_ACTION_DESCRIPTOR = msg({
	message: 'Ban',
	comment: 'Short moderation action label. Ban a member from a community by removing them and preventing rejoining.',
});
export const REMOVE_TIMEOUT_DESCRIPTOR = msg({
	message: 'Remove timeout',
	comment: 'Moderation action label that clears an active timeout on a community member.',
});
export const REPORT_USER_PROFILE_DESCRIPTOR = msg({
	message: 'Report profile',
	comment:
		"Action label in the full user profile's overflow menu, and the title of the report window it opens. Reports the user's profile: their photo, name or bio.",
});
export const TIMEOUT_DESCRIPTOR = msg({
	message: 'Timeout',
	comment: 'Moderation action label that times a member out (temporary mute) in a community.',
});
export const BLOCK_DESCRIPTOR = msg({
	message: 'Block',
	comment: 'Generic destructive action label that blocks a user.',
});
