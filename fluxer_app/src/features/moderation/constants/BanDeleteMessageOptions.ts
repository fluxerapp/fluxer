// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface BanDeleteMessageOption {
	seconds: number;
	label: MessageDescriptor;
}

export const BAN_DELETE_MESSAGE_OPTIONS: ReadonlyArray<BanDeleteMessageOption> = [
	{
		seconds: 0,
		label: msg({
			message: "Don't Delete Any",
			comment: "Message-history-deletion option when banning a member. Keeps all of the member's messages.",
		}),
	},
	{
		seconds: 3600,
		label: msg({
			message: 'Previous Hour',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last hour.",
		}),
	},
	{
		seconds: 21600,
		label: msg({
			message: 'Previous 6 Hours',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last 6 hours.",
		}),
	},
	{
		seconds: 43200,
		label: msg({
			message: 'Previous 12 Hours',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last 12 hours.",
		}),
	},
	{
		seconds: 86400,
		label: msg({
			message: 'Previous 24 Hours',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last 24 hours.",
		}),
	},
	{
		seconds: 259200,
		label: msg({
			message: 'Previous 3 Days',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last 3 days.",
		}),
	},
	{
		seconds: 604800,
		label: msg({
			message: 'Previous 7 Days',
			comment:
				"Message-history-deletion option when banning a member. Deletes the member's messages from the last 7 days.",
		}),
	},
];

export const DEFAULT_BAN_DELETE_MESSAGE_SECONDS = 60 * 60 * 24;

export const BAN_DELETE_MESSAGE_SECONDS_CHOICE_VALUES: ReadonlySet<string> = new Set(
	BAN_DELETE_MESSAGE_OPTIONS.map((option) => String(option.seconds)),
);
