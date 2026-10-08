// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {CHANNEL_SETTINGS_LABEL_DESCRIPTOR} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {msg} from '@lingui/core/macro';

export type {
	ChannelDetailsBottomSheetProps,
	ChannelDetailsTab,
	QuickActionButtonProps,
} from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheetTypes';

export const logger = new Logger('ChannelDetailsBottomSheet');
export const DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'Direct message',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const GROUP_DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'Group direct message',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const GROUP_SETTINGS_DESCRIPTOR = msg({
	message: 'Group settings',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const DM_SETTINGS_DESCRIPTOR = msg({
	message: 'DM settings',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});

export {CHANNEL_SETTINGS_LABEL_DESCRIPTOR as CHANNEL_SETTINGS_DESCRIPTOR};

export const MARKED_AS_READ_DESCRIPTOR = msg({
	message: 'Marked as read',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const CHANNEL_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Channel ID copied to clipboard',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const PINNED_GROUP_DESCRIPTOR = msg({
	message: 'Pinned group',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const FAILED_TO_PIN_GROUP_DESCRIPTOR = msg({
	message: 'Failed to pin group',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const FAILED_TO_PIN_DM_DESCRIPTOR = msg({
	message: 'Failed to pin DM',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const UNPINNED_GROUP_DESCRIPTOR = msg({
	message: 'Unpinned group',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const FAILED_TO_UNPIN_GROUP_DESCRIPTOR = msg({
	message: 'Failed to unpin group',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const FAILED_TO_UNPIN_DM_DESCRIPTOR = msg({
	message: 'Failed to unpin DM',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR = msg({
	message: 'Close your DM with {recipientUsername}? You can reopen it anytime.',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const DM_CLOSED_DESCRIPTOR = msg({
	message: 'DM closed',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const USER_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'User ID copied to clipboard',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'this channel',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const DELETE_DESCRIPTOR = msg({
	message: 'Delete {channelType}',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete {channelLabel}? Can't be undone.",
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const FAILED_TO_DELETE_CHANNEL_DESCRIPTOR = msg({
	message: 'Failed to delete channel',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const COLLAPSE_CHANNEL_TOPIC_DESCRIPTOR = msg({
	message: 'Collapse channel topic',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const EXPAND_CHANNEL_TOPIC_DESCRIPTOR = msg({
	message: 'Expand channel topic',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const CHANNEL_DETAILS_SECTIONS_DESCRIPTOR = msg({
	message: 'Channel details sections',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const GROUP_OWNER_DESCRIPTOR = msg({
	message: 'Group owner',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const ADD_FRIENDS_TO_GROUP_DESCRIPTOR = msg({
	message: 'Add friends to group',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const EDIT_CHANNEL_DESCRIPTOR = msg({
	message: 'Edit channel',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const DEBUG_USER_DESCRIPTOR = msg({
	message: 'Debug user',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
export const CATEGORY_DEFAULT_DESCRIPTOR = msg({
	message: 'Category default',
	comment:
		'Channel details bottom sheet label or action (channel type, settings, toast, confirmation, or notification option).',
});
