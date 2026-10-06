// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const OPEN_DIRECT_MESSAGE_DETAILS_FOR_DESCRIPTOR = msg({
	message: 'Open direct message details for {directMessageName}',
	comment:
		'Accessible label for the channel header title button in a DM. Opens the channel details panel. directMessageName is the recipient display name.',
});
export const OPEN_GROUP_DETAILS_FOR_DESCRIPTOR = msg({
	message: 'Open group details for {groupDMName}',
	comment: 'Accessible label for the channel header title button in a group DM. Opens the group details panel.',
});
export const OPEN_CHANNEL_DETAILS_FOR_DESCRIPTOR = msg({
	message: 'Open channel details for {channelName}',
	comment: 'Accessible label for the channel header title button in a community channel.',
});
export const OPEN_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'Open profile for {directMessageName}',
	comment: 'Accessible label for the avatar button in the channel header of a DM. Opens the recipient user profile.',
});
export const OPEN_DIRECT_MESSAGE_PROFILE_DESCRIPTOR = msg({
	message: 'Open direct message profile',
	comment:
		'Fallback accessible label for the avatar button in the channel header of a DM when the recipient name is not available.',
});
export const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Accessible label on the mobile back button in the channel header. Returns to the previous screen.',
});
export const SHOW_CHANNEL_LIST_DESCRIPTOR = msg({
	message: 'Show channel list',
	comment: 'Accessible label on the mobile channel header button that opens the channel list drawer.',
});
export const EDIT_GROUP_DETAILS_DESCRIPTOR = msg({
	message: 'Edit group details',
	comment: 'Tooltip on the channel header button in a group DM that opens the edit group modal.',
});
export const CHANNEL_ACTIONS_DESCRIPTOR = msg({
	message: 'Channel actions',
	comment: 'Tooltip on the channel header overflow menu button that opens a list of channel actions.',
});
export const VIDEO_CALL_DESCRIPTOR = msg({
	message: 'Video call',
	comment: 'Tooltip on the channel header button that starts a video call in a DM or group DM.',
});
export const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Tooltip on the channel header search button that opens the message search input.',
});
export const CREATE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Create group DM',
	comment:
		'Tooltip on the channel header plus button when viewing a one-on-one DM. Starts the create group DM flow with the current recipient.',
});
export const ADD_FRIENDS_TO_GROUP_DESCRIPTOR = msg({
	message: 'Add friends to group',
	comment: 'Tooltip on the channel header plus button in a group DM. Opens the add friends to group modal.',
});
export const MEMBERS_LIST_UNAVAILABLE_AT_THIS_SCREEN_WIDTH_DESCRIPTOR = msg({
	message: 'Member list unavailable at this screen width',
	comment:
		'Tooltip on the disabled members toggle in the channel header when the viewport is too narrow to show the panel.',
});
export const HIDE_MEMBERS_DESCRIPTOR = msg({
	message: 'Hide members',
	comment: 'Tooltip on the channel header members toggle when the members panel is currently shown.',
});
export const SHOW_MEMBERS_DESCRIPTOR = msg({
	message: 'Show members',
	comment: 'Tooltip on the channel header members toggle when the members panel is currently hidden.',
});
