// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const ADD_FRIEND_DESCRIPTOR = msg({
	message: 'Add friend',
	comment: 'Relationship action or header for sending a friend request to another user.',
});
export const ACCEPT_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Accept friend request',
	comment: 'Relationship action label for accepting an incoming friend request.',
});
export const ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR = msg({
	message: 'Accept',
	comment: 'Button or tooltip for accepting an incoming friend request.',
});
export const IGNORE_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Ignore friend request',
	comment: 'Relationship action label for ignoring an incoming friend request.',
});
export const IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR = msg({
	message: 'Ignore',
	comment: 'Button, menu item, or tooltip for ignoring an incoming friend request.',
});
export const CANCEL_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Cancel friend request',
	comment: 'Relationship action label for canceling an outgoing friend request.',
});
export const INCOMING_FRIEND_REQUEST_STATUS_DESCRIPTOR = msg({
	message: 'Incoming friend request',
	comment: 'Friend list status label for a user who sent the current user a friend request.',
});
export const OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR = msg({
	message: 'Friend request sent',
	comment: 'Status label or success message for a user the current user sent a friend request to.',
});
export const FRIEND_ADDED_DESCRIPTOR = msg({
	message: 'Friend added',
	comment: "Success title or toast shown after a user becomes the current user's friend.",
});
export const REMOVE_FRIEND_DESCRIPTOR = msg({
	message: 'Remove friend',
	comment: 'Relationship action label for removing a user from your friends.',
});
export const UNBLOCK_USER_ACTION_DESCRIPTOR = msg({
	message: 'Unblock',
	comment: 'Relationship action label for removing a block from this user.',
});
export const FRIENDS_DISABLED_DESCRIPTOR = msg({
	message: 'Friend requests are turned off on this instance.',
	comment:
		'Error shown when sending or accepting a friend request fails because the instance admins have turned off friends for the whole instance.',
});
