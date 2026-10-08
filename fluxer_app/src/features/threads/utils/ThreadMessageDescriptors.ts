// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const THREADS_DESCRIPTOR = msg({
	message: 'Threads',
	comment: 'Title of the thread browser and label of the threads button in the channel header.',
});
export const THREAD_DESCRIPTOR = msg({
	message: 'Thread',
	comment: 'Generic noun for a single thread inside a text channel.',
});
export const CREATE_THREAD_DESCRIPTOR = msg({
	message: 'Create thread',
	comment: 'Action that opens the new thread pane for a channel or a message.',
});
export const NEW_THREAD_DESCRIPTOR = msg({
	message: 'New thread',
	comment: 'Title of the pane used to start a new thread.',
});
export const THREAD_NAME_DESCRIPTOR = msg({
	message: 'Thread name',
	comment: 'Label of the thread name input.',
});
export const PRIVATE_THREAD_DESCRIPTOR = msg({
	message: 'Private thread',
	comment: 'Label of the toggle that makes a new thread private.',
});
export const PRIVATE_THREAD_HINT_DESCRIPTOR = msg({
	message: 'Only people you invite and moderators can see this thread.',
	comment: 'Help text under the private thread toggle in the new thread pane.',
});
export const START_CONVERSATION_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Enter a message to start the conversation!',
	comment: 'Placeholder of the composer in the new thread pane.',
});
export const THREAD_NAME_REQUIRED_DESCRIPTOR = msg({
	message: 'Give the thread a name before you send.',
	comment: 'Error shown when the user sends the first message of a new thread without naming it.',
});
export const OPEN_THREAD_DESCRIPTOR = msg({
	message: 'Open thread',
	comment: 'Action that opens a thread in the side panel.',
});
export const OPEN_FULL_VIEW_DESCRIPTOR = msg({
	message: 'Open in full view',
	comment: 'Action that opens a thread as the main channel view instead of the side panel.',
});
export const CLOSE_THREAD_PANEL_DESCRIPTOR = msg({
	message: 'Close',
	comment: 'Button that closes the thread side panel.',
});
export const JOIN_THREAD_DESCRIPTOR = msg({
	message: 'Join thread',
	comment: 'Action that adds the current user to a thread.',
});
export const LEAVE_THREAD_DESCRIPTOR = msg({
	message: 'Leave thread',
	comment: 'Action that removes the current user from a thread.',
});
export const LEAVE_PRIVATE_THREAD_TITLE_DESCRIPTOR = msg({
	message: 'Leave private thread',
	comment: 'Title of the confirmation dialog shown before leaving a private thread.',
});
export const LEAVE_PRIVATE_THREAD_CONFIRM_DESCRIPTOR = msg({
	message: "Leave {threadName}? It's private, so you can only come back if someone adds you again.",
	comment: 'Confirmation text before leaving a private thread. threadName is the thread name.',
});
export const REMOVE_FROM_THREAD_DESCRIPTOR = msg({
	message: 'Remove from thread',
	comment: 'Moderation action that removes the selected member from a thread.',
});
export const ARCHIVE_THREAD_DESCRIPTOR = msg({
	message: 'Close thread',
	comment: 'Moderator or owner action that archives a thread so it leaves the channel list.',
});
export const UNARCHIVE_THREAD_DESCRIPTOR = msg({
	message: 'Open thread again',
	comment: 'Action that unarchives an archived thread.',
});
export const LOCK_THREAD_DESCRIPTOR = msg({
	message: 'Lock thread',
	comment: 'Moderator action that stops non-moderators from sending messages in a thread.',
});
export const UNLOCK_THREAD_DESCRIPTOR = msg({
	message: 'Unlock thread',
	comment: 'Moderator action that lets everyone send messages in a locked thread again.',
});
export const EDIT_THREAD_DESCRIPTOR = msg({
	message: 'Edit thread',
	comment: 'Action that opens the thread settings.',
});
export const DELETE_THREAD_DESCRIPTOR = msg({
	message: 'Delete thread',
	comment: 'Destructive moderator action that deletes a thread and all its messages.',
});
export const DELETE_THREAD_CONFIRM_DESCRIPTOR = msg({
	message: 'Are you sure you want to delete {threadName}? This cannot be undone.',
	comment: 'Confirmation text before deleting a thread. threadName is the thread name.',
});
export const THREAD_DELETED_DESCRIPTOR = msg({
	message: 'Thread deleted',
	comment: 'Toast after a thread was deleted.',
});
export const MUTE_THREAD_DESCRIPTOR = msg({
	message: 'Mute thread',
	comment: 'Action that mutes notifications and unread badges for a thread.',
});
export const UNMUTE_THREAD_DESCRIPTOR = msg({
	message: 'Unmute thread',
	comment: 'Action that unmutes a thread.',
});
export const COPY_THREAD_LINK_DESCRIPTOR = msg({
	message: 'Copy link',
	comment: 'Action that copies a link to the thread.',
});
export const COPY_THREAD_ID_DESCRIPTOR = msg({
	message: 'Copy thread ID',
	comment: 'Developer mode action that copies the thread ID.',
});
export const THREAD_LINK_COPIED_DESCRIPTOR = msg({
	message: 'Thread link copied',
	comment: 'Toast after the thread link was copied.',
});
export const NOTIFICATION_DEFAULT_DESCRIPTOR = msg({
	message: 'Use channel default',
	comment: 'Thread notification option that follows the parent channel setting.',
});
export const NOTIFICATION_ALL_DESCRIPTOR = msg({
	message: 'All messages',
	comment: 'Thread notification option that notifies for every message.',
});
export const NOTIFICATION_MENTIONS_DESCRIPTOR = msg({
	message: 'Only @mentions',
	comment: 'Thread notification option that notifies only for mentions.',
});
export const NOTIFICATION_NOTHING_DESCRIPTOR = msg({
	message: 'Nothing',
	comment: 'Thread notification option that never notifies.',
});
export const THREAD_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Notification settings',
	comment: 'Submenu label for the thread notification options.',
});
export const THREAD_ARCHIVED_NOTICE_DESCRIPTOR = msg({
	message: 'This thread is closed. Sending a message will open it again.',
	comment: 'Notice above the composer of an archived thread.',
});
export const THREAD_LOCKED_NOTICE_DESCRIPTOR = msg({
	message: 'This thread is locked. Only moderators can send messages.',
	comment: 'Barrier shown instead of the composer in a locked thread.',
});
export const THREAD_JOIN_NOTICE_DESCRIPTOR = msg({
	message: 'Join this thread to follow it in your channel list and get notifications.',
	comment: 'Notice above the composer when the user is not a member of the thread.',
});
export const THREAD_MEMBERS_DESCRIPTOR = msg({
	message: 'Thread members',
	comment: 'Title of the thread member list.',
});
export const MESSAGE_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# message} other {# messages}}',
	comment: 'Message count on a thread chip and in the thread browser. count is the number of messages.',
});
export const NO_RECENT_MESSAGES_DESCRIPTOR = msg({
	message: 'There are no recent messages in this thread.',
	comment: 'Preview text on a thread chip when the thread has no messages yet.',
});
export const ACTIVE_THREADS_DESCRIPTOR = msg({
	message: 'Active',
	comment: 'Tab in the thread browser that lists active threads.',
});
export const ARCHIVED_THREADS_DESCRIPTOR = msg({
	message: 'Closed',
	comment: 'Tab in the thread browser that lists archived threads.',
});
export const JOINED_THREADS_DESCRIPTOR = msg({
	message: 'Joined',
	comment: 'Section header in the thread browser for threads the user joined.',
});
export const OTHER_THREADS_DESCRIPTOR = msg({
	message: 'Other active threads',
	comment: 'Section header in the thread browser for active threads the user did not join.',
});
export const ARCHIVED_PRIVATE_THREADS_DESCRIPTOR = msg({
	message: 'Closed private',
	comment:
		'Tab in the thread browser that lists closed private threads. Keep it short, it shares a row with Active and Closed.',
});
export const NO_THREADS_DESCRIPTOR = msg({
	message: 'There are no threads',
	comment: 'Empty state of the thread browser.',
});
export const NO_THREADS_HINT_DESCRIPTOR = msg({
	message: 'Stay focused on a conversation with a thread, a temporary text channel.',
	comment: 'Empty state explanation in the thread browser.',
});
export const LOAD_MORE_DESCRIPTOR = msg({
	message: 'Load more',
	comment: 'Button that loads the next page of archived threads.',
});
export const STARTED_A_THREAD_DESCRIPTOR = msg({
	message: 'started a thread',
	comment: 'Fragment of the thread created system message. Appears after the author name.',
});
export const SEE_ALL_THREADS_DESCRIPTOR = msg({
	message: 'See all threads',
	comment: 'Link in the thread created system message that opens the thread browser.',
});
export const STARTER_MESSAGE_DELETED_DESCRIPTOR = msg({
	message: 'Original message was deleted.',
	comment: 'Shown at the top of a thread when the message it was started from was deleted.',
});
export const HIDE_AFTER_INACTIVITY_DESCRIPTOR = msg({
	message: 'Hide after inactivity',
	comment: 'Label of the thread auto archive duration setting.',
});
export const HIDE_AFTER_INACTIVITY_HINT_DESCRIPTOR = msg({
	message: 'New threads will stop showing in the channel list after this period of inactivity.',
	comment: 'Help text for the default thread auto archive duration setting on a text channel.',
});
export const DEFAULT_HIDE_AFTER_INACTIVITY_DESCRIPTOR = msg({
	message: 'Default hide after inactivity',
	comment: 'Label of the default auto archive duration for new threads in a text channel.',
});
export const DEFAULT_THREAD_SLOWMODE_DESCRIPTOR = msg({
	message: 'Default thread slowmode',
	comment: 'Label of the slowmode copied onto new threads in a text channel.',
});
export const INVITABLE_DESCRIPTOR = msg({
	message: 'Allow anyone to invite',
	comment: 'Private thread setting that lets non-moderators add other members.',
});
export const ONE_HOUR_DESCRIPTOR = msg({
	message: '1 hour',
	comment: 'Thread auto archive duration option.',
});
export const ONE_DAY_DESCRIPTOR = msg({
	message: '24 hours',
	comment: 'Thread auto archive duration option.',
});
export const THREE_DAYS_DESCRIPTOR = msg({
	message: '3 days',
	comment: 'Thread auto archive duration option.',
});
export const ONE_WEEK_DESCRIPTOR = msg({
	message: '1 week',
	comment: 'Thread auto archive duration option.',
});
export const THREAD_SETTINGS_DESCRIPTOR = msg({
	message: 'Thread settings',
	comment: 'Title of the thread settings modal.',
});
export const SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: 'Save button in the thread settings modal.',
});
export const SLOWMODE_COOLDOWN_DESCRIPTOR = msg({
	message: 'You can start another thread in {seconds, plural, one {# second} other {# seconds}}.',
	comment: 'Cooldown notice in the new thread pane when the parent channel has slowmode. seconds is the wait time.',
});
export const THREAD_CREATE_FAILED_DESCRIPTOR = msg({
	message: 'Could not start the thread: {detail}',
	comment: 'Toast when creating a thread failed. detail is the server error message.',
});
export const THREAD_ACTION_FAILED_DESCRIPTOR = msg({
	message: 'Something went wrong: {detail}',
	comment: 'Toast when a thread action failed. detail is the server error message.',
});
export const THREAD_STARTED_BY_DESCRIPTOR = msg({
	message: 'Started by {name}',
	comment: 'Subtitle at the top of a thread. name is the display name of the member who started it.',
});
export const SEARCH_THREADS_DESCRIPTOR = msg({
	message: 'Search threads',
	comment: 'Placeholder and label of the search box at the top of the thread browser.',
});
export const NO_MATCHING_THREADS_DESCRIPTOR = msg({
	message: 'No threads match',
	comment: 'Empty state of the thread browser while searching.',
});
export const NO_MATCHING_THREADS_HINT_DESCRIPTOR = msg({
	message: 'Try a different name or check the other tab.',
	comment: 'Hint under the empty search state in the thread browser.',
});
export const THREAD_SEARCH_FAILED_DESCRIPTOR = msg({
	message: "Couldn't search threads. Try again later.",
	comment: 'Error state of the thread browser search.',
});
export const THREAD_SEARCH_INDEXING_DESCRIPTOR = msg({
	message: 'Search is getting ready for this community. Trying again shortly...',
	comment: 'Shown in the thread browser while the search index is being built.',
});
export const RETRY_SEARCH_DESCRIPTOR = msg({
	message: 'Retry',
	comment: 'Button that retries a failed thread search.',
});
