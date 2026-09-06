// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const FIVE_MINUTES_DURATION_DESCRIPTOR = msg({
	message: '5 minutes',
	comment:
		'Fixed duration label for a five-minute interval. Used as a standalone label in duration text or option lists.',
});
export const THIRTY_MINUTES_DURATION_DESCRIPTOR = msg({
	message: '30 minutes',
	comment:
		'Fixed duration label for a 30-minute interval. Used as a standalone label in duration text or option lists.',
});
export const ONE_HOUR_DURATION_DESCRIPTOR = msg({
	message: '1 hour',
	comment: 'Fixed duration label for a one-hour interval. Used as a standalone label in duration text or option lists.',
});
export const SIX_HOURS_DURATION_DESCRIPTOR = msg({
	message: '6 hours',
	comment: 'Fixed duration label for a six-hour interval. Used as a standalone label in duration text or option lists.',
});
export const TWELVE_HOURS_DURATION_DESCRIPTOR = msg({
	message: '12 hours',
	comment: 'Fixed duration label for a 12-hour interval. Used as a standalone label in duration text or option lists.',
});
export const ONE_DAY_DURATION_DESCRIPTOR = msg({
	message: '1 day',
	comment: 'Fixed duration label for a one-day interval. Used as a standalone label in duration text or option lists.',
});
export const SEVEN_DAYS_DURATION_DESCRIPTOR = msg({
	message: '7 days',
	comment:
		'Fixed duration label for a seven-day interval. Used as a standalone label in duration text or option lists.',
});
export const ONE_WEEK_DURATION_DESCRIPTOR = msg({
	message: '1 week',
	comment: 'Fixed duration label for a one-week interval. Used as a standalone label in duration text or option lists.',
});
export const ONE_MONTH_DURATION_DESCRIPTOR = msg({
	message: '1 month',
	comment:
		'Fixed duration label for a one-month interval. Used as a standalone label in duration text or option lists.',
});
export const SECONDS_DURATION_PLURAL_DESCRIPTOR = msg({
	message: '{seconds, plural, one {# second} other {# seconds}}',
	comment: 'Generic duration label for a whole number of seconds.',
});
export const MINUTES_DURATION_PLURAL_DESCRIPTOR = msg({
	message: '{minutes, plural, one {# minute} other {# minutes}}',
	comment: 'Generic duration label for a whole number of minutes.',
});
export const HOURS_DURATION_PLURAL_DESCRIPTOR = msg({
	message: '{hours, plural, one {# hour} other {# hours}}',
	comment: 'Generic duration label for a whole number of hours.',
});
export const DAYS_DURATION_PLURAL_DESCRIPTOR = msg({
	message: '{days, plural, one {# day} other {# days}}',
	comment: 'Generic duration label for a whole number of days.',
});
export const MINUTES_AND_SECONDS_DURATION_DESCRIPTOR = msg({
	message:
		'{minutes, plural, one {# minute} other {# minutes}} and {seconds, plural, one {# second} other {# seconds}}',
	comment: 'Generic duration label for a short interval that includes minutes and remaining seconds.',
});
export const HOURS_AND_MINUTES_DURATION_DESCRIPTOR = msg({
	message: '{hours, plural, one {# hour} other {# hours}} and {minutes, plural, one {# minute} other {# minutes}}',
	comment: 'Generic duration label for an interval that includes hours and remaining minutes.',
});
export const TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Try again',
	comment: 'Generic action label for retrying a failed or interrupted operation.',
});
export const SIGN_IN_DESCRIPTOR = msg({
	message: 'Sign in',
	comment: 'Generic authentication action label. Use for buttons, links, page titles, and accessible labels.',
});
export const REGISTER_DESCRIPTOR = msg({
	message: 'Register',
	comment: 'Generic authentication action label for opening or submitting account registration.',
});
export const CREATE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Create account',
	comment: 'Generic authentication action label for starting or submitting account creation.',
});
export const NEED_ACCOUNT_DESCRIPTOR = msg({
	message: 'Need an account?',
	comment: 'Short prompt shown near links that open account registration.',
});
export const ALREADY_HAVE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Already have an account?',
	comment: 'Short prompt shown near links that open sign-in.',
});
export const BACK_TO_SIGN_IN_DESCRIPTOR = msg({
	message: 'Back to sign-in',
	comment: 'Navigation label that returns from recovery, MFA, or related authentication flows to sign-in.',
});
export const CANCEL_DESCRIPTOR = msg({
	message: 'Cancel',
	comment: 'Generic secondary-button label that dismisses a modal/sheet/form without saving.',
});
export const OKAY_DESCRIPTOR = msg({
	message: 'Okay',
	comment:
		'Generic acknowledgement button on an informational alert. Closes the alert. Use for benign confirms; for "I have read and understood" use UNDERSTOOD_DESCRIPTOR.',
});
export const GO_BACK_DESCRIPTOR = msg({
	message: 'Go back',
	comment: 'Generic back-navigation label in modals, settings tabs, and nested flows.',
});
export const MORE_OPTIONS_DESCRIPTOR = msg({
	message: 'More options',
	comment: 'Generic overflow-menu trigger aria-label / tooltip ("…" or kebab menu).',
});
export const OPEN_SETTINGS_DESCRIPTOR = msg({
	message: 'Open settings',
	comment: 'Generic action label that opens the global app settings modal.',
});
export const BACK_TO_SETTINGS_DESCRIPTOR = msg({
	message: 'Back to settings',
	comment: 'Back-navigation label that returns from a nested settings pane to the settings tab list.',
});
export const GENERAL_DESCRIPTOR = msg({
	message: 'General',
	comment: 'Generic section label for broad or default settings.',
});
export const SEARCH_SETTINGS_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Search settings…',
	comment: 'Placeholder text in settings search fields. The ellipsis indicates the user can type a query.',
});
export const SEARCH_SETTINGS_FIELD_LABEL_DESCRIPTOR = msg({
	message: 'Search settings',
	comment: 'Accessible label for settings search fields.',
});
export const SETTINGS_SECTIONS_DESCRIPTOR = msg({
	message: 'Settings sections',
	comment: 'ARIA label for the section navigation inside a settings modal.',
});
export const PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR = msg({
	message: 'Primary navigation',
	comment: 'ARIA landmark label for the main app navigation area.',
});
export const OPEN_NAMED_LANDMARK_DESCRIPTOR = msg({
	message: 'Open {landmarkName}',
	comment:
		'Generic action label that opens a named app area, settings menu, or settings sheet. {landmarkName} is a shared localized label for that destination.',
});
export const CREATE_DESCRIPTOR = msg({
	message: 'Create',
	comment: 'Generic primary action label that creates the item described by the current dialog or form.',
});
export const CLAIM_ACCOUNT_DESCRIPTOR = msg({
	message: 'Claim account',
	comment: 'Generic account action label that opens or submits the flow to secure an unclaimed account.',
});
export const CREATE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Create community',
	comment: 'Generic action or title label for creating a new community.',
});
export const VERIFY_DESCRIPTOR = msg({
	message: 'Verify',
	comment: 'Generic primary action label that confirms a code, identity check, or account verification step.',
});
export const VERIFY_EMAIL_DESCRIPTOR = msg({
	message: 'Verify email',
	comment: 'Shared action or page-title label for starting or completing email address verification.',
});
export const ENABLE_TWO_FACTOR_AUTH_DESCRIPTOR = msg({
	message: 'Enable two-factor auth',
	comment: 'Generic action label that opens or starts two-factor authentication setup.',
});
export const COPY_USER_ID_DESCRIPTOR = msg({
	message: 'Copy user ID',
	comment: 'Developer-mode action that copies a user snowflake/ID to the clipboard.',
});
export const OPEN_DM_DESCRIPTOR = msg({
	message: 'Open DM',
	comment: 'Action label that opens or switches to a direct-message channel with the selected user.',
});
export const BLOCKED_USER_DM_WARNING_DESCRIPTOR = msg({
	message: "You blocked {userName}. You won't be able to send messages unless you unblock them.",
	comment:
		'Warning shown before opening or composing a DM with a user the current user has blocked. {userName} is the blocked user.',
});
export const CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR = msg({
	message: "You can't send messages in this channel.",
	comment:
		'Permission-denied message shown when the current user cannot send messages in the current channel. Keep the tone calm and factual.',
});
export const START_VOICE_CALL_DESCRIPTOR = msg({
	message: 'Start voice call',
	comment: 'Action label that initiates a voice call (DM or group DM).',
});
export const SEARCH_FRIENDS_DESCRIPTOR = msg({
	message: 'Search friends',
	comment: 'Generic search-input placeholder for filtering the current friends list.',
});
export const CREATE_CATEGORY_DESCRIPTOR = msg({
	message: 'Create category',
	comment: 'Generic action label that creates a new channel category or favorites category.',
});
export const ONLINE_DESCRIPTOR = msg({
	message: 'Online',
	comment: 'Generic presence-status label indicating the user is connected and available.',
});
export const EMAIL_DESCRIPTOR = msg({
	message: 'Email',
	comment: 'Generic form-field label for an email address.',
});
export const PASSWORD_DESCRIPTOR = msg({
	message: 'Password',
	comment: 'Generic form-field label for a password.',
});
export const VERIFICATION_CODE_DESCRIPTOR = msg({
	message: 'Verification code',
	comment: 'Generic form-field label for a one-time verification code.',
});
export const DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Direct messages',
	comment: 'Generic section heading / label for the direct messages area.',
});
export const COMMUNITIES_DESCRIPTOR = msg({
	message: 'Communities',
	comment: 'Generic section heading / label for the list of communities the user is in.',
});
export const VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'Voice channel',
	comment: 'Generic label for a voice channel type.',
});
export const TEXT_CHANNEL_DESCRIPTOR = msg({
	message: 'Text channel',
	comment: 'Generic label for a text channel type.',
});
export const STICKER_DESCRIPTOR = msg({
	message: 'Sticker',
	comment: 'Generic label for one sticker.',
});
export const STICKERS_DESCRIPTOR = msg({
	message: 'Stickers',
	comment: 'Generic tab / section / category label for stickers.',
});
export const EMOJIS_DESCRIPTOR = msg({
	message: 'Emojis',
	comment: 'Generic tab / section / category label for emojis.',
});
export const GIFS_DESCRIPTOR = msg({
	message: 'GIFs',
	comment: 'Generic tab / section / category label for animated GIFs.',
});
export const AUDIO_DESCRIPTOR = msg({
	message: 'Audio',
	comment: 'Generic tab / section / category label for audio content or settings.',
});
export const MEDIA_DESCRIPTOR = msg({
	message: 'Media',
	comment: 'Generic tab / section / category label for media content.',
});
export const FAVORITES_DESCRIPTOR = msg({
	message: 'Favorites',
	comment: 'Generic section heading / category label for favorites.',
});
export const SETTINGS_DESCRIPTOR = msg({
	message: 'Settings',
	comment: 'Generic action label / section header that opens settings.',
});
export const FOLDER_SETTINGS_DESCRIPTOR = msg({
	message: 'Folder settings',
	comment: 'Shared label for the settings menu or modal that configures a community folder.',
});
export const TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR = msg({
	message: 'Try again in a moment.',
	comment: 'Generic short retry body used in error modals and toasts.',
});
export const SOMETHING_WENT_WRONG_DESCRIPTOR = msg({
	message: 'Something went wrong',
	comment: 'Generic error modal title for an unexpected failure.',
});
export const OPEN_LINK_DESCRIPTOR = msg({
	message: 'Open link',
	comment: 'Generic action or tooltip label for opening an external or attached link.',
});
export const INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR = msg({
	message: 'That image is invalid. Try another one.',
	comment: 'Generic image upload error shown when the selected file cannot be decoded or accepted.',
});
export const MATURE_CONTENT_DESCRIPTOR = msg({
	message: 'Mature content',
	comment: 'Generic title or label for content marked for mature audiences.',
});
export const COMPLETE_MATURE_CONTENT_CHECK_DESCRIPTOR = msg({
	message: 'Complete mature content check',
	comment: 'Action label that starts the credit-card check used to unlock mature content in supported regions.',
});
export const PROCEED_DESCRIPTOR = msg({
	message: 'Proceed',
	comment: 'Generic action label that confirms proceeding past a content warning or mature-content gate.',
});
export const RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR = msg({
	message: 'Reset mature content agreement state',
	comment: 'Developer tool action that forgets a local mature-content gate acknowledgement.',
});
export const UNDERSTOOD_DESCRIPTOR = msg({
	message: 'Understood',
	comment: 'Acknowledgement button on an informational modal. Closes the modal after the user has read the message.',
});
export const CLOSE_DESCRIPTOR = msg({
	message: 'Close',
	comment: 'Aria-label / button text for dismissing a modal, sheet, popout, or overlay. Short.',
});
export const DISMISS_DESCRIPTOR = msg({
	message: 'Dismiss',
	comment: 'Generic button label that dismisses a banner, modal, sheet, or prompt without taking the primary action.',
});
export const MARK_AS_READ_DESCRIPTOR = msg({
	message: 'Mark as read',
	comment: 'Action label that clears unread indicators for the selected channel, category, community, or DM.',
});
export const HIDE_MUTED_CHANNELS_DESCRIPTOR = msg({
	message: 'Hide muted channels',
	comment: 'Toggle label in community / sidebar settings that collapses muted channels from the channel list.',
});
export const ADD_TO_FAVORITES_DESCRIPTOR = msg({
	message: 'Add to favorites',
	comment: 'Action label that pins the current channel, DM, or attachment to the favorites list.',
});
export const REMOVE_FROM_FAVORITES_DESCRIPTOR = msg({
	message: 'Remove from favorites',
	comment: 'Action label that unpins the current channel, DM, or attachment from the favorites list.',
});
export const NOTIFICATION_SETTINGS_DESCRIPTOR = msg({
	message: 'Notification settings',
	comment: 'Submenu / section header that groups per-channel or per-community notification preferences.',
});
export const COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR = msg({
	message: 'Community notification settings',
	comment: 'Shared label or heading for per-community notification settings.',
});
export const NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR = msg({
	message: 'All messages',
	comment:
		'Notification level option. Sends a push for every message in the channel or community. Paired with "Only mentions" and "Nothing".',
});
export const NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR = msg({
	message: 'Only mentions',
	comment:
		'Notification level option. Sends a push only when the user is mentioned. Paired with "All messages" and "Nothing".',
});
export const NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR = msg({
	message: 'Nothing',
	comment:
		'Notification level option. Suppresses all pushes for the channel or community. Paired with "All messages" and "Only mentions".',
});
export const VIEW_PROFILE_DESCRIPTOR = msg({
	message: 'View profile',
	comment: 'Action label that opens the user profile modal or sheet for the selected user.',
});
export const INVITE_PEOPLE_DESCRIPTOR = msg({
	message: 'Invite people',
	comment: 'Action label that opens the community invite creation modal or sheet.',
});
export const TRANSFER_OWNERSHIP_DESCRIPTOR = msg({
	message: 'Transfer ownership',
	comment: 'Action label in community settings that hands ownership of the community to another member.',
});
export const PLAY_DESCRIPTOR = msg({
	message: 'Play',
	comment: 'Generic media-player control: start playback.',
});
export const PAUSE_DESCRIPTOR = msg({
	message: 'Pause',
	comment: 'Generic media-player control: pause playback.',
});
export const VOLUME_DESCRIPTOR = msg({
	message: 'Volume',
	comment: 'Generic media-player / voice control: volume slider label.',
});
export const DOWNLOAD_DESCRIPTOR = msg({
	message: 'Download',
	comment: 'Generic action label that downloads an attachment / file.',
});
export const COPIED_DESCRIPTOR = msg({
	message: 'Copied',
	comment: 'Generic short status label confirming a value was copied to the clipboard.',
});
export const LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Link copied to clipboard',
	comment: 'Generic toast confirming that a link was copied to the system clipboard.',
});
export const COPY_TEXT_DESCRIPTOR = msg({
	message: 'Copy text',
	comment: 'Action label that copies a message body / selected text to the clipboard.',
});
export const COPY_CODE_DESCRIPTOR = msg({
	message: 'Copy code',
	comment: 'Action label that copies a code block to the clipboard.',
});
export const COPY_CHANNEL_ID_DESCRIPTOR = msg({
	message: 'Copy channel ID',
	comment: 'Developer-mode action that copies a channel snowflake/ID to the clipboard.',
});
export const DEBUG_CHANNEL_DESCRIPTOR = msg({
	message: 'Debug channel',
	comment: 'Developer-mode action that opens the channel debug panel.',
});
export const FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR = msg({
	message: 'Failed to process the cropped image. Try again.',
	comment:
		'Toast error shown by image-cropping uploaders (avatar, banner, community icon) when the cropped image cannot be processed.',
});
export const CONNECTIONS_DESCRIPTOR = msg({
	message: 'Connections',
	comment: 'Section header for the linked-accounts area in privacy & safety settings.',
});
export const TEXT_TO_SPEECH_DESCRIPTOR = msg({
	message: 'Text-to-speech',
	comment: 'Feature label / section heading for the text-to-speech feature.',
});
export const PERSONAL_NOTES_DESCRIPTOR = msg({
	message: 'Personal notes',
	comment: 'Label for the user-only private channel where they can jot notes to themselves.',
});
export const ROLES_DESCRIPTOR = msg({
	message: 'Roles',
	comment: 'Generic section, column, or submenu label for community roles.',
});
export const SCOPES_DESCRIPTOR = msg({
	message: 'Scopes',
	comment: 'Generic OAuth label for requested authorization scopes.',
});
export const EXPIRES_DESCRIPTOR = msg({
	message: 'Expires',
	comment: 'Generic column or field label for an expiration date.',
});
export const TWO_FACTOR_AUTHENTICATION_DESCRIPTOR = msg({
	message: 'Two-factor authentication',
	comment: 'Generic security setting label for two-factor authentication.',
});
export const DISABLE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Disable account',
	comment: 'Destructive action or section label for temporarily disabling the current account.',
});
export const DELETE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Delete account',
	comment: 'Destructive action or section label for permanently deleting the current account.',
});
export const FRIENDS_OF_FRIENDS_DESCRIPTOR = msg({
	message: 'Friends of friends',
	comment: 'Privacy option label for people connected through mutual friends.',
});
export const COMMUNITY_MEMBERS_DESCRIPTOR = msg({
	message: 'Community members',
	comment: 'Privacy option label for people who share a community with the user.',
});
export const ADD_NOTE_DESCRIPTOR = msg({
	message: 'Add note',
	comment: 'Action label for adding a private note about a user.',
});
export const DELETE_MESSAGE_DESCRIPTOR = msg({
	message: 'Delete message',
	comment: 'Destructive action label for deleting a message.',
});
export const UNPIN_MESSAGE_DESCRIPTOR = msg({
	message: 'Unpin message',
	comment: 'Action label for removing a pinned message from the channel pins.',
});
export const EDIT_GROUP_DESCRIPTOR = msg({
	message: 'Edit group',
	comment: 'Action label for editing a group DM (name, icon, members).',
});
export const CHANGE_NICKNAME_DESCRIPTOR = msg({
	message: 'Change nickname',
	comment: 'Action label for changing a user nickname in a community.',
});
export const CREATE_CHANNEL_DESCRIPTOR = msg({
	message: 'Create channel',
	comment: 'Action label that opens the create-channel flow.',
});
export const REMOVE_BOOKMARK_DESCRIPTOR = msg({
	message: 'Remove bookmark',
	comment: 'Action label that removes a saved bookmark.',
});
export const WATCH_STREAM_DESCRIPTOR = msg({
	message: 'Watch stream',
	comment: 'Voice action label that starts watching a remote screen share.',
});
export const INBOX_DESCRIPTOR = msg({
	message: 'Inbox',
	comment: 'Generic header / tab label for the unread / mentions / bookmarks inbox.',
});
export const MEMBERS_DESCRIPTOR = msg({
	message: 'Members',
	comment: 'Generic header / tab label for a community or group member list.',
});
export const JUMP_DESCRIPTOR = msg({
	message: 'Jump',
	comment: 'Generic short action label that scrolls/navigates to a specific message.',
});
export const DESCRIPTION_DESCRIPTOR = msg({
	message: 'Description',
	comment: 'Generic form-field label for a free-form description.',
});
export const INCOMING_CALL_DESCRIPTOR = msg({
	message: 'Incoming call',
	comment: 'Generic label for an incoming voice or video call.',
});
export const TURN_OFF_CAMERA_DESCRIPTOR = msg({
	message: 'Turn off camera',
	comment: 'Voice / call action label that disables the local camera.',
});
export const ADD_CHANNEL_DESCRIPTOR = msg({
	message: 'Add channel',
	comment: 'Generic action label that opens the create-channel flow from inside a category or sidebar.',
});
export const CLEAR_SEARCH_DESCRIPTOR = msg({
	message: 'Clear search',
	comment: 'Generic short button / icon label that clears the active search query.',
});
export const COPY_MESSAGE_LINK_DESCRIPTOR = msg({
	message: 'Copy message link',
	comment: 'Generic action label that copies a permalink to the selected message.',
});
export const ADD_REACTION_DESCRIPTOR = msg({
	message: 'Add reaction',
	comment: 'Generic action label that opens the reaction picker for a message.',
});
export const BOOKMARK_MESSAGE_DESCRIPTOR = msg({
	message: 'Bookmark message',
	comment: 'Generic action label that saves a message to the bookmarks list.',
});
export const REPORT_MESSAGE_DESCRIPTOR = msg({
	message: 'Report message',
	comment: 'Generic action label that opens the report flow targeting a specific message.',
});
export const PIN_MESSAGE_DESCRIPTOR = msg({
	message: 'Pin message',
	comment: 'Generic action label that pins a message to the channel pins.',
});
export const CHANGE_FRIEND_NICKNAME_DESCRIPTOR = msg({
	message: 'Change friend nickname',
	comment: 'Generic action label that opens the friend-nickname edit flow.',
});
export const DEBUG_USER_DESCRIPTOR = msg({
	message: 'Debug user',
	comment: 'Developer-mode action that opens a user debug panel.',
});
export const CHANNEL_DEBUG_DESCRIPTOR = msg({
	message: 'Channel debug',
	comment: 'Title of the developer-mode channel debug modal (distinct from the action label that opens it).',
});
export const MUTE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Mute community',
	comment: 'Generic action label that mutes notifications for an entire community.',
});
export const UNMUTE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unmute community',
	comment: 'Generic action label that unmutes notifications for an entire community.',
});
export const DM_CLOSED_DESCRIPTOR = msg({
	message: 'DM closed',
	comment: 'Generic success-toast label confirming that a DM channel was closed.',
});
export const DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'Direct message',
	comment:
		'Generic singular label for a direct message conversation (distinct from the plural DIRECT_MESSAGES section heading).',
});
export const USERNAME_DESCRIPTOR = msg({
	message: 'Username',
	comment: 'Generic form-field label for a username.',
});
export const USER_DEBUG_DESCRIPTOR = msg({
	message: 'User debug',
	comment: 'Title of the developer-mode user debug modal.',
});
export const MUTE_CATEGORY_DESCRIPTOR = msg({
	message: 'Mute category',
	comment: 'Action label that mutes notifications for a channel category.',
});
export const UNMUTE_CATEGORY_DESCRIPTOR = msg({
	message: 'Unmute category',
	comment: 'Action label that unmutes notifications for a channel category.',
});
export const TRANSLATE_DESCRIPTOR = msg({
	message: 'Translate',
	comment: 'Generic action label that translates a message body.',
});
export const SUPPRESS_EMBEDS_DESCRIPTOR = msg({
	message: 'Suppress embeds',
	comment: 'Action label on a message that hides link previews / embeds.',
});
export const START_VIDEO_CALL_DESCRIPTOR = msg({
	message: 'Start video call',
	comment: 'Action label that initiates a video call (DM or group DM).',
});
export const SIGN_OUT_DESCRIPTOR = msg({
	message: 'Sign out',
	comment: 'Generic action label that signs the current user out of the app.',
});
export const SCREEN_READER_DESCRIPTOR = msg({
	message: 'Screen reader',
	comment: 'Section heading for screen-reader related accessibility settings.',
});
export const REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Reverse image search',
	comment: 'Generic action label that runs a reverse-image search on the selected media.',
});
export const REPLY_DESCRIPTOR = msg({
	message: 'Reply',
	comment: 'Generic action label that opens the reply composer for a message.',
});
export const REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR = msg({
	message: 'Removed from favorites',
	comment: 'Toast confirming an item was removed from the favorites list.',
});
export const ADDED_TO_FAVORITES_TOAST_DESCRIPTOR = msg({
	message: 'Added to favorites',
	comment: 'Toast confirming an item was added to the favorites list.',
});
export const ZOOM_OUT_DESCRIPTOR = msg({
	message: 'Zoom out',
	comment: 'Generic media-viewer action label that decreases zoom.',
});
export const ZOOM_IN_DESCRIPTOR = msg({
	message: 'Zoom in',
	comment: 'Generic media-viewer action label that increases zoom.',
});
export const UNPINNED_DM_DESCRIPTOR = msg({
	message: 'Unpinned DM',
	comment: 'Generic short toast / status label confirming a DM was unpinned.',
});
export const UNCATEGORIZED_DESCRIPTOR = msg({
	message: 'Uncategorized',
	comment: 'Generic label for items that do not belong to a category.',
});
export const OFFLINE_DESCRIPTOR = msg({
	message: 'Offline',
	comment: 'Generic presence-status label indicating the user is offline.',
});
export const IDLE_DESCRIPTOR = msg({
	message: 'Idle',
	comment: 'Generic presence-status label indicating the user is idle / AFK.',
});
export const MENTIONS_DESCRIPTOR = msg({
	message: 'Mentions',
	comment: 'Generic section / tab label for the mentions inbox.',
});
export const NEVER_DESCRIPTOR = msg({
	message: 'Never',
	comment: 'Generic short option label for "never" in time / frequency dropdowns.',
});
export const PINNED_DM_DESCRIPTOR = msg({
	message: 'Pinned DM',
	comment: 'Generic short toast / status label confirming a DM was pinned.',
});
export const REACTIONS_DESCRIPTOR = msg({
	message: 'Reactions',
	comment: 'Generic short section / tab / aria label for emoji reactions.',
});
export const PRIVACY_SETTINGS_DESCRIPTOR = msg({
	message: 'Privacy settings',
	comment: 'Generic section heading / button label for the privacy settings area.',
});
export const MARK_AS_UNREAD_DESCRIPTOR = msg({
	message: 'Mark as unread',
	comment: 'Generic action label that flips a channel back to unread.',
});
export const LEAVE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Leave community',
	comment: 'Destructive action label that removes the current user from a community.',
});
export const KICK_MEMBER_DESCRIPTOR = msg({
	message: 'Kick member',
	comment: 'Moderation action label that removes a member from a community without banning.',
});
export const JOIN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Join community',
	comment: 'Generic action label that accepts a community invite / joins a community.',
});
export const INVITES_DESCRIPTOR = msg({
	message: 'Invites',
	comment: 'Generic section / tab label for the invites list.',
});
export const MUTE_FAVORITES_DESCRIPTOR = msg({
	message: 'Mute favourites',
	comment: 'Action label that mutes notifications for the favourites pseudo-guild.',
});
export const UNMUTE_FAVORITES_DESCRIPTOR = msg({
	message: 'Unmute favourites',
	comment: 'Action label that unmutes notifications for the favourites pseudo-guild.',
});
export const HIDE_FAVORITES_DESCRIPTOR = msg({
	message: 'Hide favorites',
	comment: 'Toggle / action label that collapses or hides the favorites section.',
});
export const NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Notifications',
	comment: 'Generic section header / tab label for the notifications area.',
});
export const FAILED_TO_SEND_INVITE_DESCRIPTOR = msg({
	message: "Couldn't send invite. Try again.",
	comment: 'Toast shown when sending an invite (group DM or community) fails.',
});
export const EDIT_MESSAGE_DESCRIPTOR = msg({
	message: 'Edit message',
	comment: 'Generic action label that opens the message edit composer.',
});
export const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'Generic primary button label that advances a multi-step flow.',
});
export const NEXT_DESCRIPTOR = msg({
	message: 'Next',
	comment: 'Generic primary or pagination action label that advances to the next step, page, or item.',
});
export const CAMERA_DESCRIPTOR = msg({
	message: 'Camera',
	comment: 'Generic voice/video setting label for the active camera device.',
});
export const CAMERA_ON_DESCRIPTOR = msg({
	message: 'Camera on',
	comment: 'Generic action label that enables the local camera.',
});
export const COPY_COMMUNITY_ID_DESCRIPTOR = msg({
	message: 'Copy community ID',
	comment: 'Developer-mode action that copies a community snowflake/ID to the clipboard.',
});
export const COPY_MESSAGE_ID_DESCRIPTOR = msg({
	message: 'Copy message ID',
	comment: 'Developer-mode action that copies a message snowflake/ID to the clipboard.',
});
export const COPY_USERNAME_DESCRIPTOR = msg({
	message: 'Copy username',
	comment: 'Action that copies a user username (handle) to the clipboard.',
});
export const COPY_LINK_DESCRIPTOR = msg({
	message: 'Copy link',
	comment: 'Generic action label that copies a URL to the clipboard.',
});
export const ACTIVE_NOW_DESCRIPTOR = msg({
	message: 'Active now',
	comment: 'Status label indicating a user is currently active.',
});
export const DELETE_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Delete attachment',
	comment: 'Generic action label that removes an attachment from the upload tray / message.',
});
export const EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR = msg({
	message: 'Expression picker categories',
	comment: 'Aria label for the expression picker category nav.',
});
export const UNKNOWN_DESCRIPTOR = msg({
	message: 'Unknown',
	comment: 'Generic fallback label when a more specific value is unavailable.',
});
export const CHANNEL_DELETED_DESCRIPTOR = msg({
	message: 'Channel deleted',
	comment: 'Toast confirming a channel was successfully deleted.',
});
export const COMMUNITY_DEFAULT_DESCRIPTOR = msg({
	message: 'Community default',
	comment: 'Notification level option indicating the channel inherits the community default.',
});
export const CUSTOM_ELLIPSIS_DESCRIPTOR = msg({
	message: 'Custom…',
	comment: 'Last "Custom…" option in dropdown menus that opens a custom-value picker.',
});
export const DOMAIN_DESCRIPTOR = msg({
	message: 'Domain',
	comment: 'Generic form-field / table-column label for an internet domain.',
});
export const APP_ZOOM_LEVEL_DESCRIPTOR = msg({
	message: 'App zoom level',
	comment: 'Setting label for the global UI zoom level.',
});
export const ENABLE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Enable notifications',
	comment: 'Generic action label / toggle that turns on push or desktop notifications.',
});
export const ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Generic radio / dropdown option label for an "enhanced" tier.',
});
export const CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR = msg({
	message: 'Channel removed from favorites',
	comment: 'Toast confirming a channel was removed from the favorites list.',
});
export const CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR = msg({
	message: 'Channel added to favorites',
	comment: 'Toast confirming a channel was added to the favorites list.',
});
export const GET_PREMIUM_DESCRIPTOR = msg({
	message: 'Get {premiumProductName}',
	comment: 'Generic upsell CTA button to start the Plutonium purchase flow. Preserve {premiumProductName}.',
});
export const MENTION_COUNT_ARIA_DESCRIPTOR = msg({
	message: '{mentionCount} mentions',
	comment: 'Generic aria-label for an unread-mentions badge. Preserve {mentionCount}.',
});
export const SENT_DESCRIPTOR = msg({
	message: 'Sent',
	comment: 'Generic short status label confirming an item (invite, gift, theme link) was sent.',
});
export const SOUNDS_DESCRIPTOR = msg({
	message: 'Sounds',
	comment: 'Section heading / tab label for the notification-sounds area.',
});
