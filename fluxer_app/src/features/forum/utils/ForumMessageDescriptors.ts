// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const FORUM_CHANNEL_DESC_DESCRIPTOR = msg({
	message: 'Posts organized by topic and tags',
	comment: 'Description of the forum channel type option in the channel create modal.',
});
export const MEDIA_CHANNEL_DESC_DESCRIPTOR = msg({
	message: 'Posts built around images and videos',
	comment: 'Description of the media channel type option in the channel create modal.',
});
export const FORUM_DESCRIPTOR = msg({
	message: 'Forum',
	comment: 'Channel type chip label for a forum channel.',
});
export const MEDIA_DESCRIPTOR = msg({
	message: 'Media',
	comment: 'Channel type chip label for a media channel.',
});
export const NEW_POST_DESCRIPTOR = msg({
	message: 'New post',
	comment: 'Button in a forum channel that opens the composer for a new post.',
});
export const POST_TITLE_DESCRIPTOR = msg({
	message: 'Post title',
	comment: 'Label and placeholder of the title input in the new forum post composer.',
});
export const POST_TITLE_REQUIRED_DESCRIPTOR = msg({
	message: 'Give the post a title before you send.',
	comment: 'Error shown when the user sends a new forum post without a title.',
});
export const POST_MESSAGE_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Enter a message',
	comment: 'Placeholder of the message composer in the new forum post composer.',
});
export const POST_MEDIA_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Add a description (optional)',
	comment: 'Placeholder of the message composer in the new media post composer.',
});
export const POST_MEDIA_REQUIRED_DESCRIPTOR = msg({
	message: 'Attach an image or a video to post in a media channel.',
	comment: 'Error shown when the user sends a new media channel post without an image or video attachment.',
});
export const POST_FAVORITE_MEDIA_UNSUPPORTED_DESCRIPTOR = msg({
	message: 'Saved media cannot start a post. Upload the file instead.',
	comment:
		'Error shown when the user picks a saved favourite media item while writing a new forum or media channel post.',
});

export const POST_TAG_REQUIRED_DESCRIPTOR = msg({
	message: 'Pick at least one tag for this post.',
	comment: 'Error shown when the forum requires a tag and the new post has none.',
});
export const POST_CREATE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't create the post: {detail}",
	comment: 'Toast shown when creating a forum post fails. detail is the server error message.',
});
export const POST_ACTION_FAILED_DESCRIPTOR = msg({
	message: "Couldn't update the post: {detail}",
	comment: 'Toast shown when a forum post action such as pinning or editing tags fails. detail is the server error.',
});
export const CANCEL_POST_DESCRIPTOR = msg({
	message: 'Discard post',
	comment: 'Accessible label of the button that closes the new forum post composer.',
});
export const SEARCH_POSTS_DESCRIPTOR = msg({
	message: 'Search posts',
	comment: 'Placeholder and accessible label of the search input in a forum channel.',
});
export const SEARCH_FOR_POSTS_DESCRIPTOR = msg({
	message: 'Search or create a post...',
	comment: 'Placeholder of the search input at the top of a forum channel.',
});
export const SORT_AND_VIEW_DESCRIPTOR = msg({
	message: 'Sort & view',
	comment: 'Button in a forum channel that opens the sort order and layout options.',
});
export const SORT_BY_DESCRIPTOR = msg({
	message: 'Sort by',
	comment: 'Section title for the forum post sort order options.',
});
export const SORT_RECENT_ACTIVITY_DESCRIPTOR = msg({
	message: 'Recently active',
	comment: 'Forum sort option that orders posts by their latest message.',
});
export const SORT_CREATION_DATE_DESCRIPTOR = msg({
	message: 'Date posted',
	comment: 'Forum sort option that orders posts by when they were created.',
});
export const VIEW_AS_DESCRIPTOR = msg({
	message: 'View as',
	comment: 'Section title for the forum layout options.',
});
export const LAYOUT_LIST_DESCRIPTOR = msg({
	message: 'List',
	comment: 'Forum layout option that shows posts as rows.',
});
export const LAYOUT_GALLERY_DESCRIPTOR = msg({
	message: 'Gallery',
	comment: 'Forum layout option that shows posts as tiles.',
});
export const TAGS_DESCRIPTOR = msg({
	message: 'Tags',
	comment: 'Label of the tag filter in a forum channel and of the tags section in forum settings.',
});
export const CLEAR_TAGS_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Button in a forum channel tag filter that removes every selected tag.',
});
export const EDIT_TAGS_DESCRIPTOR = msg({
	message: 'Edit tags',
	comment: 'Post menu action that opens the tag picker for an existing forum post.',
});
export const TAG_LIMIT_DESCRIPTOR = msg({
	message: 'You can pick up to {count} tags.',
	comment: 'Hint in the forum tag picker. count is the maximum number of tags per post.',
});
export const MODERATED_TAG_HINT_DESCRIPTOR = msg({
	message: 'Only moderators can apply this tag.',
	comment: 'Tooltip on a forum tag that only moderators can add or remove.',
});
export const PIN_POST_DESCRIPTOR = msg({
	message: 'Pin post',
	comment: 'Post menu action that pins a forum post to the top of the forum.',
});
export const UNPIN_POST_DESCRIPTOR = msg({
	message: 'Unpin post',
	comment: 'Post menu action that unpins a forum post.',
});
export const PINNED_DESCRIPTOR = msg({
	message: 'Pinned',
	comment: 'Label on a pinned forum post card.',
});
export const NEW_BADGE_DESCRIPTOR = msg({
	message: 'New',
	comment: 'Badge on a forum post card for a post created since the user last opened the forum.',
});
export const UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: '{count, plural, one {# new message} other {# new messages}}',
	comment: 'Label on a joined forum post card with unread messages. count is the number of unread messages.',
});
export const HAS_UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: 'New messages',
	comment: 'Label on a joined forum post card with unread messages when the exact number is unknown.',
});
export const ORIGINAL_MESSAGE_DELETED_DESCRIPTOR = msg({
	message: 'Original message was deleted',
	comment: 'Preview on a forum post card whose first message was deleted.',
});
export const NO_POSTS_DESCRIPTOR = msg({
	message: 'There are no posts yet',
	comment: 'Title of the empty state in a forum channel without posts.',
});
export const NO_POSTS_HINT_DESCRIPTOR = msg({
	message: 'Be the first to start a conversation here.',
	comment: 'Body of the empty state in a forum channel without posts.',
});
export const NO_MATCHING_POSTS_DESCRIPTOR = msg({
	message: 'No posts match your search',
	comment: 'Title of the empty state in a forum channel when the search or tag filter finds nothing.',
});
export const NO_MATCHING_POSTS_HINT_DESCRIPTOR = msg({
	message: 'Try different words or clear the tag filter.',
	comment: 'Body of the empty state in a forum channel when the search or tag filter finds nothing.',
});
export const SEARCH_INDEXING_DESCRIPTOR = msg({
	message: 'Search is getting ready for this community. Trying again shortly...',
	comment: 'Notice in a forum channel while the server is still building the post search index.',
});
export const SEARCH_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load posts. Try again later.",
	comment: 'Notice in a forum channel when loading posts fails.',
});
export const RETRY_DESCRIPTOR = msg({
	message: 'Retry',
	comment: 'Button that retries loading forum posts.',
});
export const OLDER_POSTS_DESCRIPTOR = msg({
	message: 'Older posts',
	comment: 'Section title above archived posts in a forum channel.',
});
export const LOAD_OLDER_POSTS_DESCRIPTOR = msg({
	message: 'Load older posts',
	comment: 'Button at the bottom of a forum channel that loads archived posts.',
});
export const GUIDELINES_DESCRIPTOR = msg({
	message: 'Post guidelines',
	comment: 'Title of the forum guidelines banner and label of the guidelines field in forum settings.',
});
export const GUIDELINES_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Let people know how to post in this forum',
	comment: 'Placeholder of the guidelines field in forum settings.',
});
export const SHOW_GUIDELINES_DESCRIPTOR = msg({
	message: 'Show guidelines',
	comment: 'Button in the forum header that expands the post guidelines.',
});
export const HIDE_GUIDELINES_DESCRIPTOR = msg({
	message: 'Hide guidelines',
	comment: 'Button in the forum header that collapses the post guidelines.',
});
export const POST_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# message} other {# messages}}',
	comment: 'Message count on a forum post card. count is the number of replies.',
});
export const DEFAULT_REACTION_DESCRIPTOR = msg({
	message: 'Default reaction',
	comment: 'Label of the forum setting that picks the reaction button shown on every post.',
});
export const DEFAULT_REACTION_HINT_DESCRIPTOR = msg({
	message: 'Shown on every post so people can react with one click.',
	comment: 'Help text of the default reaction setting in forum settings.',
});
export const PICK_EMOJI_DESCRIPTOR = msg({
	message: 'Pick an emoji',
	comment: 'Button that opens the emoji picker in forum settings.',
});
export const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Button that clears the default reaction or deletes a tag in forum settings.',
});
export const DEFAULT_SORT_ORDER_DESCRIPTOR = msg({
	message: 'Default sort order',
	comment: 'Label of the forum setting that picks how posts are ordered by default.',
});
export const DEFAULT_LAYOUT_DESCRIPTOR = msg({
	message: 'Default layout',
	comment: 'Label of the forum setting that picks the default post layout.',
});
export const POST_SLOWMODE_DESCRIPTOR = msg({
	message: 'Slowmode for creating posts',
	comment: 'Label of the forum setting for how often a member can create a post.',
});
export const REQUIRE_TAG_DESCRIPTOR = msg({
	message: 'Require people to select tags when posting',
	comment: 'Label of the forum setting that makes at least one tag mandatory on new posts.',
});
export const HIDE_MEDIA_DOWNLOAD_DESCRIPTOR = msg({
	message: 'Hide media download options',
	comment: 'Label of the media channel setting that hides download, open and copy actions on images and videos.',
});
export const HIDE_MEDIA_DOWNLOAD_HINT_DESCRIPTOR = msg({
	message: 'Hides the download, open in browser and copy buttons on images and videos in this channel.',
	comment: 'Help text of the media channel setting that hides media download options.',
});
export const ADD_TAG_DESCRIPTOR = msg({
	message: 'Add tag',
	comment: 'Button in forum settings that opens the tag editor for a new tag.',
});
export const EDIT_TAG_DESCRIPTOR = msg({
	message: 'Edit tag',
	comment: 'Title of the tag editor for an existing forum tag.',
});
export const CREATE_TAG_DESCRIPTOR = msg({
	message: 'Create tag',
	comment: 'Title of the tag editor for a new forum tag.',
});
export const TAG_NAME_DESCRIPTOR = msg({
	message: 'Tag name',
	comment: 'Label of the name input in the forum tag editor.',
});
export const TAG_EMOJI_DESCRIPTOR = msg({
	message: 'Emoji',
	comment: 'Label of the emoji picker in the forum tag editor.',
});
export const TAG_MODERATED_DESCRIPTOR = msg({
	message: 'Only moderators can use this tag',
	comment: 'Label of the switch in the forum tag editor that restricts a tag to moderators.',
});
export const TAG_MODERATED_HINT_DESCRIPTOR = msg({
	message: 'Members who can manage posts can add or remove it.',
	comment: 'Help text of the moderated switch in the forum tag editor.',
});
export const DELETE_TAG_DESCRIPTOR = msg({
	message: 'Delete tag',
	comment: 'Button in the forum tag editor that deletes the tag.',
});
export const SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: 'Button that saves the forum tag editor or tag picker.',
});
export const CANCEL_DESCRIPTOR = msg({
	message: 'Cancel',
	comment: 'Button that closes a forum dialog without saving.',
});
export const TAGS_HINT_DESCRIPTOR = msg({
	message: 'Tags help people find posts. You can add up to {count} tags.',
	comment: 'Help text of the tags section in forum settings. count is the maximum number of tags.',
});
export const NO_TAGS_DESCRIPTOR = msg({
	message: 'This forum has no tags yet.',
	comment: 'Shown in forum settings when the forum has no tags.',
});
export const TAG_SAVE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't save the tag: {detail}",
	comment: 'Toast shown when saving a forum tag fails. detail is the server error.',
});
export const NEW_POSTS_NOTIFICATION_DESCRIPTOR = msg({
	message: 'Show new posts as unread',
	comment: 'Forum notification option that marks the forum unread when someone creates a post.',
});
export const NEW_POST_IN_FORUM_DESCRIPTOR = msg({
	message: 'New post in #{forumName}',
	comment: 'Title suffix of the desktop notification for a new forum post. forumName is the forum channel name.',
});
export const CREATE_POSTS_PERMISSION_DESCRIPTOR = msg({
	message: 'Create posts',
	comment: 'Permission name in forum and media channel permissions. Lets members create posts.',
});
export const CREATE_POSTS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Allows members to create posts in this channel.',
	comment: 'Description of the Create posts permission in forum and media channel permissions.',
});
export const SEND_MESSAGES_IN_POSTS_PERMISSION_DESCRIPTOR = msg({
	message: 'Send messages in posts',
	comment: 'Permission name in forum and media channel permissions. Lets members reply in posts.',
});
export const SEND_MESSAGES_IN_POSTS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Allows members to send messages in posts.',
	comment: 'Description of the Send messages in posts permission in forum and media channel permissions.',
});
export const MANAGE_POSTS_PERMISSION_DESCRIPTOR = msg({
	message: 'Manage posts',
	comment: 'Permission name in forum and media channel permissions. Lets members moderate posts.',
});
export const MANAGE_POSTS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Allows members to rename, close, lock, pin and delete posts, and to edit their tags.',
	comment: 'Description of the Manage posts permission in forum and media channel permissions.',
});
export const CHANNEL_NAME_DESCRIPTOR = msg({
	message: 'Channel name',
	comment: 'Label of the name input in forum channel settings.',
});
export const FORUM_SETTINGS_SAVED_DESCRIPTOR = msg({
	message: 'Channel updated',
	comment: 'Toast shown after forum channel settings are saved.',
});
export const SEND_MESSAGE_IN_POST_DESCRIPTOR = msg({
	message: 'Send a message in "{name}"',
	comment: 'Placeholder of the message box inside a forum post. name is the post title.',
});
export const SEARCHING_POSTS_DESCRIPTOR = msg({
	message: 'Searching...',
	comment: 'Status under the forum search box while results load.',
});
export const SEARCH_RESULT_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# matching post} other {# matching posts}}',
	comment: 'Status under the forum search box once results are in.',
});
export const NO_MATCHING_POSTS_SHORT_DESCRIPTOR = msg({
	message: 'No matching posts',
	comment: 'Status under the forum search box when nothing matches.',
});
export const CREATE_POST_SHORTCUT_HINT_DESCRIPTOR = msg({
	message: 'to create a post',
	comment:
		'Follows a Shift + Enter key hint under the forum search box. Starts lowercase because it continues the key hint.',
});
export const POST_DESCRIPTOR = msg({
	message: 'Post',
	comment: 'Primary button that publishes a new forum post.',
});
export const PREVIEW_DESCRIPTOR = msg({
	message: 'Preview',
	comment: 'Tooltip and label of the toggle that previews a new forum post.',
});
export const EDIT_DESCRIPTOR = msg({
	message: 'Edit',
	comment: 'Tooltip of the toggle that returns from the preview to editing a new forum post.',
});
export const PREVIEW_EMPTY_DESCRIPTOR = msg({
	message: 'Nothing to preview yet.',
	comment: 'Shown in the post preview when the message is empty.',
});
export const ADD_TAGS_DESCRIPTOR = msg({
	message: 'Add tags',
	comment: 'Button in the new post composer that opens the tag picker.',
});
export const TAGS_REQUIRED_DESCRIPTOR = msg({
	message: 'Required',
	comment: 'Marker next to the tag picker when the forum requires at least one tag.',
});
export const ATTACH_MEDIA_DESCRIPTOR = msg({
	message: 'Add images or files',
	comment: 'Accessible label of the attach button in the new post composer.',
});
export const ADD_EMOJI_DESCRIPTOR = msg({
	message: 'Add emoji',
	comment: 'Accessible label of the emoji button in the new post composer.',
});
export const POST_TITLE_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Give your post a title',
	comment: 'Placeholder of the title field in the new post composer.',
});
export const REACT_TO_POST_DESCRIPTOR = msg({
	message: 'React to post',
	comment: 'Button under the first message of a forum post that adds a reaction to it.',
});
export const FOLLOW_POST_DESCRIPTOR = msg({
	message: 'Follow',
	comment:
		'Button under the first message of a forum post. Following a post adds it to the channel list and sends notifications.',
});
export const FOLLOWING_POST_DESCRIPTOR = msg({
	message: 'Following',
	comment: 'Pressed state of the follow button under the first message of a forum post.',
});
export const COPY_POST_LINK_DESCRIPTOR = msg({
	message: 'Copy post link',
	comment: 'Accessible label of the copy link button under the first message of a forum post.',
});
export const POST_LINK_COPIED_DESCRIPTOR = msg({
	message: 'Post link copied',
	comment: 'Toast after the forum post link was copied.',
});
