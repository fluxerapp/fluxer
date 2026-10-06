// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const EMOJI_CREATE_SUMMARY = msg({
	message: '{actor} added the emoji {emoji}',
	comment:
		'Activity log summary for a custom emoji that was uploaded or copied to the community. {actor} is the member who added it, shown as a clickable user chip, or the word System. {emoji} is the emoji, shown as its image when it still exists, followed by its name between colons, for example :blobcat:.',
});
export const EMOJI_CREATE_UNNAMED_SUMMARY = msg({
	message: '{actor} added an emoji',
	comment:
		'Activity log summary for a custom emoji that was added to the community when its name was not recorded. {actor} is the member who added it, shown as a clickable user chip, or the word System.',
});
export const EMOJI_RENAME_SUMMARY = msg({
	message: '{actor} renamed the emoji {oldEmoji} to {newEmoji}',
	comment:
		'Activity log summary for a custom emoji that got a new name. Only the name changed. The emoji was not replaced with a different one. {actor} is the member who renamed it, shown as a clickable user chip, or the word System. {oldEmoji} is the previous name between colons, shown without an image. {newEmoji} is the new name between colons, shown after the emoji image when the emoji still exists.',
});
export const EMOJI_UPDATE_SUMMARY = msg({
	message: '{actor} updated the emoji {emoji}',
	comment:
		'Activity log summary for a custom emoji that was edited when no change to show was recorded. {actor} is the member who edited it, shown as a clickable user chip, or the word System. {emoji} is the emoji, shown as its image when it still exists, followed by its name between colons, for example :blobcat:.',
});
export const EMOJI_UPDATE_UNNAMED_SUMMARY = msg({
	message: '{actor} updated an emoji',
	comment:
		'Activity log summary for a custom emoji that was edited when neither a change nor the emoji name is known. {actor} is the member who edited it, shown as a clickable user chip, or the word System.',
});
export const EMOJI_DELETE_SUMMARY = msg({
	message: '{actor} deleted the emoji {emoji}',
	comment:
		'Activity log summary for a custom emoji that was deleted from the community. {actor} is the member who deleted it, shown as a clickable user chip, or the word System. {emoji} is the name of the deleted emoji between colons, for example :blobcat:, shown without an image.',
});
export const EMOJI_DELETE_UNNAMED_SUMMARY = msg({
	message: '{actor} deleted an emoji',
	comment:
		'Activity log summary for a custom emoji that was deleted when its name was not recorded. {actor} is the member who deleted it, shown as a clickable user chip, or the word System.',
});
export const EMOJI_UPLOADER_ROW = msg({
	message: 'The emoji was uploaded by {user}',
	comment:
		'Activity log detail line under a deleted emoji, shown only when someone other than the person who deleted it had uploaded it. {user} is the user who uploaded the emoji, shown as a clickable user chip.',
});
export const STICKER_CREATE_SUMMARY = msg({
	message: '{actor} added the sticker {name}',
	comment:
		'Activity log summary for a sticker that was uploaded or copied to the community. {actor} is the member who added it, shown as a clickable user chip, or the word System. {name} is the sticker name, shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const STICKER_CREATE_UNNAMED_SUMMARY = msg({
	message: '{actor} added a sticker',
	comment:
		'Activity log summary for a sticker that was added to the community when its name was not recorded. {actor} is the member who added it, shown as a clickable user chip, or the word System.',
});
export const STICKER_DESCRIPTION_SET_ROW = msg({
	message: 'Set the description to {text}',
	comment:
		'Activity log detail line under an added or edited sticker, for a sticker description that was set where there was none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the new description, shown as boxed text.',
});
export const STICKER_RENAME_SUMMARY = msg({
	message: '{actor} renamed the sticker {oldName} to {newName}',
	comment:
		'Activity log summary for a sticker that got a new name and no other change. Only the name changed. The sticker was not replaced with a different one. {actor} is the member who renamed it, shown as a clickable user chip, or the word System. {oldName} is the previous sticker name and {newName} is the new one, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const STICKER_UPDATE_SUMMARY = msg({
	message: '{actor} updated the sticker {name}',
	comment:
		'Activity log summary for a sticker that was edited. The changes are listed as detail lines below it. {actor} is the member who edited it, shown as a clickable user chip, or the word System. {name} is the sticker name, shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const STICKER_UPDATE_UNNAMED_SUMMARY = msg({
	message: '{actor} updated a sticker',
	comment:
		'Activity log summary for a sticker that was edited when the sticker name is not known. The changes are listed as detail lines below it. {actor} is the member who edited it, shown as a clickable user chip, or the word System.',
});
export const STICKER_NAME_CHANGED_ROW = msg({
	message: 'Changed the name from {oldName} to {newName}',
	comment:
		'Activity log detail line under an edited sticker, for a sticker that got a new name. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Keep the word name so the line says what changed. {oldName} is the previous sticker name and {newName} is the new one, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const STICKER_DESCRIPTION_CHANGED_ROW = msg({
	message: 'Changed the description from {oldText} to {newText}',
	comment:
		'Activity log detail line under an edited sticker, for a sticker description that was replaced. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldText} is the previous description and {newText} is the new one, both shown as boxed text.',
});
export const STICKER_DESCRIPTION_REMOVED_ROW = msg({
	message: 'Removed the description {text}',
	comment:
		'Activity log detail line under an edited sticker, for a sticker description that was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {text} is the removed description itself, shown as boxed text.',
});
export const STICKER_DELETE_SUMMARY = msg({
	message: '{actor} deleted the sticker {name}',
	comment:
		'Activity log summary for a sticker that was deleted from the community. {actor} is the member who deleted it, shown as a clickable user chip, or the word System. {name} is the name of the deleted sticker, shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const STICKER_DELETE_UNNAMED_SUMMARY = msg({
	message: '{actor} deleted a sticker',
	comment:
		'Activity log summary for a sticker that was deleted when its name was not recorded. {actor} is the member who deleted it, shown as a clickable user chip, or the word System.',
});
export const STICKER_UPLOADER_ROW = msg({
	message: 'The sticker was uploaded by {user}',
	comment:
		'Activity log detail line under a deleted sticker, shown only when someone other than the person who deleted it had uploaded it. {user} is the user who uploaded the sticker, shown as a clickable user chip.',
});
export const MESSAGE_DELETE_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} deleted a message in {channel}',
	comment:
		'Activity log summary for one message that was deleted. {actor} is the member who deleted it, shown as a clickable user chip, or the word System. {channel} is the channel the message was in, shown as a channel name chip, or the deleted-channel label when the channel no longer exists.',
});
export const MESSAGE_DELETE_SUMMARY = msg({
	message: '{actor} deleted a message',
	comment:
		'Activity log summary for one message that was deleted when the channel was not recorded. {actor} is the member who deleted it, shown as a clickable user chip, or the word System.',
});
export const MESSAGE_BULK_DELETE_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} deleted {count, plural, one {# message} other {# messages}} in {channel}',
	comment:
		'Activity log summary for several messages that were deleted together. {actor} is the member who deleted them, shown as a clickable user chip, or the word System. {count} is an ICU plural with the number of deleted messages, and # shows that number. {channel} is the channel the messages were in, shown as a channel name chip, or the deleted-channel label when the channel no longer exists.',
});
export const MESSAGE_BULK_DELETE_SUMMARY = msg({
	message: '{actor} deleted {count, plural, one {# message} other {# messages}}',
	comment:
		'Activity log summary for several messages that were deleted together when the channel was not recorded. {actor} is the member who deleted them, shown as a clickable user chip, or the word System. {count} is an ICU plural with the number of deleted messages, and # shows that number.',
});
export const MESSAGE_BULK_DELETE_UNCOUNTED_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} deleted some messages in {channel}',
	comment:
		'Activity log summary for several messages that were deleted together when their number was not recorded. {actor} is the member who deleted them, shown as a clickable user chip, or the word System. {channel} is the channel the messages were in, shown as a channel name chip, or the deleted-channel label when the channel no longer exists.',
});
export const MESSAGE_BULK_DELETE_UNCOUNTED_SUMMARY = msg({
	message: '{actor} deleted some messages',
	comment:
		'Activity log summary for several messages that were deleted together when neither their number nor the channel was recorded. {actor} is the member who deleted them, shown as a clickable user chip, or the word System.',
});
export const MESSAGE_PIN_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} pinned a message in {channel}',
	comment:
		'Activity log summary for a message that was pinned. Pinned means the message was added to the pinned messages of the channel. {actor} is the member who pinned it, shown as a clickable user chip, or the word System. {channel} is the channel the message is in, shown as a channel name chip, or the deleted-channel label when the channel no longer exists.',
});
export const MESSAGE_PIN_SUMMARY = msg({
	message: '{actor} pinned a message',
	comment:
		'Activity log summary for a message that was pinned when the channel was not recorded. Pinned means the message was added to the pinned messages of its channel. {actor} is the member who pinned it, shown as a clickable user chip, or the word System.',
});
export const MESSAGE_UNPIN_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} unpinned a message in {channel}',
	comment:
		'Activity log summary for a message that was unpinned. Unpinned means the message was taken off the pinned messages of the channel. It is the opposite of pinned. The message was not deleted. {actor} is the member who unpinned it, shown as a clickable user chip, or the word System. {channel} is the channel the message is in, shown as a channel name chip, or the deleted-channel label when the channel no longer exists.',
});
export const MESSAGE_UNPIN_SUMMARY = msg({
	message: '{actor} unpinned a message',
	comment:
		'Activity log summary for a message that was unpinned when the channel was not recorded. Unpinned means the message was taken off the pinned messages of its channel. It is the opposite of pinned. The message was not deleted. {actor} is the member who unpinned it, shown as a clickable user chip, or the word System.',
});
