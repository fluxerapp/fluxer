// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const INVITE_CREATE_FOR_CHANNEL_SUMMARY = msg({
	message: '{actor} created the invite {code} for {channel}.',
	comment:
		'Activity log summary for a new invite link. {actor} is the member who created the invite, shown as a clickable user chip, or the word System. {code} is the invite code, shown as boxed text. {channel} is the channel the invite leads to, shown as a channel chip.',
});
export const INVITE_CREATE_SUMMARY = msg({
	message: '{actor} created the invite {code}.',
	comment:
		'Activity log summary for a new invite link when its channel was not recorded. {actor} is the member who created the invite, shown as a clickable user chip, or the word System. {code} is the invite code, shown as boxed text.',
});
export const INVITE_NEVER_EXPIRES_ROW = msg({
	message: 'The invite was set to never expire.',
	comment:
		'Activity log detail under a created or deleted invite link. It records the expiry setting the invite was made with. The invite had no expiry time, so it would never stop working on its own. It has no placeholders.',
});
export const INVITE_EXPIRES_AFTER_ROW = msg({
	message: 'The invite was set to expire after {duration}.',
	comment:
		'Activity log detail under a created or deleted invite link. It records the expiry setting the invite was made with. It does not say the invite has expired. {duration} is how long the invite was set to stay valid after it was made, shown as a formatted length of time such as "7 days" or "30 minutes".',
});
export const INVITE_UNLIMITED_USES_ROW = msg({
	message: 'The invite was set to allow unlimited uses.',
	comment:
		'Activity log detail under a created or deleted invite link. It records that the invite was made with no limit on how many times it can be used. It has no placeholders.',
});
export const INVITE_USE_LIMIT_ROW = msg({
	message: '{count, plural, one {The invite was limited to # use.} other {The invite was limited to # uses.}}',
	comment:
		'Activity log detail under a created or deleted invite link. It records the use limit the invite was made with, not how many times it was used. {count} is an ICU plural with the most times the invite could be used before it stopped working.',
});
export const INVITE_TEMPORARY_MEMBERSHIP_ROW = msg({
	message: 'The invite was set to grant temporary membership.',
	comment:
		'Activity log detail under a created or deleted invite link. It records that the Temporary membership switch was on for this invite. People who join with it are kicked when they go offline unless they get a role. It has no placeholders.',
});
export const INVITE_UPDATE_SUMMARY = msg({
	message: '{actor} updated the invite {code}.',
	comment:
		'Activity log summary for a change to an invite link. {actor} is the member who changed the invite, shown as a clickable user chip, or the word System. {code} is the invite code, shown as boxed text.',
});
export const INVITE_DELETE_FOR_CHANNEL_SUMMARY = msg({
	message: '{actor} deleted the invite {code} for {channel}.',
	comment:
		'Activity log summary for a deleted invite link. {actor} is the member who deleted the invite, shown as a clickable user chip, or the word System. {code} is the invite code, shown as boxed text. {channel} is the channel the invite led to, shown as a channel chip.',
});
export const INVITE_DELETE_SUMMARY = msg({
	message: '{actor} deleted the invite {code}.',
	comment:
		'Activity log summary for a deleted invite link when its channel was not recorded. {actor} is the member who deleted the invite, shown as a clickable user chip, or the word System. {code} is the invite code, shown as boxed text.',
});
export const INVITE_CREATED_BY_ON_ROW = msg({
	message: 'The invite was created by {user} on {date}.',
	comment:
		'Activity log detail under a deleted invite link, used when someone other than the member who deleted it had created it. {user} is the member who created the invite, shown as a clickable user chip. {date} is when the invite was created, shown as a formatted date and time.',
});
export const INVITE_CREATED_BY_ROW = msg({
	message: 'The invite was created by {user}.',
	comment:
		'Activity log detail under a deleted invite link, used when someone other than the member who deleted it had created it and the creation time was not recorded. {user} is the member who created the invite, shown as a clickable user chip.',
});
export const INVITE_CREATED_ON_ROW = msg({
	message: 'The invite was created on {date}.',
	comment:
		'Activity log detail under a deleted invite link, used when the member who deleted it had also created it or the creator was not recorded. {date} is when the invite was created, shown as a formatted date and time.',
});
export const INVITE_NEVER_USED_ROW = msg({
	message: 'The invite was never used.',
	comment:
		'Activity log detail under a deleted invite link. It says nobody had used the invite before it was deleted. It has no placeholders.',
});
export const INVITE_USE_COUNT_ROW = msg({
	message: '{count, plural, one {The invite was used # time.} other {The invite was used # times.}}',
	comment:
		'Activity log detail under a deleted invite link. {count} is an ICU plural with how many times the invite had been used before it was deleted.',
});
export const INVITE_EXPIRES_ON_ROW = msg({
	message: 'The invite was scheduled to expire on {date}.',
	comment:
		'Activity log detail under a deleted invite link. It records the date the invite would have stopped working. The invite was deleted and may never have reached that date, so do not say it expired. {date} is that scheduled expiry, shown as a formatted date and time.',
});
export const WEBHOOK_CREATE_IN_CHANNEL_SUMMARY = msg({
	message: '{actor} created the webhook {name} in {channel}.',
	comment:
		'Activity log summary for a new webhook. {actor} is the member who created the webhook, shown as a clickable user chip, or the word System. {name} is the name of the webhook, shown in bold by the app. Do not add ** or <b> around it. {channel} is the channel the webhook posts in, shown as a channel chip.',
});
export const WEBHOOK_CREATE_SUMMARY = msg({
	message: '{actor} created the webhook {name}.',
	comment:
		'Activity log summary for a new webhook when its channel was not recorded. {actor} is the member who created the webhook, shown as a clickable user chip, or the word System. {name} is the name of the webhook, shown in bold by the app. Do not add ** or <b> around it.',
});
export const WEBHOOK_CREATE_UNNAMED_SUMMARY = msg({
	message: '{actor} created a webhook.',
	comment:
		'Activity log summary for a new webhook when its name was not recorded. {actor} is the member who created the webhook, shown as a clickable user chip, or the word System.',
});
export const WEBHOOK_RENAME_SUMMARY = msg({
	message: '{actor} renamed the webhook {oldName} to {newName}.',
	comment:
		'Activity log summary for a webhook whose name was the only thing changed. {actor} is the member who renamed the webhook, shown as a clickable user chip, or the word System. {oldName} is the previous name of the webhook and {newName} is its new name, both shown in bold by the app. Do not add ** or <b> around them.',
});
export const WEBHOOK_UPDATE_SUMMARY = msg({
	message: '{actor} updated the webhook {name}.',
	comment:
		'Activity log summary for changes to a webhook. The changes are listed as details under it. {actor} is the member who changed the webhook, shown as a clickable user chip, or the word System. {name} is the name of the webhook, shown in bold by the app. Do not add ** or <b> around it.',
});
export const WEBHOOK_UPDATE_UNNAMED_SUMMARY = msg({
	message: '{actor} updated a webhook.',
	comment:
		'Activity log summary for changes to a webhook whose name is not known. The changes are listed as details under it. {actor} is the member who changed the webhook, shown as a clickable user chip, or the word System.',
});
export const WEBHOOK_NAME_CHANGED_ROW = msg({
	message: 'Changed the name from {oldName} to {newName}.',
	comment:
		'Activity log detail under a changed webhook. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldName} is the previous name of the webhook and {newName} is its new name, both shown in bold by the app. Do not add ** or <b> around them.',
});
export const WEBHOOK_CHANNEL_CHANGED_ROW = msg({
	message: 'Moved from {oldChannel} to {newChannel}.',
	comment:
		'Activity log detail under a changed webhook that was moved to a different channel. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldChannel} is the channel the webhook posted in before the move and {newChannel} is the channel it posts in after the move, both shown as channel chips.',
});
export const WEBHOOK_AVATAR_ADDED_ROW = msg({
	message: 'Added an avatar.',
	comment:
		'Activity log detail under a changed webhook that got an avatar image when it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It has no placeholders.',
});
export const WEBHOOK_AVATAR_CHANGED_ROW = msg({
	message: 'Changed the avatar.',
	comment:
		'Activity log detail under a changed webhook whose avatar image was replaced with a different one. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It has no placeholders.',
});
export const WEBHOOK_AVATAR_REMOVED_ROW = msg({
	message: 'Removed the avatar.',
	comment:
		'Activity log detail under a changed webhook whose avatar image was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It has no placeholders.',
});
export const WEBHOOK_DELETE_FROM_CHANNEL_SUMMARY = msg({
	message: '{actor} deleted the webhook {name} from {channel}.',
	comment:
		'Activity log summary for a deleted webhook. {actor} is the member who deleted the webhook, shown as a clickable user chip, or the word System. {name} is the name the webhook had, shown in bold by the app. Do not add ** or <b> around it. {channel} is the channel the webhook posted in, shown as a channel chip.',
});
export const WEBHOOK_DELETE_SUMMARY = msg({
	message: '{actor} deleted the webhook {name}.',
	comment:
		'Activity log summary for a deleted webhook when its channel was not recorded. {actor} is the member who deleted the webhook, shown as a clickable user chip, or the word System. {name} is the name the webhook had, shown in bold by the app. Do not add ** or <b> around it.',
});
export const WEBHOOK_DELETE_UNNAMED_SUMMARY = msg({
	message: '{actor} deleted a webhook.',
	comment:
		'Activity log summary for a deleted webhook when its name was not recorded. {actor} is the member who deleted the webhook, shown as a clickable user chip, or the word System.',
});
export const WEBHOOK_CREATED_BY_ROW = msg({
	message: 'The webhook was created by {user}.',
	comment:
		'Activity log detail under a deleted webhook, used when someone other than the member who deleted it had created it. {user} is the member who created the webhook, shown as a clickable user chip.',
});
