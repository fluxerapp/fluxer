// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const GUILD_SETTINGS_UPDATED_SUMMARY = msg({
	message: '{actor} updated the community settings',
	comment:
		'Activity log summary when a member changed community settings. Each change is listed as a detail line under it. {actor} is the member who made the change, shown as a clickable user chip, or the word System.',
});
export const GUILD_RENAMED_SUMMARY = msg({
	message: '{actor} renamed the community from {oldName} to {newName}',
	comment:
		'Activity log summary when the only community setting a member changed was the community name. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the previous community name and {newName} is the new community name, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const GUILD_OWNERSHIP_TRANSFERRED_SUMMARY = msg({
	message: '{actor} transferred community ownership to {user}',
	comment:
		'Activity log summary when the community owner made another member the owner of the community. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {user} is the new owner, shown as a clickable user chip.',
});
export const GUILD_VANITY_URL_SET_SUMMARY = msg({
	message: '{actor} set the vanity URL to {code}',
	comment:
		'Activity log summary when a member gave the community a vanity URL, the custom invite link of the community, when it had none. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {code} is the new vanity URL code, shown as boxed text.',
});
export const GUILD_VANITY_URL_CHANGED_SUMMARY = msg({
	message: '{actor} changed the vanity URL from {oldCode} to {newCode}',
	comment:
		'Activity log summary when a member replaced the vanity URL, the custom invite link of the community. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldCode} is the previous vanity URL code and {newCode} is the new vanity URL code, both shown as boxed text.',
});
export const GUILD_VANITY_URL_REMOVED_SUMMARY = msg({
	message: '{actor} removed the vanity URL {code}',
	comment:
		'Activity log summary when a member removed the vanity URL, the custom invite link of the community. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {code} is the vanity URL code that was removed, shown as boxed text.',
});
export const GUILD_INVITES_PAUSED_SUMMARY = msg({
	message: '{actor} paused invites',
	comment:
		'Activity log summary when a member paused invites. Pausing invites disables every invite link of the community, so nobody new can join through one until invites are enabled again. Existing members are not affected. Use the same verb as the Pause invites button. {actor} is the member who made the change, shown as a clickable user chip, or the word System.',
});
export const GUILD_INVITES_RESUMED_SUMMARY = msg({
	message: '{actor} resumed invites',
	comment:
		'Activity log summary when a member resumed invites after they were paused. Resuming invites enables the invite links of the community again, so people can join through them. It is the reverse of pausing invites and matches the Enable invites button. It does not mean invites were repeated or sent again. {actor} is the member who made the change, shown as a clickable user chip, or the word System.',
});

export const GUILD_RENAMED_ROW = msg({
	message: 'Renamed the community from {oldName} to {newName}',
	comment:
		'Activity log detail line under a community settings update, shown when the community name changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldName} is the previous community name and {newName} is the new community name, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const GUILD_ICON_ADDED_ROW = msg({
	message: 'Added a community icon',
	comment:
		'Activity log detail line under a community settings update, shown when the community got an icon where it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_ICON_CHANGED_ROW = msg({
	message: 'Changed the community icon',
	comment:
		'Activity log detail line under a community settings update, shown when the community icon was replaced with a different image. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_ICON_REMOVED_ROW = msg({
	message: 'Removed the community icon',
	comment:
		'Activity log detail line under a community settings update, shown when the community icon was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_BANNER_ADDED_ROW = msg({
	message: 'Added a community banner',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a banner image where it had none. This is the banner of the community itself, not the banner on the community profile of a member. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_BANNER_CHANGED_ROW = msg({
	message: 'Changed the community banner',
	comment:
		'Activity log detail line under a community settings update, shown when the community banner image was replaced with a different image. This is the banner of the community itself, not the banner on the community profile of a member. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_BANNER_REMOVED_ROW = msg({
	message: 'Removed the community banner',
	comment:
		'Activity log detail line under a community settings update, shown when the community banner image was removed. This is the banner of the community itself, not the banner on the community profile of a member. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_DETACHED_BANNER_TURNED_ON_ROW = msg({
	message: 'Turned on the detached banner',
	comment:
		'Activity log detail line under a community settings update, shown when the Detached banner setting was turned on. Use the same name for the setting as the community settings page. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_DETACHED_BANNER_TURNED_OFF_ROW = msg({
	message: 'Turned off the detached banner',
	comment:
		'Activity log detail line under a community settings update, shown when the Detached banner setting was turned off. Use the same name for the setting as the community settings page. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_INVITE_BACKGROUND_ADDED_ROW = msg({
	message: 'Added an invite background',
	comment:
		'Activity log detail line under a community settings update, shown when the community got an invite background, the image behind its invite page, where it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_INVITE_BACKGROUND_CHANGED_ROW = msg({
	message: 'Changed the invite background',
	comment:
		'Activity log detail line under a community settings update, shown when the invite background, the image behind the invite page of the community, was replaced with a different image. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_INVITE_BACKGROUND_REMOVED_ROW = msg({
	message: 'Removed the invite background',
	comment:
		'Activity log detail line under a community settings update, shown when the invite background, the image behind the invite page of the community, was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_INVITE_BACKGROUND_ALIGNMENT_CHANGED_ROW = msg({
	message: 'Changed the invite background alignment from {oldAlignment} to {newAlignment}',
	comment:
		'Activity log detail line under a community settings update, shown when the alignment of the invite background image changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldAlignment} is the previous alignment and {newAlignment} is the new alignment, each shown as Centered, Left aligned or Right aligned.',
});
export const GUILD_CHAT_EMBED_BACKGROUND_ADDED_ROW = msg({
	message: 'Added a chat embed background',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a chat embed background, the image shown behind its invite embeds in chat, where it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_CHAT_EMBED_BACKGROUND_CHANGED_ROW = msg({
	message: 'Changed the chat embed background',
	comment:
		'Activity log detail line under a community settings update, shown when the chat embed background, the image shown behind invite embeds of the community in chat, was replaced with a different image. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_CHAT_EMBED_BACKGROUND_REMOVED_ROW = msg({
	message: 'Removed the chat embed background',
	comment:
		'Activity log detail line under a community settings update, shown when the chat embed background, the image shown behind invite embeds of the community in chat, was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_SYSTEM_CHANNEL_SET_ROW = msg({
	message: 'Set the system channel to {channel}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a system channel, where the app posts messages such as member joins, when it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {channel} is the new system channel, shown as a channel chip.',
});
export const GUILD_SYSTEM_CHANNEL_CHANGED_ROW = msg({
	message: 'Changed the system channel from {oldChannel} to {newChannel}',
	comment:
		'Activity log detail line under a community settings update, shown when the system channel, where the app posts messages such as member joins, was moved to another channel. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldChannel} is the previous system channel and {newChannel} is the new system channel, both shown as channel chips.',
});
export const GUILD_SYSTEM_CHANNEL_REMOVED_ROW = msg({
	message: 'Removed the system channel',
	comment:
		'Activity log detail line under a community settings update, shown when the community stopped having a system channel, where the app posts messages such as member joins. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_JOIN_MESSAGES_HIDDEN_ROW = msg({
	message: 'Hid join messages in the system channel',
	comment:
		'Activity log detail line under a community settings update, shown when the Hide join messages setting was turned on, so the system channel no longer announces new members. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_JOIN_MESSAGES_SHOWN_ROW = msg({
	message: 'Stopped hiding join messages in the system channel',
	comment:
		'Activity log detail line under a community settings update, shown when the Hide join messages setting was turned off, so the system channel announces new members again. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_DEFAULT_NOTIFICATIONS_CHANGED_ROW = msg({
	message: 'Changed the default notification setting from {oldSetting} to {newSetting}',
	comment:
		'Activity log detail line under a community settings update, shown when the default notification setting for members changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldSetting} is the previous setting and {newSetting} is the new setting, each shown as All messages or Mentions only.',
});
export const GUILD_AFK_CHANNEL_SET_ROW = msg({
	message: 'Set the AFK channel to {channel}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got an AFK channel, the voice channel idle members are moved to, when it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {channel} is the new AFK channel, shown as a channel chip.',
});
export const GUILD_AFK_CHANNEL_CHANGED_ROW = msg({
	message: 'Changed the AFK channel from {oldChannel} to {newChannel}',
	comment:
		'Activity log detail line under a community settings update, shown when the AFK channel, the voice channel idle members are moved to, was changed to another channel. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldChannel} is the previous AFK channel and {newChannel} is the new AFK channel, both shown as channel chips.',
});
export const GUILD_AFK_CHANNEL_REMOVED_ROW = msg({
	message: 'Removed the AFK channel',
	comment:
		'Activity log detail line under a community settings update, shown when the community stopped having an AFK channel, the voice channel idle members are moved to. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_AFK_TIMEOUT_CHANGED_ROW = msg({
	message: 'Changed the AFK timeout from {oldDuration} to {newDuration}',
	comment:
		'Activity log detail line under a community settings update, shown when the time before idle voice members are moved to the AFK channel changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldDuration} is the previous timeout and {newDuration} is the new timeout, each shown as a formatted duration phrase such as 5 minutes or 1 hour.',
});
export const GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_ALLOWED_ROW = msg({
	message: 'Allowed flexible text channel names',
	comment:
		'Activity log detail line under a community settings update, shown when the setting that allows flexible text channel names, such as capital letters and spaces, was turned on. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Allowed is the past tense verb here, not an adjective.',
});
export const GUILD_FLEXIBLE_TEXT_CHANNEL_NAMES_DISALLOWED_ROW = msg({
	message: 'Stopped allowing flexible text channel names',
	comment:
		'Activity log detail line under a community settings update, shown when the setting that allows flexible text channel names, such as capital letters and spaces, was turned off. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_OWNER_CROWN_HIDDEN_ROW = msg({
	message: 'Hid the community owner crown',
	comment:
		'Activity log detail line under a community settings update, shown when the Hide community owner crown setting was turned on, so the crown icon no longer marks the owner in the member list. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_OWNER_CROWN_SHOWN_ROW = msg({
	message: 'Stopped hiding the community owner crown',
	comment:
		'Activity log detail line under a community settings update, shown when the Hide community owner crown setting was turned off, so the crown icon marks the owner in the member list again. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_MESSAGE_HISTORY_THRESHOLD_SET_ROW = msg({
	message: 'Set the message history threshold to {date}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a message history threshold, the date before which new members cannot see messages, when it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {date} is the threshold, shown as a formatted date and time.',
});
export const GUILD_MESSAGE_HISTORY_THRESHOLD_CHANGED_ROW = msg({
	message: 'Changed the message history threshold from {oldDate} to {newDate}',
	comment:
		'Activity log detail line under a community settings update, shown when the message history threshold, the date before which new members cannot see messages, changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldDate} is the previous threshold and {newDate} is the new threshold, both shown as formatted dates and times.',
});
export const GUILD_MESSAGE_HISTORY_THRESHOLD_REMOVED_ROW = msg({
	message: 'Removed the message history threshold',
	comment:
		'Activity log detail line under a community settings update, shown when the message history threshold, the date before which new members cannot see messages, was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_INVITES_PAUSED_ROW = msg({
	message: 'Paused invites',
	comment:
		'Activity log detail line under a community settings update, shown when invites were paused. Pausing invites disables every invite link of the community, so nobody new can join through one until invites are enabled again. Existing members are not affected. Use the same verb as the Pause invites button. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a status label.',
});
export const GUILD_INVITES_RESUMED_ROW = msg({
	message: 'Resumed invites',
	comment:
		'Activity log detail line under a community settings update, shown when invites were resumed after they were paused. Resuming invites enables the invite links of the community again, so people can join through them. It is the reverse of pausing invites and matches the Enable invites button. It does not mean invites were repeated or sent again. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_EMOJI_CLONING_ALLOWED_ROW = msg({
	message: "Allowed others to clone this community's emojis",
	comment:
		'Activity log detail line under a community settings update, shown when the Let others clone your emojis setting was turned on. Members of other communities can now clone the custom emojis of this community into their own communities. Use the same word for clone as the Clone emoji and Clone sticker menu items. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not the label of the setting.',
});
export const GUILD_EMOJI_CLONING_STOPPED_ROW = msg({
	message: "Stopped allowing others to clone this community's emojis",
	comment:
		'Activity log detail line under a community settings update, shown when the Let others clone your emojis setting was turned off. Members of other communities can no longer clone the custom emojis of this community into their own communities. It is the reverse of the line for turning the setting on, so keep the two worded alike. Use the same word for clone as the Clone emoji and Clone sticker menu items. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_STICKER_CLONING_ALLOWED_ROW = msg({
	message: "Allowed others to clone this community's stickers",
	comment:
		'Activity log detail line under a community settings update, shown when the Let others clone your stickers setting was turned on. Members of other communities can now clone the stickers of this community into their own communities. Use the same word for clone as the Clone emoji and Clone sticker menu items. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not the label of the setting.',
});
export const GUILD_STICKER_CLONING_STOPPED_ROW = msg({
	message: "Stopped allowing others to clone this community's stickers",
	comment:
		'Activity log detail line under a community settings update, shown when the Let others clone your stickers setting was turned off. Members of other communities can no longer clone the stickers of this community into their own communities. It is the reverse of the line for turning the setting on, so keep the two worded alike. Use the same word for clone as the Clone emoji and Clone sticker menu items. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_VERIFICATION_LEVEL_CHANGED_ROW = msg({
	message: 'Changed the verification level from {oldLevel} to {newLevel}',
	comment:
		'Activity log detail line under a community settings update, shown when the member verification level changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldLevel} is the previous level and {newLevel} is the new level, each shown as None, Low, Medium, High or Very high.',
});
export const GUILD_EXPLICIT_CONTENT_FILTER_CHANGED_ROW = msg({
	message: 'Changed the explicit content filter from {oldFilter} to {newFilter}',
	comment:
		'Activity log detail line under a community settings update, shown when the explicit content filter setting changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldFilter} is the previous setting and {newFilter} is the new setting, each shown as Off, Filter members without roles or Filter everyone.',
});
export const GUILD_MATURE_CONTENT_TURNED_ON_ROW = msg({
	message: 'Turned on mature content',
	comment:
		'Activity log detail line under a community settings update, shown when the Mature content setting of the community was turned on. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_MATURE_CONTENT_TURNED_OFF_ROW = msg({
	message: 'Turned off mature content',
	comment:
		'Activity log detail line under a community settings update, shown when the Mature content setting of the community was turned off. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_CONTENT_WARNING_TURNED_ON_ROW = msg({
	message: 'Turned on the content warning',
	comment:
		'Activity log detail line under a community settings update, shown when the community started showing a content warning before members can view it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_CONTENT_WARNING_TURNED_OFF_ROW = msg({
	message: 'Turned off the content warning',
	comment:
		'Activity log detail line under a community settings update, shown when the community stopped showing a content warning before members can view it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_CUSTOM_WARNING_TEXT_SET_ROW = msg({
	message: 'Set the custom warning text to {text}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got custom content warning text where it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the new warning text, shown as boxed text.',
});
export const GUILD_CUSTOM_WARNING_TEXT_CHANGED_ROW = msg({
	message: 'Changed the custom warning text from {oldText} to {newText}',
	comment:
		'Activity log detail line under a community settings update, shown when the custom content warning text changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldText} is the previous warning text and {newText} is the new warning text, both shown as boxed text.',
});
export const GUILD_CUSTOM_WARNING_TEXT_REMOVED_ROW = msg({
	message: 'Removed the custom warning text',
	comment:
		'Activity log detail line under a community settings update, shown when the custom content warning text was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_MODERATION_2FA_REQUIRED_ROW = msg({
	message: 'Required 2FA for moderation actions',
	comment:
		'Activity log detail line under a community settings update, shown when the setting that requires moderators to have two-factor authentication (2FA) turned on before they can take moderation actions was turned on. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Required is the past tense verb here. It is not an adjective or a notice that the reader needs 2FA.',
});
export const GUILD_MODERATION_2FA_NOT_REQUIRED_ROW = msg({
	message: 'Stopped requiring 2FA for moderation actions',
	comment:
		'Activity log detail line under a community settings update, shown when the setting that requires moderators to have two-factor authentication (2FA) turned on before they can take moderation actions was turned off. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const GUILD_OWNERSHIP_TRANSFERRED_ROW = msg({
	message: 'Transferred community ownership to {user}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a new owner. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {user} is the new owner, shown as a clickable user chip.',
});
export const GUILD_VANITY_URL_SET_ROW = msg({
	message: 'Set the vanity URL to {code}',
	comment:
		'Activity log detail line under a community settings update, shown when the community got a vanity URL, its custom invite link, when it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {code} is the new vanity URL code, shown as boxed text.',
});
export const GUILD_VANITY_URL_CHANGED_ROW = msg({
	message: 'Changed the vanity URL from {oldCode} to {newCode}',
	comment:
		'Activity log detail line under a community settings update, shown when the vanity URL, the custom invite link of the community, was replaced. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldCode} is the previous vanity URL code and {newCode} is the new vanity URL code, both shown as boxed text.',
});
export const GUILD_VANITY_URL_REMOVED_ROW = msg({
	message: 'Removed the vanity URL {code}',
	comment:
		'Activity log detail line under a community settings update, shown when the vanity URL, the custom invite link of the community, was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {code} is the vanity URL code that was removed, shown as boxed text.',
});
