// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const CHANNEL_CREATE_TEXT_SUMMARY = msg({
	message: '{actor} created the text channel {channel}.',
	comment:
		'Activity log summary when a text channel was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_TEXT_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the text channel {channel} in {category}.',
	comment:
		'Activity log summary when a text channel was created inside a category. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name. {category} is the category that holds it, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_VOICE_SUMMARY = msg({
	message: '{actor} created the voice channel {channel}.',
	comment:
		'Activity log summary when a voice channel was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_VOICE_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the voice channel {channel} in {category}.',
	comment:
		'Activity log summary when a voice channel was created inside a category. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name. {category} is the category that holds it, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_LINK_SUMMARY = msg({
	message: '{actor} created the link channel {channel}.',
	comment:
		'Activity log summary when a link channel was created. A link channel is a channel list entry that opens a web address. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_LINK_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the link channel {channel} in {category}.',
	comment:
		'Activity log summary when a link channel was created inside a category. A link channel is a channel list entry that opens a web address. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name. {category} is the category that holds it, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_GENERIC_SUMMARY = msg({
	message: '{actor} created the channel {channel}.',
	comment:
		'Activity log summary when a channel of a type this app version cannot name was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_GENERIC_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the channel {channel} in {category}.',
	comment:
		'Activity log summary when a channel of a type this app version cannot name was created inside a category. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name. {category} is the category that holds it, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_CATEGORY_SUMMARY = msg({
	message: '{actor} created the category {channel}.',
	comment:
		'Activity log summary when a channel category was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new category, shown as a channel chip with its name.',
});

export const CHANNEL_LINK_SET_ROW = msg({
	message: 'Set the link to {url}.',
	comment:
		'Activity log detail row under a created or updated link channel, saying which web address the channel opens. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {url} is the web address, shown as boxed text.',
});
export const CHANNEL_TOPIC_SET_ROW = msg({
	message: 'Set the topic to {text}.',
	comment:
		'Activity log detail row under a created or updated channel, giving its new topic. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the topic, shown as boxed text.',
});
export const CHANNEL_MARKED_MATURE_ROW = msg({
	message: 'Marked the channel as containing mature content.',
	comment:
		'Activity log detail row under a created or updated channel whose Mature content override setting was set to On. The channel is now treated as mature whatever its category or the community is set to. Mature content means content for adults only. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_MARKED_NOT_MATURE_ROW = msg({
	message: 'Marked the channel as not containing mature content.',
	comment:
		'Activity log detail row under a created or updated channel whose Mature content override setting was set to Off. The channel is not treated as mature, even when its category or the community is. Mature content means content for adults only. Keep the negation. This row is the opposite of "Marked the channel as containing mature content." It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_CONTENT_WARNING_ADDED_WITH_TEXT_ROW = msg({
	message: 'Added a content warning with the text {text}.',
	comment:
		'Activity log detail row under a created or updated channel that now shows a content warning before members can view it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {text} is the custom warning text, shown as boxed text.',
});
export const CHANNEL_CONTENT_WARNING_ADDED_ROW = msg({
	message: 'Added a content warning.',
	comment:
		'Activity log detail row under a created or updated channel that now shows a content warning, with no custom text, before members can view it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_SLOWMODE_SET_ROW = msg({
	message: 'Set slowmode to {duration}.',
	comment:
		'Activity log detail row under a created or updated channel that now has slowmode, the wait between messages from one member. Use the same term for slowmode as the Slowmode channel setting. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {duration} is the wait, shown as a formatted duration such as "30 seconds" or "1 hour".',
});
export const CHANNEL_AUDIO_QUALITY_SET_ROW = msg({
	message: 'Set the audio quality to {kbps} kbps.',
	comment:
		'Activity log detail row under a created voice channel whose audio bitrate differs from the default. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {kbps} is a plain number of kilobits per second, and kbps is the unit abbreviation.',
});
export const CHANNEL_USER_LIMIT_SET_ROW = msg({
	message: 'Limited the channel to {count, plural, one {# user} other {# users}}.',
	comment:
		'Activity log detail row under a created or updated voice channel that now has a maximum number of people who can join it at once. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {count} is an ICU plural with that maximum.',
});
export const CHANNEL_CONNECTION_LIMIT_SET_ROW = msg({
	message: 'Allowed up to {count, plural, one {# connection} other {# connections}} per member.',
	comment:
		'Activity log detail row under a created voice channel that allows a non-default number of simultaneous connections, such as several devices, from each member. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {count} is an ICU plural with the number of connections allowed per member.',
});
export const CHANNEL_VOICE_REGION_SET_ROW = msg({
	message: 'Set the voice region to {region}.',
	comment:
		'Activity log detail row under a created or updated voice channel that now uses a fixed voice server region. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {region} is the region identifier, shown in bold. The app adds the bold, so do not add ** or <b>.',
});

export const CHANNEL_RENAME_TEXT_SUMMARY = msg({
	message: '{actor} renamed the text channel {oldName} to {newName}.',
	comment:
		'Activity log summary when the only change to a text channel was its name. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const CHANNEL_RENAME_VOICE_SUMMARY = msg({
	message: '{actor} renamed the voice channel {oldName} to {newName}.',
	comment:
		'Activity log summary when the only change to a voice channel was its name. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const CHANNEL_RENAME_LINK_SUMMARY = msg({
	message: '{actor} renamed the link channel {oldName} to {newName}.',
	comment:
		'Activity log summary when the only change to a link channel was its name. A link channel is a channel list entry that opens a web address. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const CHANNEL_RENAME_CATEGORY_SUMMARY = msg({
	message: '{actor} renamed the category {oldName} to {newName}.',
	comment:
		'Activity log summary when the only change to a channel category was its name. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const CHANNEL_RENAME_GENERIC_SUMMARY = msg({
	message: '{actor} renamed the channel {oldName} to {newName}.',
	comment:
		'Activity log summary when the only change to a channel of a type this app version cannot name was its name. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});

export const CHANNEL_OVERRIDES_CHANGED_TEXT_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the text channel {channel}.',
	comment:
		'Activity log summary for an older entry that only recorded that the permission overrides of a text channel were changed, without saying which. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERRIDES_CHANGED_VOICE_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the voice channel {channel}.',
	comment:
		'Activity log summary for an older entry that only recorded that the permission overrides of a voice channel were changed, without saying which. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERRIDES_CHANGED_LINK_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the link channel {channel}.',
	comment:
		'Activity log summary for an older entry that only recorded that the permission overrides of a link channel were changed, without saying which. A link channel is a channel list entry that opens a web address. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERRIDES_CHANGED_CATEGORY_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the category {channel}.',
	comment:
		'Activity log summary for an older entry that only recorded that the permission overrides of a channel category were changed, without saying which. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the category, shown as a channel chip with its name.',
});
export const CHANNEL_OVERRIDES_CHANGED_GENERIC_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the channel {channel}.',
	comment:
		'Activity log summary for an older entry that only recorded that the permission overrides of a channel of a type this app version cannot name were changed, without saying which. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});

export const CHANNEL_UPDATE_TEXT_SUMMARY = msg({
	message: '{actor} updated the text channel {channel}.',
	comment:
		'Activity log summary when settings of a text channel were changed. The changes are listed as detail rows below it. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_UPDATE_VOICE_SUMMARY = msg({
	message: '{actor} updated the voice channel {channel}.',
	comment:
		'Activity log summary when settings of a voice channel were changed. The changes are listed as detail rows below it. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_UPDATE_LINK_SUMMARY = msg({
	message: '{actor} updated the link channel {channel}.',
	comment:
		'Activity log summary when settings of a link channel were changed. A link channel is a channel list entry that opens a web address. The changes are listed as detail rows below it. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_UPDATE_CATEGORY_SUMMARY = msg({
	message: '{actor} updated the category {channel}.',
	comment:
		'Activity log summary when settings of a channel category were changed. The changes are listed as detail rows below it. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the category, shown as a channel chip with its name.',
});
export const CHANNEL_UPDATE_GENERIC_SUMMARY = msg({
	message: '{actor} updated the channel {channel}.',
	comment:
		'Activity log summary when settings of a channel of a type this app version cannot name were changed. The changes are listed as detail rows below it. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});

export const CHANNEL_NAME_CHANGED_ROW = msg({
	message: 'Changed the name from {oldName} to {newName}.',
	comment:
		'Activity log detail row under an updated channel whose name changed along with other settings. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldName} is the name before the change and {newName} the name after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});
export const CHANNEL_MOVED_INTO_CATEGORY_ROW = msg({
	message: 'Moved the channel into the category {category}.',
	comment:
		'Activity log detail row under an updated channel that was placed inside a category and was not in any category before. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {category} is the category it joined, shown as a channel chip with its name.',
});
export const CHANNEL_MOVED_OUT_OF_CATEGORY_ROW = msg({
	message: 'Removed the channel from the category {category}.',
	comment:
		'Activity log detail row under an updated channel that was taken out of its category. The channel was not deleted and was not moved to another category. It now sits outside any category. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {category} is the category it left, shown as a channel chip with its name.',
});
export const CHANNEL_MOVED_BETWEEN_CATEGORIES_ROW = msg({
	message: 'Moved the channel from the category {oldCategory} to the category {newCategory}.',
	comment:
		'Activity log detail row under an updated channel that was moved from one category to another. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldCategory} is the category it left and {newCategory} the category it joined, both shown as channel chips with their names.',
});
export const CHANNEL_LINK_CHANGED_ROW = msg({
	message: 'Changed the link from {oldUrl} to {newUrl}.',
	comment:
		'Activity log detail row under an updated link channel whose web address changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldUrl} is the address before the change and {newUrl} the address after, both shown as boxed text.',
});
export const CHANNEL_LINK_REMOVED_ROW = msg({
	message: 'Removed the link.',
	comment:
		'Activity log detail row under an updated link channel whose web address was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_TOPIC_REMOVED_ROW = msg({
	message: 'Removed the topic {text}.',
	comment:
		'Activity log detail row under an updated channel whose topic was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {text} is the topic that was removed, shown as boxed text.',
});
export const CHANNEL_TOPIC_CHANGED_ROW = msg({
	message: 'Changed the topic from {oldText} to {newText}.',
	comment:
		'Activity log detail row under an updated channel whose topic changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldText} is the topic before the change and {newText} the topic after, both shown as boxed text.',
});
export const CHANNEL_MATURE_FOLLOWS_COMMUNITY_ROW = msg({
	message: 'Made the channel inherit its mature content setting.',
	comment:
		'Activity log detail row under an updated channel whose Mature content override setting was set to Inherit. The channel no longer has a choice of its own and takes the setting from its category, or from the community when the channel has no category. Mature content means content for adults only. Keep the channel as the thing that changed and do not say the channel was created. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_CONTENT_WARNING_REMOVED_ROW = msg({
	message: 'Removed the content warning.',
	comment:
		'Activity log detail row under an updated channel that no longer shows a content warning before members can view it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_CONTENT_WARNING_TEXT_SET_ROW = msg({
	message: 'Set the content warning text to {text}.',
	comment:
		'Activity log detail row under an updated channel whose content warning got custom text where it had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the new warning text, shown as boxed text.',
});
export const CHANNEL_CONTENT_WARNING_TEXT_CHANGED_ROW = msg({
	message: 'Changed the content warning text from {oldText} to {newText}.',
	comment:
		'Activity log detail row under an updated channel whose custom content warning text changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldText} is the warning text before the change and {newText} the text after, both shown as boxed text.',
});
export const CHANNEL_CONTENT_WARNING_TEXT_REMOVED_ROW = msg({
	message: 'Removed the content warning text.',
	comment:
		'Activity log detail row under an updated channel whose custom content warning text was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_SLOWMODE_CHANGED_ROW = msg({
	message: 'Changed slowmode from {oldDuration} to {newDuration}.',
	comment:
		'Activity log detail row under an updated channel whose slowmode, the wait between messages from one member, changed. Use the same term for slowmode as the Slowmode channel setting. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldDuration} is the wait before the change and {newDuration} the wait after, both shown as formatted durations such as "30 seconds" or "1 hour".',
});
export const CHANNEL_SLOWMODE_TURNED_OFF_ROW = msg({
	message: 'Turned off slowmode.',
	comment:
		'Activity log detail row under an updated channel that no longer has slowmode, the wait between messages from one member. Use the same term for slowmode as the Slowmode channel setting. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_AUDIO_QUALITY_CHANGED_ROW = msg({
	message: 'Changed the audio quality from {oldKbps} kbps to {newKbps} kbps.',
	comment:
		'Activity log detail row under an updated voice channel whose audio bitrate changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldKbps} is the bitrate before the change and {newKbps} the bitrate after, both plain numbers of kilobits per second, and kbps is the unit abbreviation.',
});
export const CHANNEL_USER_LIMIT_CHANGED_ROW = msg({
	message: 'Changed the user limit from {oldCount} to {newCount}.',
	comment:
		'Activity log detail row under an updated voice channel whose maximum number of people who can join at once changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldCount} is the maximum before the change and {newCount} the maximum after, both plain numbers.',
});
export const CHANNEL_USER_LIMIT_REMOVED_ROW = msg({
	message: 'Removed the user limit.',
	comment:
		'Activity log detail row under an updated voice channel that no longer limits how many people can join at once. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const CHANNEL_CONNECTION_LIMIT_CHANGED_ROW = msg({
	message: 'Changed the connections per member from {oldCount} to {newCount}.',
	comment:
		'Activity log detail row under an updated voice channel whose number of simultaneous connections, such as several devices, allowed from each member changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldCount} is the number before the change and {newCount} the number after, both plain numbers.',
});
export const CHANNEL_VOICE_REGION_AUTOMATIC_ROW = msg({
	message: 'Set the voice region to automatic.',
	comment:
		'Activity log detail row under an updated voice channel that no longer uses a fixed voice server region and now picks one automatically. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here.',
});
export const CHANNEL_VOICE_REGION_CHANGED_ROW = msg({
	message: 'Changed the voice region from {oldRegion} to {newRegion}.',
	comment:
		'Activity log detail row under an updated voice channel whose fixed voice server region changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldRegion} is the region identifier before the change and {newRegion} the identifier after, both shown in bold. The app adds the bold, so do not add ** or <b>.',
});

export const CHANNEL_DELETE_TEXT_SUMMARY = msg({
	message: '{actor} deleted the text channel {channel}.',
	comment:
		'Activity log summary when a text channel was deleted. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});
export const CHANNEL_DELETE_VOICE_SUMMARY = msg({
	message: '{actor} deleted the voice channel {channel}.',
	comment:
		'Activity log summary when a voice channel was deleted. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});
export const CHANNEL_DELETE_LINK_SUMMARY = msg({
	message: '{actor} deleted the link channel {channel}.',
	comment:
		'Activity log summary when a link channel was deleted. A link channel is a channel list entry that opens a web address. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});
export const CHANNEL_DELETE_CATEGORY_SUMMARY = msg({
	message: '{actor} deleted the category {channel}.',
	comment:
		'Activity log summary when a channel category was deleted. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the deleted category, shown as a channel chip with the name it had.',
});
export const CHANNEL_DELETE_GENERIC_SUMMARY = msg({
	message: '{actor} deleted the channel {channel}.',
	comment:
		'Activity log summary when a channel of a type this app version cannot name was deleted. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});

export const CHANNEL_OVERWRITE_CREATE_ROLE_SUMMARY = msg({
	message: '{actor} added a permission override for the role {role} in {channel}.',
	comment:
		'Activity log summary when a channel got a permission override for a role. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {role} is the role, shown as a role chip with its name. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_CREATE_MEMBER_SUMMARY = msg({
	message: '{actor} added a permission override for {user} in {channel}.',
	comment:
		'Activity log summary when a channel got a permission override for one member. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {user} is the member the override applies to, shown as a clickable user chip. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_CREATE_EVERYONE_SUMMARY = msg({
	message: '{actor} added a permission override for @everyone in {channel}.',
	comment:
		'Activity log summary when a channel got a permission override for every member of the community. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. Keep @everyone unchanged. It is the name of the role that includes all members. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_UPDATE_ROLE_SUMMARY = msg({
	message: '{actor} updated the permission override for the role {role} in {channel}.',
	comment:
		'Activity log summary when the permission override for a role in a channel was changed. The permission changes are listed as detail rows below it. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {role} is the role, shown as a role chip with its name. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_UPDATE_MEMBER_SUMMARY = msg({
	message: '{actor} updated the permission override for {user} in {channel}.',
	comment:
		'Activity log summary when the permission override for one member in a channel was changed. The permission changes are listed as detail rows below it. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {user} is the member the override applies to, shown as a clickable user chip. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_UPDATE_EVERYONE_SUMMARY = msg({
	message: '{actor} updated the permission override for @everyone in {channel}.',
	comment:
		'Activity log summary when the permission override for every member of the community in a channel was changed. The permission changes are listed as detail rows below it. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. Keep @everyone unchanged. It is the name of the role that includes all members. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_DELETE_ROLE_SUMMARY = msg({
	message: '{actor} removed the permission override for the role {role} from {channel}.',
	comment:
		'Activity log summary when the permission override for a role was removed from a channel, so the role has its usual permissions there again. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {role} is the role, shown as a role chip with its name. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_DELETE_MEMBER_SUMMARY = msg({
	message: '{actor} removed the permission override for {user} from {channel}.',
	comment:
		'Activity log summary when the permission override for one member was removed from a channel, so the member has their usual permissions there again. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {user} is the member the override applied to, shown as a clickable user chip. {channel} is the channel, shown as a channel chip with its name.',
});
export const CHANNEL_OVERWRITE_DELETE_EVERYONE_SUMMARY = msg({
	message: '{actor} removed the permission override for @everyone from {channel}.',
	comment:
		'Activity log summary when the permission override for every member of the community was removed from a channel. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. Keep @everyone unchanged. It is the name of the role that includes all members. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the channel, shown as a channel chip with its name.',
});

export const CHANNEL_OVERWRITE_ALLOWED_ROW = msg({
	message: 'Allowed {permissions}.',
	comment:
		'Activity log detail row under an added or updated channel permission override, listing permissions the override now explicitly allows. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {permissions} is a list of permission names joined into one phrase, for example "View channel and Send messages". A long list ends with a count, for example "View channel, Send messages, and 3 more permissions".',
});
export const CHANNEL_OVERWRITE_DENIED_ROW = msg({
	message: 'Denied {permissions}.',
	comment:
		'Activity log detail row under an added or updated channel permission override, listing permissions the override now explicitly denies. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {permissions} is a list of permission names joined into one phrase, for example "View channel and Send messages". A long list ends with a count, for example "View channel, Send messages, and 3 more permissions".',
});
export const CHANNEL_OVERWRITE_STOPPED_OVERRIDING_ROW = msg({
	message: 'Removed the override for {permissions}.',
	comment:
		'Activity log detail row under an updated channel permission override, listing permissions the override no longer allows or denies. Those permissions follow the usual permissions of the role or member again. The override itself was not removed. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {permissions} is a list of permission names joined into one phrase, for example "View channel and Send messages". A long list ends with a count, for example "View channel, Send messages, and 3 more permissions".',
});
export const CHANNEL_OVERWRITE_REMOVED_ALLOWED_ROW = msg({
	message: 'The override allowed {permissions}.',
	comment:
		'Activity log detail row under a removed channel permission override, listing the permissions it explicitly allowed before it was removed. It describes the removed override. It does not mean these permissions are allowed now. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {permissions} is a list of permission names joined into one phrase, for example "View channel and Send messages". A long list ends with a count, for example "View channel, Send messages, and 3 more permissions".',
});
export const CHANNEL_OVERWRITE_REMOVED_DENIED_ROW = msg({
	message: 'The override denied {permissions}.',
	comment:
		'Activity log detail row under a removed channel permission override, listing the permissions it explicitly denied before it was removed. It describes the removed override. It does not mean these permissions are denied now. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. Use the same word for override as the channel permissions settings, such as Add override. {permissions} is a list of permission names joined into one phrase, for example "View channel and Send messages". A long list ends with a count, for example "View channel, Send messages, and 3 more permissions".',
});
