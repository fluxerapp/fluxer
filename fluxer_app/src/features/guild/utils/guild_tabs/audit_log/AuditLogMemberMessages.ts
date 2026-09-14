// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const MEMBER_KICK_SUMMARY = msg({
	message: '{actor} kicked {target}.',
	comment:
		'Activity log summary for a member who was kicked from the community. {actor} is the member who kicked them, shown as a clickable user chip, or the word System. {target} is the member who was kicked, shown as a clickable user chip.',
});
export const MEMBER_PRUNE_SUMMARY = msg({
	message: '{actor} pruned inactive members.',
	comment:
		'Activity log summary for inactive members who were removed from the community in one go. {actor} is the member who started the prune, shown as a clickable user chip, or the word System.',
});
export const MEMBER_BAN_ADD_SUMMARY = msg({
	message: '{actor} banned {target}.',
	comment:
		'Activity log summary for a user who was banned from the community with no end date. A ban removes the user from the community and stops them from joining again. It is not the feature that blocks a user. {actor} is the member who made the ban, shown as a clickable user chip, or the word System. {target} is the banned user, shown as a clickable user chip.',
});
export const MEMBER_BAN_ADD_TEMPORARY_SUMMARY = msg({
	message: '{actor} banned {target} for {duration}.',
	comment:
		'Activity log summary for a user who was banned from the community for a limited time. A ban removes the user from the community and stops them from joining again. It is not the feature that blocks a user. {actor} is the member who made the ban, shown as a clickable user chip, or the word System. {target} is the banned user, shown as a clickable user chip. {duration} is how long the ban lasts, shown as a formatted duration phrase such as "3 days" or "12 hours".',
});
export const MEMBER_BAN_DELETED_MESSAGES_ROW = msg({
	message: "Deleted {duration} of the member's recent message history.",
	comment:
		'Activity log detail line under a ban, shown when the ban also deleted the messages the banned user sent shortly before it. This matches the "Delete message history" choice in the ban dialog. "The member" is the banned user, a single person. {duration} is how far back from the ban the messages were deleted, shown as a full duration phrase such as "1 hour" or "7 days". The number and unit change at runtime, so do not put an article, adjective or case ending next to {duration} that has to agree with it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_BAN_REMOVE_SUMMARY = msg({
	message: '{actor} unbanned {target}.',
	comment:
		'Activity log summary for a user whose ban from the community was lifted. A ban is the community ban, not the feature that blocks a user. {actor} is the member who lifted the ban, shown as a clickable user chip, or the word System. {target} is the unbanned user, shown as a clickable user chip.',
});
export const MEMBER_BAN_ORIGINAL_MODERATOR_AND_DATE_ROW = msg({
	message: 'Originally banned by {user} on {date}.',
	comment:
		'Activity log detail line under a lifted ban, describing the ban that was lifted. A ban is the community ban, not the feature that blocks a user. Shown when someone other than the person who lifted the ban had made it. {user} is the member who made the original ban, shown as a clickable user chip. {date} is when the original ban was made, shown as a formatted date and time.',
});
export const MEMBER_BAN_ORIGINAL_DATE_ROW = msg({
	message: 'Originally banned on {date}.',
	comment:
		'Activity log detail line under a lifted ban, describing the ban that was lifted. A ban is the community ban, not the feature that blocks a user. Shown when the person who lifted the ban had also made it, or when it is not known who made it. {date} is when the original ban was made, shown as a formatted date and time.',
});
export const MEMBER_BAN_ORIGINAL_MODERATOR_ROW = msg({
	message: 'Originally banned by {user}.',
	comment:
		'Activity log detail line under a lifted ban, describing the ban that was lifted. A ban is the community ban, not the feature that blocks a user. Shown when someone other than the person who lifted the ban had made it and the date of the ban was not recorded. {user} is the member who made the original ban, shown as a clickable user chip.',
});
export const MEMBER_BAN_EXPIRY_PENDING_ROW = msg({
	message: 'The ban was scheduled to expire on {date}.',
	comment:
		'Activity log detail line under a lifted ban, for a temporary ban that was lifted before it ended on its own. The ban never reached this date, so do not say that it expired. A ban is the community ban, not the feature that blocks a user. {date} is when the ban would have ended, shown as a formatted date and time.',
});
export const MEMBER_BAN_EXPIRY_PASSED_ROW = msg({
	message: 'The ban had already expired on {date}.',
	comment:
		'Activity log detail line under a lifted ban, for a temporary ban that had already ended on its own before it was lifted. A ban is the community ban, not the feature that blocks a user. {date} is when the ban ended, shown as a formatted date and time.',
});
export const MEMBER_NICKNAME_SET_SUMMARY = msg({
	message: '{actor} set the nickname of {target} to {nick}.',
	comment:
		'Activity log summary for a nickname that someone gave to another member who had no nickname in the community. {actor} is the member who set the nickname, shown as a clickable user chip, or the word System. {target} is the member who got the nickname, shown as a clickable user chip. {nick} is the new nickname. The app shows it in bold, so do not add ** or <b>.',
});
export const MEMBER_NICKNAME_CHANGED_SUMMARY = msg({
	message: '{actor} changed the nickname of {target} from {oldNick} to {newNick}.',
	comment:
		'Activity log summary for a nickname that someone changed for another member in the community. {actor} is the member who changed the nickname, shown as a clickable user chip, or the word System. {target} is the member whose nickname changed, shown as a clickable user chip. {oldNick} is the previous nickname and {newNick} is the new nickname. Keep {oldNick} as the old value and {newNick} as the new value. The app shows both in bold, so do not add ** or <b>.',
});
export const MEMBER_NICKNAME_REMOVED_SUMMARY = msg({
	message: '{actor} removed the nickname {nick} from {target}.',
	comment:
		'Activity log summary for a nickname that someone removed from another member in the community. {actor} is the member who removed the nickname, shown as a clickable user chip, or the word System. {nick} is the nickname that was removed. The app shows it in bold, so do not add ** or <b>. {target} is the member who lost the nickname, shown as a clickable user chip.',
});
export const MEMBER_OWN_NICKNAME_SET_SUMMARY = msg({
	message: '{actor} set their nickname to {nick}.',
	comment:
		'Activity log summary for a member who gave themselves a nickname in the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {nick} is the new nickname. The app shows it in bold, so do not add ** or <b>.',
});
export const MEMBER_OWN_NICKNAME_CHANGED_SUMMARY = msg({
	message: '{actor} changed their nickname from {oldNick} to {newNick}.',
	comment:
		'Activity log summary for a member who changed their own nickname in the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {oldNick} is the previous nickname and {newNick} is the new nickname. Keep {oldNick} as the old value and {newNick} as the new value. The app shows both in bold, so do not add ** or <b>.',
});
export const MEMBER_OWN_NICKNAME_REMOVED_SUMMARY = msg({
	message: '{actor} removed their nickname {nick}.',
	comment:
		'Activity log summary for a member who removed their own nickname in the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {nick} is the nickname that was removed. The app shows it in bold, so do not add ** or <b>.',
});
export const MEMBER_TIMEOUT_SET_SUMMARY = msg({
	message: '{actor} timed out {target} until {date}.',
	comment:
		'Activity log summary for a member who was given a timeout. A timeout is the moderation action that stops a member from sending messages, reacting and joining voice until it ends. It is not a ban, a kick or a community mute. Use the same term as the "Timeout" and "Time out members" labels. {actor} is the member who gave the timeout, shown as a clickable user chip, or the word System. {target} is the member in timeout, shown as a clickable user chip. {date} is when the timeout ends, shown as a formatted date and time.',
});
export const MEMBER_TIMEOUT_CHANGED_SUMMARY = msg({
	message: "{actor} changed {target}'s block on sending messages, reacting and joining voice to end at {date}.",
	comment:
		'Activity log summary for a block on a member that was already running and was given a new end. The block stops the member from sending messages, reacting and joining voice, and it lifts on its own at the new end. Spell the effect out the way the English does. Do not name it with a word for a ban, a kick, a mute, a suspension or a waiting period, and do not say that time ran out. Keep the three things that are blocked. This is a past tense record of an action that already happened, never an instruction to the reader. {actor} is the member who moved the end of the block, shown as a clickable user chip, or the word System. {target} is the blocked member, shown as a clickable user chip. {date} is the new end of the block, shown as a formatted date and time.',
});
export const MEMBER_TIMEOUT_REMOVED_SUMMARY = msg({
	message: '{actor} removed the timeout for {target}.',
	comment:
		'Activity log summary for a timeout that was ended early. A timeout is the moderation action that stops a member from sending messages, reacting and joining voice until it ends. It is not a ban, a kick or a community mute. {actor} is the member who removed the timeout, shown as a clickable user chip, or the word System. {target} is the member whose timeout was removed, shown as a clickable user chip.',
});
export const MEMBER_SERVER_MUTE_APPLIED_SUMMARY = msg({
	message: "{actor} turned off {target}'s microphone in this community.",
	comment:
		'Activity log summary for a moderator who turned off the microphone of a member in every voice channel of this community, so nobody can hear the member until it is turned back on. Describe the effect the way the English does. Do not name the action with a noun such as a community mute, which reads as muting the whole community and collides with the setting that silences community notifications. The action that stops a member from hearing others is a different action and must read clearly differently. This is a past tense record of an action that already happened, never an instruction to the reader. {actor} is the member who turned the microphone off, shown as a clickable user chip, or the word System. {target} is the member whose microphone was turned off, shown as a clickable user chip.',
});
export const MEMBER_SERVER_MUTE_REMOVED_SUMMARY = msg({
	message: "{actor} turned {target}'s microphone back on in this community.",
	comment:
		'Activity log summary for a moderator who turned the microphone of a member back on in the voice channels of this community, so others can hear the member again. Describe the effect the way the English does. Do not name the action with a noun such as a community mute, which reads as muting the whole community. The action that lets a member hear others again is a different action and must read clearly differently. This is a past tense record of an action that already happened, never an instruction to the reader. {actor} is the member who turned the microphone back on, shown as a clickable user chip, or the word System. {target} is the member who can be heard again, shown as a clickable user chip.',
});
export const MEMBER_SERVER_DEAFEN_APPLIED_SUMMARY = msg({
	message: '{actor} stopped {target} from hearing others in this community.',
	comment:
		'Activity log summary for a moderator who stopped a member from hearing anyone in the voice channels of this community until it is undone. This is about what the member hears and not about the microphone of the member, so it must read clearly differently from the row where a moderator turns the microphone off. Describe the effect the way the English does. Do not name the action with a noun such as a community deafen. This is a past tense record of an action that already happened, never an instruction to the reader. {actor} is the member who stopped it, shown as a clickable user chip, or the word System. {target} is the member who can no longer hear others, shown as a clickable user chip.',
});
export const MEMBER_SERVER_DEAFEN_REMOVED_SUMMARY = msg({
	message: '{actor} let {target} hear others in this community again.',
	comment:
		'Activity log summary for a moderator who let a member hear the voice channels of this community again. This is about what the member hears and not about the microphone of the member, so it must read clearly differently from the row where a moderator turns the microphone back on. Describe the effect the way the English does. Do not name the action with a noun such as a community deafen. This is a past tense record of an action that already happened, never an instruction to the reader. {actor} is the member who lifted it, shown as a clickable user chip, or the word System. {target} is the member who can hear others again, shown as a clickable user chip.',
});
export const MEMBER_ROLE_ADDED_SUMMARY = msg({
	message: '{actor} gave {target} the role {role}.',
	comment:
		'Activity log summary for a role that was given to a member. {actor} is the member who gave the role, shown as a clickable user chip, or the word System. {target} is the member who got the role, shown as a clickable user chip. {role} is the role, shown as a role chip with its name.',
});
export const MEMBER_ROLE_REMOVED_SUMMARY = msg({
	message: '{actor} removed the role {role} from {target}.',
	comment:
		'Activity log summary for a role that was taken away from a member. {actor} is the member who removed the role, shown as a clickable user chip, or the word System. {role} is the role, shown as a role chip with its name. {target} is the member who lost the role, shown as a clickable user chip.',
});
export const MEMBER_ROLES_UPDATED_SUMMARY = msg({
	message: '{actor} updated the roles of {target}.',
	comment:
		'Activity log summary for a change to the roles of a member when several roles changed at once or the changed role was not recorded. Each role that changed is listed as a detail line below. {actor} is the member who changed the roles, shown as a clickable user chip, or the word System. {target} is the member whose roles changed, shown as a clickable user chip.',
});
export const MEMBER_ROLE_ADDED_BY_SYSTEM_SUMMARY = msg({
	message: 'The system gave {target} the role {role}.',
	comment:
		'Activity log summary for a role that the app gave a member on its own, with no person involved, for example after a purchase. "The system" is the app itself acting automatically. The role goes to the member, not the other way round. {target} is the member who got the role, shown as a clickable user chip. {role} is the role, shown as a role chip with its name.',
});
export const MEMBER_ROLE_REMOVED_BY_SYSTEM_SUMMARY = msg({
	message: 'The system removed the role {role} from {target}.',
	comment:
		'Activity log summary for a role that the app took away from a member on its own, with no person involved. "The system" is the app itself acting automatically. The role is taken away from the member, and the role itself is not deleted. {role} is the role, shown as a role chip with its name. {target} is the member who lost the role, shown as a clickable user chip.',
});
export const MEMBER_OWN_AVATAR_SET_SUMMARY = msg({
	message: '{actor} set their community profile avatar.',
	comment:
		'Activity log summary for a member who added an avatar to their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member, not to the community. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_AVATAR_CHANGED_SUMMARY = msg({
	message: '{actor} changed their community profile avatar.',
	comment:
		'Activity log summary for a member who replaced the avatar on their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_AVATAR_REMOVED_SUMMARY = msg({
	message: '{actor} removed their community profile avatar.',
	comment:
		'Activity log summary for a member who removed the avatar from their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_BANNER_SET_SUMMARY = msg({
	message: '{actor} set their community profile banner.',
	comment:
		'Activity log summary for a member who added a banner image to their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member, not to the community. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_BANNER_CHANGED_SUMMARY = msg({
	message: '{actor} changed their community profile banner.',
	comment:
		'Activity log summary for a member who replaced the banner image on their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_BANNER_REMOVED_SUMMARY = msg({
	message: '{actor} removed their community profile banner.',
	comment:
		'Activity log summary for a member who removed the banner image from their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_BIO_SET_SUMMARY = msg({
	message: '{actor} set their community profile bio to {text}.',
	comment:
		'Activity log summary for a member who added a bio to their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {text} is the new bio, shown as boxed text and shortened when long.',
});
export const MEMBER_OWN_BIO_CHANGED_SUMMARY = msg({
	message: '{actor} changed their community profile bio to {text}.',
	comment:
		'Activity log summary for a member who changed the bio on their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {text} is the new bio, shown as boxed text and shortened when long.',
});
export const MEMBER_OWN_BIO_REMOVED_SUMMARY = msg({
	message: '{actor} removed their community profile bio.',
	comment:
		'Activity log summary for a member who removed the bio from their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_PRONOUNS_SET_SUMMARY = msg({
	message: '{actor} set their community profile pronouns to {text}.',
	comment:
		'Activity log summary for a member who added or changed the pronouns on their community profile. Pronouns are the words the member wants others to use for them, such as they/them. A community profile is the profile a member uses only in this community, in place of their global profile. The pronouns belong to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {text} is the new pronouns as the member wrote them, shown as boxed text.',
});
export const MEMBER_OWN_PRONOUNS_REMOVED_SUMMARY = msg({
	message: '{actor} removed their community profile pronouns.',
	comment:
		'Activity log summary for a member who removed the pronouns from their community profile. Pronouns are the words the member wants others to use for them, such as they/them. A community profile is the profile a member uses only in this community, in place of their global profile. The pronouns belong to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_ACCENT_COLOR_SET_SUMMARY = msg({
	message: '{actor} set their community profile accent color to {color}.',
	comment:
		'Activity log summary for a member who added or changed the accent color on their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The accent color belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip. {color} is the new color as a hex code such as #FF0000, shown next to a dot in that color.',
});
export const MEMBER_OWN_ACCENT_COLOR_REMOVED_SUMMARY = msg({
	message: '{actor} removed their community profile accent color.',
	comment:
		'Activity log summary for a member who removed the accent color from their community profile. A community profile is the profile a member uses only in this community, in place of their global profile. The accent color belongs to the member, not to the community. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_OWN_PROFILE_UPDATED_SUMMARY = msg({
	message: '{actor} updated their community profile.',
	comment:
		'Activity log summary for a member who changed several things about their own community profile or membership at once, or when the change was not recorded. A community profile is the profile a member uses only in this community, in place of their global profile. It belongs to the member, not to the community. Each change is listed as a detail line below. "Their" means the same member as {actor}. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_UPDATED_SUMMARY = msg({
	message: '{actor} updated {target}.',
	comment:
		'Activity log summary for a change to a member of the community when several things changed at once, or when the change was not recorded. Each change is listed as a detail line below. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {target} is the member who was changed, shown as a clickable user chip.',
});
export const MEMBER_NICKNAME_SET_ROW = msg({
	message: 'Set the nickname to {nick}.',
	comment:
		'Activity log detail line under a change to a member, for a nickname given to a member who had none. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {nick} is the new nickname. The app shows it in bold, so do not add ** or <b>.',
});
export const MEMBER_NICKNAME_CHANGED_ROW = msg({
	message: 'Changed the nickname from {oldNick} to {newNick}.',
	comment:
		'Activity log detail line under a change to a member, for a changed nickname. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldNick} is the previous nickname and {newNick} is the new nickname. Keep {oldNick} as the old value and {newNick} as the new value. The app shows both in bold, so do not add ** or <b>.',
});
export const MEMBER_NICKNAME_REMOVED_ROW = msg({
	message: 'Removed the nickname {nick}.',
	comment:
		'Activity log detail line under a change to a member, for a removed nickname. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {nick} is the nickname that was removed. The app shows it in bold, so do not add ** or <b>.',
});
export const MEMBER_ROLE_ADDED_ROW = msg({
	message: 'Added the role {role}.',
	comment:
		'Activity log detail line under a change to the roles of a member, for one role the member got. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {role} is the role, shown as a role chip with its name.',
});
export const MEMBER_ROLE_REMOVED_ROW = msg({
	message: 'Removed the role {role}.',
	comment:
		'Activity log detail line under a change to the roles of a member, for one role the member lost. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {role} is the role, shown as a role chip with its name.',
});
export const MEMBER_TIMEOUT_SET_ROW = msg({
	message: 'Blocked the member from sending messages, reacting and joining voice until {date}.',
	comment:
		'Activity log detail line under a change to a member, for a block a moderator put on the member. The block stops the member from sending messages, reacting and joining voice, and it lifts on its own at the given date. Spell the effect out the way the English does. Do not name it with a word for a ban, a kick, a mute, a suspension or a waiting period, and do not say that time ran out. Keep the three things that are blocked. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {date} is when the block ends, shown as a formatted date and time.',
});
export const MEMBER_TIMEOUT_CHANGED_ROW = msg({
	message: 'Changed the block on sending messages, reacting and joining voice to end at {date}.',
	comment:
		'Activity log detail line under a change to a member, for a block that was already running and was given a new end. The block stops the member from sending messages, reacting and joining voice, and it lifts on its own at the new end. Spell the effect out the way the English does. Do not name it with a word for a ban, a kick, a mute, a suspension or a waiting period, and do not say that time ran out. Keep the three things that are blocked. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {date} is the new end of the block, shown as a formatted date and time.',
});
export const MEMBER_TIMEOUT_REMOVED_ROW = msg({
	message: 'Removed the timeout.',
	comment:
		'Activity log detail line under a change to a member, for a timeout that was ended early. A timeout is the moderation action that stops a member from sending messages, reacting and joining voice until it ends. It is not a ban, a kick or a community mute. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_SERVER_MUTE_APPLIED_ROW = msg({
	message: "Turned off the member's microphone in this community.",
	comment:
		'Activity log detail line under a change to a member, for a moderator who turned off the microphone of the member in every voice channel of this community, so nobody can hear the member until it is turned back on. Describe the effect the way the English does. Do not name the action with a noun such as a community mute, which reads as muting the whole community. The row where a moderator stops the member from hearing others is a different action and must read clearly differently. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_SERVER_MUTE_REMOVED_ROW = msg({
	message: "Turned the member's microphone back on in this community.",
	comment:
		'Activity log detail line under a change to a member, for a moderator who turned the microphone of the member back on in the voice channels of this community, so others can hear the member again. Describe the effect the way the English does. Do not name the action with a noun such as a community mute, which reads as muting the whole community. The row where a moderator lets the member hear others again is a different action and must read clearly differently. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_SERVER_DEAFEN_APPLIED_ROW = msg({
	message: 'Stopped the member from hearing others in this community.',
	comment:
		'Activity log detail line under a change to a member, for a moderator who stopped the member from hearing anyone in the voice channels of this community until it is undone. This is about what the member hears and not about the microphone of the member, so it must read clearly differently from the row where a moderator turns the microphone off. Describe the effect the way the English does. Do not name the action with a noun such as a community deafen. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_SERVER_DEAFEN_REMOVED_ROW = msg({
	message: 'Let the member hear others in this community again.',
	comment:
		'Activity log detail line under a change to a member, for a moderator who let the member hear the voice channels of this community again. This is about what the member hears and not about the microphone of the member, so it must read clearly differently from the row where a moderator turns the microphone back on. Describe the effect the way the English does. Do not name the action with a noun such as a community deafen. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_AVATAR_SET_ROW = msg({
	message: "Added the member's community profile avatar.",
	comment:
		'Activity log detail line under a change to a member, for an avatar added to the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as an avatar of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Added is past tense here.',
});
export const MEMBER_AVATAR_CHANGED_ROW = msg({
	message: "Changed the member's community profile avatar.",
	comment:
		'Activity log detail line under a change to a member, for a replaced avatar on the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as an avatar of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_AVATAR_REMOVED_ROW = msg({
	message: "Removed the member's community profile avatar.",
	comment:
		'Activity log detail line under a change to a member, for an avatar removed from the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The avatar belongs to the member and not to the community, so keep the member as the owner. It is not the community icon. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_BANNER_SET_ROW = msg({
	message: "Added the member's community profile banner.",
	comment:
		'Activity log detail line under a change to a member, for a banner image added to the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as the banner of the community itself, which is a different setting. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Added is past tense here.',
});
export const MEMBER_BANNER_CHANGED_ROW = msg({
	message: "Changed the member's community profile banner.",
	context: 'audit-log-member-profile',
	comment:
		'Activity log detail line under a change to a member, for a replaced banner image on the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as the banner of the community itself, which is a different setting. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_BANNER_REMOVED_ROW = msg({
	message: "Removed the member's community profile banner.",
	context: 'audit-log-member-profile',
	comment:
		'Activity log detail line under a change to a member, for a banner image removed from the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The banner belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as the banner of the community itself, which is a different setting. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_BIO_SET_ROW = msg({
	message: "Set the member's community profile bio to {text}.",
	comment:
		'Activity log detail line under a change to a member, for a bio added to the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as a bio of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the new bio, shown as boxed text and shortened when long.',
});
export const MEMBER_BIO_CHANGED_ROW = msg({
	message: "Changed the member's community profile bio to {text}.",
	comment:
		'Activity log detail line under a change to a member, for a changed bio on the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as a bio of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {text} is the new bio, shown as boxed text and shortened when long.',
});
export const MEMBER_BIO_REMOVED_ROW = msg({
	message: "Removed the member's community profile bio.",
	comment:
		'Activity log detail line under a change to a member, for a bio removed from the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The bio belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as a bio of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_PRONOUNS_SET_ROW = msg({
	message: "Set the member's community profile pronouns to {text}.",
	comment:
		'Activity log detail line under a change to a member, for pronouns added or changed on the community profile of the member. Pronouns are the words the member wants others to use for them, such as they/them. A community profile is the profile a member uses only in this community, in place of their global profile. The pronouns belong to the member and not to the community, so keep the member as the owner. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {text} is the new pronouns as the member wrote them, shown as boxed text.',
});
export const MEMBER_PRONOUNS_REMOVED_ROW = msg({
	message: "Removed the member's community profile pronouns.",
	comment:
		'Activity log detail line under a change to a member, for pronouns removed from the community profile of the member. Pronouns are the words the member wants others to use for them, such as they/them. A community profile is the profile a member uses only in this community, in place of their global profile. The pronouns belong to the member and not to the community, so keep the member as the owner. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_ACCENT_COLOR_SET_ROW = msg({
	message: "Set the member's community profile accent color to {color}.",
	comment:
		'Activity log detail line under a change to a member, for an accent color added or changed on the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The accent color belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as the color of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {color} is the new color as a hex code such as #FF0000, shown next to a dot in that color.',
});
export const MEMBER_ACCENT_COLOR_REMOVED_ROW = msg({
	message: "Removed the member's community profile accent color.",
	comment:
		'Activity log detail line under a change to a member, for an accent color removed from the community profile of the member. A community profile is the profile a member uses only in this community, in place of their global profile. The accent color belongs to the member and not to the community, so keep the member as the owner and never let the phrase read as the color of the community itself. It continues the entry summary, so it has no subject. It is a past tense record of an action that already happened, never an instruction to the reader. Do not write it as a command, a button label or in first or second person.',
});
export const MEMBER_NO_LONGER_TEMPORARY_ROW = msg({
	message: 'The member is no longer temporary.',
	comment:
		'Activity log detail line under a change to a member who had joined with temporary membership and became a regular member, which happens when a temporary member gets a role.',
});
export const MEMBER_MOVE_SUMMARY = msg({
	message: '{actor} moved {target} from {oldChannel} to {newChannel}.',
	comment:
		'Activity log summary for a member who was moved from one voice channel to another. {actor} is the member who did the move, shown as a clickable user chip, or the word System. {target} is the member who was moved, shown as a clickable user chip. {oldChannel} is the voice channel they were in and {newChannel} is the voice channel they were moved to, both shown as channel chips with the channel name.',
});
export const MEMBER_MOVE_TO_SUMMARY = msg({
	message: '{actor} moved {target} to {channel}.',
	comment:
		'Activity log summary for a member who was moved to a voice channel when the channel they came from was not recorded. {actor} is the member who did the move, shown as a clickable user chip, or the word System. {target} is the member who was moved, shown as a clickable user chip. {channel} is the voice channel they were moved to, shown as a channel chip with the channel name.',
});
export const MEMBER_MOVE_UNKNOWN_CHANNEL_SUMMARY = msg({
	message: '{actor} moved {target} to a different voice channel.',
	comment:
		'Activity log summary for a member who was moved to another voice channel when the channel they were moved to was not recorded. {actor} is the member who did the move, shown as a clickable user chip, or the word System. {target} is the member who was moved, shown as a clickable user chip.',
});
export const MEMBER_OWN_MOVE_SUMMARY = msg({
	message: '{actor} moved from {oldChannel} to {newChannel}.',
	comment:
		'Activity log summary for a member who moved themselves from one voice channel to another using moderator tools. {actor} is that member, shown as a clickable user chip. {oldChannel} is the voice channel they were in and {newChannel} is the voice channel they moved to, both shown as channel chips with the channel name.',
});
export const MEMBER_OWN_MOVE_TO_SUMMARY = msg({
	message: '{actor} moved to {channel}.',
	comment:
		'Activity log summary for a member who moved themselves to a voice channel using moderator tools, when the channel they came from was not recorded. {actor} is that member, shown as a clickable user chip. {channel} is the voice channel they moved to, shown as a channel chip with the channel name.',
});
export const MEMBER_OWN_MOVE_UNKNOWN_CHANNEL_SUMMARY = msg({
	message: '{actor} moved to a different voice channel.',
	comment:
		'Activity log summary for a member who moved themselves to another voice channel using moderator tools, when the channel they moved to was not recorded. {actor} is that member, shown as a clickable user chip.',
});
export const MEMBER_DISCONNECT_SUMMARY = msg({
	message: '{actor} disconnected {target} from {channel}.',
	comment:
		'Activity log summary for a member who was removed from a voice channel by someone else. {actor} is the member who disconnected them, shown as a clickable user chip, or the word System. {target} is the member who was disconnected, shown as a clickable user chip. {channel} is the voice channel they were in, shown as a channel chip with the channel name.',
});
export const MEMBER_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY = msg({
	message: '{actor} disconnected {target} from voice.',
	comment:
		'Activity log summary for a member who was removed from a voice channel by someone else when the channel was not recorded. {actor} is the member who disconnected them, shown as a clickable user chip, or the word System. {target} is the member who was disconnected, shown as a clickable user chip.',
});
export const MEMBER_OWN_DISCONNECT_SUMMARY = msg({
	message: '{actor} disconnected from {channel}.',
	comment:
		'Activity log summary for a member who removed themselves from a voice channel using moderator tools. {actor} is that member, shown as a clickable user chip. {channel} is the voice channel they left, shown as a channel chip with the channel name.',
});
export const MEMBER_OWN_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY = msg({
	message: '{actor} disconnected from voice.',
	comment:
		'Activity log summary for a member who removed themselves from a voice channel using moderator tools, when the channel was not recorded. {actor} is that member, shown as a clickable user chip.',
});
export const BOT_ADD_SUMMARY = msg({
	message: '{actor} added the bot {target}.',
	comment:
		'Activity log summary for a bot that was added to the community. {actor} is the member who added the bot, shown as a clickable user chip, or the word System. {target} is the bot, shown as a clickable user chip.',
});
export const BOT_ADD_TEMPORARY_SUMMARY = msg({
	message: '{actor} added the bot {target} as a temporary member.',
	comment:
		'Activity log summary for a bot that was added to the community with temporary membership. {actor} is the member who added the bot, shown as a clickable user chip, or the word System. {target} is the bot, shown as a clickable user chip.',
});
