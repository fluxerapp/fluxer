// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const ROLE_CREATE_SUMMARY = msg({
	message: '{actor} created the role {role}.',
	comment:
		'Activity log summary for a role that was created in the community. {actor} is the member who created it, shown as a clickable user chip, or the word System. {role} is the new role, shown in bold with its name, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_MOVE_UP_IN_ROLE_LIST_SUMMARY = msg({
	message: '{actor} moved the role {role} up in the role list.',
	comment:
		'Activity log summary for a role that was moved higher in the community role list, where higher roles rank above lower ones. {actor} is the member who moved it, shown as a clickable user chip, or the word System. {role} is the role that moved, shown in bold with its name, @everyone for the default role every member has, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_MOVE_DOWN_IN_ROLE_LIST_SUMMARY = msg({
	message: '{actor} moved the role {role} down in the role list.',
	comment:
		'Activity log summary for a role that was moved lower in the community role list, where higher roles rank above lower ones. {actor} is the member who moved it, shown as a clickable user chip, or the word System. {role} is the role that moved, shown in bold with its name, @everyone for the default role every member has, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_SET_MEMBER_LIST_POSITION_SUMMARY = msg({
	message: '{actor} set a member list position for the role {role}.',
	comment:
		'Activity log summary for a role that was given its own place in the order of role groups in the member list. Until then that order followed the role list. The member list position is where the group of members with this role appears in the member list. It is not a position of the member list itself. {actor} is the member who set it, shown as a clickable user chip, or the word System. {role} is the role, shown in bold with its name, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_RESET_MEMBER_LIST_POSITION_SUMMARY = msg({
	message: '{actor} reset the member list position for the role {role}.',
	comment:
		'Activity log summary for a role that lost its own place in the order of role groups in the member list, so that order follows the role list again. Reset means the position went back to the default. The member list position is where the group of members with this role appears in the member list. It is not a position of the member list itself. {actor} is the member who reset it, shown as a clickable user chip, or the word System. {role} is the role, shown in bold with its name, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_MOVE_UP_IN_MEMBER_LIST_SUMMARY = msg({
	message: '{actor} moved the role {role} up in the member list.',
	comment:
		'Activity log summary for a role whose group of members was moved higher in the member list. {actor} is the member who moved it, shown as a clickable user chip, or the word System. {role} is the role, shown in bold with its name, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_MOVE_DOWN_IN_MEMBER_LIST_SUMMARY = msg({
	message: '{actor} moved the role {role} down in the member list.',
	comment:
		'Activity log summary for a role whose group of members was moved lower in the member list. {actor} is the member who moved it, shown as a clickable user chip, or the word System. {role} is the role, shown in bold with its name, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_RENAME_SUMMARY = msg({
	message: '{actor} renamed the role {oldName} to {newName}.',
	comment:
		'Activity log summary for a role that got a new name and no other change. {actor} is the member who renamed it, shown as a clickable user chip, or the word System. {oldName} is the role name before the change and {newName} is the role name after it. Do not swap them. The app shows both in bold, so do not add ** or <b> around them. Use the same word for role as the rest of the app.',
});
export const ROLE_UPDATE_SUMMARY = msg({
	message: '{actor} updated the role {role}.',
	comment:
		'Activity log summary for a role that was edited. The changes are listed below it as detail lines when they are known. {actor} is the member who edited it, shown as a clickable user chip, or the word System. {role} is the role, shown in bold with its name, @everyone for the default role every member has, or deleted-role when its name is unknown. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_DELETE_SUMMARY = msg({
	message: '{actor} deleted the role {role}.',
	comment:
		'Activity log summary for a role that was deleted from the community. {actor} is the member who deleted it, shown as a clickable user chip, or the word System. {role} is the deleted role, shown in bold with the name it had when it was deleted, or deleted-role when that name was not recorded. The app adds the bold, so do not add ** or <b>. Use the same word for role as the rest of the app.',
});
export const ROLE_NAME_CHANGE_ROW = msg({
	message: 'Changed the name from {oldName} to {newName}.',
	comment:
		'Activity log detail line under an edited role that got a new name along with other changes. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldName} is the role name before the change and {newName} is the role name after it. Do not swap them. The app shows both in bold, so do not add ** or <b> around them.',
});
export const ROLE_PERMISSIONS_GRANT_ROW = msg({
	message: 'Granted {permissions}.',
	comment:
		'Activity log detail line under a created or edited role, listing the permissions the role now gives its members. The actor in the summary granted them to the role. Do not write it as someone receiving them. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {permissions} is a list of permission names that the app joins with commas and the word for "and" in your language. When the list is long it ends with a count of the permissions not named, such as "3 more permissions".',
});
export const ROLE_PERMISSIONS_REVOKE_ROW = msg({
	message: 'Revoked {permissions}.',
	comment:
		'Activity log detail line under an edited role, listing the permissions the role no longer gives its members. The actor in the summary took them away from the role. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {permissions} is a list of permission names that the app joins with commas and the word for "and" in your language. When the list is long it ends with a count of the permissions not named, such as "3 more permissions".',
});
export const ROLE_COLOR_SET_ROW = msg({
	message: 'Set the role color to {color}.',
	comment:
		'Activity log detail line under a created or edited role that got a name color when it had none before. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {color} is the color as a hex code such as #E67E22, shown in bold next to a dot in that color. Do not add ** or <b> around it. Use the same word for role as the rest of the app.',
});
export const ROLE_COLOR_REMOVE_ROW = msg({
	message: 'Removed the role color.',
	comment:
		'Activity log detail line under an edited role that no longer has a name color. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_COLOR_CHANGE_ROW = msg({
	message: 'Changed the role color from {oldColor} to {newColor}.',
	comment:
		'Activity log detail line under an edited role whose name color was replaced with another color. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldColor} is the color before the change and {newColor} is the color after it. Do not swap them. Each is a hex code such as #E67E22, shown in bold next to a dot in that color. Do not add ** or <b> around them. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_CREATE_ROW = msg({
	message: 'Showed members with this role in their own section in the member list.',
	comment:
		'Activity log detail line under a newly created role that was created with the Show this role separately setting turned on. Members with the role appear in their own section in the member list. This line is only used when a role is created. When an existing role is edited, the app uses "Started displaying members with this role separately." instead. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a settings label. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_START_ROW = msg({
	message: 'Started displaying members with this role separately.',
	comment:
		'Activity log detail line under an existing role that was edited to turn on the Show this role separately setting. Members with the role now appear in their own section in the member list. Separately describes how the members are displayed, not the role. This line is only used when an existing role is edited. When a role is created with the setting on, the app uses "Showed members with this role in their own section in the member list." instead. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a settings label. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_STOP_ROW = msg({
	message: 'Stopped displaying members with this role separately.',
	comment:
		'Activity log detail line under an existing role that was edited to turn off the Show this role separately setting. Members with the role no longer appear in their own section in the member list. Separately describes how the members are displayed, not the role. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a settings label. Use the same word for role as the rest of the app.',
});
export const ROLE_MENTIONABLE_ALLOW_ROW = msg({
	message: 'Allowed all members to mention this role.',
	comment:
		'Activity log detail line under a created or edited role whose Allow mentions for this role setting was turned on. Every member can now mention the role to notify the members who have it. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a statement of the current state. Use the same word for role and for mention as the rest of the app.',
});
export const ROLE_MENTIONABLE_DISALLOW_ROW = msg({
	message: 'Limited mentions of this role to members with permission to mention any role.',
	comment:
		'Activity log detail line under an edited role whose Allow mentions for this role setting was turned off. Members with the permission to mention @everyone, @here and any role can still mention this role. Other members can no longer mention it. Do not write it as if nobody can mention the role, and do not use a double negative. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. It is not a statement of the current state. Use the same word for role and for mention as the rest of the app.',
});
export const ROLE_EMOJI_SET_ROW = msg({
	message: 'Set the role icon to {emoji}.',
	comment:
		'Activity log detail line under a created or edited role that got a standard emoji as the icon shown next to the role name, when it had no emoji icon before. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. {emoji} is that emoji character, shown in bold. Do not add ** or <b> around it. Use the same word for role as the rest of the app.',
});
export const ROLE_EMOJI_CHANGE_ROW = msg({
	message: 'Changed the role icon from {oldEmoji} to {newEmoji}.',
	comment:
		'Activity log detail line under an edited role whose standard emoji icon was replaced with another emoji. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. {oldEmoji} is the emoji character before the change and {newEmoji} is the emoji character after it. Do not swap them. Both are shown in bold. Do not add ** or <b> around them. Use the same word for role as the rest of the app.',
});
export const ROLE_EMOJI_REMOVE_ROW = msg({
	message: 'Removed the role icon.',
	comment:
		'Activity log detail line under an edited role whose standard emoji icon was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_ICON_SET_ROW = msg({
	message: 'Set a custom role icon.',
	comment:
		'Activity log detail line under a created or edited role that got an uploaded image as the icon shown next to the role name, when it had no uploaded icon before. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. Use the same word for role as the rest of the app.',
});
export const ROLE_ICON_CHANGE_ROW = msg({
	message: 'Changed the custom role icon.',
	comment:
		'Activity log detail line under an edited role whose uploaded icon image was replaced with another image. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_ICON_REMOVE_ROW = msg({
	message: 'Removed the custom role icon.',
	comment:
		'Activity log detail line under an edited role whose uploaded icon image was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_POSITION_UP_ROW = msg({
	message: 'Moved the role up in the role list.',
	comment:
		'Activity log detail line under an edited role that was moved higher in the community role list, where higher roles rank above lower ones. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_POSITION_DOWN_ROW = msg({
	message: 'Moved the role down in the role list.',
	comment:
		'Activity log detail line under an edited role that was moved lower in the community role list, where higher roles rank above lower ones. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_POSITION_SET_ROW = msg({
	message: 'Set a member list position for the role.',
	comment:
		'Activity log detail line under an edited role that was given its own place in the order of role groups in the member list. Until then that order followed the role list. The member list position is where the group of members with this role appears in the member list. It is not a position of the member list itself. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Set is past tense here. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_POSITION_RESET_ROW = msg({
	message: 'Reset the member list position for the role.',
	comment:
		'Activity log detail line under an edited role that lost its own place in the order of role groups in the member list, so that order follows the role list again. Reset means the position went back to the default. The member list position is where the group of members with this role appears in the member list. It is not a position of the member list itself. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Reset is past tense here. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_POSITION_UP_ROW = msg({
	message: 'Moved the role up in the member list.',
	comment:
		'Activity log detail line under an edited role whose group of members was moved higher in the member list. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_HOIST_POSITION_DOWN_ROW = msg({
	message: 'Moved the role down in the member list.',
	comment:
		'Activity log detail line under an edited role whose group of members was moved lower in the member list. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. Do not write it as a command, a button label or in first or second person. Use the same word for role as the rest of the app.',
});
export const ROLE_DELETED_PERMISSIONS_ROW = msg({
	message: 'The role granted {permissions}.',
	comment:
		'Activity log detail line under a deleted role, listing the permissions the role gave its members before it was deleted. The role is the subject. {permissions} is a list of permission names that the app joins with commas and the word for "and" in your language. When the list is long it ends with a count of the permissions not named, such as "3 more permissions". Use the same word for role as the rest of the app.',
});
