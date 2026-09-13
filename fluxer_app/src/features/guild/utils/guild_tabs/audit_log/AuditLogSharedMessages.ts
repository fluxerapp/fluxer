// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const DELETED_CHANNEL_LABEL = msg({
	message: 'deleted-channel',
	comment:
		'Activity log name shown in place of a channel name when the channel no longer exists and its name was not recorded. The app puts a # sign in front of it, so it reads like a channel name, for example "Deleted #deleted-channel." Translate it. Write it in lowercase with a hyphen between the words, like a channel name, where your script allows it. Use the same word for channel as the rest of the app.',
});
export const DELETED_CATEGORY_LABEL = msg({
	message: 'deleted-category',
	comment:
		'Activity log name shown in place of a category name when the category no longer exists and its name was not recorded. It has no # sign in front of it, for example "Deleted deleted-category." Translate it. Write it in lowercase with a hyphen between the words, like a channel name, where your script allows it. Use the same word for category as the rest of the app.',
});
export const DELETED_ROLE_LABEL = msg({
	message: 'deleted-role',
	comment:
		'Activity log name shown in place of a role name when the role no longer exists and its name was not recorded, for example "Deleted the role deleted-role." Translate it. Write it in lowercase with a hyphen between the words, like the other deleted labels, where your script allows it. Use the same word for role as the rest of the app.',
});
export const SYSTEM_ACTOR_LABEL = msg({
	message: 'System',
	comment:
		'Activity log name shown as the one who made a change when the app made it automatically and no person did. It fills the {actor} slot at the start of activity log sentences.',
});
export const BAN_REASON_LABEL = msg({
	message: 'Ban reason',
	comment:
		'Activity log label next to the reason that was given when a member was banned. Shown in the expanded entry details, like the Reason label.',
});
export const MORE_PERMISSIONS_DESCRIPTOR = msg({
	message: '{count, plural, one {# more permission} other {# more permissions}}',
	comment:
		'Activity log item that stands in for the permission names left out of a long list. It is the last item of a list joined with "and", for example "Send messages, Manage roles, and 3 more permissions". The app joins the list and adds the commas and the word for "and" in your language, so do not add them here. It counts the other permissions in the same list. It never means that permissions were added. {count} is an ICU plural with the number of permissions left out, and # shows that number.',
});
export const UNKNOWN_ACTION_SUMMARY = msg({
	message: '{actor} made a change that this version of the app cannot show.',
	comment:
		'Activity log summary for a kind of change this app version cannot describe, usually because the server is newer than the app. {actor} is the member who made the change, shown as a clickable user chip, or the word System.',
});
export const CENTERED_LABEL = msg({
	message: 'Centered',
	comment: 'Audit log value for invite splash alignment: centered image placement.',
});
export const LEFT_ALIGNED_LABEL = msg({
	message: 'Left aligned',
	comment: 'Audit log value for invite splash alignment: left image placement.',
});
export const RIGHT_ALIGNED_LABEL = msg({
	message: 'Right aligned',
	comment: 'Audit log value for invite splash alignment: right image placement.',
});
export const VERIFICATION_LEVEL_NONE_LABEL = msg({
	message: 'None',
	comment:
		'Activity log value naming the lowest member verification level of a community, where no verification is required.',
});
export const VERIFICATION_LEVEL_LOW_LABEL = msg({
	message: 'Low',
	comment: 'Activity log value naming the low member verification level of a community.',
});
export const VERIFICATION_LEVEL_MEDIUM_LABEL = msg({
	message: 'Medium',
	comment: 'Activity log value naming the medium member verification level of a community.',
});
export const VERIFICATION_LEVEL_HIGH_LABEL = msg({
	message: 'High',
	comment: 'Activity log value naming the high member verification level of a community.',
});
export const VERIFICATION_LEVEL_VERY_HIGH_LABEL = msg({
	message: 'Very high',
	comment: 'Activity log value naming the highest member verification level of a community.',
});
export const EXPLICIT_CONTENT_FILTER_OFF_LABEL = msg({
	message: 'Off',
	comment: 'Activity log value meaning the explicit content filter of a community is turned off.',
});
export const EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_LABEL = msg({
	message: 'Filter members without roles',
	comment:
		'Activity log value for the explicit content filter setting that filters messages from members who have no roles.',
});
export const EXPLICIT_CONTENT_FILTER_EVERYONE_LABEL = msg({
	message: 'Filter everyone',
	comment: 'Activity log value for the explicit content filter setting that filters messages from every member.',
});
export const DEFAULT_NOTIFICATIONS_ALL_MESSAGES_LABEL = msg({
	message: 'All messages',
	comment:
		'Activity log value for the default notification setting of a community where members are notified about every message.',
});
export const DEFAULT_NOTIFICATIONS_MENTIONS_ONLY_LABEL = msg({
	message: 'Mentions only',
	comment:
		'Activity log value for the default notification setting of a community where members are notified only when mentioned.',
});
