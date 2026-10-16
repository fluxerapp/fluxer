// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	DOCK_BADGE_DESCRIPTOR,
	MASTER_VOLUME_DESCRIPTOR,
	MESSAGE_BADGE_DESCRIPTOR,
	OUTPUT_VOLUME_DESCRIPTOR,
	SOUND_VOLUME_DESCRIPTOR,
	TASKBAR_BADGE_DESCRIPTOR,
	UNREAD_BADGE_DESCRIPTOR,
	UNREAD_DESCRIPTOR,
	UNREAD_MESSAGE_BADGE_DESCRIPTOR,
	VOLUME_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	ENABLE_UNREAD_MESSAGE_BADGE_DESCRIPTOR,
	PUSH_NOTIFICATION_INACTIVE_TIMEOUT_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const APPLICATION_ICON_BADGE_DESCRIPTOR = msg({
	message: 'Application icon badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const SHOW_A_RED_BADGE_ON_THE_APP_ICON_DESCRIPTOR = msg({
	message: 'Show a red badge on the app icon when you have unread messages',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const IDLE_DESCRIPTOR = msg({
	message: 'Idle',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IDLE_TIMEOUT_DESCRIPTOR = msg({
	message: 'Idle timeout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AWAY_DESCRIPTOR = msg({
	message: 'Away',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INACTIVE_DESCRIPTOR = msg({
	message: 'Inactive',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INACTIVE_TIMEOUT_DESCRIPTOR = msg({
	message: 'Inactive timeout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MOBILE_PUSH_TIMEOUT_DESCRIPTOR = msg({
	message: 'Mobile push timeout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PUSH_TIMEOUT_DESCRIPTOR = msg({
	message: 'Push timeout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESKTOP_AWAY_DESCRIPTOR = msg({
	message: 'Desktop away',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_HOW_LONG_YOU_NEED_TO_BE_INACTIVE_DESCRIPTOR = msg({
	message: 'Desktop inactivity before mobile push',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const NOTIFICATION_VOLUME_DESCRIPTOR = msg({
	message: 'Notification volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADJUST_THE_MASTER_VOLUME_FOR_ALL_NOTIFICATION_SOUNDS_DESCRIPTOR = msg({
	message: 'Adjust the master volume for all notification sounds',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const notificationsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'notifications-unread-badge',
		tabType: 'notifications',
		sectionId: 'notifications',
		label: ENABLE_UNREAD_MESSAGE_BADGE_DESCRIPTOR,
		keywords: [
			UNREAD_DESCRIPTOR,
			UNREAD_BADGE_DESCRIPTOR,
			UNREAD_MESSAGE_BADGE_DESCRIPTOR,
			APPLICATION_ICON_BADGE_DESCRIPTOR,
			MESSAGE_BADGE_DESCRIPTOR,
			DOCK_BADGE_DESCRIPTOR,
			TASKBAR_BADGE_DESCRIPTOR,
		],
		description: SHOW_A_RED_BADGE_ON_THE_APP_ICON_DESCRIPTOR,
	},
	{
		id: 'notifications-afk-timeout',
		tabType: 'notifications',
		sectionId: 'notifications',
		label: PUSH_NOTIFICATION_INACTIVE_TIMEOUT_DESCRIPTOR,
		keywords: [
			IDLE_DESCRIPTOR,
			IDLE_TIMEOUT_DESCRIPTOR,
			AWAY_DESCRIPTOR,
			INACTIVE_DESCRIPTOR,
			INACTIVE_TIMEOUT_DESCRIPTOR,
			MOBILE_PUSH_TIMEOUT_DESCRIPTOR,
			PUSH_TIMEOUT_DESCRIPTOR,
			DESKTOP_AWAY_DESCRIPTOR,
		],
		description: CONTROL_HOW_LONG_YOU_NEED_TO_BE_INACTIVE_DESCRIPTOR,
	},
	{
		id: 'notifications-master-volume',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: MASTER_VOLUME_DESCRIPTOR,
		keywords: [
			VOLUME_DESCRIPTOR,
			MASTER_VOLUME_DESCRIPTOR,
			OUTPUT_VOLUME_DESCRIPTOR,
			SOUND_VOLUME_DESCRIPTOR,
			NOTIFICATION_VOLUME_DESCRIPTOR,
		],
		description: ADJUST_THE_MASTER_VOLUME_FOR_ALL_NOTIFICATION_SOUNDS_DESCRIPTOR,
	},
];
