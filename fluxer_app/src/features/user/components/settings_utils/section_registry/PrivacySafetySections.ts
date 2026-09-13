// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	COMMUNITY_MEMBERS_DESCRIPTOR,
	DIRECT_MESSAGES_DESCRIPTOR,
	FRIENDS_OF_FRIENDS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ADD_FRIEND_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	DELETE_DESCRIPTOR,
	DIRECT_MESSAGE_DESCRIPTOR,
	DM_DESCRIPTOR,
	FRIENDS_DESCRIPTOR,
	REMOVE_DESCRIPTOR,
	VOICE_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Friend requests',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Friend request',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WHO_CAN_FRIEND_DESCRIPTOR = msg({
	message: 'Who can friend',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTUAL_FRIENDS_DESCRIPTOR = msg({
	message: 'Mutual friends',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTUAL_GUILDS_DESCRIPTOR = msg({
	message: 'Mutual communities',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DM_PERMISSIONS_DESCRIPTOR = msg({
	message: 'DM permissions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WHO_CAN_DM_DESCRIPTOR = msg({
	message: 'Who can DM',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_DM_DESCRIPTOR = msg({
	message: 'Bot DM',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMMUNITY_DM_DESCRIPTOR = msg({
	message: 'Community DM',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INCOMING_DM_DESCRIPTOR = msg({
	message: 'Incoming DM',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PRIVACY_DESCRIPTOR = msg({
	message: 'Privacy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REQUESTS_DESCRIPTOR = msg({
	message: 'Requests',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INCOMING_CALLS_DESCRIPTOR = msg({
	message: 'Incoming calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WHO_CAN_CALL_DESCRIPTOR = msg({
	message: 'Who can call',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CALL_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Call permissions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BLOCK_CALLS_DESCRIPTOR = msg({
	message: 'Block calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRIENDS_ONLY_CALLS_DESCRIPTOR = msg({
	message: 'Friends-only calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CALL_PRIVACY_DESCRIPTOR = msg({
	message: 'Call privacy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SILENT_DESCRIPTOR = msg({
	message: 'Silent',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SILENT_CALLS_DESCRIPTOR = msg({
	message: 'Silent calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RING_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Ring behavior',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTE_CALLS_DESCRIPTOR = msg({
	message: 'Mute calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NO_RING_DESCRIPTOR = msg({
	message: 'No ring',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GROUP_CHAT_DESCRIPTOR = msg({
	message: 'Group chat',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GROUP_DM_DESCRIPTOR = msg({
	message: 'Group DM',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADD_TO_GROUP_DESCRIPTOR = msg({
	message: 'Add to group',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WHO_CAN_ADD_DESCRIPTOR = msg({
	message: 'Who can add',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GROUP_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Group permissions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GROUP_PRIVACY_DESCRIPTOR = msg({
	message: 'Group privacy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXTENDED_NETWORK_DESCRIPTOR = msg({
	message: 'Extended network',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMMUNITY_DESCRIPTOR = msg({
	message: 'Community',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GUILD_MEMBERS_DESCRIPTOR = msg({
	message: 'Community members',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROFILE_PRIVACY_DESCRIPTOR = msg({
	message: 'Profile privacy',
	comment: 'Privacy settings section for who can see full profile details.',
});
const CHOOSE_WHO_CAN_SEE_YOUR_PROFILE_DETAILS_DESCRIPTOR = msg({
	message: 'Choose who can see your profile details',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const BIO_DESCRIPTOR = msg({
	message: 'Bio',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PRONOUNS_DESCRIPTOR = msg({
	message: 'Pronouns',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_HOW_MATURE_OR_SENSITIVE_MEDIA_IS_FILTERED_DESCRIPTOR = msg({
	message: 'Control how mature or sensitive media is filtered in different contexts.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const SENSITIVE_CONTENT_2_DESCRIPTOR = msg({
	message: 'Sensitive content',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SAFETY_DESCRIPTOR = msg({
	message: 'Safety',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SENSITIVE_MEDIA_DESCRIPTOR = msg({
	message: 'Sensitive media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MATURE_CONTENT_DESCRIPTOR = msg({
	message: 'Mature content',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MATURE_MEDIA_DESCRIPTOR = msg({
	message: 'Mature media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FILTER_DESCRIPTOR = msg({
	message: 'Filter',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FILTERING_DESCRIPTOR = msg({
	message: 'Filtering',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BLUR_DESCRIPTOR = msg({
	message: 'Blur',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEDIA_FILTER_DESCRIPTOR = msg({
	message: 'Media filter',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTENT_FILTER_DESCRIPTOR = msg({
	message: 'Content filter',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPORT_DESCRIPTOR = msg({
	message: 'Export',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DATA_DESCRIPTOR = msg({
	message: 'Data',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOWNLOAD_DESCRIPTOR = msg({
	message: 'Download',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GDPR_DESCRIPTOR = msg({
	message: 'GDPR',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HARVEST_DESCRIPTOR = msg({
	message: 'Harvest',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MY_DATA_DESCRIPTOR = msg({
	message: 'My data',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCOUNT_DATA_DESCRIPTOR = msg({
	message: 'Account data',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BULK_DELETE_DESCRIPTOR = msg({
	message: 'Bulk delete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DELETE_MESSAGES_DESCRIPTOR = msg({
	message: 'Delete messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DELETE_ALL_MESSAGES_DESCRIPTOR = msg({
	message: 'Delete all messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PURGE_DESCRIPTOR = msg({
	message: 'Purge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PURGE_MESSAGES_DESCRIPTOR = msg({
	message: 'Purge messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_DELETION_DESCRIPTOR = msg({
	message: 'Message deletion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRIENDS_AND_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Friends & direct messages',
	context: 'privacy-settings-section',
	comment: 'Privacy settings section for who can send friend requests and direct messages.',
});
const COMMUNICATION_DESCRIPTOR = msg({
	message: 'Communication',
	context: 'privacy-settings-section',
	comment: 'Privacy settings section for calls and group chat invites.',
});
const SENSITIVE_CONTENT_DESCRIPTOR = msg({
	message: 'Sensitive content',
	context: 'privacy-settings-section',
	comment: 'Privacy settings section for sensitive media filtering.',
});
const DATA_EXPORT_DESCRIPTOR = msg({
	message: 'Data export',
	context: 'privacy-settings-section',
	comment: 'Privacy settings section for requesting an account data export.',
});
const DATA_DELETION_DESCRIPTOR = msg({
	message: 'Data deletion',
	context: 'privacy-settings-section',
	comment: 'Privacy settings section for deleting account data.',
});
const ACTIVITY_SHARING_DESCRIPTOR = msg({
	message: 'Activity sharing',
	comment: 'Privacy settings section for sharing voice activity with friends in active now.',
});
const ACTIVE_NOW_DESCRIPTOR = msg({
	message: 'Active now',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHAT_FRIENDS_SEE_ON_THEIR_ACTIVE_NOW_PANEL_DESCRIPTOR = msg({
	message: 'Choose what friends see in their active now panel',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'Voice activity',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
export const privacySafetySections = [
	{
		id: 'profile-privacy',
		tabType: 'privacy_safety',
		label: PROFILE_PRIVACY_DESCRIPTOR,
		description: CHOOSE_WHO_CAN_SEE_YOUR_PROFILE_DETAILS_DESCRIPTOR,
		keywords: [
			PROFILE_PRIVACY_DESCRIPTOR,
			PRIVACY_DESCRIPTOR,
			BIO_DESCRIPTOR,
			PRONOUNS_DESCRIPTOR,
			FRIENDS_DESCRIPTOR,
			COMMUNITY_DESCRIPTOR,
			COMMUNITY_MEMBERS_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'connections',
		tabType: 'privacy_safety',
		label: FRIENDS_AND_DIRECT_MESSAGES_DESCRIPTOR,
		keywords: [
			FRIEND_REQUESTS_DESCRIPTOR,
			FRIEND_REQUEST_DESCRIPTOR,
			WHO_CAN_FRIEND_DESCRIPTOR,
			FRIENDS_DESCRIPTOR,
			ADD_FRIEND_DESCRIPTOR,
			FRIENDS_OF_FRIENDS_DESCRIPTOR,
			MUTUAL_FRIENDS_DESCRIPTOR,
			COMMUNITY_MEMBERS_DESCRIPTOR,
			MUTUAL_GUILDS_DESCRIPTOR,
			DIRECT_MESSAGES_DESCRIPTOR,
			DM_DESCRIPTOR,
			DM_PERMISSIONS_DESCRIPTOR,
			WHO_CAN_DM_DESCRIPTOR,
			BOT_DM_DESCRIPTOR,
			COMMUNITY_DM_DESCRIPTOR,
			INCOMING_DM_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'communication',
		tabType: 'privacy_safety',
		label: COMMUNICATION_DESCRIPTOR,
		keywords: [
			DM_DESCRIPTOR,
			DIRECT_MESSAGE_DESCRIPTOR,
			PRIVACY_DESCRIPTOR,
			WHO_CAN_DM_DESCRIPTOR,
			FRIENDS_DESCRIPTOR,
			REQUESTS_DESCRIPTOR,
			FRIEND_REQUESTS_DESCRIPTOR,
			INCOMING_CALLS_DESCRIPTOR,
			WHO_CAN_CALL_DESCRIPTOR,
			CALL_PERMISSIONS_DESCRIPTOR,
			BLOCK_CALLS_DESCRIPTOR,
			FRIENDS_ONLY_CALLS_DESCRIPTOR,
			CALL_PRIVACY_DESCRIPTOR,
			SILENT_DESCRIPTOR,
			SILENT_CALLS_DESCRIPTOR,
			RING_BEHAVIOR_DESCRIPTOR,
			MUTE_CALLS_DESCRIPTOR,
			NO_RING_DESCRIPTOR,
			GROUP_CHAT_DESCRIPTOR,
			GROUP_DM_DESCRIPTOR,
			ADD_TO_GROUP_DESCRIPTOR,
			WHO_CAN_ADD_DESCRIPTOR,
			GROUP_PERMISSIONS_DESCRIPTOR,
			GROUP_PRIVACY_DESCRIPTOR,
			FRIENDS_OF_FRIENDS_DESCRIPTOR,
			MUTUAL_FRIENDS_DESCRIPTOR,
			EXTENDED_NETWORK_DESCRIPTOR,
			COMMUNITY_DESCRIPTOR,
			GUILD_MEMBERS_DESCRIPTOR,
			COMMUNITY_MEMBERS_DESCRIPTOR,
			COMMUNITY_MEMBERS_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'active-now',
		tabType: 'privacy_safety',
		label: ACTIVITY_SHARING_DESCRIPTOR,
		description: CHOOSE_WHAT_FRIENDS_SEE_ON_THEIR_ACTIVE_NOW_PANEL_DESCRIPTOR,
		keywords: [
			ACTIVE_NOW_DESCRIPTOR,
			VOICE_ACTIVITY_DESCRIPTOR,
			VOICE_DESCRIPTOR,
			FRIENDS_DESCRIPTOR,
			PRIVACY_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'sensitive-content',
		tabType: 'privacy_safety',
		label: SENSITIVE_CONTENT_DESCRIPTOR,
		description: CONTROL_HOW_MATURE_OR_SENSITIVE_MEDIA_IS_FILTERED_DESCRIPTOR,
		keywords: [
			SENSITIVE_CONTENT_2_DESCRIPTOR,
			SENSITIVE_MEDIA_DESCRIPTOR,
			MATURE_CONTENT_DESCRIPTOR,
			MATURE_MEDIA_DESCRIPTOR,
			FILTER_DESCRIPTOR,
			FILTERING_DESCRIPTOR,
			BLUR_DESCRIPTOR,
			MEDIA_FILTER_DESCRIPTOR,
			CONTENT_FILTER_DESCRIPTOR,
			PRIVACY_DESCRIPTOR,
			SAFETY_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'data-export',
		tabType: 'privacy_safety',
		label: DATA_EXPORT_DESCRIPTOR,
		keywords: [
			EXPORT_DESCRIPTOR,
			DATA_DESCRIPTOR,
			DOWNLOAD_DESCRIPTOR,
			GDPR_DESCRIPTOR,
			HARVEST_DESCRIPTOR,
			MY_DATA_DESCRIPTOR,
			ACCOUNT_DATA_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'data-deletion',
		tabType: 'privacy_safety',
		label: DATA_DELETION_DESCRIPTOR,
		keywords: [
			DELETE_DESCRIPTOR,
			DATA_DESCRIPTOR,
			PRIVACY_DESCRIPTOR,
			GDPR_DESCRIPTOR,
			REMOVE_DESCRIPTOR,
			BULK_DELETE_DESCRIPTOR,
			DELETE_MESSAGES_DESCRIPTOR,
			DELETE_ALL_MESSAGES_DESCRIPTOR,
			PURGE_DESCRIPTOR,
			PURGE_MESSAGES_DESCRIPTOR,
			MESSAGE_DELETION_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
