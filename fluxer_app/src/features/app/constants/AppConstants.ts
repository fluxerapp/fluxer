// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	IDLE_DESCRIPTOR,
	OFFLINE_DESCRIPTOR,
	ONLINE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {OAuth2Scope} from '@fluxer/constants/src/OAuth2Constants';
import {isStatusType, normalizeStatus, type StatusType, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ACCESS_YOUR_BASIC_PROFILE_INFORMATION_USERNAME_AVATAR_ETC_DESCRIPTOR = msg({
	message: 'Access your basic profile information (username, avatar, etc.)',
	comment: 'OAuth scope description shown on the consent screen for the identify scope.',
});
const VIEW_YOUR_EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'View your email address',
	comment: 'Short label in the app constants.',
});
const VIEW_THE_COMMUNITIES_YOU_ARE_A_MEMBER_OF_DESCRIPTOR = msg({
	message: 'View the communities you are a member of',
	comment: 'Short label in the app constants.',
});
const VIEW_YOUR_CONNECTED_ACCOUNTS_DESCRIPTOR = msg({
	message: 'View your connected accounts',
	comment: 'Short label in the app constants.',
});
const ADD_A_BOT_TO_A_COMMUNITY_WITH_REQUESTED_DESCRIPTOR = msg({
	message: 'Add a bot to a community with requested permissions',
	comment: 'OAuth scope description shown on the consent screen for the bot scope.',
});
const DO_NOT_DISTURB_DESCRIPTOR = msg({
	message: 'Do not disturb',
	comment: 'Short label in the app constants.',
});
const INVISIBLE_DESCRIPTOR = msg({
	message: 'Invisible',
	comment: 'Short label in the app constants.',
});
export const STATUS_UNTIL_I_CHANGE_IT_DESCRIPTOR = msg({
	message: 'Until I change it',
	comment: 'Status duration option meaning the selected presence stays active until the user changes it manually.',
});
const OAuth2ScopeDescriptorsInternal: Record<OAuth2Scope, MessageDescriptor> = {
	identify: ACCESS_YOUR_BASIC_PROFILE_INFORMATION_USERNAME_AVATAR_ETC_DESCRIPTOR,
	email: VIEW_YOUR_EMAIL_ADDRESS_DESCRIPTOR,
	guilds: VIEW_THE_COMMUNITIES_YOU_ARE_A_MEMBER_OF_DESCRIPTOR,
	connections: VIEW_YOUR_CONNECTED_ACCOUNTS_DESCRIPTOR,
	bot: ADD_A_BOT_TO_A_COMMUNITY_WITH_REQUESTED_DESCRIPTOR,
};

export function getOAuth2ScopeDescription(i18n: I18n, scope: OAuth2Scope | string): string | undefined {
	const descriptor = OAuth2ScopeDescriptorsInternal[scope as OAuth2Scope];
	return descriptor ? i18n._(descriptor) : undefined;
}

const StatusTypeToLabelDescriptorsInternal: Record<StatusType, MessageDescriptor> = {
	[StatusTypes.ONLINE]: ONLINE_DESCRIPTOR,
	[StatusTypes.DND]: DO_NOT_DISTURB_DESCRIPTOR,
	[StatusTypes.IDLE]: IDLE_DESCRIPTOR,
	[StatusTypes.INVISIBLE]: INVISIBLE_DESCRIPTOR,
	[StatusTypes.OFFLINE]: OFFLINE_DESCRIPTOR,
};

export function getStatusTypeLabel(i18n: I18n, statusType: StatusType | string): string {
	const normalized = isStatusType(statusType) ? statusType : normalizeStatus(statusType);
	return i18n._(StatusTypeToLabelDescriptorsInternal[normalized]);
}
