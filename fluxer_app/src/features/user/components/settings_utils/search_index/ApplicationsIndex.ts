// SPDX-License-Identifier: AGPL-3.0-or-later

import {SCOPES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	APPLICATIONS_DESCRIPTOR,
	AUTHORIZATION_DESCRIPTOR,
	DEVELOPER_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const APPLICATIONS_BOTS_DESCRIPTOR = msg({
	message: 'Applications & bots',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const BOTS_DESCRIPTOR = msg({
	message: 'Bots',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INTERFACE_DESCRIPTOR = msg({
	message: 'Interface',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CREATE_APPLICATION_DESCRIPTOR = msg({
	message: 'Create application',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOCUMENTATION_DESCRIPTOR = msg({
	message: 'Documentation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CREATE_AND_MANAGE_APPLICATIONS_AND_BOTS_FOR_YOUR_DESCRIPTOR = msg({
	message: 'Create and manage applications and bots for your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const APPLICATION_INFORMATION_DESCRIPTOR = msg({
	message: 'Application information',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const APPLICATION_NAME_DESCRIPTOR = msg({
	message: 'Application name',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PUBLIC_BOT_DESCRIPTOR = msg({
	message: 'Public bot',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REDIRECT_ADDRESS_DESCRIPTOR = msg({
	message: 'Redirect address',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTHORIZATION_CODE_GRANT_DESCRIPTOR = msg({
	message: 'Authorization code grant',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALLOWED_REDIRECTS_DESCRIPTOR = msg({
	message: 'Allowed redirects',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EDIT_APPLICATION_BASICS_AND_REDIRECT_URIS_DESCRIPTOR = msg({
	message: 'Edit application basics and redirect URIs',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const BOT_PROFILE_DESCRIPTOR = msg({
	message: 'Bot profile',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const BOT_AVATAR_DESCRIPTOR = msg({
	message: 'Bot avatar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_USERNAME_DESCRIPTOR = msg({
	message: 'Bot username',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_BIOGRAPHY_DESCRIPTOR = msg({
	message: 'Bot biography',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_BANNER_DESCRIPTOR = msg({
	message: 'Bot banner',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRIENDLY_BOT_DESCRIPTOR = msg({
	message: 'Friendly bot',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EDIT_THE_BOT_AVATAR_TAG_BIO_BANNER_AND_DESCRIPTOR = msg({
	message: 'Edit the bot avatar, tag, bio, banner, and friend request behavior',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const OAUTH2_URL_BUILDER_DESCRIPTOR = msg({
	message: 'OAuth2 URL builder',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const LINK_BUILDER_DESCRIPTOR = msg({
	message: 'Link builder',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTHORIZE_LINK_DESCRIPTOR = msg({
	message: 'Authorize link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Bot permissions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_AUTHORIZE_LINK_DESCRIPTOR = msg({
	message: 'Copy authorization link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BUILD_AN_AUTHORIZATION_URL_WITH_SCOPES_REDIRECTS_AND_DESCRIPTOR = msg({
	message: 'Build an authorization URL with scopes, redirects, and bot permissions',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SECRETS_TOKENS_DESCRIPTOR = msg({
	message: 'Secrets & tokens',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const CLIENT_SECRET_DESCRIPTOR = msg({
	message: 'Client secret',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOT_TOKEN_DESCRIPTOR = msg({
	message: 'Bot token',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REGENERATE_DESCRIPTOR = msg({
	message: 'Regenerate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOKEN_DESCRIPTOR = msg({
	message: 'Token',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SECRET_DESCRIPTOR = msg({
	message: 'Secret',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIEW_AND_REGENERATE_CLIENT_SECRETS_AND_BOT_TOKENS_DESCRIPTOR = msg({
	message: 'View and regenerate client secrets and bot tokens',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const applicationsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'applications-dev',
		tabType: 'applications',
		label: APPLICATIONS_BOTS_DESCRIPTOR,
		keywords: [
			APPLICATIONS_DESCRIPTOR,
			BOTS_DESCRIPTOR,
			DEVELOPER_DESCRIPTOR,
			INTERFACE_DESCRIPTOR,
			AUTHORIZATION_DESCRIPTOR,
			CREATE_APPLICATION_DESCRIPTOR,
			DOCUMENTATION_DESCRIPTOR,
		],
		description: CREATE_AND_MANAGE_APPLICATIONS_AND_BOTS_FOR_YOUR_DESCRIPTOR,
	},
	{
		id: 'applications-info',
		tabType: 'applications',
		label: APPLICATION_INFORMATION_DESCRIPTOR,
		keywords: [
			APPLICATION_NAME_DESCRIPTOR,
			PUBLIC_BOT_DESCRIPTOR,
			REDIRECT_ADDRESS_DESCRIPTOR,
			AUTHORIZATION_CODE_GRANT_DESCRIPTOR,
			ALLOWED_REDIRECTS_DESCRIPTOR,
		],
		description: EDIT_APPLICATION_BASICS_AND_REDIRECT_URIS_DESCRIPTOR,
	},
	{
		id: 'applications-bot-profile',
		tabType: 'applications',
		label: BOT_PROFILE_DESCRIPTOR,
		keywords: [
			BOT_PROFILE_DESCRIPTOR,
			BOT_AVATAR_DESCRIPTOR,
			BOT_USERNAME_DESCRIPTOR,
			BOT_BIOGRAPHY_DESCRIPTOR,
			BOT_BANNER_DESCRIPTOR,
			FRIENDLY_BOT_DESCRIPTOR,
		],
		description: EDIT_THE_BOT_AVATAR_TAG_BIO_BANNER_AND_DESCRIPTOR,
	},
	{
		id: 'applications-oauth-builder',
		tabType: 'applications',
		label: OAUTH2_URL_BUILDER_DESCRIPTOR,
		keywords: [
			AUTHORIZATION_DESCRIPTOR,
			LINK_BUILDER_DESCRIPTOR,
			AUTHORIZE_LINK_DESCRIPTOR,
			SCOPES_DESCRIPTOR,
			BOT_PERMISSIONS_DESCRIPTOR,
			COPY_AUTHORIZE_LINK_DESCRIPTOR,
		],
		description: BUILD_AN_AUTHORIZATION_URL_WITH_SCOPES_REDIRECTS_AND_DESCRIPTOR,
	},
	{
		id: 'applications-secrets',
		tabType: 'applications',
		label: SECRETS_TOKENS_DESCRIPTOR,
		keywords: [
			CLIENT_SECRET_DESCRIPTOR,
			BOT_TOKEN_DESCRIPTOR,
			REGENERATE_DESCRIPTOR,
			TOKEN_DESCRIPTOR,
			SECRET_DESCRIPTOR,
		],
		description: VIEW_AND_REGENERATE_CLIENT_SECRETS_AND_BOT_TOKENS_DESCRIPTOR,
	},
];
