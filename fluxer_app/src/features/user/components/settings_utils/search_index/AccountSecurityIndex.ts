// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DELETE_ACCOUNT_DESCRIPTOR,
	DISABLE_ACCOUNT_DESCRIPTOR,
	EMAIL_DESCRIPTOR,
	PASSWORD_DESCRIPTOR,
	TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {shouldShowClaimedAccountSettings} from '@app/features/user/components/settings_utils/search_index/SearchIndexHelpers';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	CODES_DESCRIPTOR,
	LOGIN_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const MAIL_DESCRIPTOR = msg({
	message: 'Mail',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADDRESS_DESCRIPTOR = msg({
	message: 'Address',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTACT_DESCRIPTOR = msg({
	message: 'Contact',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_YOUR_EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'Change your email address',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CREDENTIALS_DESCRIPTOR = msg({
	message: 'Credentials',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SECURITY_DESCRIPTOR = msg({
	message: 'Security',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_PASSWORD_DESCRIPTOR = msg({
	message: 'Change password',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Change your password',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const TWO_FACTOR_DESCRIPTOR = msg({
	message: 'Two-factor',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MULTI_FACTOR_DESCRIPTOR = msg({
	message: 'Multi-factor',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTHENTICATION_DESCRIPTOR = msg({
	message: 'Authentication',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTHENTICATOR_DESCRIPTOR = msg({
	message: 'Authenticator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ONE_TIME_PASSWORD_DESCRIPTOR = msg({
	message: 'One-time password',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENABLE_TWO_FACTOR_AUTHENTICATION_DESCRIPTOR = msg({
	message: 'Enable two-factor authentication',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const BACKUP_CODES_DESCRIPTOR = msg({
	message: 'Backup codes',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const RECOVERY_DESCRIPTOR = msg({
	message: 'Recovery',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWO_FACTOR_BACKUP_DESCRIPTOR = msg({
	message: 'Two-factor backup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIEW_OR_REGENERATE_BACKUP_CODES_DESCRIPTOR = msg({
	message: 'View or regenerate backup codes',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PASSKEYS_DESCRIPTOR = msg({
	message: 'Passkeys',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const PASSKEY_DESCRIPTOR = msg({
	message: 'Passkey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WEB_AUTHENTICATION_DESCRIPTOR = msg({
	message: 'Web authentication',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PASSWORDLESS_DESCRIPTOR = msg({
	message: 'Passwordless',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FACE_RECOGNITION_DESCRIPTOR = msg({
	message: 'Face recognition',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BIOMETRIC_DESCRIPTOR = msg({
	message: 'Biometric',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BIOMETRICS_DESCRIPTOR = msg({
	message: 'Biometrics',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FINGERPRINT_DESCRIPTOR = msg({
	message: 'Fingerprint',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SECURITY_KEY_DESCRIPTOR = msg({
	message: 'Security key',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HARDWARE_KEY_DESCRIPTOR = msg({
	message: 'Hardware key',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADD_PASSKEY_DESCRIPTOR = msg({
	message: 'Add passkey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const USE_PASSKEYS_TO_SIGN_IN_WITHOUT_A_PASSWORD_DESCRIPTOR = msg({
	message: 'Use passkeys to sign in without a password',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PASSKEY_TWO_FACTOR_DESCRIPTOR = msg({
	message: 'Require a passkey as your second factor',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const SECOND_FACTOR_DESCRIPTOR = msg({
	message: 'Second factor',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REQUIRE_PASSKEY_DESCRIPTOR = msg({
	message: 'Require passkey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWO_FA_DESCRIPTOR = msg({
	message: '2FA',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ASK_FOR_A_PASSKEY_AFTER_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Ask for a passkey after your password when you sign in',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const DISABLE_DESCRIPTOR = msg({
	message: 'Disable',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DEACTIVATE_DESCRIPTOR = msg({
	message: 'Deactivate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SUSPEND_DESCRIPTOR = msg({
	message: 'Suspend',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEMPORARILY_DISABLE_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Temporarily disable your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CLOSE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Close account',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DELETE_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Delete your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const accountSecurityIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'account-email',
		tabType: 'account_security',
		sectionId: 'account',
		label: EMAIL_DESCRIPTOR,
		keywords: [EMAIL_DESCRIPTOR, MAIL_DESCRIPTOR, ADDRESS_DESCRIPTOR, CONTACT_DESCRIPTOR],
		description: CHANGE_YOUR_EMAIL_ADDRESS_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-password',
		tabType: 'account_security',
		sectionId: 'account',
		label: PASSWORD_DESCRIPTOR,
		keywords: [
			PASSWORD_DESCRIPTOR,
			CREDENTIALS_DESCRIPTOR,
			LOGIN_DESCRIPTOR,
			SECURITY_DESCRIPTOR,
			CHANGE_PASSWORD_DESCRIPTOR,
		],
		description: CHANGE_YOUR_PASSWORD_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-2fa',
		tabType: 'account_security',
		sectionId: 'security',
		label: TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
		keywords: [
			TWO_FACTOR_DESCRIPTOR,
			MULTI_FACTOR_DESCRIPTOR,
			AUTHENTICATION_DESCRIPTOR,
			AUTHENTICATOR_DESCRIPTOR,
			ONE_TIME_PASSWORD_DESCRIPTOR,
			SECURITY_DESCRIPTOR,
		],
		description: ENABLE_TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-backup-codes',
		tabType: 'account_security',
		sectionId: 'security',
		label: BACKUP_CODES_DESCRIPTOR,
		keywords: [BACKUP_CODES_DESCRIPTOR, RECOVERY_DESCRIPTOR, CODES_DESCRIPTOR, TWO_FACTOR_BACKUP_DESCRIPTOR],
		description: VIEW_OR_REGENERATE_BACKUP_CODES_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-passkeys',
		tabType: 'account_security',
		sectionId: 'security',
		label: PASSKEYS_DESCRIPTOR,
		keywords: [
			PASSKEY_DESCRIPTOR,
			PASSKEYS_DESCRIPTOR,
			WEB_AUTHENTICATION_DESCRIPTOR,
			PASSWORDLESS_DESCRIPTOR,
			FACE_RECOGNITION_DESCRIPTOR,
			BIOMETRIC_DESCRIPTOR,
			BIOMETRICS_DESCRIPTOR,
			FINGERPRINT_DESCRIPTOR,
			SECURITY_KEY_DESCRIPTOR,
			HARDWARE_KEY_DESCRIPTOR,
			ADD_PASSKEY_DESCRIPTOR,
		],
		description: USE_PASSKEYS_TO_SIGN_IN_WITHOUT_A_PASSWORD_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-passkey-two-factor',
		tabType: 'account_security',
		sectionId: 'security',
		label: PASSKEY_TWO_FACTOR_DESCRIPTOR,
		keywords: [
			PASSKEY_DESCRIPTOR,
			PASSKEYS_DESCRIPTOR,
			TWO_FACTOR_DESCRIPTOR,
			SECOND_FACTOR_DESCRIPTOR,
			REQUIRE_PASSKEY_DESCRIPTOR,
			TWO_FA_DESCRIPTOR,
			MULTI_FACTOR_DESCRIPTOR,
			AUTHENTICATION_DESCRIPTOR,
			SECURITY_DESCRIPTOR,
		],
		description: ASK_FOR_A_PASSKEY_AFTER_YOUR_PASSWORD_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-disable',
		tabType: 'account_security',
		sectionId: 'danger_zone',
		label: DISABLE_ACCOUNT_DESCRIPTOR,
		keywords: [DISABLE_DESCRIPTOR, DEACTIVATE_DESCRIPTOR, SUSPEND_DESCRIPTOR],
		description: TEMPORARILY_DISABLE_YOUR_ACCOUNT_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'account-delete',
		tabType: 'account_security',
		sectionId: 'danger_zone',
		label: DELETE_ACCOUNT_DESCRIPTOR,
		keywords: [DELETE_DESCRIPTOR, REMOVE_DESCRIPTOR, CLOSE_ACCOUNT_DESCRIPTOR, DEACTIVATE_DESCRIPTOR],
		description: DELETE_YOUR_ACCOUNT_DESCRIPTOR,
	},
];
