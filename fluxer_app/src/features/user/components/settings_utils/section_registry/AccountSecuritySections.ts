// SPDX-License-Identifier: AGPL-3.0-or-later

import {EMAIL_DESCRIPTOR, PASSWORD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {shouldShowClaimedAccountSections} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryHelpers';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	DELETE_DESCRIPTOR,
	REMOVE_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
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
const CREDENTIALS_DESCRIPTOR = msg({
	message: 'Credentials',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOGIN_DESCRIPTOR = msg({
	message: 'Login',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SECURITY_2_DESCRIPTOR = msg({
	message: 'Security',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_PASSWORD_DESCRIPTOR = msg({
	message: 'Change password',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_2FA_DESCRIPTOR = msg({
	message: '2FA',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWO_FACTOR_DESCRIPTOR = msg({
	message: 'Two-factor',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MFA_DESCRIPTOR = msg({
	message: 'MFA',
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
const TOTP_DESCRIPTOR = msg({
	message: 'TOTP',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OTP_DESCRIPTOR = msg({
	message: 'OTP',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BACKUP_CODES_DESCRIPTOR = msg({
	message: 'Backup codes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RECOVERY_DESCRIPTOR = msg({
	message: 'Recovery',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CODES_DESCRIPTOR = msg({
	message: 'Codes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_2FA_BACKUP_DESCRIPTOR = msg({
	message: '2FA backup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PASSKEY_DESCRIPTOR = msg({
	message: 'Passkey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PASSKEYS_DESCRIPTOR = msg({
	message: 'Passkeys',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WEBAUTHN_DESCRIPTOR = msg({
	message: 'WebAuthn',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PASSWORDLESS_DESCRIPTOR = msg({
	message: 'Passwordless',
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
const YUBIKEY_DESCRIPTOR = msg({
	message: 'YubiKey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CLOSE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Close account',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCOUNT_REMOVAL_DESCRIPTOR = msg({
	message: 'Account removal',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PERMANENT_DELETION_DESCRIPTOR = msg({
	message: 'Permanent deletion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCOUNT_DESCRIPTOR = msg({
	message: 'Account',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SIGN_IN_DETAILS_DESCRIPTOR = msg({
	message: 'Sign-in details',
	context: 'security-settings-section',
	comment: 'Account and security settings section for email address and password controls.',
});
const SECURITY_DESCRIPTOR = msg({
	message: 'Security',
	context: 'security-settings-section',
	comment: 'Account and security settings section for password, MFA, and sessions.',
});
const DANGER_ZONE_DESCRIPTOR = msg({
	message: 'Danger zone',
	context: 'security-settings-section',
	comment: 'Account and security settings section for destructive account actions.',
});
const AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: 'Authorized apps',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BLOCKED_USERS_DESCRIPTOR = msg({
	message: 'Blocked users',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINKED_DEVICES_DESCRIPTOR = msg({
	message: 'Linked devices',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
export const accountSecuritySections = [
	{
		id: 'account',
		tabType: 'account_security',
		label: SIGN_IN_DETAILS_DESCRIPTOR,
		keywords: [
			ACCOUNT_DESCRIPTOR,
			EMAIL_DESCRIPTOR,
			MAIL_DESCRIPTOR,
			ADDRESS_DESCRIPTOR,
			CONTACT_DESCRIPTOR,
			PASSWORD_DESCRIPTOR,
			CREDENTIALS_DESCRIPTOR,
			LOGIN_DESCRIPTOR,
			CHANGE_PASSWORD_DESCRIPTOR,
		],
		isAdvanced: false,
		isVisible: shouldShowClaimedAccountSections,
	},
	{
		id: 'security',
		tabType: 'account_security',
		label: SECURITY_DESCRIPTOR,
		keywords: [
			PASSWORD_DESCRIPTOR,
			CREDENTIALS_DESCRIPTOR,
			LOGIN_DESCRIPTOR,
			SECURITY_2_DESCRIPTOR,
			CHANGE_PASSWORD_DESCRIPTOR,
			MESSAGE_2FA_DESCRIPTOR,
			TWO_FACTOR_DESCRIPTOR,
			MFA_DESCRIPTOR,
			AUTHENTICATION_DESCRIPTOR,
			AUTHENTICATOR_DESCRIPTOR,
			TOTP_DESCRIPTOR,
			OTP_DESCRIPTOR,
			BACKUP_CODES_DESCRIPTOR,
			RECOVERY_DESCRIPTOR,
			CODES_DESCRIPTOR,
			MESSAGE_2FA_BACKUP_DESCRIPTOR,
			PASSKEY_DESCRIPTOR,
			PASSKEYS_DESCRIPTOR,
			WEBAUTHN_DESCRIPTOR,
			PASSWORDLESS_DESCRIPTOR,
			'Face ID',
			'Touch ID',
			BIOMETRIC_DESCRIPTOR,
			BIOMETRICS_DESCRIPTOR,
			FINGERPRINT_DESCRIPTOR,
			SECURITY_KEY_DESCRIPTOR,
			YUBIKEY_DESCRIPTOR,
			AUTHORIZED_APPS_DESCRIPTOR,
			LINKED_DEVICES_DESCRIPTOR,
		],
		isAdvanced: false,
		isVisible: shouldShowClaimedAccountSections,
	},
	{
		id: 'danger_zone',
		tabType: 'account_security',
		label: DANGER_ZONE_DESCRIPTOR,
		keywords: [
			DELETE_DESCRIPTOR,
			REMOVE_DESCRIPTOR,
			CLOSE_ACCOUNT_DESCRIPTOR,
			ACCOUNT_REMOVAL_DESCRIPTOR,
			PERMANENT_DELETION_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'blocked_users',
		tabType: 'account_security',
		label: BLOCKED_USERS_DESCRIPTOR,
		keywords: [BLOCKED_USERS_DESCRIPTOR],
		isAdvanced: false,
		isVisible: shouldShowClaimedAccountSections,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
