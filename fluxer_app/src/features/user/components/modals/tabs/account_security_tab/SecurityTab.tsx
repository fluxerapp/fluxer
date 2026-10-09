// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {BackupCodesModal} from '@app/features/auth/components/modals/BackupCodesModal';
import {BackupCodesViewModal} from '@app/features/auth/components/modals/BackupCodesViewModal';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {MfaTotpDisableModal} from '@app/features/auth/components/modals/MfaTotpDisableModal';
import {MfaTotpEnableModal} from '@app/features/auth/components/modals/MfaTotpEnableModal';
import {PasskeyNameModal} from '@app/features/auth/components/modals/PasskeyNameModal';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {
	CLAIM_ACCOUNT_DESCRIPTOR,
	TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import {
	AccountSecurityCard,
	AccountSecurityEmptyRow,
	AccountSecurityRow,
	AccountSecuritySwitchRow,
} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import styles from '@app/features/user/components/modals/tabs/account_security_tab/SecurityTab.module.css';
import type {User} from '@app/features/user/models/User';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {pushApiErrorModal} from '@app/lib/forms';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DELETE_PASSKEY_DESCRIPTOR = msg({
	message: 'Delete passkey',
	comment:
		'Security settings: confirmation modal title and primary button for removing a registered passkey. Destructive action; keep plain and direct.',
});
const COULD_NOT_DELETE_PASSKEY_DESCRIPTOR = msg({
	message: "Couldn't delete passkey",
	comment: 'Title of the error modal shown when removing a registered passkey fails.',
});
const VERIFY_EMAIL_BEFORE_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Verify your email before adding an authenticator app.',
	comment:
		'Security settings warning shown when an unverified account cannot enable authenticator-app two-factor authentication.',
});
const VERIFY_EMAIL_BEFORE_PASSKEY_DESCRIPTOR = msg({
	message: 'Verify your email before adding a passkey.',
	comment: 'Security settings warning shown when an unverified account cannot register a new passkey.',
});
const PASSKEY_TWO_FACTOR_ENABLE_TITLE_DESCRIPTOR = msg({
	message: 'Use passkeys for two-factor authentication?',
	comment: 'Security settings: confirmation modal title for turning on passkey-based two-factor authentication.',
});
const PASSKEY_TWO_FACTOR_ENABLE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"You'll need one of your passkeys to sign in on a new device. If you lose every passkey you can be locked out of your account. Backup codes will be saved for you so you still have a way in.",
	comment: 'Security settings: confirmation modal body for turning on passkey-based two-factor authentication.',
});
const PASSKEY_TWO_FACTOR_ENABLE_CONFIRM_DESCRIPTOR = msg({
	message: 'Turn on',
	comment: 'Security settings: confirm button for turning on passkey-based two-factor authentication.',
});
const PASSKEY_TWO_FACTOR_DISABLE_TITLE_DESCRIPTOR = msg({
	message: 'Stop using passkeys for two-factor authentication?',
	comment: 'Security settings: confirmation modal title for turning off passkey-based two-factor authentication.',
});
const PASSKEY_TWO_FACTOR_DISABLE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Your account will no longer be protected by two-factor authentication when you sign in with your password. You'll also lose elevated permissions in servers that require two-factor authentication.",
	comment:
		'Security settings: confirmation modal body for turning off passkey-based two-factor authentication when passkeys are the only second factor on the account.',
});
const PASSKEY_TWO_FACTOR_DISABLE_DESCRIPTION_WITH_TOTP_DESCRIPTOR = msg({
	message:
		"You won't be asked for a passkey after your password anymore. Your authenticator app stays as your second factor.",
	comment:
		'Security settings: confirmation modal body for turning off passkey-based two-factor authentication when the account also has an authenticator app.',
});
const PASSKEY_TWO_FACTOR_DISABLE_CONFIRM_DESCRIPTOR = msg({
	message: 'Turn off',
	comment: 'Security settings: confirm button for turning off passkey-based two-factor authentication.',
});
const COULD_NOT_UPDATE_PASSKEY_TWO_FACTOR_DESCRIPTOR = msg({
	message: "Couldn't update passkey two-factor authentication",
	comment: 'Title of the error modal shown when the passkey two-factor setting could not be saved.',
});
const APPS_AND_DEVICES_DESCRIPTOR = msg({
	message: 'Apps and devices',
	comment: 'Security settings card title for third-party app access and signed-in devices.',
});
const TWO_FACTOR_CARD_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Ask for a second step after your password when you sign in.',
	comment: 'Security settings: description under the two-factor authentication card title.',
});
const STATUS_ON_DESCRIPTOR = msg({
	message: 'On',
	comment:
		'Security settings: status badge next to the two-factor authentication title when it is turned on. One word.',
});
const STATUS_OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment:
		'Security settings: status badge next to the two-factor authentication title when it is turned off. One word.',
});
const PASSKEY_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# passkey} other {# passkeys}}',
	comment: 'Security settings: badge next to the Passkeys title showing how many passkeys are registered.',
});
const TURN_OFF_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Turn off authenticator app',
	comment:
		'Security settings: accessible name for the button that removes authenticator-app two-factor authentication.',
});
const SET_UP_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Set up authenticator app',
	comment: 'Security settings: accessible name for the button that starts authenticator-app setup.',
});
const RENAME_PASSKEY_NAMED_DESCRIPTOR = msg({
	message: 'Rename passkey {passkeyName}',
	comment:
		'Security settings: accessible name for the rename button on a passkey row. {passkeyName} is the name the user gave the passkey.',
});
const DELETE_PASSKEY_NAMED_DESCRIPTOR = msg({
	message: 'Delete passkey {passkeyName}',
	comment:
		'Security settings: accessible name for the delete button on a passkey row. {passkeyName} is the name the user gave the passkey.',
});
const MANAGE_AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: 'Manage authorized apps',
	comment: 'Security settings: accessible name for the button that opens the authorized apps list.',
});
const MANAGE_DEVICES_DESCRIPTOR = msg({
	message: 'Manage devices',
	comment: 'Security settings: accessible name for the button that opens the signed-in devices list.',
});
const MANAGE_APPS_AND_DEVICES_WITH_ACCESS_TO_YOUR_DESCRIPTOR = msg({
	message: 'Manage apps and devices with access to your account',
	comment: 'Security settings section description for account access controls.',
});
const AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: 'Authorized apps',
	comment: 'Security settings row label for OAuth applications authorized by the user.',
});
const REVIEW_APPS_THAT_CAN_ACCESS_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Review apps that can access your account.',
	comment: 'Security settings row description for authorized apps.',
});
const LINKED_DEVICES_DESCRIPTOR = msg({
	message: 'Devices',
	comment: 'Security settings row label for signed-in devices linked to the account.',
});
const REVIEW_SIGNED_IN_DEVICES_DESCRIPTOR = msg({
	message: "Review signed-in devices and sign out of sessions you don't recognize.",
	comment: 'Security settings row description for linked devices.',
});
const logger = new Logger('SecurityTab');

interface SecurityTabProps {
	user: User;
	isClaimed: boolean;
	passkeys: ReadonlyArray<WebAuthnCredential>;
	authorizedAppsSubmitting?: boolean;
	onManageAuthorizedApps?: () => void;
	onManageLinkedDevices?: () => void;
}

export const SecurityTabContent: React.FC<SecurityTabProps> = observer(
	({user, isClaimed, passkeys, authorizedAppsSubmitting, onManageAuthorizedApps, onManageLinkedDevices}) => {
		const {i18n} = useLingui();
		const hasTotpMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.TOTP) ?? false;
		const hasPasskeyMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.WEBAUTHN) ?? false;
		const hasAnyMfa = hasTotpMfa || hasPasskeyMfa;
		const needsEmailVerification = user.email != null && user.verified === false;
		const hasReachedPasskeyLimit = passkeys.length >= 10;
		const canAddSecurityCredential = !needsEmailVerification;
		const canAddPasskey = canAddSecurityCredential && !hasReachedPasskeyLimit;
		const registerPasskey = async (name: string) => {
			try {
				const options = await UserCommands.getWebAuthnRegistrationOptions();
				const credential = await WebAuthnUtils.performRegistration(options);
				await UserCommands.registerWebAuthnCredential(credential, options.challenge, name);
			} catch (error) {
				logger.error('Failed to add passkey', error);
				throw error;
			}
		};
		const handleAddPasskey = () => {
			if (!canAddPasskey) {
				return;
			}
			ModalCommands.push(
				modal(() => (
					<PasskeyNameModal
						onSubmit={registerPasskey}
						data-flx="user.account-security-tab.security-tab.handle-add-passkey.passkey-name-modal"
					/>
				)),
			);
		};
		const handleRenamePasskey = async (credentialId: string) => {
			ModalCommands.push(
				modal(() => (
					<PasskeyNameModal
						onSubmit={async (name: string) => {
							try {
								await UserCommands.renameWebAuthnCredential(credentialId, name);
							} catch (error) {
								logger.error('Failed to rename passkey', error);
								throw error;
							}
						}}
						data-flx="user.account-security-tab.security-tab.handle-rename-passkey.passkey-name-modal"
					/>
				)),
			);
		};
		const applyPasskeyTwoFactor = async (enabled: boolean) => {
			try {
				const backupCodes = await UserCommands.setWebAuthnTwoFactor(enabled);
				if (backupCodes && backupCodes.length > 0) {
					ModalCommands.pushWithKey(
						modal(() => (
							<BackupCodesModal
								backupCodes={backupCodes}
								user={user}
								data-flx="user.account-security-tab.security-tab.apply-passkey-two-factor.backup-codes-modal"
							/>
						)),
						'backup-codes',
					);
				}
			} catch (error) {
				logger.error('Failed to update passkey two-factor authentication', error);
				pushApiErrorModal(i18n, error, i18n._(COULD_NOT_UPDATE_PASSKEY_TWO_FACTOR_DESCRIPTOR));
			}
		};
		const handleTogglePasskeyTwoFactor = (enabled: boolean) => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(
							enabled ? PASSKEY_TWO_FACTOR_ENABLE_TITLE_DESCRIPTOR : PASSKEY_TWO_FACTOR_DISABLE_TITLE_DESCRIPTOR,
						)}
						description={i18n._(
							enabled
								? PASSKEY_TWO_FACTOR_ENABLE_DESCRIPTION_DESCRIPTOR
								: hasTotpMfa
									? PASSKEY_TWO_FACTOR_DISABLE_DESCRIPTION_WITH_TOTP_DESCRIPTOR
									: PASSKEY_TWO_FACTOR_DISABLE_DESCRIPTION_DESCRIPTOR,
						)}
						primaryText={i18n._(
							enabled ? PASSKEY_TWO_FACTOR_ENABLE_CONFIRM_DESCRIPTOR : PASSKEY_TWO_FACTOR_DISABLE_CONFIRM_DESCRIPTOR,
						)}
						primaryVariant={enabled ? 'primary' : 'danger'}
						onPrimary={() => applyPasskeyTwoFactor(enabled)}
						data-flx="user.account-security-tab.security-tab.handle-toggle-passkey-two-factor.confirm-modal"
					/>
				)),
			);
		};
		const handleDeletePasskey = (credentialId: string) => {
			const passkey = passkeys.find((p) => p.id === credentialId);
			const isLastPasskeyWithMfa = hasPasskeyMfa && passkeys.length === 1;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_PASSKEY_DESCRIPTOR)}
						description={
							<div data-flx="user.account-security-tab.security-tab.handle-delete-passkey.div">
								{passkey ? (
									<Trans>
										Are you sure you want to delete the passkey{' '}
										<strong data-flx="user.account-security-tab.security-tab.handle-delete-passkey.strong">
											{passkey.name}
										</strong>
										?
									</Trans>
								) : (
									<Trans>Are you sure you want to delete this passkey?</Trans>
								)}
								{isLastPasskeyWithMfa && (
									<div
										className={styles.warningText}
										data-flx="user.account-security-tab.security-tab.handle-delete-passkey.warning-text"
									>
										{hasTotpMfa ? (
											<Trans>
												This is your last passkey. Once it's gone you won't be able to use a passkey as your second
												factor.
											</Trans>
										) : (
											<Trans>
												This is your last passkey and passkeys are your second factor. Deleting it turns off two-factor
												authentication for your account, and your current backup codes will stop working.
											</Trans>
										)}
									</div>
								)}
							</div>
						}
						primaryText={i18n._(DELETE_PASSKEY_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await UserCommands.deleteWebAuthnCredential(credentialId);
							} catch (error) {
								logger.error('Failed to delete passkey', error);
								pushApiErrorModal(i18n, error, i18n._(COULD_NOT_DELETE_PASSKEY_DESCRIPTOR));
							}
						}}
						data-flx="user.account-security-tab.security-tab.handle-delete-passkey.confirm-modal"
					/>
				)),
			);
		};
		if (!isClaimed) {
			return (
				<SettingsTabSection
					title={<Trans>Security features</Trans>}
					description={
						<Trans>Claim your account to access security features like two-factor authentication and passkeys.</Trans>
					}
					data-flx="user.account-security-tab.security-tab.security-tab-content.settings-tab-section"
				>
					<Button
						className={styles.claimButton}
						fitContent
						onClick={() => openClaimAccountModal()}
						data-flx="user.account-security-tab.security-tab.security-tab-content.claim-button.open-claim-account-modal"
					>
						{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
					</Button>
				</SettingsTabSection>
			);
		}
		const totpDescription = hasTotpMfa ? (
			<Trans>Codes from your authenticator app are your second step when you sign in.</Trans>
		) : (
			<Trans>Use an authenticator app to generate codes for two-factor authentication</Trans>
		);
		return (
			<>
				<AccountSecurityCard
					title={i18n._(TWO_FACTOR_AUTHENTICATION_DESCRIPTOR)}
					description={i18n._(TWO_FACTOR_CARD_DESCRIPTION_DESCRIPTOR)}
					status={
						hasAnyMfa
							? {label: i18n._(STATUS_ON_DESCRIPTOR), tone: 'success'}
							: {label: i18n._(STATUS_OFF_DESCRIPTOR), tone: 'muted'}
					}
					data-flx="user.account-security-tab.security-tab.two-factor-card"
				>
					<AccountSecurityRow
						label={<Trans>Authenticator app</Trans>}
						description={totpDescription}
						warning={
							needsEmailVerification && !hasTotpMfa ? i18n._(VERIFY_EMAIL_BEFORE_AUTHENTICATOR_APP_DESCRIPTOR) : null
						}
						data-flx="user.account-security-tab.security-tab.authenticator-app-row"
					>
						{hasTotpMfa ? (
							<Button
								variant="secondary"
								small={true}
								aria-label={i18n._(TURN_OFF_AUTHENTICATOR_APP_DESCRIPTOR)}
								onClick={() =>
									ModalCommands.push(
										modal(() => (
											<MfaTotpDisableModal data-flx="user.account-security-tab.security-tab.security-tab-content.mfa-totp-disable-modal" />
										)),
									)
								}
								data-flx="user.account-security-tab.security-tab.security-tab-content.button.push"
							>
								<Trans>Turn off</Trans>
							</Button>
						) : (
							<Button
								small={true}
								disabled={!canAddSecurityCredential}
								aria-label={i18n._(SET_UP_AUTHENTICATOR_APP_DESCRIPTOR)}
								onClick={() =>
									ModalCommands.push(
										modal(() => (
											<MfaTotpEnableModal
												user={user}
												data-flx="user.account-security-tab.security-tab.security-tab-content.mfa-totp-enable-modal"
											/>
										)),
									)
								}
								data-flx="user.account-security-tab.security-tab.security-tab-content.button.push--2"
							>
								<Trans>Set up</Trans>
							</Button>
						)}
					</AccountSecurityRow>
					{hasAnyMfa && (
						<AccountSecurityRow
							label={<Trans>Backup codes</Trans>}
							description={<Trans>View and manage your backup codes for account recovery</Trans>}
							data-flx="user.account-security-tab.security-tab.backup-codes-row"
						>
							<Button
								variant="secondary"
								small={true}
								onClick={() =>
									ModalCommands.push(
										modal(() => (
											<BackupCodesViewModal
												user={user}
												data-flx="user.account-security-tab.security-tab.security-tab-content.backup-codes-view-modal"
											/>
										)),
									)
								}
								data-flx="user.account-security-tab.security-tab.security-tab-content.button.push--3"
							>
								<Trans>View codes</Trans>
							</Button>
						</AccountSecurityRow>
					)}
				</AccountSecurityCard>
				<AccountSecurityCard
					title={<Trans>Passkeys</Trans>}
					description={<Trans>Use passkeys to sign in without a password</Trans>}
					status={
						passkeys.length > 0
							? {
									label: i18n._(PASSKEY_COUNT_DESCRIPTOR, {count: passkeys.length}),
									tone: 'neutral',
								}
							: undefined
					}
					action={
						<Button
							small={true}
							variant={passkeys.length > 0 ? 'secondary' : 'primary'}
							disabled={!canAddPasskey}
							onClick={handleAddPasskey}
							data-flx="user.account-security-tab.security-tab.security-tab-content.button.add-passkey"
						>
							<Trans>Add passkey</Trans>
						</Button>
					}
					data-flx="user.account-security-tab.security-tab.passkeys-card"
				>
					{needsEmailVerification && (
						<AccountSecurityEmptyRow data-flx="user.account-security-tab.security-tab.passkeys-verify-email">
							{i18n._(VERIFY_EMAIL_BEFORE_PASSKEY_DESCRIPTOR)}
						</AccountSecurityEmptyRow>
					)}
					{passkeys.length === 0 && !needsEmailVerification && (
						<AccountSecurityEmptyRow data-flx="user.account-security-tab.security-tab.passkeys-empty">
							<Trans>No passkeys yet. Add one to sign in with your fingerprint, face or a security key.</Trans>
						</AccountSecurityEmptyRow>
					)}
					{passkeys.map((passkey) => {
						const createdDate = DateUtils.getRelativeDateString(new Date(passkey.created_at), i18n);
						const lastUsedDate = passkey.last_used_at
							? DateUtils.getRelativeDateString(new Date(passkey.last_used_at), i18n)
							: null;
						const passkeyName = passkey.name;
						return (
							<AccountSecurityRow
								key={passkey.id}
								label={passkeyName}
								description={
									lastUsedDate ? (
										<Trans>
											Added {createdDate}, last used {lastUsedDate}
										</Trans>
									) : (
										<Trans>Added {createdDate}, never used</Trans>
									)
								}
								data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-item"
							>
								<Button
									variant="secondary"
									small={true}
									aria-label={i18n._(RENAME_PASSKEY_NAMED_DESCRIPTOR, {passkeyName})}
									onClick={() => handleRenamePasskey(passkey.id)}
									data-flx="user.account-security-tab.security-tab.security-tab-content.button.rename-passkey"
								>
									<Trans>Rename</Trans>
								</Button>
								<Button
									variant="ghost"
									small={true}
									aria-label={i18n._(DELETE_PASSKEY_NAMED_DESCRIPTOR, {passkeyName})}
									onClick={() => handleDeletePasskey(passkey.id)}
									data-flx="user.account-security-tab.security-tab.security-tab-content.button.delete-passkey"
								>
									<Trans>Delete</Trans>
								</Button>
							</AccountSecurityRow>
						);
					})}
					{passkeys.length > 0 && (
						<AccountSecuritySwitchRow data-flx="user.account-security-tab.security-tab.passkey-two-factor-row">
							<Switch
								label={<Trans>Require a passkey as your second factor</Trans>}
								description={<Trans>Ask for a passkey after your password when you sign in</Trans>}
								value={hasPasskeyMfa}
								onChange={handleTogglePasskeyTwoFactor}
								data-flx="user.account-security-tab.security-tab.security-tab-content.switch.passkey-two-factor"
							/>
						</AccountSecuritySwitchRow>
					)}
				</AccountSecurityCard>
				{(onManageAuthorizedApps || onManageLinkedDevices) && (
					<AccountSecurityCard
						title={i18n._(APPS_AND_DEVICES_DESCRIPTOR)}
						description={i18n._(MANAGE_APPS_AND_DEVICES_WITH_ACCESS_TO_YOUR_DESCRIPTOR)}
						data-flx="user.account-security-tab.security-tab.security-tab-content.account-access"
					>
						{onManageAuthorizedApps && (
							<AccountSecurityRow
								label={i18n._(AUTHORIZED_APPS_DESCRIPTOR)}
								description={i18n._(REVIEW_APPS_THAT_CAN_ACCESS_YOUR_ACCOUNT_DESCRIPTOR)}
								data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.authorized-apps-row"
							>
								<Button
									variant="secondary"
									small={true}
									submitting={authorizedAppsSubmitting}
									aria-label={i18n._(MANAGE_AUTHORIZED_APPS_DESCRIPTOR)}
									onClick={onManageAuthorizedApps}
									data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.button.manage-authorized-apps"
								>
									<Trans>Manage</Trans>
								</Button>
							</AccountSecurityRow>
						)}
						{onManageLinkedDevices && (
							<AccountSecurityRow
								label={i18n._(LINKED_DEVICES_DESCRIPTOR)}
								description={i18n._(REVIEW_SIGNED_IN_DEVICES_DESCRIPTOR)}
								data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-row"
							>
								<Button
									variant="secondary"
									small={true}
									aria-label={i18n._(MANAGE_DEVICES_DESCRIPTOR)}
									onClick={onManageLinkedDevices}
									data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.button.manage-linked-devices"
								>
									<Trans>Manage</Trans>
								</Button>
							</AccountSecurityRow>
						)}
					</AccountSecurityCard>
				)}
			</>
		);
	},
);
