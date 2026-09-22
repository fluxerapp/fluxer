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
const ACCOUNT_ACCESS_DESCRIPTOR = msg({
	message: 'Account access',
	comment: 'Security settings section for third-party app access and signed-in devices.',
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
		return (
			<>
				<SettingsTabSection
					title={i18n._(TWO_FACTOR_AUTHENTICATION_DESCRIPTOR)}
					description={<Trans>Add an extra layer of security to your account</Trans>}
					data-flx="user.account-security-tab.security-tab.security-tab-content.settings-tab-section--2"
				>
					<div className={styles.row} data-flx="user.account-security-tab.security-tab.security-tab-content.row">
						<div
							className={styles.rowContent}
							data-flx="user.account-security-tab.security-tab.security-tab-content.row-content"
						>
							<div
								className={styles.label}
								data-flx="user.account-security-tab.security-tab.security-tab-content.label"
							>
								<Trans>Authenticator app</Trans>
							</div>
							<div
								className={styles.description}
								data-flx="user.account-security-tab.security-tab.security-tab-content.description"
							>
								{hasTotpMfa ? (
									<Trans>Two-factor authentication is enabled</Trans>
								) : (
									<Trans>Use an authenticator app to generate codes for two-factor authentication</Trans>
								)}
							</div>
							{needsEmailVerification && !hasTotpMfa && (
								<div
									className={styles.warningText}
									data-flx="user.account-security-tab.security-tab.security-tab-content.warning-text"
								>
									{i18n._(VERIFY_EMAIL_BEFORE_AUTHENTICATOR_APP_DESCRIPTOR)}
								</div>
							)}
						</div>
						{hasTotpMfa ? (
							<Button
								variant="danger"
								small={true}
								onClick={() =>
									ModalCommands.push(
										modal(() => (
											<MfaTotpDisableModal data-flx="user.account-security-tab.security-tab.security-tab-content.mfa-totp-disable-modal" />
										)),
									)
								}
								data-flx="user.account-security-tab.security-tab.security-tab-content.button.push"
							>
								<Trans>Disable</Trans>
							</Button>
						) : (
							<Button
								small={true}
								disabled={!canAddSecurityCredential}
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
								<Trans>Enable</Trans>
							</Button>
						)}
					</div>
					{hasAnyMfa && (
						<div
							className={styles.divider}
							data-flx="user.account-security-tab.security-tab.security-tab-content.divider"
						>
							<div className={styles.row} data-flx="user.account-security-tab.security-tab.security-tab-content.row--2">
								<div
									className={styles.rowContent}
									data-flx="user.account-security-tab.security-tab.security-tab-content.row-content--2"
								>
									<div
										className={styles.label}
										data-flx="user.account-security-tab.security-tab.security-tab-content.label--2"
									>
										<Trans>Backup codes</Trans>
									</div>
									<div
										className={styles.description}
										data-flx="user.account-security-tab.security-tab.security-tab-content.description--2"
									>
										<Trans>View and manage your backup codes for account recovery</Trans>
									</div>
								</div>
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
							</div>
						</div>
					)}
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Passkeys</Trans>}
					description={<Trans>Use passkeys to sign in without a password</Trans>}
					data-flx="user.account-security-tab.security-tab.security-tab-content.settings-tab-section--3"
				>
					<div className={styles.row} data-flx="user.account-security-tab.security-tab.security-tab-content.row--3">
						<div
							className={styles.rowContent}
							data-flx="user.account-security-tab.security-tab.security-tab-content.row-content--3"
						>
							<div
								className={styles.label}
								data-flx="user.account-security-tab.security-tab.security-tab-content.label--3"
							>
								<Trans>Registered passkeys</Trans>
							</div>
							{needsEmailVerification && (
								<div
									className={styles.warningText}
									data-flx="user.account-security-tab.security-tab.security-tab-content.warning-text--2"
								>
									{i18n._(VERIFY_EMAIL_BEFORE_PASSKEY_DESCRIPTOR)}
								</div>
							)}
						</div>
						<Button
							small={true}
							disabled={!canAddPasskey}
							onClick={handleAddPasskey}
							data-flx="user.account-security-tab.security-tab.security-tab-content.button.add-passkey"
						>
							<Trans>Add passkey</Trans>
						</Button>
					</div>
					{passkeys.length > 0 && (
						<div
							className={styles.divider}
							data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-two-factor-divider"
						>
							<Switch
								label={<Trans>Require a passkey as your second factor</Trans>}
								description={<Trans>Ask for a passkey after your password when you sign in</Trans>}
								value={hasPasskeyMfa}
								onChange={handleTogglePasskeyTwoFactor}
								data-flx="user.account-security-tab.security-tab.security-tab-content.switch.passkey-two-factor"
							/>
						</div>
					)}
					{passkeys.length > 0 && (
						<div
							className={styles.divider}
							data-flx="user.account-security-tab.security-tab.security-tab-content.divider--2"
						>
							<div
								className={styles.passkeyList}
								data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-list"
							>
								{passkeys.map((passkey) => {
									const createdDate = DateUtils.getRelativeDateString(new Date(passkey.created_at), i18n);
									const lastUsedDate = passkey.last_used_at
										? DateUtils.getRelativeDateString(new Date(passkey.last_used_at), i18n)
										: null;
									return (
										<div
											key={passkey.id}
											className={styles.passkeyItem}
											data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-item"
										>
											<div
												className={styles.passkeyInfo}
												data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-info"
											>
												<div
													className={styles.passkeyName}
													data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-name"
												>
													{passkey.name}
												</div>
												<div
													className={styles.passkeyDetails}
													data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-details"
												>
													{lastUsedDate ? (
														<Trans>
															Added: {createdDate} • last used: {lastUsedDate}
														</Trans>
													) : (
														<Trans>Added: {createdDate}</Trans>
													)}
												</div>
											</div>
											<div
												className={styles.passkeyActions}
												data-flx="user.account-security-tab.security-tab.security-tab-content.passkey-actions"
											>
												<Button
													variant="secondary"
													small={true}
													onClick={() => handleRenamePasskey(passkey.id)}
													data-flx="user.account-security-tab.security-tab.security-tab-content.button.rename-passkey"
												>
													<Trans>Rename</Trans>
												</Button>
												<Button
													variant="danger"
													small={true}
													onClick={() => handleDeletePasskey(passkey.id)}
													data-flx="user.account-security-tab.security-tab.security-tab-content.button.delete-passkey"
												>
													<Trans>Delete</Trans>
												</Button>
											</div>
										</div>
									);
								})}
							</div>
						</div>
					)}
				</SettingsTabSection>
				{(onManageAuthorizedApps || onManageLinkedDevices) && (
					<SettingsTabSection
						title={i18n._(ACCOUNT_ACCESS_DESCRIPTOR)}
						description={i18n._(MANAGE_APPS_AND_DEVICES_WITH_ACCESS_TO_YOUR_DESCRIPTOR)}
						data-flx="user.account-security-tab.security-tab.security-tab-content.account-access"
					>
						{onManageAuthorizedApps && (
							<div
								className={styles.row}
								data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.authorized-apps-row"
							>
								<div
									className={styles.rowContent}
									data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.authorized-apps-row-content"
								>
									<div
										className={styles.label}
										data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.authorized-apps-label"
									>
										{i18n._(AUTHORIZED_APPS_DESCRIPTOR)}
									</div>
									<div
										className={styles.description}
										data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.authorized-apps-description"
									>
										{i18n._(REVIEW_APPS_THAT_CAN_ACCESS_YOUR_ACCOUNT_DESCRIPTOR)}
									</div>
								</div>
								<Button
									small={true}
									submitting={authorizedAppsSubmitting}
									onClick={onManageAuthorizedApps}
									data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.button.manage-authorized-apps"
								>
									<Trans>Manage</Trans>
								</Button>
							</div>
						)}
						{onManageLinkedDevices && (
							<div
								className={styles.divider}
								data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-divider"
							>
								<div
									className={styles.row}
									data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-row"
								>
									<div
										className={styles.rowContent}
										data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-row-content"
									>
										<div
											className={styles.label}
											data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-label"
										>
											{i18n._(LINKED_DEVICES_DESCRIPTOR)}
										</div>
										<div
											className={styles.description}
											data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.devices-description"
										>
											{i18n._(REVIEW_SIGNED_IN_DEVICES_DESCRIPTOR)}
										</div>
									</div>
									<Button
										small={true}
										onClick={onManageLinkedDevices}
										data-flx="user.account-security-tab.security-tab.security-tab-content.account-access.button.manage-linked-devices"
									>
										<Trans>Manage</Trans>
									</Button>
								</div>
							</div>
						)}
					</SettingsTabSection>
				)}
			</>
		);
	},
);
