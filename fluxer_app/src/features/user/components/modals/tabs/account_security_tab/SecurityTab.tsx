// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
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
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/tabs/account_security_tab/SecurityTab.module.css';
import type {User} from '@app/features/user/models/User';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import type {AccountSecurityCapabilities} from '@app/features/user/utils/AccountSecurityCapabilities';
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
	message: "Review signed-in devices and sign out sessions you don't recognize.",
	comment: 'Security settings row description for linked devices.',
});
const logger = new Logger('SecurityTab');

interface SecurityTabProps {
	user: User;
	capabilities: AccountSecurityCapabilities;
	isClaimed: boolean;
	passkeys: ReadonlyArray<WebAuthnCredential>;
	authorizedAppsSubmitting?: boolean;
	onManageAuthorizedApps?: () => void;
	onManageLinkedDevices?: () => void;
}

export const SecurityTabContent: React.FC<SecurityTabProps> = observer(
	({
		user,
		capabilities,
		isClaimed,
		passkeys,
		authorizedAppsSubmitting,
		onManageAuthorizedApps,
		onManageLinkedDevices,
	}) => {
		const {i18n} = useLingui();
		const hasTotpMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.TOTP) ?? false;
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
		const handleDeletePasskey = (credentialId: string) => {
			const passkey = passkeys.find((p) => p.id === credentialId);
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_PASSKEY_DESCRIPTOR)}
						description={
							passkey ? (
								<div data-flx="user.account-security-tab.security-tab.handle-delete-passkey.div">
									<Trans>
										Are you sure you want to delete the passkey{' '}
										<strong data-flx="user.account-security-tab.security-tab.handle-delete-passkey.strong">
											{passkey.name}
										</strong>
										?
									</Trans>
								</div>
							) : (
								<Trans>Are you sure you want to delete this passkey?</Trans>
							)
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
				{capabilities.canManageLocalTotp && (
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
						{hasTotpMfa && (
							<div
								className={styles.divider}
								data-flx="user.account-security-tab.security-tab.security-tab-content.divider"
							>
								<div
									className={styles.row}
									data-flx="user.account-security-tab.security-tab.security-tab-content.row--2"
								>
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
				)}
				{capabilities.canManageLocalPasskeys && (
					<SettingsTabSection
						title={<Trans>Passkeys</Trans>}
						description={<Trans>Use passkeys for passwordless sign-in and two-factor authentication</Trans>}
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
				)}
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
