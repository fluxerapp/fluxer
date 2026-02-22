/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as UserActionCreators from '@app/actions/UserActionCreators';
import {BackupCodesViewModal} from '@app/components/modals/BackupCodesViewModal';
import {openClaimAccountModal} from '@app/components/modals/ClaimAccountModal';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {MfaTotpDisableModal} from '@app/components/modals/MfaTotpDisableModal';
import {MfaTotpEnableModal} from '@app/components/modals/MfaTotpEnableModal';
import {PasskeyNameModal} from '@app/components/modals/PasskeyNameModal';
import {PhoneAddModal} from '@app/components/modals/PhoneAddModal';
import {SettingsTabSection} from '@app/components/modals/shared/SettingsTabLayout';
import styles from '@app/components/modals/tabs/account_security_tab/SecurityTab.module.css';
import {Button} from '@app/components/uikit/button/Button';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {Logger} from '@app/lib/Logger';
import type {UserRecord} from '@app/records/UserRecord';
import * as DateUtils from '@app/utils/DateUtils';
import * as WebAuthnUtils from '@app/utils/WebAuthnUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const logger = new Logger('SecurityTab');

const maskPhone = (phone: string): string => {
	if (phone.length <= 4) {
		return phone.replace(/./g, '*');
	}
	const lastTwo = phone.slice(-2);
	const masked = phone.slice(0, -2).replace(/\d/g, '*');
	return `${masked}${lastTwo}`;
};

interface SecurityTabProps {
	user: UserRecord;
	isClaimed: boolean;
	hasSmsMfa: boolean;
	hasTotpMfa: boolean;
	isSmsMfaDisabledForUser: boolean;
	passkeys: Array<UserActionCreators.WebAuthnCredential>;
	loadingPasskeys: boolean;
	enablingSmsMfa: boolean;
	disablingSmsMfa: boolean;
	showMaskedPhone: boolean;
	loadPasskeys: () => Promise<void>;
	setEnablingSmsMfa: React.Dispatch<React.SetStateAction<boolean>>;
	setDisablingSmsMfa: React.Dispatch<React.SetStateAction<boolean>>;
	setShowMaskedPhone: (show: boolean) => void;
}

export const SecurityTabContent: React.FC<SecurityTabProps> = observer(
	({
		user,
		isClaimed,
		hasSmsMfa,
		hasTotpMfa,
		isSmsMfaDisabledForUser,
		passkeys,
		loadingPasskeys,
		enablingSmsMfa,
		disablingSmsMfa,
		showMaskedPhone,
		loadPasskeys,
		setEnablingSmsMfa,
		setDisablingSmsMfa,
		setShowMaskedPhone,
	}) => {
		const {t, i18n} = useLingui();

		const handleAddPasskey = async () => {
			try {
				const options = await UserActionCreators.getWebAuthnRegistrationOptions();
				const credential = await WebAuthnUtils.performRegistration(options);

				ModalActionCreators.push(
					modal(() => (
						<PasskeyNameModal
							onSubmit={async (name: string) => {
								await UserActionCreators.registerWebAuthnCredential(credential, options.challenge, name);
								await loadPasskeys();
							}}
						/>
					)),
				);
			} catch (error) {
				logger.error('Failed to add passkey', error);
			}
		};

		const handleRenamePasskey = async (credentialId: string) => {
			ModalActionCreators.push(
				modal(() => (
					<PasskeyNameModal
						onSubmit={async (name: string) => {
							try {
								await UserActionCreators.renameWebAuthnCredential(credentialId, name);
								await loadPasskeys();
							} catch (error) {
								logger.error('Failed to rename passkey', error);
							}
						}}
					/>
				)),
			);
		};

		const handleDeletePasskey = (credentialId: string) => {
			const passkey = passkeys.find((p) => p.id === credentialId);
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Delete Passkey`}
						description={
							passkey ? (
								<div>
									<Trans>
										Are you sure you want to delete the passkey <strong>{passkey.name}</strong>?
									</Trans>
								</div>
							) : (
								<Trans>Are you sure you want to delete this passkey?</Trans>
							)
						}
						primaryText={t`Delete Passkey`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await UserActionCreators.deleteWebAuthnCredential(credentialId);
								await loadPasskeys();
							} catch (error) {
								logger.error('Failed to delete passkey', error);
							}
						}}
					/>
				)),
			);
		};

		const handleEnableSmsMfa = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Enable SMS Two-Factor Authentication`}
						description={
							<Trans>
								SMS two-factor authentication adds an additional layer of security to your account by requiring a
								verification code sent to your phone number when signing in.
							</Trans>
						}
						primaryText={t`Enable SMS 2FA`}
						primaryVariant="primary"
						onPrimary={async () => {
							setEnablingSmsMfa(true);
							try {
								await UserActionCreators.enableSmsMfa();
							} catch (error) {
								logger.error('Failed to enable SMS MFA', error);
							} finally {
								setEnablingSmsMfa(false);
							}
						}}
					/>
				)),
			);
		};

		const handleDisableSmsMfa = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Disable SMS Two-Factor Authentication`}
						description={
							<Trans>
								Are you sure you want to disable SMS two-factor authentication? This will make your account less secure.
							</Trans>
						}
						primaryText={t`Disable SMS 2FA`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							setDisablingSmsMfa(true);
							try {
								await UserActionCreators.disableSmsMfa();
							} catch (error) {
								logger.error('Failed to disable SMS MFA', error);
							} finally {
								setDisablingSmsMfa(false);
							}
						}}
					/>
				)),
			);
		};

		if (!isClaimed) {
			return (
				<SettingsTabSection
					title={<Trans>Security Features</Trans>}
					description={
						<Trans>Claim your account to access security features like two-factor authentication and passkeys.</Trans>
					}
				>
					<Button className={styles.claimButton} fitContent onClick={() => openClaimAccountModal()}>
						<Trans>Claim Account</Trans>
					</Button>
				</SettingsTabSection>
			);
		}

		return (
			<>
				<SettingsTabSection
					title={<Trans>Two-Factor Authentication</Trans>}
					description={<Trans>Add an extra layer of security to your account</Trans>}
				>
					<div className={styles.row}>
						<div className={styles.rowContent}>
							<div className={styles.label}>
								<Trans>Authenticator App</Trans>
							</div>
							<div className={styles.description}>
								{hasTotpMfa ? (
									<Trans>Two-factor authentication is enabled</Trans>
								) : (
									<Trans>Use an authenticator app to generate codes for two-factor authentication</Trans>
								)}
							</div>
						</div>
						{hasTotpMfa ? (
							<Button
								variant="danger-secondary"
								small={true}
								onClick={() => ModalActionCreators.push(modal(() => <MfaTotpDisableModal />))}
							>
								<Trans>Disable</Trans>
							</Button>
						) : (
							<Button
								small={true}
								onClick={() => ModalActionCreators.push(modal(() => <MfaTotpEnableModal user={user} />))}
							>
								<Trans>Enable</Trans>
							</Button>
						)}
					</div>

					{hasTotpMfa && (
						<div className={styles.divider}>
							<div className={styles.row}>
								<div className={styles.rowContent}>
									<div className={styles.label}>
										<Trans>Backup Codes</Trans>
									</div>
									<div className={styles.description}>
										<Trans>View and manage your backup codes for account recovery</Trans>
									</div>
								</div>
								<Button
									variant="secondary"
									small={true}
									onClick={() => ModalActionCreators.push(modal(() => <BackupCodesViewModal user={user} />))}
								>
									<Trans>View Codes</Trans>
								</Button>
							</div>
						</div>
					)}
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Passkeys</Trans>}
					description={<Trans>Use passkeys for passwordless sign-in and two-factor authentication</Trans>}
				>
					<div className={styles.row}>
						<div className={styles.rowContent}>
							<div className={styles.label}>
								<Trans>Registered Passkeys</Trans>
							</div>
							<div className={styles.description}>
								{passkeys.length === 0
									? t`No passkeys registered`
									: passkeys.length === 1
										? t`${passkeys.length} passkey registered (max 10)`
										: t`${passkeys.length} passkeys registered (max 10)`}
							</div>
						</div>
						<Button small={true} disabled={loadingPasskeys || passkeys.length >= 10} onClick={handleAddPasskey}>
							<Trans>Add Passkey</Trans>
						</Button>
					</div>

					{passkeys.length > 0 && (
						<div className={styles.divider}>
							<div className={styles.passkeyList}>
								{passkeys.map((passkey) => {
									const createdDate = DateUtils.getRelativeDateString(new Date(passkey.created_at), i18n);
									const lastUsedDate = passkey.last_used_at
										? DateUtils.getRelativeDateString(new Date(passkey.last_used_at), i18n)
										: null;

									return (
										<div key={passkey.id} className={styles.passkeyItem}>
											<div className={styles.passkeyInfo}>
												<div className={styles.passkeyName}>{passkey.name}</div>
												<div className={styles.passkeyDetails}>
													{lastUsedDate ? (
														<Trans>
															Added: {createdDate} • Last used: {lastUsedDate}
														</Trans>
													) : (
														<Trans>Added: {createdDate}</Trans>
													)}
												</div>
											</div>
											<div className={styles.passkeyActions}>
												<Button variant="secondary" small={true} onClick={() => handleRenamePasskey(passkey.id)}>
													<Trans>Rename</Trans>
												</Button>
												<Button variant="danger-secondary" small={true} onClick={() => handleDeletePasskey(passkey.id)}>
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

				{user.mfaEnabled && (
					<>
						<SettingsTabSection
							title={<Trans>Phone Number</Trans>}
							description={<Trans>Manage your phone number for SMS two-factor authentication</Trans>}
						>
							<div className={styles.row}>
								<div className={styles.rowContent}>
									<div className={styles.label}>
										<Trans>Phone Number</Trans>
									</div>
									{user.phone ? (
										<div className={styles.phoneRow}>
											<span className={`${styles.phoneText} ${showMaskedPhone ? styles.phoneTextSelectable : ''}`}>
												{showMaskedPhone ? user.phone : maskPhone(user.phone)}
											</span>
											<button
												type="button"
												className={styles.toggleButton}
												onClick={() => setShowMaskedPhone(!showMaskedPhone)}
											>
												{showMaskedPhone ? t`Hide` : t`Reveal`}
											</button>
										</div>
									) : (
										<div className={styles.description}>
											<Trans>Add a phone number to enable SMS two-factor authentication</Trans>
										</div>
									)}
								</div>
								{user.phone ? (
									<Button
										variant="danger-secondary"
										small={true}
										onClick={() => {
											ModalActionCreators.push(
												modal(() => (
													<ConfirmModal
														title={t`Remove Phone Number`}
														description={
															<>
																<div>
																	<Trans>
																		Are you sure you want to remove your phone number <strong>{user.phone}</strong>?
																	</Trans>
																</div>
																{hasSmsMfa && (
																	<div>
																		<Trans>
																			<strong>Warning:</strong> This will also disable SMS two-factor authentication.
																		</Trans>
																	</div>
																)}
															</>
														}
														primaryText={t`Remove Phone`}
														primaryVariant="danger-primary"
														onPrimary={async () => {
															try {
																await UserActionCreators.removePhone();
															} catch (error) {
																logger.error('Failed to remove phone', error);
															}
														}}
													/>
												)),
											);
										}}
									>
										<Trans>Remove</Trans>
									</Button>
								) : (
									<Button small={true} onClick={() => ModalActionCreators.push(modal(() => <PhoneAddModal />))}>
										<Trans>Add Phone</Trans>
									</Button>
								)}
							</div>
						</SettingsTabSection>

						{user.phone && (
							<SettingsTabSection
								title={<Trans>SMS Two-Factor Authentication</Trans>}
								description={<Trans>Receive verification codes via SMS as a backup authentication method</Trans>}
							>
								<div className={styles.row}>
									<div className={styles.rowContent}>
										<div className={styles.label}>
											<Trans>SMS Backup</Trans>
										</div>
										<div className={styles.description}>
											{hasSmsMfa ? (
												<Trans>SMS two-factor authentication is enabled</Trans>
											) : (
												<Trans>Enable SMS codes as a backup for your authenticator app</Trans>
											)}
										</div>
									</div>
									{hasSmsMfa ? (
										<Button
											variant="danger-secondary"
											small={true}
											disabled={disablingSmsMfa}
											onClick={handleDisableSmsMfa}
										>
											<Trans>Disable</Trans>
										</Button>
									) : isSmsMfaDisabledForUser ? (
										<Tooltip text={t`SMS backup is disabled for partners`}>
											<div>
												<Button small={true} disabled={true}>
													<Trans>Enable</Trans>
												</Button>
											</div>
										</Tooltip>
									) : (
										<Button small={true} disabled={enablingSmsMfa} onClick={handleEnableSmsMfa}>
											<Trans>Enable</Trans>
										</Button>
									)}
								</div>
							</SettingsTabSection>
						)}
					</>
				)}
			</>
		);
	},
);
