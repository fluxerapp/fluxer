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
import {openClaimAccountModal} from '@app/components/modals/ClaimAccountModal';
import {EmailVerificationAlert} from '@app/components/modals/components/EmailVerificationAlert';
import {UnclaimedAccountAlert} from '@app/components/modals/components/UnclaimedAccountAlert';
import {EmailChangeModal} from '@app/components/modals/EmailChangeModal';
import {PasswordChangeModal} from '@app/components/modals/PasswordChangeModal';
import {SettingsTabSection} from '@app/components/modals/shared/SettingsTabLayout';
import styles from '@app/components/modals/tabs/account_security_tab/AccountTab.module.css';
import {Button} from '@app/components/uikit/button/Button';
import type {UserRecord} from '@app/records/UserRecord';
import * as DateUtils from '@app/utils/DateUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const maskEmail = (email: string): string => {
	const [username, domain] = email.split('@');
	const maskedUsername = username.replace(/./g, '*');
	return `${maskedUsername}@${domain}`;
};

interface AccountTabProps {
	user: UserRecord;
	isClaimed: boolean;
	showMaskedEmail: boolean;
	setShowMaskedEmail: (show: boolean) => void;
}

export const AccountTabContent: React.FC<AccountTabProps> = observer(
	({user, isClaimed, showMaskedEmail, setShowMaskedEmail}) => {
		const {t, i18n} = useLingui();
		return (
			<>
				{!isClaimed && <UnclaimedAccountAlert />}

				<SettingsTabSection
					title={<Trans>Email Settings</Trans>}
					description={<Trans>Manage the email address you use to sign in to Fluxer</Trans>}
				>
					{isClaimed ? (
						<>
							<div className={styles.row}>
								<div className={styles.rowContent}>
									<div className={styles.label}>
										<Trans>Email Address</Trans>
									</div>
									<div className={styles.emailRow}>
										<span className={`${styles.emailText} ${showMaskedEmail ? styles.emailTextSelectable : ''}`}>
											{showMaskedEmail ? user.email : maskEmail(user.email!)}
										</span>
										<button
											type="button"
											className={styles.toggleButton}
											onClick={() => setShowMaskedEmail(!showMaskedEmail)}
										>
											{showMaskedEmail ? t`Hide` : t`Reveal`}
										</button>
									</div>
								</div>
								<Button
									small={true}
									onClick={() => ModalActionCreators.push(modal(() => <EmailChangeModal user={user} />))}
								>
									<Trans>Change Email</Trans>
								</Button>
							</div>

							{user.email && !user.verified && <EmailVerificationAlert />}
						</>
					) : (
						<div className={styles.row}>
							<div className={styles.rowContent}>
								<div className={styles.label}>
									<Trans>Email Address</Trans>
								</div>
								<div className={styles.warningText}>
									<Trans>No email address set</Trans>
								</div>
							</div>
							<Button small={true} className={styles.claimButton} fitContent onClick={() => openClaimAccountModal()}>
								<Trans>Add Email</Trans>
							</Button>
						</div>
					)}
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Password</Trans>}
					description={<Trans>Change your password to keep your account secure</Trans>}
				>
					<div className={styles.row}>
						{isClaimed ? (
							<>
								<div className={styles.rowContent}>
									<div className={styles.label}>
										<Trans>Current Password</Trans>
									</div>
									<div className={styles.description}>
										{user.passwordLastChangedAt ? (
											<Trans>Last changed: {DateUtils.getRelativeDateString(user.passwordLastChangedAt, i18n)}</Trans>
										) : (
											<Trans>Last changed: Never</Trans>
										)}
									</div>
								</div>
								<Button small={true} onClick={() => ModalActionCreators.push(modal(() => <PasswordChangeModal />))}>
									<Trans>Change Password</Trans>
								</Button>
							</>
						) : (
							<>
								<div className={styles.rowContent}>
									<div className={styles.label}>
										<Trans>Password</Trans>
									</div>
									<div className={styles.warningText}>
										<Trans>No password set</Trans>
									</div>
								</div>
								<Button small={true} className={styles.claimButton} fitContent onClick={() => openClaimAccountModal()}>
									<Trans>Set Password</Trans>
								</Button>
							</>
						)}
					</div>
				</SettingsTabSection>
			</>
		);
	},
);
