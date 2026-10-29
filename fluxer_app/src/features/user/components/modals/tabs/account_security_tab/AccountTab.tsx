// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailVerificationAlert} from '@app/features/app/components/dialogs/components/EmailVerificationAlert';
import {UnclaimedAccountAlert} from '@app/features/app/components/dialogs/components/UnclaimedAccountAlert';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {EmailChangeModal} from '@app/features/user/components/modals/EmailChangeModal';
import {PasswordChangeModal} from '@app/features/user/components/modals/PasswordChangeModal';
import {
	AccountSecurityCard,
	AccountSecurityRow,
} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import styles from '@app/features/user/components/modals/tabs/account_security_tab/AccountTab.module.css';
import {
	RecoveryKitReminderAlert,
	RecoveryKitRow,
} from '@app/features/user/components/modals/tabs/account_security_tab/RecoveryKitSettings';
import {
	canConnectSso,
	SsoConnectRow,
} from '@app/features/user/components/modals/tabs/account_security_tab/SsoConnectRow';
import type {User} from '@app/features/user/models/User';
import {needsEmailConfirmation} from '@app/features/user/utils/AccountGates';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {
	HIDE_EMAIL_TOGGLE_DESCRIPTOR,
	REVEAL_DESCRIPTOR as REVEAL_EMAIL_TOGGLE_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {AccountTiers, AccountVerificationNeeds} from '@fluxer/constants/src/AccountTierConstants';
import {hasUserTrait, UserTraits} from '@fluxer/constants/src/UserTraitConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SIGN_IN_METHODS_DESCRIPTOR = msg({
	message: 'Sign-in methods',
	comment: 'Accessible name for the group of email, password and recovery kit rows in account settings.',
});
const maskEmail = (email: string): string => {
	const [username, domain] = email.split('@');
	const maskedUsername = username.replace(/./g, '*');
	return `${maskedUsername}@${domain}`;
};

function isSsoRequired(): boolean {
	return RuntimeConfig.getSnapshot().sso?.enforced === true;
}

function credentialsManagedBySso(user: User): boolean {
	return isSsoRequired() && hasUserTrait(user.traits, UserTraits.SSO);
}

interface AccountTabProps {
	user: User;
	showMaskedEmail: boolean;
	setShowMaskedEmail: (show: boolean) => void;
}

export const AccountTabContent: React.FC<AccountTabProps> = observer(({user, showMaskedEmail, setShowMaskedEmail}) => {
	const {i18n} = useLingui();
	const usesUsernameSignIn = RuntimeConfig.usesUsernameSignIn;
	const need = user.verificationNeed;
	const needsPassword = need === AccountVerificationNeeds.SET_PASSWORD;
	const isClaimed = user.accountTier !== AccountTiers.UNCLAIMED;
	const credentialsManaged = credentialsManagedBySso(user);
	const showsRecoveryKit = usesUsernameSignIn && isClaimed && !isSsoRequired();
	const email = user.email;
	let emailRow: React.ReactNode;
	if (usesUsernameSignIn) {
		emailRow = null;
	} else if (email != null) {
		emailRow = (
			<AccountSecurityRow
				label={<Trans>Email address</Trans>}
				description={
					<span
						className={styles.emailRow}
						data-flx="user.account-security-tab.account-tab.account-tab-content.email-value"
					>
						<span
							className={`${styles.emailText} ${showMaskedEmail ? styles.emailTextSelectable : ''}`}
							data-flx="user.account-security-tab.account-tab.account-tab-content.email-text"
						>
							{showMaskedEmail ? email : maskEmail(email)}
						</span>
						<button
							type="button"
							className={styles.toggleButton}
							onClick={() => setShowMaskedEmail(!showMaskedEmail)}
							data-flx="user.account-security-tab.account-tab.account-tab-content.toggle-button"
						>
							{showMaskedEmail ? i18n._(HIDE_EMAIL_TOGGLE_DESCRIPTOR) : i18n._(REVEAL_EMAIL_TOGGLE_DESCRIPTOR)}
						</button>
					</span>
				}
				data-flx="user.account-security-tab.account-tab.account-tab-content.email-row"
			>
				{RuntimeConfig.emailsEnabled && isClaimed && !credentialsManaged && (
					<Button
						variant="secondary"
						small={true}
						onClick={() =>
							ModalCommands.push(
								modal(() => (
									<EmailChangeModal
										user={user}
										data-flx="user.account-security-tab.account-tab.account-tab-content.email-change-modal"
									/>
								)),
							)
						}
						data-flx="user.account-security-tab.account-tab.account-tab-content.button.change-email"
					>
						<Trans>Change email</Trans>
					</Button>
				)}
			</AccountSecurityRow>
		);
	} else {
		emailRow = (
			<AccountSecurityRow
				label={<Trans>Email address</Trans>}
				warning={<Trans>No email address set</Trans>}
				data-flx="user.account-security-tab.account-tab.account-tab-content.email-row--unclaimed"
			>
				{!isClaimed && !credentialsManaged && (
					<Button
						small={true}
						onClick={() => openClaimAccountModal()}
						data-flx="user.account-security-tab.account-tab.account-tab-content.button.add-email"
					>
						<Trans>Add email</Trans>
					</Button>
				)}
			</AccountSecurityRow>
		);
	}
	let passwordRow: React.ReactNode;
	if (need === AccountVerificationNeeds.ADD_EMAIL) {
		passwordRow = null;
	} else if (credentialsManaged) {
		passwordRow = (
			<AccountSecurityRow
				label={<Trans>Password</Trans>}
				description={
					<Trans comment="Description of the password row in account settings when the instance requires single sign-on and the sign-in provider manages the account's email and password.">
						Your sign-in provider manages your email and password.
					</Trans>
				}
				data-flx="user.account-security-tab.account-tab.account-tab-content.password-row--sso"
			/>
		);
	} else if (needsPassword) {
		passwordRow = (
			<AccountSecurityRow
				label={<Trans>Password</Trans>}
				warning={<Trans>No password set</Trans>}
				data-flx="user.account-security-tab.account-tab.account-tab-content.password-row--unclaimed"
			>
				<Button
					small={true}
					onClick={() => openClaimAccountModal()}
					data-flx="user.account-security-tab.account-tab.account-tab-content.button.set-password"
				>
					<Trans>Set password</Trans>
				</Button>
			</AccountSecurityRow>
		);
	} else if (!user.hasPassword && RuntimeConfig.emailsEnabled) {
		passwordRow = (
			<AccountSecurityRow
				label={<Trans>Password</Trans>}
				description={
					user.accountTier === AccountTiers.VERIFIED ? (
						<Trans comment="Description of the password row in account settings when the account has a confirmed email but no password.">
							You sign in with a code sent to your email. A password is optional.
						</Trans>
					) : (
						<Trans comment="Description of the password row in account settings when the account email is not confirmed yet.">
							Confirm your email first. Then you can set a password.
						</Trans>
					)
				}
				data-flx="user.account-security-tab.account-tab.account-tab-content.password-row--set"
			>
				{user.accountTier === AccountTiers.VERIFIED && (
					<Button
						variant="secondary"
						small={true}
						onClick={() =>
							ModalCommands.push(
								modal(() => (
									<PasswordChangeModal data-flx="user.account-security-tab.account-tab.account-tab-content.password-set-modal" />
								)),
							)
						}
						data-flx="user.account-security-tab.account-tab.account-tab-content.button.set-password--claimed"
					>
						<Trans>Set password</Trans>
					</Button>
				)}
			</AccountSecurityRow>
		);
	} else {
		passwordRow = (
			<AccountSecurityRow
				label={<Trans>Password</Trans>}
				description={
					user.passwordLastChangedAt ? (
						<Trans>Last changed: {DateUtils.getRelativeDateString(user.passwordLastChangedAt, i18n)}</Trans>
					) : (
						<Trans>Last changed: never</Trans>
					)
				}
				data-flx="user.account-security-tab.account-tab.account-tab-content.password-row"
			>
				<Button
					variant="secondary"
					small={true}
					onClick={() =>
						ModalCommands.push(
							modal(() => (
								<PasswordChangeModal data-flx="user.account-security-tab.account-tab.account-tab-content.password-change-modal" />
							)),
						)
					}
					data-flx="user.account-security-tab.account-tab.account-tab-content.button.change-password"
				>
					<Trans>Change password</Trans>
				</Button>
			</AccountSecurityRow>
		);
	}
	return (
		<>
			{needsPassword && !credentialsManaged && (
				<UnclaimedAccountAlert data-flx="user.account-security-tab.account-tab.account-tab-content.unclaimed-account-alert" />
			)}
			{showsRecoveryKit && (
				<RecoveryKitReminderAlert
					user={user}
					data-flx="user.account-security-tab.account-tab.account-tab-content.recovery-kit-reminder-alert"
				/>
			)}
			<AccountSecurityCard
				aria-label={i18n._(SIGN_IN_METHODS_DESCRIPTOR)}
				data-flx="user.account-security-tab.account-tab.account-tab-content.rows"
			>
				{emailRow}
				{passwordRow}
				{showsRecoveryKit && (
					<RecoveryKitRow
						user={user}
						data-flx="user.account-security-tab.account-tab.account-tab-content.recovery-kit-row"
					/>
				)}
				{canConnectSso(user) && (
					<SsoConnectRow data-flx="user.account-security-tab.account-tab.account-tab-content.sso-connect-row" />
				)}
			</AccountSecurityCard>
			{needsEmailConfirmation(user) && (
				<EmailVerificationAlert data-flx="user.account-security-tab.account-tab.account-tab-content.email-verification-alert" />
			)}
		</>
	);
});
