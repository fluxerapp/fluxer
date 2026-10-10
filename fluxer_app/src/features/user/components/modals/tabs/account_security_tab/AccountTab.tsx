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
import type {User} from '@app/features/user/models/User';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const HIDE_EMAIL_TOGGLE_DESCRIPTOR = msg({
	message: 'Hide',
	comment: 'Account settings: button that hides the user email address (currently revealed). Verb, sentence case.',
});
const REVEAL_EMAIL_TOGGLE_DESCRIPTOR = msg({
	message: 'Reveal',
	comment: 'Account settings: button that reveals the masked user email address. Verb, sentence case.',
});
const SIGN_IN_METHODS_DESCRIPTOR = msg({
	message: 'Sign-in methods',
	comment: 'Accessible name for the group of email, password and recovery kit rows in account settings.',
});
const maskEmail = (email: string): string => {
	const [username, domain] = email.split('@');
	const maskedUsername = username.replace(/./g, '*');
	return `${maskedUsername}@${domain}`;
};

interface AccountTabProps {
	user: User;
	isClaimed: boolean;
	showMaskedEmail: boolean;
	setShowMaskedEmail: (show: boolean) => void;
}

export const AccountTabContent: React.FC<AccountTabProps> = observer(
	({user, isClaimed, showMaskedEmail, setShowMaskedEmail}) => {
		const {i18n} = useLingui();
		const usesUsernameSignIn = RuntimeConfig.usesUsernameSignIn;
		const emailRow = usesUsernameSignIn ? null : isClaimed ? (
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
							{showMaskedEmail ? user.email : maskEmail(user.email!)}
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
				{RuntimeConfig.emailsEnabled && (
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
		) : (
			<AccountSecurityRow
				label={<Trans>Email address</Trans>}
				warning={<Trans>No email address set</Trans>}
				data-flx="user.account-security-tab.account-tab.account-tab-content.email-row--unclaimed"
			>
				<Button
					small={true}
					onClick={() => openClaimAccountModal()}
					data-flx="user.account-security-tab.account-tab.account-tab-content.button.add-email"
				>
					<Trans>Add email</Trans>
				</Button>
			</AccountSecurityRow>
		);
		const passwordRow = isClaimed ? (
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
		) : (
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
		return (
			<>
				{!isClaimed && (
					<UnclaimedAccountAlert data-flx="user.account-security-tab.account-tab.account-tab-content.unclaimed-account-alert" />
				)}
				{usesUsernameSignIn && isClaimed && (
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
					{usesUsernameSignIn && isClaimed && (
						<RecoveryKitRow
							user={user}
							data-flx="user.account-security-tab.account-tab.account-tab-content.recovery-kit-row"
						/>
					)}
				</AccountSecurityCard>
				{RuntimeConfig.emailsEnabled && isClaimed && !usesUsernameSignIn && user.email && !user.verified && (
					<EmailVerificationAlert data-flx="user.account-security-tab.account-tab.account-tab-content.email-verification-alert" />
				)}
			</>
		);
	},
);
