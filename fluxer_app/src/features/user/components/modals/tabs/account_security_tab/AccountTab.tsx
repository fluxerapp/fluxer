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
			<>
				<div className={styles.row} data-flx="user.account-security-tab.account-tab.account-tab-content.email-row">
					<div
						className={styles.rowContent}
						data-flx="user.account-security-tab.account-tab.account-tab-content.email-row-content"
					>
						<div
							className={styles.label}
							data-flx="user.account-security-tab.account-tab.account-tab-content.email-label"
						>
							<Trans>Email address</Trans>
						</div>
						<div
							className={styles.emailRow}
							data-flx="user.account-security-tab.account-tab.account-tab-content.email-value-row"
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
								data-flx="user.account-security-tab.account-tab.account-tab-content.toggle-email-visibility"
							>
								{showMaskedEmail ? i18n._(HIDE_EMAIL_TOGGLE_DESCRIPTOR) : i18n._(REVEAL_EMAIL_TOGGLE_DESCRIPTOR)}
							</button>
						</div>
					</div>
					{RuntimeConfig.emailsEnabled && (
						<Button
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
							data-flx="user.account-security-tab.account-tab.account-tab-content.change-email-button"
						>
							<Trans>Change email</Trans>
						</Button>
					)}
				</div>
				{RuntimeConfig.emailsEnabled && user.email && !user.verified && (
					<EmailVerificationAlert data-flx="user.account-security-tab.account-tab.account-tab-content.email-verification-alert" />
				)}
			</>
		) : (
			<div className={styles.row} data-flx="user.account-security-tab.account-tab.account-tab-content.email-row">
				<div
					className={styles.rowContent}
					data-flx="user.account-security-tab.account-tab.account-tab-content.email-row-content"
				>
					<div
						className={styles.label}
						data-flx="user.account-security-tab.account-tab.account-tab-content.email-label"
					>
						<Trans>Email address</Trans>
					</div>
					<div
						className={styles.warningText}
						data-flx="user.account-security-tab.account-tab.account-tab-content.email-warning"
					>
						<Trans>No email address set</Trans>
					</div>
				</div>
				<Button
					small={true}
					className={styles.claimButton}
					fitContent
					onClick={() => openClaimAccountModal()}
					data-flx="user.account-security-tab.account-tab.account-tab-content.add-email-button"
				>
					<Trans>Add email</Trans>
				</Button>
			</div>
		);
		const passwordRow = (
			<div
				className={emailRow ? styles.divider : undefined}
				data-flx="user.account-security-tab.account-tab.account-tab-content.password-block"
			>
				<div className={styles.row} data-flx="user.account-security-tab.account-tab.account-tab-content.password-row">
					{isClaimed ? (
						<>
							<div
								className={styles.rowContent}
								data-flx="user.account-security-tab.account-tab.account-tab-content.password-row-content"
							>
								<div
									className={styles.label}
									data-flx="user.account-security-tab.account-tab.account-tab-content.password-label"
								>
									<Trans>Password</Trans>
								</div>
								<div
									className={styles.description}
									data-flx="user.account-security-tab.account-tab.account-tab-content.password-description"
								>
									{user.passwordLastChangedAt ? (
										<Trans>Last changed: {DateUtils.getRelativeDateString(user.passwordLastChangedAt, i18n)}</Trans>
									) : (
										<Trans>Last changed: never</Trans>
									)}
								</div>
							</div>
							<Button
								small={true}
								onClick={() =>
									ModalCommands.push(
										modal(() => (
											<PasswordChangeModal data-flx="user.account-security-tab.account-tab.account-tab-content.password-change-modal" />
										)),
									)
								}
								data-flx="user.account-security-tab.account-tab.account-tab-content.change-password-button"
							>
								<Trans>Change password</Trans>
							</Button>
						</>
					) : (
						<>
							<div
								className={styles.rowContent}
								data-flx="user.account-security-tab.account-tab.account-tab-content.password-row-content"
							>
								<div
									className={styles.label}
									data-flx="user.account-security-tab.account-tab.account-tab-content.password-label"
								>
									<Trans>Password</Trans>
								</div>
								<div
									className={styles.warningText}
									data-flx="user.account-security-tab.account-tab.account-tab-content.password-warning"
								>
									<Trans>No password set</Trans>
								</div>
							</div>
							<Button
								small={true}
								className={styles.claimButton}
								fitContent
								onClick={() => openClaimAccountModal()}
								data-flx="user.account-security-tab.account-tab.account-tab-content.set-password-button"
							>
								<Trans>Set password</Trans>
							</Button>
						</>
					)}
				</div>
			</div>
		);
		return (
			<>
				{!isClaimed && (
					<UnclaimedAccountAlert data-flx="user.account-security-tab.account-tab.account-tab-content.unclaimed-account-alert" />
				)}
				{usesUsernameSignIn && isClaimed && (
					<RecoveryKitReminderAlert
						user={user}
						data-flx="user.account-security-tab.account-tab.account-tab-content.recovery-kit-reminder"
					/>
				)}
				<div className={styles.accountRows} data-flx="user.account-security-tab.account-tab.account-tab-content.rows">
					{emailRow}
					{passwordRow}
					{usesUsernameSignIn && isClaimed && (
						<RecoveryKitRow
							user={user}
							data-flx="user.account-security-tab.account-tab.account-tab-content.recovery-kit-row"
						/>
					)}
				</div>
			</>
		);
	},
);
