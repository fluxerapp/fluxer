// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountRow} from '@app/features/auth/components/accounts/AccountRow';
import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import {useAccountSwitcherLogic} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const CHOOSE_AN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Choose an account',
	comment: 'OAuth authorization step title. The user must choose which account to authorize with.',
});
const CLIENT_WILL_USE_THIS_ACCOUNT_DESCRIPTOR = msg({
	message: '{clientName} will use this account for this authorization.',
	comment:
		'OAuth authorization account step description. {clientName} is the OAuth application name requesting access.',
});
const ADD_AN_ACCOUNT_TO_CONTINUE_DESCRIPTOR = msg({
	message: 'Add an account to continue.',
	comment: 'OAuth authorization empty state shown when there are no saved accounts to choose from.',
});
const ADD_AN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Add an account',
	comment: 'OAuth authorization account step button. Opens the flow to sign in with another account.',
});
const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'OAuth authorization primary button. Advances to the next step after choosing an account.',
});

interface OAuthAccountStepProps {
	clientLabel: string;
	onCancel: () => void;
	onContinue: () => void;
}

export const OAuthAccountStep: React.FC<OAuthAccountStepProps> = observer(({clientLabel, onCancel, onContinue}) => {
	const {i18n} = useLingui();
	const {accounts, currentAccount, isBusy, handleSwitchAccount, handleReLogin, handleAddAccount} =
		useAccountSwitcherLogic({
			redirectAfterSwitch: null,
			redirectAfterLogin: null,
		});
	const canContinue = Boolean(currentAccount && currentAccount.isValid !== false);
	const handleAccountClick = useCallback(
		(account: Account) => {
			if (isBusy) {
				return;
			}
			if (account.isValid === false) {
				handleReLogin(account.userId);
				return;
			}
			if (account.userId === currentAccount?.userId) {
				return;
			}
			void handleSwitchAccount(account.userId);
		},
		[currentAccount?.userId, handleReLogin, handleSwitchAccount, isBusy],
	);
	return (
		<div className={styles.page} data-flx="auth.o-auth-authorize-page.account-step.page">
			<div className={styles.heroCard} data-flx="auth.o-auth-authorize-page.account-step.hero-card">
				<div className={styles.heroCopy} data-flx="auth.o-auth-authorize-page.account-step.hero-copy">
					<h1 className={styles.heroTitle} data-flx="auth.o-auth-authorize-page.account-step.hero-title">
						{i18n._(CHOOSE_AN_ACCOUNT_DESCRIPTOR)}
					</h1>
					<p className={styles.heroDescription} data-flx="auth.o-auth-authorize-page.account-step.hero-description">
						{i18n._(CLIENT_WILL_USE_THIS_ACCOUNT_DESCRIPTOR, {clientName: clientLabel})}
					</p>
				</div>
			</div>
			<div className={styles.accountList} data-flx="auth.o-auth-authorize-page.account-step.account-list">
				{accounts.length === 0 ? (
					<div className={styles.emptyState} data-flx="auth.o-auth-authorize-page.account-step.empty-state">
						{i18n._(ADD_AN_ACCOUNT_TO_CONTINUE_DESCRIPTOR)}
					</div>
				) : (
					accounts.map((account) => {
						const isCurrent = account.userId === currentAccount?.userId;
						const canClick = !isCurrent || account.isValid === false;
						return (
							<AccountRow
								key={account.userId}
								account={account}
								variant="manage"
								isCurrent={isCurrent}
								isExpired={account.isValid === false}
								showInstance
								onClick={canClick ? () => handleAccountClick(account) : undefined}
								data-flx="auth.o-auth-authorize-page.account-step.account-row"
							/>
						);
					})
				)}
			</div>
			<Button
				type="button"
				variant="secondary"
				leftIcon={
					<PlusIcon size={remFromPx(18)} weight="bold" data-flx="auth.o-auth-authorize-page.account-step.plus-icon" />
				}
				onClick={handleAddAccount}
				disabled={isBusy}
				fitContainer
				data-flx="auth.o-auth-authorize-page.account-step.button.add-account"
			>
				{i18n._(ADD_AN_ACCOUNT_DESCRIPTOR)}
			</Button>
			<div className={styles.sectionDivider} data-flx="auth.o-auth-authorize-page.account-step.section-divider" />
			<div className={styles.actions} data-flx="auth.o-auth-authorize-page.account-step.actions">
				<div className={styles.actionButton} data-flx="auth.o-auth-authorize-page.account-step.action-button.cancel">
					<Button
						type="button"
						variant="secondary"
						onClick={onCancel}
						disabled={isBusy}
						className={styles.actionButton}
						data-flx="auth.o-auth-authorize-page.account-step.button.cancel"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
				</div>
				<div className={styles.actionButton} data-flx="auth.o-auth-authorize-page.account-step.action-button.continue">
					<Button
						type="button"
						onClick={onContinue}
						disabled={!canContinue || isBusy}
						className={styles.actionButton}
						data-flx="auth.o-auth-authorize-page.account-step.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		</div>
	);
});
