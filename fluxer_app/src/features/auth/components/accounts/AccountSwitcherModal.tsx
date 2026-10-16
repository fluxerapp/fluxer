// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useModalBackHandler} from '@app/features/app/hooks/useModalBackHandler';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {useAccountDisplayIdentityViewFormatter} from '@app/features/auth/components/accounts/AccountIdentity';
import styles from '@app/features/auth/components/accounts/AccountSwitcherModal.module.css';
import {AccountSwitcherAuthFlow} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow';
import {AccountSwitcherAuthPurposeType} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {
	AccountSwitcherActionType,
	AccountSwitcherView,
	type AccountSwitcherViewState,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherModalTypes';
import {AccountSwitcherOverlay} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherOverlay';
import {ConfirmAccountActionStep} from '@app/features/auth/components/accounts/account_switcher_modal/ConfirmAccountActionStep';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {AuthRegisterDraftContext, useAuthRegisterDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {useAccountSwitcherLogic} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {ADD_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

const SIGN_IN_AGAIN_DESCRIPTOR = msg({
	message: 'Sign in again',
	comment: 'Short modal title for re-authenticating an expired account from the account switcher.',
});
const MANAGE_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Manage accounts',
	comment: 'Accessible label for the account switcher surface that lists every saved account.',
});
const SIGN_OUT_OF_THIS_ACCOUNT_DESCRIPTOR = msg({
	message: 'Sign out of this account',
	comment: 'Confirmation title used when the saved account has no cached profile name to show.',
});

export interface AccountSwitcherModalProps {
	readonly closeCurrentAccountOnSelect: boolean;
	readonly initialReloginAccount: Account | null;
	readonly redirectAfterLogin: string | null;
	readonly redirectAfterSwitch: string | null;
	readonly switchAccount: ((accountKey: string) => Promise<void>) | null;
	readonly 'data-flx'?: string;
}

function resolveOverlayLabel(i18n: I18n, viewState: AccountSwitcherViewState): string {
	if (viewState.view === AccountSwitcherView.AUTH) {
		if (viewState.purpose.type === AccountSwitcherAuthPurposeType.RELOGIN) {
			return i18n._(SIGN_IN_AGAIN_DESCRIPTOR);
		}
		return i18n._(ADD_ACCOUNT_DESCRIPTOR);
	}
	return i18n._(MANAGE_ACCOUNTS_DESCRIPTOR);
}

const AccountSwitcherModal = observer(
	({
		closeCurrentAccountOnSelect,
		initialReloginAccount,
		redirectAfterLogin,
		redirectAfterSwitch,
		switchAccount,
	}: AccountSwitcherModalProps) => {
		const {i18n} = useLingui();
		const getAccountIdentityView = useAccountDisplayIdentityViewFormatter();
		const authRegisterDraftContextValue = useAuthRegisterDraft();
		const [isConfirmPending, setIsConfirmPending] = useState(false);
		const [viewState, setViewState] = useState<AccountSwitcherViewState>(() => {
			if (initialReloginAccount != null) {
				return {
					view: AccountSwitcherView.AUTH,
					purpose: {type: AccountSwitcherAuthPurposeType.RELOGIN, account: initialReloginAccount},
				};
			}
			return {view: AccountSwitcherView.ACCOUNTS};
		});
		const startAddAccount = useCallback(() => {
			setViewState({view: AccountSwitcherView.AUTH, purpose: {type: AccountSwitcherAuthPurposeType.ADD}});
		}, []);
		const startRelogin = useCallback((account: Account) => {
			setViewState({
				view: AccountSwitcherView.AUTH,
				purpose: {type: AccountSwitcherAuthPurposeType.RELOGIN, account},
			});
		}, []);
		const {accounts, currentAccountKey, isBusy, handleSelectAccount, handleLogout, handleLogoutStoredAccount} =
			useAccountSwitcherLogic({
				onSelectCurrent: closeCurrentAccountOnSelect ? ModalCommands.pop : null,
				onSessionExpired: startRelogin,
				redirectAfterSwitch,
				switchAccount,
			});
		const handleAccountRemove = useCallback(
			(account: Account) => {
				if (isBusy) {
					return;
				}
				let type: AccountSwitcherActionType = AccountSwitcherActionType.SAVED;
				if (getAccountKey(account) === currentAccountKey) {
					type = AccountSwitcherActionType.CURRENT;
				}
				setViewState({view: AccountSwitcherView.CONFIRM, action: {type, account}});
			},
			[currentAccountKey, isBusy],
		);
		const backToAccounts = useCallback(() => {
			AuthenticationCommands.clearMfaTicket();
			setViewState({view: AccountSwitcherView.ACCOUNTS});
		}, []);
		const closeModal = useCallback(() => {
			ModalCommands.pop();
		}, []);
		const handleAuthLoginComplete = useCallback(() => {
			ModalCommands.popAll();
		}, []);
		const cancelConfirm = useCallback(() => {
			if (isConfirmPending) {
				return;
			}
			setViewState({view: AccountSwitcherView.ACCOUNTS});
		}, [isConfirmPending]);
		const confirmAccountAction = useCallback(async () => {
			if (viewState.view !== AccountSwitcherView.CONFIRM || isConfirmPending) {
				return;
			}
			const {action} = viewState;
			setIsConfirmPending(true);
			try {
				if (action.type === AccountSwitcherActionType.CURRENT) {
					await handleLogout();
					return;
				}
				await handleLogoutStoredAccount(action.account);
				setViewState({view: AccountSwitcherView.ACCOUNTS});
			} finally {
				setIsConfirmPending(false);
			}
		}, [handleLogout, handleLogoutStoredAccount, isConfirmPending, viewState]);
		let handleModalBack = closeModal;
		if (viewState.view === AccountSwitcherView.AUTH) {
			handleModalBack = backToAccounts;
		} else if (viewState.view === AccountSwitcherView.CONFIRM) {
			handleModalBack = cancelConfirm;
		}
		useModalBackHandler(handleModalBack);
		if (viewState.view === AccountSwitcherView.CONFIRM) {
			const identityView = getAccountIdentityView(viewState.action.account);
			const displayName = identityView.displayLabel;
			let confirmTitle: React.ReactNode = i18n._(SIGN_OUT_OF_THIS_ACCOUNT_DESCRIPTOR);
			if (identityView.isAvailable) {
				confirmTitle = <Trans>Sign out of {displayName}</Trans>;
			}
			const confirmDisabled = isBusy || isConfirmPending;
			return (
				<Modal.Root
					size="small"
					centered
					disableHistoryManagement
					onClose={cancelConfirm}
					data-flx="auth.accounts.account-switcher-modal.confirm.modal-root"
				>
					<Modal.Header
						title={confirmTitle}
						onClose={cancelConfirm}
						data-flx="auth.accounts.account-switcher-modal.confirm.modal-header"
					/>
					<Modal.Content
						className={styles.content}
						data-flx="auth.accounts.account-switcher-modal.confirm.modal-content"
					>
						<ConfirmAccountActionStep
							action={viewState.action}
							hasMultipleAccounts={accounts.length > 1}
							data-flx="auth.accounts.account-switcher-modal.confirm.confirm-account-action-step"
						/>
					</Modal.Content>
					<Modal.Footer className={styles.footer} data-flx="auth.accounts.account-switcher-modal.confirm.modal-footer">
						<Button
							variant="secondary"
							onClick={cancelConfirm}
							disabled={confirmDisabled}
							data-flx="auth.accounts.account-switcher-modal.confirm.button.cancel"
						>
							<Trans>Cancel</Trans>
						</Button>
						<Button
							variant="danger"
							onClick={confirmAccountAction}
							disabled={isBusy}
							submitting={isConfirmPending}
							data-flx="auth.accounts.account-switcher-modal.confirm.button.sign-out"
						>
							<Trans>Sign out</Trans>
						</Button>
					</Modal.Footer>
				</Modal.Root>
			);
		}
		let authContent: React.ReactNode = null;
		let onBackdropClick = closeModal;
		if (viewState.view === AccountSwitcherView.AUTH) {
			authContent = (
				<AccountSwitcherAuthFlow
					purpose={viewState.purpose}
					redirectAfterLogin={redirectAfterLogin}
					onBackToAccounts={backToAccounts}
					onLoginComplete={handleAuthLoginComplete}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow"
				/>
			);
			onBackdropClick = backToAccounts;
		}
		return (
			<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
				<AccountSwitcherOverlay
					ariaLabel={resolveOverlayLabel(i18n, viewState)}
					view={viewState.view}
					accounts={accounts}
					currentAccountKey={currentAccountKey}
					isBusy={isBusy}
					authContent={authContent}
					onAccountClick={handleSelectAccount}
					onAccountRemove={handleAccountRemove}
					onAddAccount={startAddAccount}
					onBackdropClick={onBackdropClick}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay"
				/>
			</AuthRegisterDraftContext.Provider>
		);
	},
);

export default AccountSwitcherModal;
