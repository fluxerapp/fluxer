// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountRow} from '@app/features/auth/components/accounts/AccountRow';
import styles from '@app/features/auth/components/accounts/AccountSwitcherModal.module.css';
import {
	AccountSwitcherActionType,
	type ConfirmAction,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherModalTypes';
import {flxElementClassName} from '@app/lib/react';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

interface ConfirmAccountActionStepProps {
	readonly action: ConfirmAction;
	readonly hasMultipleAccounts: boolean;
}

function renderConfirmDescription(action: ConfirmAction, hasMultipleAccounts: boolean): React.ReactNode {
	if (action.type !== AccountSwitcherActionType.CURRENT) {
		return <Trans>Signing out will remove this account from the device.</Trans>;
	}
	if (hasMultipleAccounts) {
		return <Trans>Signing out will bring you to the sign-in screen so you can pick another account.</Trans>;
	}
	return <Trans>Signing out will bring you to the sign-in screen.</Trans>;
}

export function ConfirmAccountActionStep({
	action,
	hasMultipleAccounts,
}: ConfirmAccountActionStepProps): React.ReactElement {
	return (
		<flx-auth-confirm-account-action-step
			className={flxElementClassName(styles.confirmPanel)}
			data-flx="auth.accounts.account-switcher-modal.confirm-account-action-step.panel"
		>
			<AccountRow
				account={action.account}
				isCurrent={action.type === AccountSwitcherActionType.CURRENT}
				isExpired={action.account.isValid === false}
				data-flx="auth.accounts.account-switcher-modal.confirm-account-action-step.account-row"
			/>
			<p
				className={styles.confirmText}
				data-flx="auth.accounts.account-switcher-modal.confirm-account-action-step.confirm-text"
			>
				{renderConfirmDescription(action, hasMultipleAccounts)}
			</p>
		</flx-auth-confirm-account-action-step>
	);
}
