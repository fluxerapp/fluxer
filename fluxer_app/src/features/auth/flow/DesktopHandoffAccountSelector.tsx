// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountSelector} from '@app/features/auth/components/accounts/AccountSelector';
import type {DesktopHandoffAccountCredentials} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const FAILED_TO_GENERATE_TOKEN_DESCRIPTOR = msg({
	message: 'Failed to generate token',
	comment: 'Short label in the authentication desktop handoff account selector. Keep the tone plain and specific.',
});

interface DesktopHandoffAccountSelectorProps {
	excludeCurrentUser?: boolean;
	onSelectNewAccount: () => void;
	onReLoginAccount: (account: Account) => void;
	onAccountSelected: (payload: DesktopHandoffAccountCredentials) => void;
}

const DesktopHandoffAccountSelector = observer(function DesktopHandoffAccountSelector({
	excludeCurrentUser = false,
	onSelectNewAccount,
	onReLoginAccount,
	onAccountSelected,
}: DesktopHandoffAccountSelectorProps) {
	const {i18n} = useLingui();
	const [isLoading, setIsLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const currentAccountKey = Accounts.currentAccountKey;
	const allAccounts = switcherAccounts();
	const accounts = excludeCurrentUser
		? allAccounts.filter((account) => account.storageKey !== currentAccountKey)
		: allAccounts;
	const handleSelectAccount = useCallback(
		async (account: Account) => {
			if (account.isValid === false) {
				onReLoginAccount(account);
				return;
			}
			setIsLoading(true);
			setError(null);
			try {
				const {token, userId, runtimeSnapshot} = await Accounts.prepareAccountCredentials(account.storageKey);
				if (!token) {
					throw new Error('Failed to generate token');
				}
				onAccountSelected({token, userId, runtimeSnapshot});
			} catch (err) {
				if (err instanceof SessionExpiredError) {
					onReLoginAccount(Accounts.getAccount(account.storageKey) ?? account);
				} else {
					setError(
						err && typeof err === 'object' && 'body' in err
							? FormUtils.extractErrorMessage(i18n, err)
							: i18n._(FAILED_TO_GENERATE_TOKEN_DESCRIPTOR),
					);
				}
			} finally {
				setIsLoading(false);
			}
		},
		[onAccountSelected, onReLoginAccount, i18n],
	);
	return (
		<AccountSelector
			accounts={accounts}
			currentAccountKey={currentAccountKey}
			title={<Trans>Choose an account</Trans>}
			description={<Trans>Select the account you want to sign in with on your new device.</Trans>}
			disabled={isLoading}
			error={error}
			onSelectAccount={handleSelectAccount}
			onAddAccount={onSelectNewAccount}
			addButtonLabel={<Trans>Add a different account</Trans>}
			scrollerKey="desktop-handoff-scroller"
			data-flx="auth.flow.desktop-handoff-account-selector.account-selector"
		/>
	);
});

export default DesktopHandoffAccountSelector;
