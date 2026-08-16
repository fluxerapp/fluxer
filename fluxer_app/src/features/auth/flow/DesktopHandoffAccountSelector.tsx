// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountSelector} from '@app/features/auth/components/accounts/AccountSelector';
import AccountManager from '@app/features/auth/state/AccountManager';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const SESSION_EXPIRED_PLEASE_SIGN_IN_AGAIN_DESCRIPTOR = msg({
	message: 'Session expired. Sign in again.',
	comment:
		'Toast error shown in the desktop-handoff account picker when the session for the chosen account has expired.',
});
const FAILED_TO_GENERATE_TOKEN_DESCRIPTOR = msg({
	message: 'Failed to generate token',
	comment: 'Short label in the authentication desktop handoff account selector. Keep the tone plain and specific.',
});

interface DesktopHandoffAccountSelectorProps {
	excludeCurrentUser?: boolean;
	onSelectNewAccount: () => void;
	onAccountSelected: (payload: {token: string; userId: string}) => void;
}

const DesktopHandoffAccountSelector = observer(function DesktopHandoffAccountSelector({
	excludeCurrentUser = false,
	onSelectNewAccount,
	onAccountSelected,
}: DesktopHandoffAccountSelectorProps) {
	const {i18n} = useLingui();
	const [isLoading, setIsLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const currentUserId = AccountManager.currentUserId;
	const allAccounts = AccountManager.orderedAccounts;
	const accounts = excludeCurrentUser ? allAccounts.filter((account) => account.userId !== currentUserId) : allAccounts;
	const handleSelectAccount = useCallback(
		async (account: Account) => {
			setIsLoading(true);
			setError(null);
			try {
				const {token, userId} = await AccountManager.generateTokenForAccount(account.userId);
				if (!token) {
					throw new Error('Failed to generate token');
				}
				onAccountSelected({token, userId});
			} catch (err) {
				if (err instanceof SessionExpiredError) {
					setError(i18n._(SESSION_EXPIRED_PLEASE_SIGN_IN_AGAIN_DESCRIPTOR));
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
		[onAccountSelected, i18n],
	);
	return (
		<AccountSelector
			accounts={accounts}
			title={<Trans>Choose an account</Trans>}
			description={<Trans>Select the account you want to sign in with on the desktop app.</Trans>}
			disabled={isLoading}
			error={error}
			clickableRows
			onSelectAccount={handleSelectAccount}
			onAddAccount={onSelectNewAccount}
			addButtonLabel={<Trans>Add a different account</Trans>}
			scrollerKey="desktop-handoff-scroller"
			data-flx="auth.flow.desktop-handoff-account-selector.account-selector"
		/>
	);
});

export default DesktopHandoffAccountSelector;
