// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {useAccountDisplayIdentityView} from '@app/features/auth/components/accounts/AccountIdentity';
import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import Accounts from '@app/features/auth/state/Accounts';
import type {Account} from '@app/features/platform/state/AuthSession';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const AuthorizingAccountRow = observer(function AuthorizingAccountRow({
	account,
}: {
	readonly account: Account;
}): React.ReactElement {
	const avatarUrl = useAccountAvatarURL(account);
	const identityView = useAccountDisplayIdentityView(account);
	const displayName = identityView.displayLabel;
	return (
		<p className={styles.authorizingAccount} data-flx="auth.o-auth-authorize-page.authorizing-account.row">
			<span
				className={styles.authorizingAccountAvatar}
				data-flx="auth.o-auth-authorize-page.authorizing-account.avatar"
			>
				<MockAvatar
					size={24}
					avatarUrl={avatarUrl}
					userTag={identityView.tagLabel}
					data-flx="auth.o-auth-authorize-page.authorizing-account.mock-avatar"
				/>
			</span>
			<span
				className={styles.authorizingAccountText}
				dir="auto"
				data-flx="auth.o-auth-authorize-page.authorizing-account.text"
			>
				<Trans comment="Line on the app authorization page that names the account the app will get access to. The tag wraps the account display name.">
					Signed in as{' '}
					<strong data-flx="auth.oauth-authorize-page.o-auth-authorizing-account.authorizing-account-row.strong">
						{displayName}
					</strong>
				</Trans>
			</span>
		</p>
	);
});

export const OAuthAuthorizingAccount = observer(function OAuthAuthorizingAccount(): React.ReactElement | null {
	const account = Accounts.currentAccount;
	if (account == null) {
		return null;
	}
	return (
		<AuthorizingAccountRow account={account} data-flx="auth.o-auth-authorize-page.authorizing-account.account-row" />
	);
});
