// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Button} from '@app/features/ui/button/Button';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {connectSso} from '@app/features/user/commands/SsoLinkCommands';
import {AccountSecurityRow} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const SINGLE_SIGN_ON_DESCRIPTOR = msg({
	message: 'Single sign-on',
	comment:
		'Label of the account settings row for connecting single sign-on when the admin has not given the provider a name.',
});
const CONNECT_SSO_FAILED_DESCRIPTOR = msg({
	message: "Couldn't connect single sign-on",
	comment: 'Error title shown when starting to connect single sign-on from account settings fails.',
});

export function canConnectSso(user: User): boolean {
	return (
		RuntimeConfig.getSnapshot().sso?.enabled === true &&
		!isDesktop() &&
		!user.traits.some((trait) => trait.startsWith('sso_identity:'))
	);
}

export const SsoConnectRow = observer(() => {
	const {i18n} = useLingui();
	const [connecting, setConnecting] = useState(false);
	const providerName = RuntimeConfig.getSnapshot().sso?.display_name ?? i18n._(SINGLE_SIGN_ON_DESCRIPTOR);
	const connect = useCallback(async () => {
		setConnecting(true);
		try {
			await connectSso();
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error, i18n._(CONNECT_SSO_FAILED_DESCRIPTOR));
			setConnecting(false);
		}
	}, [i18n]);
	return (
		<AccountSecurityRow
			label={<bdi data-flx="user.account-security-tab.sso-connect-row.bdi">{providerName}</bdi>}
			description={
				<Trans comment="Description of the account settings row that connects the account to the instance's single sign-on provider. providerName is the provider name typed by the admin.">
					Lets you sign in to this account with {providerName}.
				</Trans>
			}
			data-flx="user.account-security-tab.sso-connect-row"
		>
			<Button
				small={true}
				variant="secondary"
				submitting={connecting}
				onClick={connect}
				data-flx="user.account-security-tab.sso-connect-row.button.connect"
			>
				<Trans comment="Button in account settings that connects the account to single sign-on.">Connect</Trans>
			</Button>
		</AccountSecurityRow>
	);
});
