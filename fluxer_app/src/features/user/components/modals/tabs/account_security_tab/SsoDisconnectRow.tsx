// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitedConfirmModal} from '@app/features/app/components/alerts/RateLimitedConfirmModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {disconnectSso} from '@app/features/user/commands/SsoLinkCommands';
import {AccountSecurityRow} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import {SINGLE_SIGN_ON_DESCRIPTOR} from '@app/features/user/components/modals/tabs/account_security_tab/SsoConnectRow';
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import {LocalCredentialKinds} from '@fluxer/constants/src/AccountTierConstants';
import {UserTraits} from '@fluxer/constants/src/UserTraitConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const DISCONNECT_SSO_BUTTON_DESCRIPTOR = msg({
	message: 'Disconnect',
	context: 'sso',
	comment: 'Button that disconnects the account from single sign-on.',
});
const DISCONNECT_SSO_FAILED_DESCRIPTOR = msg({
	message: "Couldn't disconnect single sign-on",
	comment: 'Error title shown when disconnecting single sign-on fails.',
});

export function isSsoLinked(user: User): boolean {
	return user.traits.includes(UserTraits.SSO);
}

function isSsoEnforced(): boolean {
	return RuntimeConfig.getSnapshot().sso?.enforced === true;
}

function offersEmailCodes(): boolean {
	return RuntimeConfig.getSnapshot().features.email_codes === true;
}

function canDisconnectSso(user: User): boolean {
	if (isSsoEnforced()) return false;
	return (
		user.localCredential !== LocalCredentialKinds.NONE ||
		(offersEmailCodes() && user.email != null && user.verified === true && user.emailBounced !== true)
	);
}

function getDisconnectDescription(canDisconnect: boolean, providerName: string | null) {
	if (canDisconnect) {
		return providerName == null ? (
			<Trans comment="Description of the account settings row that disconnects the account from single sign-on when the admin has not given the provider a name.">
				You can sign in to this account with single sign-on.
			</Trans>
		) : (
			<Trans comment="Description of the account settings row that disconnects the account from the instance's single sign-on provider. providerName is the provider name typed by the admin.">
				You can sign in to this account with {providerName}.
			</Trans>
		);
	}
	if (isSsoEnforced()) {
		return providerName == null ? (
			<Trans comment="Description of the account settings row for single sign-on when the instance requires it and the admin has not given the provider a name.">
				This instance requires you to sign in with single sign-on.
			</Trans>
		) : (
			<Trans comment="Description of the account settings row for single sign-on when the instance requires it. providerName is the provider name typed by the admin.">
				This instance requires you to sign in with {providerName}.
			</Trans>
		);
	}
	if (offersEmailCodes()) {
		return providerName == null ? (
			<Trans comment="Description of the account settings row for single sign-on when the account has no other way to sign in, the instance allows sign-in with codes sent by email and the admin has not given the provider a name.">
				Add a password or passkey, or confirm your email address to sign in with emailed codes, before you disconnect
				single sign-on.
			</Trans>
		) : (
			<Trans comment="Description of the account settings row for single sign-on when the account has no other way to sign in and the instance allows sign-in with codes sent by email. providerName is the provider name typed by the admin.">
				Add a password or passkey, or confirm your email address to sign in with emailed codes, before you disconnect{' '}
				{providerName}.
			</Trans>
		);
	}
	return providerName == null ? (
		<Trans comment="Description of the account settings row for single sign-on when the account has no other way to sign in and the admin has not given the provider a name.">
			Add a password or passkey to your account before you disconnect single sign-on.
		</Trans>
	) : (
		<Trans comment="Description of the account settings row for single sign-on when the account has no other way to sign in. providerName is the provider name typed by the admin.">
			Add a password or passkey to your account before you disconnect {providerName}.
		</Trans>
	);
}

export const SsoDisconnectRow = observer(({user}: {user: User}) => {
	const {i18n} = useLingui();
	const providerName = RuntimeConfig.getSnapshot().sso?.display_name ?? null;
	const canDisconnect = canDisconnectSso(user);
	const handleDisconnect = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={
						providerName == null ? (
							<Trans comment="Confirmation title when disconnecting the current account from single sign-on and the admin has not given the provider a name.">
								Disconnect single sign-on?
							</Trans>
						) : (
							<Trans comment="Confirmation title when disconnecting the current account from the instance's single sign-on provider. providerName is the provider name typed by the admin.">
								Disconnect {providerName}?
							</Trans>
						)
					}
					description={
						providerName == null ? (
							<Trans comment="Warning shown before disconnecting single sign-on when the admin has not given the provider a name.">
								You won't be able to sign in with single sign-on anymore. You can still use your other sign-in methods.
							</Trans>
						) : (
							<Trans comment="Warning shown before disconnecting single sign-on. providerName is the provider name typed by the admin.">
								You won't be able to sign in with {providerName} anymore. You can still use your other sign-in methods.
							</Trans>
						)
					}
					primaryText={i18n._(DISCONNECT_SSO_BUTTON_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await disconnectSso();
						} catch (error) {
							if (error instanceof HttpError && error.status === 429) {
								const retryAfterMs = resolveRetryAfterMs(error);

								ModalCommands.push(
									modal(() => (
										<RateLimitedConfirmModal
											title={i18n._(DISCONNECT_SSO_FAILED_DESCRIPTOR)}
											retryAfter={retryAfterMs === null ? undefined : retryAfterMs / 1000}
											data-flx="user.account-security-tab.sso-disconnect-row.rate-limited-modal"
										/>
									)),
								);
								return;
							}

							FormUtils.pushApiErrorModal(i18n, error, i18n._(DISCONNECT_SSO_FAILED_DESCRIPTOR));
						}
					}}
					data-flx="user.account-security-tab.sso-disconnect-row.confirm-modal"
				/>
			)),
		);
	}, [i18n, providerName]);

	return (
		<AccountSecurityRow
			label={
				<bdi data-flx="user.account-security-tab.sso-disconnect-row.provider-name">
					{providerName ?? i18n._(SINGLE_SIGN_ON_DESCRIPTOR)}
				</bdi>
			}
			description={getDisconnectDescription(canDisconnect, providerName)}
			data-flx="user.account-security-tab.sso-disconnect-row"
		>
			{canDisconnect && (
				<Button
					variant="secondary"
					small={true}
					onClick={handleDisconnect}
					data-flx="user.account-security-tab.sso-disconnect-row.button.disconnect"
				>
					{i18n._(DISCONNECT_SSO_BUTTON_DESCRIPTOR)}
				</Button>
			)}
		</AccountSecurityRow>
	);
});
