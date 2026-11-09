// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
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
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserTraits} from '@fluxer/constants/src/UserTraitConstants';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const SINGLE_SIGN_ON_DESCRIPTOR = msg({
	message: 'Single sign-on',
	comment: 'Fallback label for the SSO provider in account security when no display name is configured.',
});
const DISCONNECT_SSO_BUTTON_DESCRIPTOR = msg({
	message: 'Disconnect',
	comment: 'Button to disconnect the account from single sign-on.',
});
const DISCONNECT_SSO_FAILED_DESCRIPTOR = msg({
	message: "Couldn't disconnect single sign-on",
	comment: 'Error title shown when disconnecting single sign-on fails.',
});

export function isSsoLinked(user: User): boolean {
	return user.traits.includes(UserTraits.SSO);
}

export function canDisconnectSso(user: User): boolean {
	if (!isSsoLinked(user) || RuntimeConfig.getSnapshot().sso?.enforced === true) return false;
	if (user.hasPassword) return true;
	return (
		RuntimeConfig.getSnapshot().features.identifier_first === true &&
		!RuntimeConfig.usesUsernameSignIn &&
		RuntimeConfig.emailsEnabled &&
		user.email != null &&
		user.verified === true &&
		user.emailBounced !== true
	);
}

function getDisconnectDescription(canDisconnect: boolean, isEnforced: boolean, providerName: string) {
	if (canDisconnect)
		return (
			<Trans comment="Description of the account settings row that disconnects the account from the instance's single sign-on provider.">
				Connected. You can disconnect this sign-in method.
			</Trans>
		);
	if (isEnforced)
		return (
			<Trans comment="Description of the account settings row that disconnects the account from the instance's single sign-on provider. providerName is the provider name typed by the admin.">
				This instance requires {providerName}.
			</Trans>
		);
	return (
		<Trans comment="Description of the account settings row that disconnects the account from the instance's single sign-on provider. providerName is the provider name typed by the admin.">
			Add a local sign-in method before disconnecting {providerName}.
		</Trans>
	);
}

function isLocalSignInMethodRequiredError(error: unknown): boolean {
	if (!(error instanceof HttpError)) return false;

	return isRecord(error.body) && error.body.code === APIErrorCodes.LOCAL_SIGN_IN_METHOD_REQUIRED;
}

export const SsoDisconnectRow = observer(({user}: {user: User}) => {
	const {i18n} = useLingui();
	const providerName = RuntimeConfig.getSnapshot().sso?.display_name ?? i18n._(SINGLE_SIGN_ON_DESCRIPTOR);
	const canDisconnect = canDisconnectSso(user);
	const isEnforced = RuntimeConfig.getSnapshot().sso?.enforced === true;
	const handleDisconnect = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={
						<Trans comment="Confirmation title when disconnecting the current account from the instance's single sign-on provider.">
							Disconnect {providerName}?
						</Trans>
					}
					description={
						<Trans comment="Warning shown before disconnecting single sign-on.">
							You will disconnect {providerName} from your account. You will be able to sign in to your account using a
							different method.
						</Trans>
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

							if (isLocalSignInMethodRequiredError(error)) {
								ModalCommands.push(
									modal(() => (
										<GenericErrorModal
											title={i18n._(DISCONNECT_SSO_FAILED_DESCRIPTOR)}
											message={getDisconnectDescription(false, false, providerName)}
											data-flx="user.account-security-tab.sso-disconnect-row.local-sign-in-required-modal"
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
	}, [i18n]);

	return (
		<AccountSecurityRow
			label={<bdi data-flx="user.account-security-tab.sso-disconnect-row.provider-name">{providerName}</bdi>}
			description={getDisconnectDescription(canDisconnect, isEnforced, providerName)}
			data-flx="user.account-security-tab.sso-disconnect-row"
		>
			{canDisconnect && (
				<Button
					variant="secondary"
					small={true}
					onClick={handleDisconnect}
					data-flx="user.account-security-tab.sso-disconnect-row.button.disconnect"
				>
					<Trans>Disconnect</Trans>
				</Button>
			)}
		</AccountSecurityRow>
	);
});
