// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import authStyles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow.module.css';
import pageStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthSsoPanel, isInstanceSsoAvailable, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import Theme from '@app/features/theme/state/Theme';
import {flxElementClassName} from '@app/lib/react';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface AccountSwitcherRegisterStepProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly onRegister: (response: AuthenticationCommands.TokenResponse) => Promise<void>;
	readonly onBrowserLogin: (payload: LoginSuccessPayload) => Promise<void>;
}

export const AccountSwitcherRegisterStep = observer(function AccountSwitcherRegisterStep({
	runtimeSnapshot,
	onRegister,
	onBrowserLogin,
}: AccountSwitcherRegisterStepProps): React.ReactElement {
	const ssoConfig = resolveAuthPanelSso(runtimeSnapshot);
	if (isInstanceSsoAvailable(ssoConfig) && ssoConfig?.enforced === true) {
		return (
			<AuthSsoPanel
				runtimeSnapshot={runtimeSnapshot}
				onBrowserLoginSuccess={onBrowserLogin}
				dataFlx="auth.accounts.account-switcher-modal.account-switcher-register-step.sso-panel"
				data-flx="auth.accounts.account-switcher-modal.account-switcher-register-step.auth-sso-panel"
			/>
		);
	}
	return (
		<>
			<h1
				className={pageStyles.title}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-register-step.title"
			>
				<Trans>Create an account</Trans>
			</h1>
			<flx-auth-account-switcher-register-step
				className={flxElementClassName(authStyles.inlineRegisterContainer)}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-register-step.container"
			>
				<AuthRegisterFormCore
					fields={{
						showEmail: true,
						showPassword: true,
						showPasswordConfirmation: true,
						showUsernameValidation: true,
					}}
					submitLabel={<Trans>Create account</Trans>}
					redirectPath=""
					runtimeSnapshot={runtimeSnapshot}
					onRegister={onRegister}
					showLegalConsent
					theme={Theme.themePreference}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-register-step.auth-register-form-core"
				/>
			</flx-auth-account-switcher-register-step>
		</>
	);
});
