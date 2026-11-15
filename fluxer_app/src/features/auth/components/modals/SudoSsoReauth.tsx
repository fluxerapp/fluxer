// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {finishSsoReauthInThisWindow} from '@app/features/auth/components/modals/SudoSsoReauthCompletion';
import {AuthSsoPanel} from '@app/features/auth/flow/AuthSsoPanel';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Trans} from '@lingui/react/macro';
import {useCallback, useState} from 'react';

export function SudoSsoReauth() {
	const [wrongAccount, setWrongAccount] = useState(false);
	const handleBrowserLoginSuccess = useCallback(async (payload: LoginSuccessPayload) => {
		setWrongAccount(!(await finishSsoReauthInThisWindow(payload, Authentication.currentUserId)));
	}, []);
	return (
		<>
			<Modal.Description data-flx="auth.sudo-sso-reauth.hint">
				<Trans comment="Shown above the single sign-on button when confirming it's you. Signing in can leave the page, and the person repeats the action afterwards.">
					After you sign in, try the action again.
				</Trans>
			</Modal.Description>
			{wrongAccount && (
				<Modal.Description role="alert" data-flx="auth.sudo-sso-reauth.wrong-account">
					<Trans comment="Error shown when the person confirmed it's them by signing in to a different account than the one they are using.">
						You signed in to a different account. Sign in with the account you're using here.
					</Trans>
				</Modal.Description>
			)}
			<AuthSsoPanel
				redirectPath={RouterUtils.getCurrentPath()}
				showTitle={false}
				onBrowserLoginSuccess={handleBrowserLoginSuccess}
				dataFlx="auth.sudo-verification-modal.sso-reauth"
				data-flx="auth.sudo-sso-reauth.auth-sso-panel"
			/>
		</>
	);
}
