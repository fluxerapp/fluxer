// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {DesktopHandoffMode, isApprovalFlowMode} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {isHandoffRequest, useDesktopHandoffFlow} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {HandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

interface DesktopHandoffMfaStepProps {
	readonly fallbackRedirectPath: string;
	readonly onLoginComplete?: (() => void) | null;
}

export const DesktopHandoffMfaStep = observer(function DesktopHandoffMfaStep({
	fallbackRedirectPath,
	onLoginComplete,
}: DesktopHandoffMfaStepProps) {
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const isHandoff = isHandoffRequest(params);
	const rawRedirect = params.get('redirect_to');
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const mfaRuntimeSnapshot = Authentication.currentMfaRuntimeSnapshot;
	const hasStoredAccounts = switcherAccounts().length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isHandoff,
		hasStoredAccounts,
		initialMode: DesktopHandoffMode.IDLE,
	});
	const handleMfaSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (mfaRuntimeSnapshot == null) {
				return;
			}
			if (isHandoff) {
				await Accounts.refreshStoredAccount({
					userId: payload.userId,
					token: payload.token,
					userData: payload.userData,
					runtimeSnapshot: mfaRuntimeSnapshot,
				});
				await handoff.start({...payload, runtimeSnapshot: mfaRuntimeSnapshot});
				return;
			}
			await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot: mfaRuntimeSnapshot});
			if (onLoginComplete != null) {
				onLoginComplete();
			}
			AuthenticationCommands.clearMfaTicket();
			RouterUtils.replaceWith(safeRedirectTargetOrFallback(rawRedirect, fallbackRedirectPath));
		},
		[fallbackRedirectPath, handoff, isHandoff, mfaRuntimeSnapshot, onLoginComplete, rawRedirect],
	);
	const handleCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
	}, []);
	const handleHandoffRetry = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
		handoff.retry();
	}, [handoff]);
	if (mfaTicket == null || mfaMethods == null || mfaRuntimeSnapshot == null) {
		return null;
	}
	if (isHandoff && isApprovalFlowMode(handoff.mode)) {
		return (
			<HandoffApprovalFlow
				mode={handoff.mode}
				error={handoff.error}
				clientInfo={handoff.clientInfo}
				onProceedToCodeInput={handoff.proceedToCodeInput}
				onSubmitCode={handoff.submitCode}
				onApprove={handoff.approve}
				onDeny={handoff.deny}
				onRetry={handleHandoffRetry}
				data-flx="auth.flow.desktop-handoff-mfa-step.handoff-approval-flow"
			/>
		);
	}
	return (
		<MfaScreen
			challenge={{ticket: mfaTicket, ...mfaMethods}}
			onSuccess={handleMfaSuccess}
			onCancel={handleCancel}
			data-flx="auth.flow.desktop-handoff-mfa-step.mfa-screen"
		/>
	);
});
