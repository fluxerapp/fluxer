// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthSsoPanel, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import {AUTH_LOGIN_METHOD_QUERY_PARAM} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {AuthClientPermissionStep} from '@app/features/auth/flow/client_intro/AuthClientPermissionStep';
import {AuthClientPreferencesStep} from '@app/features/auth/flow/client_intro/AuthClientPreferencesStep';
import {useDesktopClientIntroFlow} from '@app/features/auth/flow/client_intro/useDesktopClientIntroFlow';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import type {PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const RegisterPageStep = Object.freeze({
	PREFERENCES: 'preferences',
	PERMISSION_MICROPHONE: 'permission_microphone',
	PERMISSION_CAMERA: 'permission_camera',
	PERMISSION_SCREEN: 'permission_screen',
	PERMISSION_INPUT_MONITORING: 'permission_input_monitoring',
	FORM: 'form',
} as const);

type RegisterPageStep = (typeof RegisterPageStep)[keyof typeof RegisterPageStep];

const REGISTER_PAGE_STEPS: ReadonlyArray<RegisterPageStep> = Object.freeze(Object.values(RegisterPageStep));

const REGISTER_PERMISSION_STEPS: Readonly<Record<PermissionKind, RegisterPageStep>> = Object.freeze({
	microphone: RegisterPageStep.PERMISSION_MICROPHONE,
	camera: RegisterPageStep.PERMISSION_CAMERA,
	screen: RegisterPageStep.PERMISSION_SCREEN,
	'input-monitoring': RegisterPageStep.PERMISSION_INPUT_MONITORING,
});

function selectRegisterPageStep(showPreferences: boolean, permission: PermissionKind | null): RegisterPageStep {
	if (showPreferences) return RegisterPageStep.PREFERENCES;
	if (permission != null) return REGISTER_PERMISSION_STEPS[permission];
	return RegisterPageStep.FORM;
}

function isSsoEnforcedForSnapshot(runtimeSnapshot: RuntimeConfigSnapshot): boolean {
	const ssoConfig = resolveAuthPanelSso(runtimeSnapshot);
	return Boolean(ssoConfig?.enabled && ssoConfig.enforced);
}

interface RegisterPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const RegisterPageContent = observer(function RegisterPageContent({runtimeSnapshot}: RegisterPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = new URLSearchParams(location.search);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const redirectTo = safeRedirect ?? '/';
	const loginPath = setPathQueryParams('/login', {
		redirect_to: safeRedirect,
		[AUTH_LOGIN_METHOD_QUERY_PARAM.name]: AUTH_LOGIN_METHOD_QUERY_PARAM.value,
	});
	const clientIntro = useDesktopClientIntroFlow(isDesktop(), {skipWelcome: true});
	const registerStep = selectRegisterPageStep(clientIntro.showPreferences, clientIntro.permission);
	const usernameSignIn = usesUsernameSignIn(runtimeSnapshot.features);
	if (isSsoEnforcedForSnapshot(runtimeSnapshot)) {
		return (
			<div className={sharedStyles.container} data-flx="auth.register-page.register-page-content.sso-container">
				<AuthSsoPanel
					redirectPath={redirectTo}
					runtimeSnapshot={runtimeSnapshot}
					dataFlx="auth.register-page.register-page-content.sso-panel"
					data-flx="auth.register-page.register-page-content.auth-sso-panel"
				/>
			</div>
		);
	}
	const renderRegistrationStep = (): ReactNode => {
		if (registerStep === RegisterPageStep.PREFERENCES) {
			return (
				<AuthClientPreferencesStep
					onContinue={clientIntro.completePreferences}
					data-flx="auth.register-page.register-page-content.client-preferences-step"
				/>
			);
		}
		if (clientIntro.permission != null) {
			return (
				<AuthClientPermissionStep
					kind={clientIntro.permission}
					index={clientIntro.permissionIndex}
					count={clientIntro.permissionCount}
					onAllowed={clientIntro.advancePermission}
					onNotNow={clientIntro.skipPermission}
					data-flx="auth.register-page.register-page-content.client-permission-step"
				/>
			);
		}
		return (
			<>
				<h1 className={sharedStyles.title} data-flx="auth.register-page.register-page-content.h1">
					<Trans>Create an account</Trans>
				</h1>
				<div className={sharedStyles.container} data-flx="auth.register-page.register-page-content.div">
					<AuthRegisterFormCore
						fields={{
							showEmail: !usernameSignIn,
							showPassword: true,
							showPasswordConfirmation: true,
							showUsernameValidation: true,
							requireUsername: usernameSignIn,
						}}
						submitLabel={<Trans>Create account</Trans>}
						redirectPath={redirectTo}
						offerRecoveryKit={usernameSignIn}
						runtimeSnapshot={runtimeSnapshot}
						onAuthenticated={runtimeTarget.reset}
						data-flx="auth.register-page.register-page-content.auth-register-form-core"
					/>
					<AuthBottomLink
						variant="login"
						to={loginPath}
						data-flx="auth.register-page.register-page-content.auth-bottom-link"
					/>
				</div>
			</>
		);
	};
	return (
		<SteppedCarousel
			step={registerStep}
			steps={REGISTER_PAGE_STEPS}
			focusOnStepChange
			ariaLabel={i18n._(REGISTER_DESCRIPTOR)}
			data-flx="auth.register-page.register-page-content.stepped-carousel"
		>
			{renderRegistrationStep()}
		</SteppedCarousel>
	);
});
const RegisterPage = observer(function RegisterPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(REGISTER_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	return (
		<AuthRuntimeTargetGate data-flx="auth.register-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<RegisterPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.register-page.register-page-content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default RegisterPage;
