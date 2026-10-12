// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthLoginStepCarousel} from '@app/features/auth/flow/AuthLoginStepCarousel';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {AuthSsoPanel, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';
import {useCallback, useMemo} from 'react';

export type ResourceAuthMode = 'login' | 'register';
export interface ResourceAuthState<T> {
	loading: boolean;
	error: boolean;
	resource: T | null;
}
export interface ResourceAuthPageConfig<T> {
	kind: 'gift' | 'theme';
	dataFlxPrefix: (mode: ResourceAuthMode) => string;
	useResource: (id: string, snapshot: RuntimeConfigSnapshot) => ResourceAuthState<T>;
	renderHeader: (resource: T, mode: ResourceAuthMode, sso: boolean) => ReactNode;
	renderBlockingState?: (resource: T, prefix: string) => ReactNode;
	documentTitle: (i18n: I18n) => string;
	errorTitle: (i18n: I18n) => ReactNode;
	errorText: ReactNode;
	registerPath: (id: string) => string;
	loginPath: (id: string, safeRedirect: string | null) => string;
	redirectPath: (id: string, safeRedirect: string | null) => string;
	fallbackRedirectPath: (id: string) => string;
	registerRedirectPath: (id: string, safeRedirect: string | null) => string;
	onComplete: (id: string, i18n: I18n, snapshot: RuntimeConfigSnapshot) => void | Promise<void>;
	preferLogin?: boolean;
	headerOutsideForm?: boolean;
	submitLabel: ReactNode;
	extraContent?: ReactNode;
}
interface ResourceAuthProps<T> {
	id: string;
	config: ResourceAuthPageConfig<T>;
}
interface ResourceAuthContentProps<T> extends ResourceAuthProps<T> {
	mode: ResourceAuthMode;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

const ResourceAuthContent = observer(function ResourceAuthContent<T>({
	id,
	config,
	mode,
	runtimeSnapshot,
}: ResourceAuthContentProps<T>) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const safeRedirect = safeRedirectTarget(params.get('redirect_to'));
	const state = config.useResource(id, runtimeSnapshot);
	useFluxerDocumentTitle(config.documentTitle(i18n));
	// Registration keeps the standard auth card presentation even while resource loading blocks the form.
	useAuthPresentation({variant: AuthCardVariant.STANDARD, enabled: mode === 'register'});
	const onLoginComplete = useCallback(async () => {
		const completion = config.onComplete(id, i18n, runtimeSnapshot);
		if (completion) await completion;
		runtimeTarget.reset();
	}, [id, config, i18n, runtimeSnapshot, runtimeTarget]);
	const onRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				runtimeSnapshot,
				...(userData ? {userData} : {}),
			});
			await config.onComplete(id, i18n, runtimeSnapshot);
		},
		[id, config, i18n, runtimeSnapshot],
	);
	const prefix = config.dataFlxPrefix(mode);
	const contentPrefix = mode === 'login' ? `${prefix}.${config.kind}-login-page-container` : prefix;
	if (state.loading) return <AuthLoadingState data-flx={`${contentPrefix}.auth-loading-state`} />;
	if (state.error || !state.resource)
		return (
			<AuthErrorState
				title={config.errorTitle(i18n)}
				text={config.errorText}
				action={<AuthRuntimeTargetResetAction data-flx={`${contentPrefix}.auth-runtime-target-reset-action`} />}
				data-flx={`${contentPrefix}.auth-error-state`}
			/>
		);
	const blocking = config.renderBlockingState?.(state.resource, contentPrefix);
	if (blocking) return blocking;
	if (mode === 'login')
		return (
			<AuthLoginStepCarousel
				ariaLabel={config.documentTitle(i18n)}
				mfaFallbackRedirectPath={config.fallbackRedirectPath(id)}
				onLoginComplete={onLoginComplete}
				carouselDataFlx={`${prefix}.container-carousel`}
				mfaStepDataFlx={`${contentPrefix}.desktop-handoff-mfa-step`}
				data-flx="auth.flow.resource-auth-pages.resource-auth-content.auth-login-step-carousel"
			>
				<AuthLoginLayout
					redirectPath={config.redirectPath(id, safeRedirect)}
					inviteCode={null}
					desktopHandoff={isHandoffRequest(params)}
					excludeCurrentUser={false}
					extraTopContent={
						<>
							<DesktopDeepLinkPrompt
								code={id}
								kind={config.kind}
								preferLogin={config.preferLogin}
								data-flx={`${prefix}.desktop-deep-link-prompt`}
							/>
							{config.renderHeader(state.resource, 'login', false)}
						</>
					}
					forgotPasswordAction={null}
					showTitle={false}
					title={null}
					onBackActionChange={null}
					completeLoginRedirectPath={null}
					forceCredentials={false}
					startWithAddAccount={false}
					runtimeTarget={runtimeTarget}
					ssoRedirectPath={null}
					suppressInlineBackButtons={false}
					initialIdentifier={null}
					registerLink={
						<AuthRouterLink
							to={config.registerPath(id)}
							search={safeRedirect == null ? undefined : {redirect_to: safeRedirect}}
							data-flx={`${prefix}.auth-router-link`}
						>
							{i18n._(REGISTER_DESCRIPTOR)}
						</AuthRouterLink>
					}
					onLoginComplete={onLoginComplete}
					data-flx={`${prefix}.auth-login-layout`}
				/>
			</AuthLoginStepCarousel>
		);
	const runtimeSso = resolveAuthPanelSso(runtimeSnapshot);
	const sso = runtimeSso?.enabled === true && runtimeSso.enforced === true;
	const usernameSignIn = usesUsernameSignIn(runtimeSnapshot.features);
	const header = (
		<>
			<DesktopDeepLinkPrompt
				code={id}
				kind={config.kind}
				data-flx={`${prefix}.desktop-deep-link-prompt${sso ? '.sso' : ''}`}
			/>
			{config.renderHeader(state.resource, 'register', sso)}
		</>
	);
	const form = (
		<div className={sharedStyles.container} data-flx={`${prefix}.${sso ? 'sso-container' : 'div'}`}>
			{config.headerOutsideForm ? null : header}
			{sso ? (
				<AuthSsoPanel
					redirectPath={config.redirectPath(id, safeRedirect)}
					runtimeSnapshot={runtimeSnapshot}
					dataFlx={`${prefix}.sso-panel`}
					data-flx={`${prefix}.auth-sso-panel`}
				/>
			) : (
				<AuthRegisterFormCore
					fields={
						usernameSignIn
							? {
									showPassword: true,
									showPasswordConfirmation: true,
									showUsernameValidation: true,
									requireUsername: true,
								}
							: {showUsername: false}
					}
					submitLabel={config.submitLabel}
					redirectPath={config.registerRedirectPath(id, safeRedirect)}
					offerRecoveryKit={usernameSignIn}
					onRegister={onRegisterComplete}
					runtimeSnapshot={runtimeSnapshot}
					onAuthenticated={runtimeTarget.reset}
					extraContent={config.extraContent}
					data-flx={`${prefix}.auth-minimal-register-form-core`}
				/>
			)}
			<AuthBottomLink
				variant="login"
				to={config.loginPath(id, safeRedirect)}
				data-flx={`${prefix}.auth-bottom-link${sso ? '.sso' : ''}`}
			/>
		</div>
	);
	return config.headerOutsideForm ? (
		<>
			{header}
			{form}
		</>
	) : (
		form
	);
});
function ResourceAuthPage<T>({id, config, mode}: ResourceAuthProps<T> & {mode: ResourceAuthMode}) {
	return (
		<AuthRuntimeTargetGate data-flx={`${config.dataFlxPrefix(mode)}.runtime-target-gate`}>
			{(snapshot) => (
				<ResourceAuthContent
					id={id}
					config={config}
					mode={mode}
					runtimeSnapshot={snapshot}
					data-flx="auth.flow.resource-auth-pages.resource-auth-page.resource-auth-content"
				/>
			)}
		</AuthRuntimeTargetGate>
	);
}
export function ResourceLoginPage<T>(props: ResourceAuthProps<T>) {
	return (
		<ResourceAuthPage
			data-flx="auth.flow.resource-auth-pages.resource-login-page.resource-auth-page"
			{...props}
			mode="login"
		/>
	);
}
export function ResourceRegisterPage<T>(props: ResourceAuthProps<T>) {
	return (
		<ResourceAuthPage
			data-flx="auth.flow.resource-auth-pages.resource-register-page.resource-auth-page"
			{...props}
			mode="register"
		/>
	);
}
