// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthSsoPanel, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {InviteHeader} from '@app/features/auth/flow/InviteHeader';
import {linkRegisterFields, readRegistrationUrlCode} from '@app/features/auth/flow/RegisterFormFields';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {CREATE_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	InviteAuthPageState,
	InvitesPausedPanel,
	useInviteAuthPage,
} from '@app/features/invite/components/pages/InviteAuthPageState';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

interface InviteRegisterPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const InviteRegisterPageContent = observer(function InviteRegisterPageContent({
	runtimeSnapshot,
}: InviteRegisterPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const {code} = useParams() as {code: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const ssoRedirectPath = useMemo(() => {
		return setPathQueryParams(Routes.inviteRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	const loginPath = safeRedirect
		? setPathQueryParams(Routes.inviteLogin(code), {redirect_to: safeRedirect})
		: Routes.inviteLogin(code);

	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	const {invite, loading, guildInvite, isGroupDM, isInvitesDisabled, isRaidDetected} = useInviteAuthPage(
		code,
		runtimeSnapshot,
	);
	if (!invite)
		return (
			<InviteAuthPageState
				loading={loading}
				prefix="invite.invite-register-page"
				data-flx="invite.invite-register-page.invite-register-page-content.invite-auth-page-state"
			/>
		);
	if (isInvitesDisabled && !isGroupDM)
		return (
			<InvitesPausedPanel
				code={code}
				guildInvite={guildInvite}
				isRaidDetected={isRaidDetected}
				prefix="invite.invite-register-page"
				data-flx="invite.invite-register-page.invite-register-page-content.invites-paused-panel"
			/>
		);

	const runtimeSso = resolveAuthPanelSso(runtimeSnapshot);
	if (runtimeSso?.enabled === true && runtimeSso.enforced === true) {
		return (
			<>
				<DesktopDeepLinkPrompt
					code={code}
					kind="invite"
					data-flx="invite.invite-register-page.desktop-deep-link-prompt.sso"
				/>
				<InviteHeader invite={invite} data-flx="invite.invite-register-page.invite-header.sso" />
				<div className={sharedStyles.container} data-flx="invite.invite-register-page.sso-container">
					<AuthSsoPanel
						redirectPath={ssoRedirectPath}
						runtimeSnapshot={runtimeSnapshot}
						dataFlx="invite.invite-register-page.sso-panel"
						data-flx="invite.invite-register-page.auth-sso-panel"
					/>
					<AuthBottomLink variant="login" to={loginPath} data-flx="invite.invite-register-page.auth-bottom-link.sso" />
				</div>
			</>
		);
	}
	const usernameSignIn = usesUsernameSignIn(runtimeSnapshot.features);
	return (
		<>
			<DesktopDeepLinkPrompt
				code={code}
				kind="invite"
				data-flx="invite.invite-register-page.desktop-deep-link-prompt--2"
			/>
			<InviteHeader invite={invite} data-flx="invite.invite-register-page.invite-header" />
			<div className={sharedStyles.container} data-flx="invite.invite-register-page.div--4">
				<AuthRegisterFormCore
					fields={linkRegisterFields(runtimeSnapshot, readRegistrationUrlCode(params))}
					submitLabel={i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
					redirectPath="/"
					offerRecoveryKit={usernameSignIn}
					inviteCode={code}
					runtimeSnapshot={runtimeSnapshot}
					onAuthenticated={runtimeTarget.reset}
					data-flx="invite.invite-register-page.auth-minimal-register-form-core"
				/>
				<AuthBottomLink variant="login" to={loginPath} data-flx="invite.invite-register-page.auth-bottom-link" />
			</div>
		</>
	);
});

const InviteRegisterPage = observer(() => (
	<AuthRuntimeTargetGate data-flx="invite.invite-register-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<InviteRegisterPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="invite.invite-register-page.invite-register-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default InviteRegisterPage;
