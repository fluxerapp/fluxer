// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthLoginStepCarousel} from '@app/features/auth/flow/AuthLoginStepCarousel';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {InviteHeader} from '@app/features/auth/flow/InviteHeader';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	InviteAuthPageState,
	InvitesPausedPanel,
	useInviteAuthPage,
} from '@app/features/invite/components/pages/InviteAuthPageState';
import {ACCEPT_INVITE_DESCRIPTOR} from '@app/features/invite/utils/InviteMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

interface InviteLoginPageProps {
	readonly code: string;
	readonly invite: Invite;
}

const InviteLoginPage = observer(function InviteLoginPage({code, invite}: InviteLoginPageProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const safeRedirect = safeRedirectTarget(params.get('redirect_to'));
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect == null ? undefined : {redirect_to: safeRedirect};
	const redirectPath = useMemo(() => {
		return setPathQueryParams(Routes.inviteRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			inviteCode={code}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
			extraTopContent={
				<div className={sharedStyles.loginHeader} data-flx="invite.invite-login-page.login-header">
					<DesktopDeepLinkPrompt
						code={code}
						kind="invite"
						preferLogin={true}
						data-flx="invite.invite-login-page.desktop-deep-link-prompt"
					/>
					<InviteHeader invite={invite} data-flx="invite.invite-login-page.invite-header" />
				</div>
			}
			forgotPasswordAction={null}
			showTitle={false}
			title={null}
			onLoginComplete={runtimeTarget.reset}
			onBackActionChange={null}
			completeLoginRedirectPath={Routes.ME}
			forceCredentials={false}
			startWithAddAccount={false}
			runtimeTarget={runtimeTarget}
			ssoRedirectPath={null}
			suppressInlineBackButtons={false}
			initialIdentifier={null}
			registerLink={
				<AuthRouterLink
					to={Routes.inviteRegister(code)}
					search={registerSearch}
					data-flx="invite.invite-login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			data-flx="invite.invite-login-page.auth-login-layout"
		/>
	);
});
interface InviteLoginPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const InviteLoginPageContent = observer(function InviteLoginPageContent({
	runtimeSnapshot,
}: InviteLoginPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const {code} = useParams() as {code: string};

	const {invite, loading, guildInvite, isGroupDM, isInvitesDisabled, isRaidDetected} = useInviteAuthPage(
		code,
		runtimeSnapshot,
	);
	if (!invite)
		return (
			<InviteAuthPageState
				loading={loading}
				prefix="invite.invite-login-page.invite-login-page-container"
				data-flx="invite.invite-login-page.invite-login-page-content.invite-auth-page-state"
			/>
		);
	if (isInvitesDisabled && !isGroupDM)
		return (
			<InvitesPausedPanel
				code={code}
				guildInvite={guildInvite}
				isRaidDetected={isRaidDetected}
				preferLogin={true}
				prefix="invite.invite-login-page.invite-login-page-container"
				data-flx="invite.invite-login-page.invite-login-page-content.invites-paused-panel"
			/>
		);

	return (
		<AuthLoginStepCarousel
			ariaLabel={i18n._(ACCEPT_INVITE_DESCRIPTOR)}
			mfaFallbackRedirectPath="/"
			onLoginComplete={runtimeTarget.reset}
			carouselDataFlx="invite.invite-login-page.container-carousel"
			mfaStepDataFlx="invite.invite-login-page.invite-login-page-container.desktop-handoff-mfa-step"
			data-flx="invite.invite-login-page.invite-login-page-content.auth-login-step-carousel"
		>
			<InviteLoginPage
				code={code}
				invite={invite}
				data-flx="invite.invite-login-page.invite-login-page-container.invite-login-page"
			/>
		</AuthLoginStepCarousel>
	);
});

const InviteLoginPageContainer = observer(() => (
	<AuthRuntimeTargetGate data-flx="invite.invite-login-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<InviteLoginPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="invite.invite-login-page.invite-login-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default InviteLoginPageContainer;
