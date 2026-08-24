// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	isApprovalFlowMode,
	isHandoffRequest,
	useDesktopHandoffFlow,
} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import {GuildInviteHeader, InviteHeader} from '@app/features/auth/flow/InviteHeader';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import {safeRedirectTarget, safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import {
	CREATE_ACCOUNT_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import Invites from '@app/features/invite/state/Invites';
import {isGroupDmInvite, isGuildInvite} from '@app/features/invite/types/InviteTypes';
import {
	ACCEPT_INVITE_DESCRIPTOR,
	INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR,
	INVITE_NOT_FOUND_TITLE_DESCRIPTOR,
	INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR,
	INVITES_PAUSED_DESCRIPTOR,
	RAID_INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR,
	RAID_INVITES_PAUSED_DESCRIPTOR,
} from '@app/features/invite/utils/InviteMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {getGuildSplashURL} from '@app/features/user/utils/AvatarUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {GuildFeatures, GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo} from 'react';

const INVITE_LOGIN_PAGE_STEP_ORDER = ['default', 'mfa'] as const;

interface InviteLoginPageProps {
	code: string;
	invite: Invite;
}

const InviteLoginPage = observer(function InviteLoginPage({code, invite}: InviteLoginPageProps) {
	const {i18n} = useLingui();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect ? {redirect_to: safeRedirect} : undefined;
	const redirectPath = useMemo(() => {
		return setPathQueryParams(Routes.inviteRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			desktopHandoff={isHandoff}
			inviteCode={code}
			extraTopContent={
				<>
					<DesktopDeepLinkPrompt
						code={code}
						kind="invite"
						preferLogin={true}
						data-flx="invite.invite-login-page.desktop-deep-link-prompt"
					/>
					<InviteHeader invite={invite} data-flx="invite.invite-login-page.invite-header" />
				</>
			}
			showTitle={false}
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
const InviteLoginPageMFA = observer(function InviteLoginPageMFA() {
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const isHandoff = isHandoffRequest(params);
	const rawRedirect = params['get']('redirect_to');
	const redirectTo = isHandoff ? undefined : safeRedirectTargetOrFallback(rawRedirect, '/');
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const hasStoredAccounts = AccountManager.orderedAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isHandoff,
		hasStoredAccounts,
		initialMode: 'idle',
	});
	const handleMfaSuccess = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			if (isHandoff) {
				await handoff.start({token, userId});
				return;
			}
			await AuthenticationCommands.completeLogin({token, userId});
			AuthenticationCommands.clearMfaTicket();
			RouterUtils.replaceWith(redirectTo || '/');
		},
		[handoff, isHandoff, redirectTo],
	);
	const handleCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
	}, []);
	if (!mfaTicket || !mfaMethods) {
		return null;
	}
	if (isHandoff && isApprovalFlowMode(handoff.mode)) {
		return (
			<ConnectedHandoffApprovalFlow
				handoff={handoff}
				data-flx="invite.invite-login-page.invite-login-page-mfa.connected-handoff-approval-flow"
			/>
		);
	}
	return (
		<MfaScreen
			challenge={{ticket: mfaTicket, ...mfaMethods}}
			onSuccess={handleMfaSuccess}
			onCancel={handleCancel}
			data-flx="invite.invite-login-page.invite-login-page-mfa.mfa-screen"
		/>
	);
});
const InviteLoginPageContainer = observer(() => {
	const {i18n} = useLingui();
	const loginState = Authentication.loginState;
	const {code} = useParams() as {code: string};
	useFluxerDocumentTitle(i18n._(ACCEPT_INVITE_DESCRIPTOR));
	const {setSplashUrl, setSplashCardAlignment} = useAuthLayoutContext();
	const inviteState = Invites.invites.get(code) ?? null;
	const inviteData = inviteState?.data ?? null;
	const guildInvite = inviteData && isGuildInvite(inviteData) ? inviteData : null;
	useEffect(() => {
		const currentInviteState = Invites.invites.get(code) ?? null;
		if (!currentInviteState && code) {
			void InviteCommands.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code]);
	useEffect(() => {
		if (!guildInvite) {
			return;
		}
		const guild = guildInvite.guild;
		if (guild?.splash && guild.id) {
			const splashUrl = getGuildSplashURL({id: guild.id, splash: guild.splash}) ?? null;
			setSplashUrl(splashUrl);
		}
	}, [guildInvite?.guild?.splash, guildInvite?.guild?.id, setSplashUrl]);
	useEffect(() => {
		if (guildInvite) {
			setSplashCardAlignment(guildInvite.guild.splash_card_alignment ?? GuildSplashCardAlignment.CENTER);
		} else {
			setSplashCardAlignment(GuildSplashCardAlignment.CENTER);
		}
	}, [guildInvite?.guild?.splash_card_alignment, setSplashCardAlignment]);
	if (!inviteState || inviteState.loading) {
		return <AuthLoadingState data-flx="invite.invite-login-page.invite-login-page-container.auth-loading-state" />;
	}
	if (inviteState.error || !inviteState.data) {
		return (
			<AuthErrorState
				title={i18n._(INVITE_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={i18n._(INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR)}
				data-flx="invite.invite-login-page.invite-login-page-container.auth-error-state"
			/>
		);
	}
	const invite = inviteState.data;
	const isGroupDM = isGroupDmInvite(invite);
	const guildFeatures = guildInvite?.guild.features
		? Array.isArray(guildInvite.guild.features)
			? guildInvite.guild.features
			: [...guildInvite.guild.features]
		: [];
	const isInvitesDisabled = guildFeatures.includes(GuildFeatures.INVITES_DISABLED);
	const isRaidDetected = guildFeatures.includes(GuildFeatures.RAID_DETECTED);
	if (isInvitesDisabled && !isGroupDM) {
		return (
			<div className={sharedStyles.container} data-flx="invite.invite-login-page.invite-login-page-container.div">
				<DesktopDeepLinkPrompt
					code={code}
					kind="invite"
					preferLogin={true}
					data-flx="invite.invite-login-page.invite-login-page-container.desktop-deep-link-prompt"
				/>
				{guildInvite ? (
					<GuildInviteHeader
						invite={guildInvite}
						data-flx="invite.invite-login-page.invite-login-page-container.guild-invite-header"
					/>
				) : null}
				<div
					className={sharedStyles.disabledContainer}
					data-flx="invite.invite-login-page.invite-login-page-container.div--2"
				>
					<p className={sharedStyles.disabledText} data-flx="invite.invite-login-page.invite-login-page-container.p">
						{isRaidDetected
							? i18n._(RAID_INVITES_PAUSED_DESCRIPTOR, {productName: PRODUCT_NAME})
							: i18n._(INVITES_PAUSED_DESCRIPTOR)}
					</p>
					<p
						className={sharedStyles.disabledSubtext}
						data-flx="invite.invite-login-page.invite-login-page-container.p--2"
					>
						{isRaidDetected
							? i18n._(RAID_INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)
							: i18n._(INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)}
					</p>
				</div>
				<div
					className={sharedStyles.disabledActions}
					data-flx="invite.invite-login-page.invite-login-page-container.div--3"
				>
					<AuthRouterLink
						to="/register"
						className={sharedStyles.disabledActionLink}
						data-flx="invite.invite-login-page.invite-login-page-container.auth-router-link"
					>
						<Button fitContainer data-flx="invite.invite-login-page.invite-login-page-container.button">
							{i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
						</Button>
					</AuthRouterLink>
					<AuthRouterLink
						to="/login"
						className={sharedStyles.disabledActionLink}
						data-flx="invite.invite-login-page.invite-login-page-container.auth-router-link--2"
					>
						<Button
							fitContainer
							variant="secondary"
							data-flx="invite.invite-login-page.invite-login-page-container.button--2"
						>
							{i18n._(SIGN_IN_DESCRIPTOR)}
						</Button>
					</AuthRouterLink>
				</div>
			</div>
		);
	}
	switch (loginState) {
		case 'default':
			return (
				<SteppedCarousel
					step={loginState}
					steps={INVITE_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(ACCEPT_INVITE_DESCRIPTOR)}
					data-flx="invite.invite-login-page.container-carousel"
				>
					<InviteLoginPage
						code={code}
						invite={invite}
						data-flx="invite.invite-login-page.invite-login-page-container.invite-login-page"
					/>
				</SteppedCarousel>
			);
		case 'mfa':
			return (
				<SteppedCarousel
					step={loginState}
					steps={INVITE_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(ACCEPT_INVITE_DESCRIPTOR)}
					data-flx="invite.invite-login-page.container-carousel"
				>
					<InviteLoginPageMFA data-flx="invite.invite-login-page.invite-login-page-container.invite-login-page-mfa" />
				</SteppedCarousel>
			);
		default:
			return null;
	}
});

export default InviteLoginPageContainer;
