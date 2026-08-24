// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '@app/features/auth/flow/AuthMinimalRegisterFormCore';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthSsoPanel, isRuntimeSsoEnforced} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {GuildInviteHeader, InviteHeader} from '@app/features/auth/flow/InviteHeader';
import {useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {CREATE_ACCOUNT_DESCRIPTOR, SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
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
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {GuildFeatures, GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo} from 'react';

const InviteRegisterPage = observer(function InviteRegisterPage() {
	const {i18n} = useLingui();
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
	const {setSplashUrl, setSplashCardAlignment} = useAuthLayoutContext();
	useFluxerDocumentTitle(i18n._(ACCEPT_INVITE_DESCRIPTOR));
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
		if (guild?.splash) {
			const splashUrl = AvatarUtils.getGuildSplashURL({
				id: guild.id,
				splash: guild.splash,
			});
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
		return <AuthLoadingState data-flx="invite.invite-register-page.auth-loading-state" />;
	}
	if (inviteState.error || !inviteState.data) {
		return (
			<AuthErrorState
				title={i18n._(INVITE_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={i18n._(INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR)}
				data-flx="invite.invite-register-page.auth-error-state"
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
			<div className={sharedStyles.container} data-flx="invite.invite-register-page.div">
				<DesktopDeepLinkPrompt
					code={code}
					kind="invite"
					data-flx="invite.invite-register-page.desktop-deep-link-prompt"
				/>
				{guildInvite ? (
					<GuildInviteHeader invite={guildInvite} data-flx="invite.invite-register-page.guild-invite-header" />
				) : null}
				<div className={sharedStyles.disabledContainer} data-flx="invite.invite-register-page.div--2">
					<p className={sharedStyles.disabledText} data-flx="invite.invite-register-page.p">
						{isRaidDetected
							? i18n._(RAID_INVITES_PAUSED_DESCRIPTOR, {productName: PRODUCT_NAME})
							: i18n._(INVITES_PAUSED_DESCRIPTOR)}
					</p>
					<p className={sharedStyles.disabledSubtext} data-flx="invite.invite-register-page.p--2">
						{isRaidDetected
							? i18n._(RAID_INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)
							: i18n._(INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)}
					</p>
				</div>
				<div className={sharedStyles.disabledActions} data-flx="invite.invite-register-page.div--3">
					<AuthRouterLink
						to="/register"
						className={sharedStyles.disabledActionLink}
						data-flx="invite.invite-register-page.auth-router-link"
					>
						<Button fitContainer data-flx="invite.invite-register-page.button">
							{i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
						</Button>
					</AuthRouterLink>
					<AuthRouterLink
						to="/login"
						className={sharedStyles.disabledActionLink}
						data-flx="invite.invite-register-page.auth-router-link--2"
					>
						<Button fitContainer variant="secondary" data-flx="invite.invite-register-page.button--2">
							{i18n._(SIGN_IN_DESCRIPTOR)}
						</Button>
					</AuthRouterLink>
				</div>
			</div>
		);
	}
	if (isRuntimeSsoEnforced()) {
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
						dataFlx="invite.invite-register-page.sso-panel"
						data-flx="invite.invite-register-page.auth-sso-panel"
					/>
					<AuthBottomLink variant="login" to={loginPath} data-flx="invite.invite-register-page.auth-bottom-link.sso" />
				</div>
			</>
		);
	}
	return (
		<>
			<DesktopDeepLinkPrompt
				code={code}
				kind="invite"
				data-flx="invite.invite-register-page.desktop-deep-link-prompt--2"
			/>
			<InviteHeader invite={invite} data-flx="invite.invite-register-page.invite-header" />
			<div className={sharedStyles.container} data-flx="invite.invite-register-page.div--4">
				<AuthMinimalRegisterFormCore
					submitLabel={i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
					redirectPath="/"
					inviteCode={code}
					data-flx="invite.invite-register-page.auth-minimal-register-form-core"
				/>
				<AuthBottomLink variant="login" to={loginPath} data-flx="invite.invite-register-page.auth-bottom-link" />
			</div>
		</>
	);
});

export default InviteRegisterPage;
