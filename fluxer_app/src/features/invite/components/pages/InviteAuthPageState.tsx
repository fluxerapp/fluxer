// SPDX-License-Identifier: AGPL-3.0-or-later

import {getActiveInstanceProductName} from '@app/features/app/state/ActiveInstanceProductName';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {GuildInviteHeader} from '@app/features/auth/flow/InviteHeader';
import {useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
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
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Button} from '@app/features/ui/button/Button';
import {getGuildSplashURL} from '@app/features/user/utils/AvatarUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {GuildFeatures, GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {GuildInvite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {useLingui} from '@lingui/react/macro';
import {useEffect, useMemo} from 'react';

export function useInviteAuthPage(code: string, runtimeSnapshot: RuntimeConfigSnapshot) {
	const {i18n} = useLingui();
	const inviteTarget = useMemo(
		() => instanceTargetFromSnapshot(runtimeSnapshot),
		[runtimeSnapshot.apiCodeVersion, runtimeSnapshot.apiEndpoint],
	);
	useFluxerDocumentTitle(i18n._(ACCEPT_INVITE_DESCRIPTOR));
	const {setSplashUrl, setSplashCardAlignment} = useAuthLayoutContext();
	const inviteState = Invites.getInvite(code, inviteTarget);
	const inviteData = inviteState?.data ?? null;
	const guildInvite = inviteData && isGuildInvite(inviteData) ? inviteData : null;
	useEffect(() => {
		const currentInviteState = Invites.getInvite(code, inviteTarget);
		if (!currentInviteState && code) {
			void InviteCommands.fetchWithCoalescing(code, inviteTarget).catch(() => {});
		}
	}, [code, inviteTarget]);
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
	const isGroupDM = inviteData ? isGroupDmInvite(inviteData) : false;
	const guildFeatures = guildInvite?.guild.features
		? Array.isArray(guildInvite.guild.features)
			? guildInvite.guild.features
			: [...guildInvite.guild.features]
		: [];
	const isInvitesDisabled = guildFeatures.includes(GuildFeatures.INVITES_DISABLED);
	const isRaidDetected = guildFeatures.includes(GuildFeatures.RAID_DETECTED);

	return {
		invite: inviteState?.error || inviteState?.loading ? null : inviteData,
		loading: !inviteState || inviteState.loading,
		guildInvite,
		isGroupDM,
		isInvitesDisabled,
		isRaidDetected,
	};
}
export function InviteAuthPageState({loading, prefix}: {loading: boolean; prefix: string}) {
	const {i18n} = useLingui();
	if (loading) return <AuthLoadingState data-flx={`${prefix}.auth-loading-state`} />;
	return (
		<AuthErrorState
			title={i18n._(INVITE_NOT_FOUND_TITLE_DESCRIPTOR)}
			text={i18n._(INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR)}
			action={<AuthRuntimeTargetResetAction data-flx={`${prefix}.auth-runtime-target-reset-action`} />}
			data-flx={`${prefix}.auth-error-state`}
		/>
	);
}
export function InvitesPausedPanel({
	code,
	guildInvite,
	isRaidDetected,
	preferLogin,
	prefix,
}: {
	code: string;
	guildInvite: GuildInvite | null;
	isRaidDetected: boolean;
	preferLogin?: boolean;
	prefix: string;
}) {
	const {i18n} = useLingui();
	return (
		<div className={sharedStyles.container} data-flx={`${prefix}.div`}>
			<DesktopDeepLinkPrompt
				code={code}
				kind="invite"
				preferLogin={preferLogin}
				data-flx={`${prefix}.desktop-deep-link-prompt`}
			/>
			{guildInvite ? <GuildInviteHeader invite={guildInvite} data-flx={`${prefix}.guild-invite-header`} /> : null}
			<div className={sharedStyles.disabledContainer} data-flx={`${prefix}.div--2`}>
				<p className={sharedStyles.disabledText} data-flx={`${prefix}.p`}>
					{isRaidDetected
						? i18n._(RAID_INVITES_PAUSED_DESCRIPTOR, {productName: getActiveInstanceProductName()})
						: i18n._(INVITES_PAUSED_DESCRIPTOR)}
				</p>
				<p className={sharedStyles.disabledSubtext} data-flx={`${prefix}.p--2`}>
					{isRaidDetected
						? i18n._(RAID_INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)
						: i18n._(INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR)}
				</p>
			</div>
			<div className={sharedStyles.disabledActions} data-flx={`${prefix}.div--3`}>
				<AuthRouterLink
					to="/register"
					className={sharedStyles.disabledActionLink}
					data-flx={`${prefix}.auth-router-link`}
				>
					<Button fitContainer data-flx={`${prefix}.button`}>
						{i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
					</Button>
				</AuthRouterLink>
				<AuthRouterLink
					to="/login"
					className={sharedStyles.disabledActionLink}
					data-flx={`${prefix}.auth-router-link--2`}
				>
					<Button fitContainer variant="secondary" data-flx={`${prefix}.button--2`}>
						{i18n._(SIGN_IN_DESCRIPTOR)}
					</Button>
				</AuthRouterLink>
			</div>
		</div>
	);
}
