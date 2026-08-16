// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {TopNagbarContext} from '@app/features/app/components/layout/app_layout/TopNagbarContext';
import styles from '@app/features/app/components/layout/GuildLayout.module.css';
import {GuildNavbar} from '@app/features/app/components/layout/GuildNavbar';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {GuildSidebarSkeleton} from '@app/features/app/components/skeleton/GuildSidebarSkeleton';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {MatureContentChannelGate} from '@app/features/channel/components/MatureContentChannelGate';
import Channels from '@app/features/channel/state/Channels';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildMatureContentAgree, {MatureContentGateReason} from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {
	CANCEL_DESCRIPTOR,
	DISMISS_DESCRIPTOR,
	ENABLE_TWO_FACTOR_AUTH_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {adminUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import NagbarState from '@app/features/ui/state/Nagbar';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {ElevatedPermissions, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type Icon, NetworkSlashIcon, SmileySadIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useContext, useEffect, useMemo, useRef} from 'react';

const ENABLE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Enable invites for this community',
	comment: 'Confirmation modal title for re-enabling invite links in the current community.',
});
const ENABLE_DESCRIPTOR = msg({
	message: 'Enable',
	comment: 'Primary button label in the enable-invites confirmation modal.',
});
const ENABLE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Are you sure you want to enable invites? This will allow users to join this community through invite links again.',
	comment: 'Confirmation modal body shown before re-enabling invite links in the current community.',
});
const INVITES_DISABLED_RAID_MESSAGE_DESCRIPTOR = msg({
	message:
		'Invites to {communityName} are currently disabled because {productName} detected a potential raid. New people cannot join right now.',
	comment:
		'Guild nagbar body shown when invite links are paused because raid detection is active. {communityName} is the community name and {productName} is Fluxer.',
});
const INVITES_DISABLED_MESSAGE_DESCRIPTOR = msg({
	message: 'Invites to {communityName} are currently disabled',
	comment: 'Guild nagbar body shown when invite links are paused in the current community.',
});
const ENABLE_INVITES_AGAIN_DESCRIPTOR = msg({
	message: 'Enable invites again',
	comment: 'Button label on the invites-disabled guild nagbar. Opens a confirmation to re-enable invites.',
});
const GUILD_MFA_REQUIREMENT_MESSAGE_DESCRIPTOR = msg({
	message:
		'Moderation actions in {communityName} require two-factor authentication. Enable 2FA to kick, ban, timeout, or delete messages here.',
	comment:
		'Guild nagbar body shown when moderation actions in a community require two-factor authentication. {communityName} is the community name.',
});
const STAFF_ONLY_GUILD_MESSAGE_DESCRIPTOR = msg({
	message: '{communityName} is currently only accessible to {productName} staff members',
	comment:
		'Guild nagbar body shown for staff-only communities. {communityName} is the community name and {productName} is Fluxer.',
});
const MANAGE_COMMUNITY_FEATURES_DESCRIPTOR = msg({
	message: 'Manage community features',
	comment: 'Button label on the staff-only community nagbar. Opens the admin feature-management page.',
});
const COMMUNITY_TEMPORARILY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Community temporarily unavailable',
	comment: 'Short label in the app layout guild layout.',
});
const SOMETHING_WENT_WRONG_WE_RE_WORKING_ON_IT_DESCRIPTOR = msg({
	message: "Something went wrong. We're working on it.",
	comment: 'Community layout error-boundary body shown when rendering the community pane crashes. Keep plain and calm.',
});
const THIS_IS_NOT_THE_COMMUNITY_YOU_RE_LOOKING_DESCRIPTOR = msg({
	message: "This is not the community you're looking for.",
	comment: 'Body text in the app layout guild layout.',
});
const THE_COMMUNITY_YOU_RE_LOOKING_FOR_MAY_HAVE_DESCRIPTOR = msg({
	message: "The community you're looking for may have been deleted or you may not have access to it.",
	comment:
		'Community layout empty-state body shown when the active community is missing (deleted or no longer accessible).',
});
const NO_ACCESSIBLE_CHANNELS_DESCRIPTOR = msg({
	message: 'No accessible channels',
	comment: 'Short label in the app layout guild layout.',
});
const YOU_DON_T_HAVE_ACCESS_TO_ANY_CHANNELS_DESCRIPTOR = msg({
	message: "You don't have access to any channels in this community.",
	comment:
		'Community layout empty-state body shown when the user has joined the community but cannot see any channels.',
});
const InvitesDisabledNagbar = observer(({isMobile, guildId}: {isMobile: boolean; guildId: string}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const selectedChannelId = SelectedChannel.currentChannelId;
	const canManageGuild = selectedChannelId ? Permission.can(Permissions.MANAGE_GUILD, {guildId}) : false;
	const isRaidDetected = guild?.features.has(GuildFeatures.RAID_DETECTED) ?? false;
	if (!guild) return null;
	const handleEnableInvites = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(ENABLE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR)}
					description={i18n._(ENABLE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR)}
					primaryText={i18n._(ENABLE_DESCRIPTOR)}
					primaryVariant="primary"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={async () => {
						await GuildCommands.toggleFeature(guildId, GuildFeatures.INVITES_DISABLED, false);
					}}
					data-flx="app.guild-layout.handle-enable-invites.confirm-modal"
				/>
			)),
		);
	};
	const handleDismiss = () => {
		NagbarCommands.dismissInvitesDisabledNagbar(guildId);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="rgb(234 88 12)"
			textColor="white"
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.guild-layout.invites-disabled-nagbar.nagbar"
		>
			<div
				className={isMobile ? styles.nagbarContentMobile : styles.nagbarContent}
				data-flx="app.guild-layout.invites-disabled-nagbar.nagbar-content"
			>
				<p className={styles.nagbarText} data-flx="app.guild-layout.invites-disabled-nagbar.nagbar-text">
					{isRaidDetected
						? i18n._(INVITES_DISABLED_RAID_MESSAGE_DESCRIPTOR, {
								communityName: guild.name,
								productName: PRODUCT_NAME,
							})
						: i18n._(INVITES_DISABLED_MESSAGE_DESCRIPTOR, {communityName: guild.name})}
				</p>
				<div
					className={isMobile ? styles.nagbarActions : styles.nagbarActionsDesktop}
					data-flx="app.guild-layout.invites-disabled-nagbar.nagbar-actions"
				>
					{isMobile && (
						<NagbarButton
							isMobile={isMobile}
							onClick={handleDismiss}
							data-flx="app.guild-layout.invites-disabled-nagbar.nagbar-button.dismiss"
						>
							{i18n._(DISMISS_DESCRIPTOR)}
						</NagbarButton>
					)}
					{canManageGuild && (
						<NagbarButton
							isMobile={isMobile}
							onClick={handleEnableInvites}
							data-flx="app.guild-layout.invites-disabled-nagbar.nagbar-button.enable-invites"
						>
							{i18n._(ENABLE_INVITES_AGAIN_DESCRIPTOR)}
						</NagbarButton>
					)}
				</div>
			</div>
		</Nagbar>
	);
});
const GuildMfaRequirementNagbar = observer(({isMobile, guildId}: {isMobile: boolean; guildId: string}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	if (!guild) return null;
	const handleEnableMfa = () => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="account_security"
					initialSubtab="security"
					data-flx="app.guild-layout.handle-enable-mfa.user-settings-modal"
				/>
			)),
		);
	};
	const handleDismiss = () => {
		NagbarCommands.dismissGuildMfaRequirementNagbar(guildId);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="rgb(180 83 9)"
			textColor="white"
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar"
		>
			<div
				className={isMobile ? styles.nagbarContentMobile : styles.nagbarContent}
				role="status"
				aria-live="polite"
				data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar-content"
			>
				<p className={styles.nagbarText} data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar-text">
					{i18n._(GUILD_MFA_REQUIREMENT_MESSAGE_DESCRIPTOR, {communityName: guild.name})}
				</p>
				<div
					className={isMobile ? styles.nagbarActions : styles.nagbarActionsDesktop}
					data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar-actions"
				>
					{isMobile && (
						<NagbarButton
							isMobile={isMobile}
							onClick={handleDismiss}
							data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar-button.dismiss"
						>
							{i18n._(DISMISS_DESCRIPTOR)}
						</NagbarButton>
					)}
					<NagbarButton
						isMobile={isMobile}
						onClick={handleEnableMfa}
						data-flx="app.guild-layout.guild-mfa-requirement-nagbar.nagbar-button.enable-mfa"
					>
						{i18n._(ENABLE_TWO_FACTOR_AUTH_DESCRIPTOR)}
					</NagbarButton>
				</div>
			</div>
		</Nagbar>
	);
});
const StaffOnlyGuildNagbar = observer(({isMobile, guildId}: {isMobile: boolean; guildId: string}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	if (!guild) return null;
	const handleManageFeatures = () => {
		const featuresUrl = new URL(adminUrl(`guilds/${guildId}`));
		featuresUrl.searchParams.set('tab', 'features');
		void openExternalUrl(featuresUrl.toString());
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="var(--status-danger)"
			textColor="white"
			data-flx="app.guild-layout.staff-only-guild-nagbar.nagbar"
		>
			<div
				className={isMobile ? styles.nagbarContentMobile : styles.nagbarContent}
				data-flx="app.guild-layout.staff-only-guild-nagbar.nagbar-content"
			>
				<p className={styles.nagbarText} data-flx="app.guild-layout.staff-only-guild-nagbar.nagbar-text">
					{i18n._(STAFF_ONLY_GUILD_MESSAGE_DESCRIPTOR, {
						communityName: guild.name,
						productName: PRODUCT_NAME,
					})}
				</p>
				<div
					className={isMobile ? styles.nagbarActions : styles.nagbarActionsDesktop}
					data-flx="app.guild-layout.staff-only-guild-nagbar.nagbar-actions"
				>
					<NagbarButton
						isMobile={isMobile}
						onClick={handleManageFeatures}
						data-flx="app.guild-layout.staff-only-guild-nagbar.nagbar-button.manage-features"
					>
						{i18n._(MANAGE_COMMUNITY_FEATURES_DESCRIPTOR)}
					</NagbarButton>
				</div>
			</div>
		</Nagbar>
	);
});
const GuildUnavailable = observer(function GuildUnavailable({
	icon: Icon,
	title,
	description,
}: {
	icon: Icon;
	title: string;
	description: string;
}) {
	return (
		<div
			className={styles.guildUnavailableContainer}
			data-flx="app.guild-layout.guild-unavailable.guild-unavailable-container"
		>
			<div
				className={styles.guildUnavailableContent}
				data-flx="app.guild-layout.guild-unavailable.guild-unavailable-content"
			>
				<Icon
					className={styles.guildUnavailableIcon}
					data-flx="app.guild-layout.guild-unavailable.guild-unavailable-icon"
				/>
				<h1
					className={styles.guildUnavailableTitle}
					data-flx="app.guild-layout.guild-unavailable.guild-unavailable-title"
				>
					{title}
				</h1>
				<p
					className={styles.guildUnavailableDescription}
					data-flx="app.guild-layout.guild-unavailable.guild-unavailable-description"
				>
					{description}
				</p>
			</div>
		</div>
	);
});
export const GuildLayout = observer(({children}: {children: React.ReactNode}) => {
	const {i18n} = useLingui();
	const {guildId, channelId} = useParams() as {guildId: string; channelId?: string};
	const mobileLayout = MobileLayout;
	const guild = Guilds.getGuild(guildId);
	const unavailableGuilds = GuildAvailability.unavailableGuilds;
	const channels = Channels.getGuildChannels(guildId);
	const user = Users.currentUser;
	const nagbarState = NagbarState;
	const selectedChannelId = SelectedChannel.currentChannelId;
	const channel = Channels.getChannel(selectedChannelId ?? '');
	const isStaff = user?.isStaff() ?? false;
	const invitesDisabledDismissed = NagbarState.getInvitesDisabledDismissed(guild?.id ?? '');
	const guildMfaRequirementDismissed = NagbarState.getGuildMfaRequirementDismissed(guild?.id ?? '');
	const hasMfaGatedGuildPermissions =
		guild && user
			? (PermissionUtils.computePermissions(user, guild.toJSON(), undefined, undefined, false) &
					ElevatedPermissions) !==
				PermissionUtils.NONE
			: false;
	const guildUnavailable = guildId && (unavailableGuilds.has(guildId) || guild?.unavailable);
	const guildNotFound = !guildUnavailable && !guild;
	const firstAccessibleTextChannel = useMemo(() => {
		if (!guild) return null;
		for (const ch of channels) {
			if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(ch.type)) {
				return ch;
			}
		}
		return null;
	}, [guild, channels]);
	const shouldShowInvitesDisabled = useMemo(() => {
		if (!selectedChannelId) return false;
		if (!channel?.guildId) return false;
		if (!guild) return false;
		if (nagbarState.forceHideInvitesDisabled) return false;
		if (nagbarState.forceInvitesDisabled) return true;
		const hasInvitesDisabled = guild.features.has(GuildFeatures.INVITES_DISABLED);
		if (!hasInvitesDisabled) return false;
		const canInvite = InviteUtils.canInviteToChannel(selectedChannelId, channel.guildId);
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: channel.guildId});
		if (!canInvite && !canManageGuild) return false;
		if (invitesDisabledDismissed && !nagbarState.forceInvitesDisabled) return false;
		return true;
	}, [
		selectedChannelId,
		channel,
		guild,
		invitesDisabledDismissed,
		nagbarState.forceInvitesDisabled,
		nagbarState.forceHideInvitesDisabled,
	]);
	const shouldShowStaffOnlyGuild = useMemo(() => {
		if (!selectedChannelId) return false;
		if (!channel?.guildId) return false;
		if (!guild) return false;
		if (!isStaff) return false;
		const isStaffOnly = guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF);
		return isStaffOnly;
	}, [selectedChannelId, channel, guild, isStaff]);
	const shouldShowGuildMfaRequirement = useMemo(() => {
		if (nagbarState.forceHideGuildMfaRequirement) return false;
		if (nagbarState.forceGuildMfaRequirement) return Boolean(selectedChannelId) && Boolean(guild);
		return (
			Boolean(selectedChannelId) &&
			Boolean(guild) &&
			!user?.mfaEnabled &&
			guild?.mfaLevel === GuildMFALevel.ELEVATED &&
			hasMfaGatedGuildPermissions &&
			!guildMfaRequirementDismissed
		);
	}, [
		selectedChannelId,
		guild,
		user?.mfaEnabled,
		hasMfaGatedGuildPermissions,
		guildMfaRequirementDismissed,
		nagbarState.forceGuildMfaRequirement,
		nagbarState.forceHideGuildMfaRequirement,
	]);
	const guildLevelGateReason = guild
		? GuildMatureContentAgree.getGuildLevelGateReason(guild.id)
		: MatureContentGateReason.NONE;
	const showGuildLevelGate = guildLevelGateReason !== MatureContentGateReason.NONE;
	const topNagbarCount = useContext(TopNagbarContext);
	const maxVisibleNagbars = 2;
	const availableGuildNagbarSlots = Math.max(0, maxVisibleNagbars - topNagbarCount);
	const showStaffOnlyGuildNagbar = shouldShowStaffOnlyGuild && availableGuildNagbarSlots > 0;
	const showGuildMfaRequirementNagbar =
		shouldShowGuildMfaRequirement && availableGuildNagbarSlots > (showStaffOnlyGuildNagbar ? 1 : 0);
	const showInvitesDisabledNagbar =
		shouldShowInvitesDisabled &&
		availableGuildNagbarSlots > (showStaffOnlyGuildNagbar ? 1 : 0) + (showGuildMfaRequirementNagbar ? 1 : 0);
	const guildNagbarCount =
		(showStaffOnlyGuildNagbar ? 1 : 0) + (showGuildMfaRequirementNagbar ? 1 : 0) + (showInvitesDisabledNagbar ? 1 : 0);
	const hasGuildNagbars = guildNagbarCount > 0;
	const nagbarCount = topNagbarCount + guildNagbarCount;
	const prevNagbarCount = useRef<number>(nagbarCount);
	const nagbarContextValue = nagbarCount;
	useEffect(() => {
		if (prevNagbarCount.current !== nagbarCount) {
			prevNagbarCount.current = nagbarCount;
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
	}, [nagbarCount]);
	useEffect(() => {
		if (!guild || !channelId || guildUnavailable || guildNotFound) return;
		const currentChannel = Channels.getChannel(channelId);
		const currentPath = Navigation.pathname;
		const expectedPath = Routes.guildChannel(guildId, channelId);
		if (currentPath === expectedPath && !currentChannel) {
			if (firstAccessibleTextChannel) {
				Navigation.navigateToGuild(guildId, firstAccessibleTextChannel.id, undefined, 'replace');
			}
		}
	}, [guild, guildId, channelId, firstAccessibleTextChannel, guildUnavailable, guildNotFound]);
	const guildNagbars = (
		<>
			{showStaffOnlyGuildNagbar && guildId && (
				<StaffOnlyGuildNagbar
					isMobile={mobileLayout.enabled}
					guildId={guildId}
					data-flx="app.guild-layout.staff-only-guild-nagbar"
				/>
			)}
			{showGuildMfaRequirementNagbar && guildId && (
				<GuildMfaRequirementNagbar
					isMobile={mobileLayout.enabled}
					guildId={guildId}
					data-flx="app.guild-layout.guild-mfa-requirement-nagbar"
				/>
			)}
			{showInvitesDisabledNagbar && guildId && (
				<InvitesDisabledNagbar
					isMobile={mobileLayout.enabled}
					guildId={guildId}
					data-flx="app.guild-layout.invites-disabled-nagbar"
				/>
			)}
		</>
	);
	if (VoiceCallFullscreen.isActive) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div className={styles.guildLayoutContainer} data-flx="app.guild-layout.guild-layout-container.fullscreen">
					<div
						className={clsx(styles.guildLayoutContent, styles.guildLayoutContentFullscreen)}
						data-flx="app.guild-layout.guild-layout-content.fullscreen"
					>
						<div
							key="guild-main-content"
							className={clsx(styles.guildMainContent, styles.guildMainContentFullscreen)}
							data-flx="app.guild-layout.guild-main-content.fullscreen"
						>
							{children}
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}
	if (mobileLayout.enabled) {
		if (!channelId && (guildUnavailable || guildNotFound)) {
			return (
				<TopNagbarContext.Provider value={nagbarContextValue}>
					<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content">
						<GuildSidebarSkeleton guildId={guildId} data-flx="app.guild-layout.guild-sidebar-skeleton" />
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content">
							{guildUnavailable ? (
								<GuildUnavailable
									icon={NetworkSlashIcon}
									title={i18n._(COMMUNITY_TEMPORARILY_UNAVAILABLE_DESCRIPTOR)}
									description={i18n._(SOMETHING_WENT_WRONG_WE_RE_WORKING_ON_IT_DESCRIPTOR)}
									data-flx="app.guild-layout.guild-unavailable"
								/>
							) : (
								<GuildUnavailable
									icon={SmileySadIcon}
									title={i18n._(THIS_IS_NOT_THE_COMMUNITY_YOU_RE_LOOKING_DESCRIPTOR)}
									description={i18n._(THE_COMMUNITY_YOU_RE_LOOKING_FOR_MAY_HAVE_DESCRIPTOR)}
									data-flx="app.guild-layout.guild-unavailable--2"
								/>
							)}
						</div>
					</div>
				</TopNagbarContext.Provider>
			);
		}
		if (showGuildLevelGate && guild) {
			return (
				<TopNagbarContext.Provider value={nagbarContextValue}>
					<div
						className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
						data-flx="app.guild-layout.guild-layout-container"
					>
						{guildNagbars}
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--2">
							<MatureContentChannelGate
								guildId={guild.id}
								reason={guildLevelGateReason}
								scope="guild"
								data-flx="app.guild-layout.mature-content-channel-gate"
							/>
						</div>
					</div>
				</TopNagbarContext.Provider>
			);
		}
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				{guild && (
					<div
						className={channelId ? styles.mobileNavbarHidden : styles.mobileNavbarSlot}
						data-flx="app.guild-layout.mobile-navbar"
					>
						<GuildNavbar guild={guild} data-flx="app.guild-layout.guild-navbar" />
					</div>
				)}
				{channelId && (
					<div
						className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
						data-flx="app.guild-layout.guild-layout-container--2"
					>
						{guildNagbars}
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--3">
							{children}
						</div>
					</div>
				)}
			</TopNagbarContext.Provider>
		);
	}
	if (guildUnavailable) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div
					className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
					data-flx="app.guild-layout.guild-layout-container--3"
				>
					{guildNagbars}
					<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content--2">
						<GuildSidebarSkeleton guildId={guildId} data-flx="app.guild-layout.guild-sidebar-skeleton--2" />
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--4">
							<GuildUnavailable
								icon={NetworkSlashIcon}
								title={i18n._(COMMUNITY_TEMPORARILY_UNAVAILABLE_DESCRIPTOR)}
								description={i18n._(SOMETHING_WENT_WRONG_WE_RE_WORKING_ON_IT_DESCRIPTOR)}
								data-flx="app.guild-layout.guild-unavailable--3"
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}
	if (guildNotFound) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div
					className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
					data-flx="app.guild-layout.guild-layout-container--4"
				>
					{guildNagbars}
					<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content--3">
						<GuildSidebarSkeleton guildId={guildId} data-flx="app.guild-layout.guild-sidebar-skeleton--3" />
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--5">
							<GuildUnavailable
								icon={SmileySadIcon}
								title={i18n._(THIS_IS_NOT_THE_COMMUNITY_YOU_RE_LOOKING_DESCRIPTOR)}
								description={i18n._(THE_COMMUNITY_YOU_RE_LOOKING_FOR_MAY_HAVE_DESCRIPTOR)}
								data-flx="app.guild-layout.guild-unavailable--4"
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}
	if (channelId && !Channels.getChannel(channelId) && !firstAccessibleTextChannel) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div
					className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
					data-flx="app.guild-layout.guild-layout-container--5"
				>
					{guildNagbars}
					<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content--4">
						<GuildNavbar guild={guild!} data-flx="app.guild-layout.guild-navbar--2" />
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--6">
							<GuildUnavailable
								icon={SmileySadIcon}
								title={i18n._(NO_ACCESSIBLE_CHANNELS_DESCRIPTOR)}
								description={i18n._(YOU_DON_T_HAVE_ACCESS_TO_ANY_CHANNELS_DESCRIPTOR)}
								data-flx="app.guild-layout.guild-unavailable--5"
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}
	if (showGuildLevelGate && guild) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div
					className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
					data-flx="app.guild-layout.guild-layout-container--6"
				>
					{guildNagbars}
					<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content--5">
						<GuildSidebarSkeleton guildId={guildId} data-flx="app.guild-layout.guild-sidebar-skeleton--4" />
						<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--7">
							<MatureContentChannelGate
								guildId={guild.id}
								reason={guildLevelGateReason}
								scope="guild"
								data-flx="app.guild-layout.mature-content-channel-gate--2"
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}
	return (
		<TopNagbarContext.Provider value={nagbarContextValue}>
			<div
				className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}
				data-flx="app.guild-layout.guild-layout-container--7"
			>
				{guildNagbars}
				<div className={styles.guildLayoutContent} data-flx="app.guild-layout.guild-layout-content--6">
					<GuildNavbar guild={guild!} data-flx="app.guild-layout.guild-navbar--3" />
					<div className={styles.guildMainContent} data-flx="app.guild-layout.guild-main-content--8">
						{children}
					</div>
				</div>
			</div>
		</TopNagbarContext.Provider>
	);
});
