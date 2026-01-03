/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {type Icon, NetworkSlashIcon, SmileySadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import {ChannelTypes, GuildFeatures, Permissions} from '~/Constants';
import {GuildNavbar} from '~/components/layout/GuildNavbar';
import {GuildNavbarSkeleton} from '~/components/layout/GuildNavbarSkeleton';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import ChannelStore from '~/stores/ChannelStore';
import GuildAvailabilityStore from '~/stores/GuildAvailabilityStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import NagbarStore from '~/stores/NagbarStore';
import PermissionStore from '~/stores/PermissionStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {openExternalUrl} from '~/utils/NativeUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import {adminUrl} from '~/utils/UrlUtils';
import {TopNagbarContext} from './app-layout/TopNagbarContext';
import styles from './GuildLayout.module.css';

const InvitesDisabledNagbar = observer(({isMobile, guildId}: {isMobile: boolean; guildId: string}) => {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const selectedChannelId = SelectedChannelStore.currentChannelId;
	const canManageGuild = selectedChannelId ? PermissionStore.can(Permissions.MANAGE_GUILD, {guildId}) : false;

	if (!guild) return null;

	const handleEnableInvites = () => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Enable invites for this community`}
					description={
						<Trans>
							Are you sure you want to enable invites? This will allow users to join this community through invite links
							again.
						</Trans>
					}
					primaryText={t`Enable`}
					primaryVariant="primary"
					secondaryText={t`Cancel`}
					onPrimary={async () => {
						await GuildActionCreators.toggleInvitesDisabled(guildId, false);
					}}
				/>
			)),
		);
	};

	const handleDismiss = () => {
		NagbarActionCreators.dismissInvitesDisabledNagbar(guildId);
	};

	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="rgb(234 88 12)"
			textColor="white"
			dismissible
			onDismiss={handleDismiss}
		>
			<div className={isMobile ? styles.nagbarContentMobile : styles.nagbarContent}>
				<p className={styles.nagbarText}>
					<Trans>
						Invites to <strong>{guild.name}</strong> are currently disabled
					</Trans>
				</p>
				<div className={isMobile ? styles.nagbarActions : styles.nagbarActionsDesktop}>
					{isMobile && (
						<NagbarButton isMobile={isMobile} onClick={handleDismiss}>
							<Trans>Dismiss</Trans>
						</NagbarButton>
					)}
					{canManageGuild && (
						<NagbarButton isMobile={isMobile} onClick={handleEnableInvites}>
							<Trans>Enable Invites Again</Trans>
						</NagbarButton>
					)}
				</div>
			</div>
		</Nagbar>
	);
});

const StaffOnlyGuildNagbar = observer(({isMobile, guildId}: {isMobile: boolean; guildId: string}) => {
	const guild = GuildStore.getGuild(guildId);
	if (!guild) return null;

	const handleManageFeatures = () => {
		void openExternalUrl(adminUrl(`guilds/${guildId}?tab=features`));
	};

	return (
		<Nagbar isMobile={isMobile} backgroundColor="var(--status-danger)" textColor="white">
			<div className={isMobile ? styles.nagbarContentMobile : styles.nagbarContent}>
				<p className={styles.nagbarText}>
					<Trans>
						<strong>{guild.name}</strong> is currently only accessible to Fluxer staff members
					</Trans>
				</p>
				<div className={isMobile ? styles.nagbarActions : styles.nagbarActionsDesktop}>
					<NagbarButton isMobile={isMobile} onClick={handleManageFeatures}>
						<Trans>Manage Guild Features</Trans>
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
		<div className={styles.guildUnavailableContainer}>
			<div className={styles.guildUnavailableContent}>
				<Icon className={styles.guildUnavailableIcon} />
				<h1 className={styles.guildUnavailableTitle}>{title}</h1>
				<p className={styles.guildUnavailableDescription}>{description}</p>
			</div>
		</div>
	);
});

export const GuildLayout = observer(({children}: {children: React.ReactNode}) => {
	const {t} = useLingui();
	const {guildId, channelId, messageId} = useParams() as {guildId: string; channelId?: string; messageId?: string};
	const mobileLayout = MobileLayoutStore;
	const guild = GuildStore.getGuild(guildId);
	const unavailableGuilds = GuildAvailabilityStore.unavailableGuilds;
	const channels = ChannelStore.getGuildChannels(guildId);

	const user = UserStore.currentUser;
	const nagbarState = NagbarStore;
	const selectedChannelId = SelectedChannelStore.currentChannelId;
	const channel = ChannelStore.getChannel(selectedChannelId ?? '');
	const isStaff = user?.isStaff() ?? false;

	const invitesDisabledDismissed = NagbarStore.getInvitesDisabledDismissed(guild?.id ?? '');

	const guildUnavailable = guildId && (unavailableGuilds.has(guildId) || guild?.unavailable);
	const guildNotFound = !guildUnavailable && !guild;

	const firstAccessibleTextChannel = React.useMemo(() => {
		if (!guild) return null;

		const textChannels = channels
			.filter((ch) => ch.type === ChannelTypes.GUILD_TEXT || ch.type === ChannelTypes.GUILD_VOICE)
			.sort(ChannelUtils.compareChannels);

		return textChannels.length > 0 ? textChannels[0] : null;
	}, [guild, channels]);

	const shouldShowInvitesDisabled = React.useMemo(() => {
		if (!selectedChannelId) return false;
		if (!channel?.guildId) return false;
		if (!guild) return false;

		if (nagbarState.forceHideInvitesDisabled) return false;
		if (nagbarState.forceInvitesDisabled) return true;

		if (user && !user.isClaimed() && guild.ownerId === user.id) {
			return false;
		}

		const hasInvitesDisabled = guild.features.has(GuildFeatures.INVITES_DISABLED);
		if (!hasInvitesDisabled) return false;

		const canInvite = InviteUtils.canInviteToChannel(selectedChannelId, channel.guildId);
		const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {guildId: channel.guildId});

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
		user,
	]);

	const shouldShowStaffOnlyGuild = React.useMemo(() => {
		if (!selectedChannelId) return false;
		if (!channel?.guildId) return false;
		if (!guild) return false;
		if (!isStaff) return false;

		const isStaffOnly = guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF);
		return isStaffOnly;
	}, [selectedChannelId, channel, guild, isStaff]);

	const hasGuildNagbars = shouldShowStaffOnlyGuild || shouldShowInvitesDisabled;
	const nagbarCount = (shouldShowStaffOnlyGuild ? 1 : 0) + (shouldShowInvitesDisabled ? 1 : 0);
	const prevNagbarCount = React.useRef<number>(nagbarCount);
	const hasTopNagbarAbove = React.useContext(TopNagbarContext);
	const nagbarContextValue = hasTopNagbarAbove || hasGuildNagbars;

	React.useEffect(() => {
		if (prevNagbarCount.current !== nagbarCount) {
			prevNagbarCount.current = nagbarCount;
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
	}, [nagbarCount]);

	React.useEffect(() => {
		if (!guildId) {
			NavigationActionCreators.deselectGuild();
			return;
		}
		NavigationActionCreators.selectGuild(guildId);
		return () => {
			NavigationActionCreators.deselectGuild();
		};
	}, [guildId]);

	React.useEffect(() => {
		if (!guildId) return;
		if (channelId) {
			NavigationActionCreators.selectChannel(guildId, channelId, messageId);
		}
	}, [guildId, channelId, messageId]);

	React.useEffect(() => {
		if (!guild || !channelId || guildUnavailable || guildNotFound) return;

		const currentChannel = ChannelStore.getChannel(channelId);
		const currentPath = RouterUtils.getHistory()?.location.pathname ?? '';
		const expectedPath = Routes.guildChannel(guildId, channelId);

		if (currentPath === expectedPath && !currentChannel) {
			if (firstAccessibleTextChannel) {
				RouterUtils.replaceWith(Routes.guildChannel(guildId, firstAccessibleTextChannel.id));
			}
		}
	}, [guild, guildId, channelId, firstAccessibleTextChannel, guildUnavailable, guildNotFound]);

	const guildNagbars = (
		<>
			{shouldShowStaffOnlyGuild && guildId && (
				<StaffOnlyGuildNagbar isMobile={mobileLayout.enabled} guildId={guildId} />
			)}
			{shouldShowInvitesDisabled && guildId && (
				<InvitesDisabledNagbar isMobile={mobileLayout.enabled} guildId={guildId} />
			)}
		</>
	);

	if (mobileLayout.enabled) {
		if (!channelId) {
			if (guildUnavailable || guildNotFound) {
				return (
					<TopNagbarContext.Provider value={nagbarContextValue}>
						<div className={styles.guildLayoutContent}>
							<GuildNavbarSkeleton />
							<div className={styles.guildMainContent}>
								{guildUnavailable ? (
									<GuildUnavailable
										icon={NetworkSlashIcon}
										title={t`Community temporarily unavailable`}
										description={t`We fluxed up! Hang tight, we're working on it.`}
									/>
								) : (
									<GuildUnavailable
										icon={SmileySadIcon}
										title={t`This is not the community you're looking for.`}
										description={t`The community you're looking for may have been deleted or you may not have access to it.`}
									/>
								)}
							</div>
						</div>
					</TopNagbarContext.Provider>
				);
			}
			return (
				<TopNagbarContext.Provider value={nagbarContextValue}>
					<GuildNavbar guild={guild!} />
				</TopNagbarContext.Provider>
			);
		}
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}>
					{guildNagbars}
					<div className={styles.guildMainContent}>{children}</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}

	if (guildUnavailable) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}>
					{guildNagbars}
					<div className={styles.guildLayoutContent}>
						<GuildNavbarSkeleton />
						<div className={styles.guildMainContent}>
							<GuildUnavailable
								icon={NetworkSlashIcon}
								title={t`Community temporarily unavailable`}
								description={t`We fluxed up! Hang tight, we're working on it.`}
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
				<div className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}>
					{guildNagbars}
					<div className={styles.guildLayoutContent}>
						<GuildNavbarSkeleton />
						<div className={styles.guildMainContent}>
							<GuildUnavailable
								icon={SmileySadIcon}
								title={t`This is not the community you're looking for.`}
								description={t`The community you're looking for may have been deleted or you may not have access to it.`}
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}

	if (channelId && !ChannelStore.getChannel(channelId) && !firstAccessibleTextChannel) {
		return (
			<TopNagbarContext.Provider value={nagbarContextValue}>
				<div className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}>
					{guildNagbars}
					<div className={styles.guildLayoutContent}>
						<GuildNavbar guild={guild!} />
						<div className={styles.guildMainContent}>
							<GuildUnavailable
								icon={SmileySadIcon}
								title={t`No accessible channels`}
								description={t`You don't have access to any channels in this community.`}
							/>
						</div>
					</div>
				</div>
			</TopNagbarContext.Provider>
		);
	}

	return (
		<TopNagbarContext.Provider value={nagbarContextValue}>
			<div className={hasGuildNagbars ? styles.guildLayoutContainerWithNagbar : styles.guildLayoutContainer}>
				{guildNagbars}
				<div className={styles.guildLayoutContent}>
					<GuildNavbar guild={guild!} />
					<div className={styles.guildMainContent}>{children}</div>
				</div>
			</div>
		</TopNagbarContext.Provider>
	);
});
