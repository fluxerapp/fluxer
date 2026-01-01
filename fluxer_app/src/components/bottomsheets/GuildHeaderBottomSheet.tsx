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
import {
	BellIcon,
	BellSlashIcon,
	CheckIcon,
	CopyIcon,
	FolderPlusIcon,
	GearIcon,
	PlusCircleIcon,
	ShieldIcon,
	SignOutIcon,
	UserCircleIcon,
	UserPlusIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {Permissions} from '~/Constants';
import {CategoryCreateModal} from '~/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '~/components/modals/ChannelCreateModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '~/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '~/components/modals/GuildSettingsModal';
import {InviteModal} from '~/components/modals/InviteModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import * as Sheet from '~/components/uikit/Sheet/Sheet';
import {useLeaveGuild} from '~/hooks/useLeaveGuild';
import type {GuildRecord} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {getMutedText} from '~/utils/ContextMenuUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import styles from './ChannelDetailsBottomSheet.module.css';
import headerStyles from './GuildHeaderBottomSheet.module.css';
import sharedStyles from './shared.module.css';

interface GuildHeaderBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guild: GuildRecord;
}

export const GuildHeaderBottomSheet: React.FC<GuildHeaderBottomSheetProps> = observer(({isOpen, onClose, guild}) => {
	const {t, i18n} = useLingui();
	UserSettingsStore;
	const leaveGuild = useLeaveGuild();
	const [muteSheetOpen, setMuteSheetOpen] = React.useState(false);

	const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {guildId: guild.id});
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);
	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: guild.id});
	const canViewAuditLog = PermissionStore.can(Permissions.VIEW_AUDIT_LOG, {guildId: guild.id});
	const canManageWebhooks = PermissionStore.can(Permissions.MANAGE_WEBHOOKS, {guildId: guild.id});
	const canManageEmojis = PermissionStore.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id});
	const canBanMembers = PermissionStore.can(Permissions.BAN_MEMBERS, {guildId: guild.id});

	const canAccessGuildSettings =
		canManageGuild || canManageRoles || canViewAuditLog || canManageWebhooks || canManageEmojis || canBanMembers;

	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);

	const handleToggleHideMutedChannels = (checked: boolean) => {
		const currentSettings = UserGuildSettingsStore.getSettings(guild.id);
		const currentValue = currentSettings?.hide_muted_channels ?? false;
		if (checked === currentValue) return;
		UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
	};

	const handleOpenMuteSheet = () => {
		setMuteSheetOpen(true);
	};

	const handleCloseMuteSheet = () => {
		setMuteSheetOpen(false);
	};

	const handleInviteMembers = () => {
		onClose();
		ModalActionCreators.push(modal(() => <InviteModal channelId={invitableChannelId ?? ''} />));
	};

	const handleCommunitySettings = () => {
		onClose();
		ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} />));
	};

	const handleCreateChannel = () => {
		onClose();
		ModalActionCreators.push(modal(() => <ChannelCreateModal guildId={guild.id} />));
	};

	const handleCreateCategory = () => {
		onClose();
		ModalActionCreators.push(modal(() => <CategoryCreateModal guildId={guild.id} />));
	};

	const handleNotificationSettings = () => {
		onClose();
		ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guild.id} />));
	};

	const handlePrivacySettings = () => {
		onClose();
		ModalActionCreators.push(modal(() => <GuildPrivacySettingsModal guildId={guild.id} />));
	};

	const handleEditCommunityProfile = () => {
		onClose();
		ModalActionCreators.push(modal(() => <UserSettingsModal initialGuildId={guild.id} initialTab="my_profile" />));
	};

	const handleLeaveCommunity = () => {
		onClose();
		leaveGuild(guild.id);
	};

	const handleCopyGuildId = () => {
		void TextCopyActionCreators.copy(i18n, guild.id);
		onClose();
	};

	const menuGroups: Array<MenuGroupType> = [];

	const quickActions = [];

	if (canInvite) {
		quickActions.push({
			icon: <UserPlusIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Invite Members`,
			onClick: handleInviteMembers,
		});
	}

	if (canAccessGuildSettings) {
		quickActions.push({
			icon: <GearIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Community Settings`,
			onClick: handleCommunitySettings,
		});
	}

	if (canManageChannels) {
		quickActions.push({
			icon: <PlusCircleIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Create Channel`,
			onClick: handleCreateChannel,
		});
		quickActions.push({
			icon: <FolderPlusIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Create Category`,
			onClick: handleCreateCategory,
		});
	}

	if (quickActions.length > 0) {
		menuGroups.push({
			items: quickActions,
		});
	}

	const settingsItems = [
		{
			icon: <BellIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Notification Settings`,
			onClick: handleNotificationSettings,
		},
		{
			icon: <ShieldIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Privacy Settings`,
			onClick: handlePrivacySettings,
		},
		{
			icon: <UserCircleIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Edit Community Profile`,
			onClick: handleEditCommunityProfile,
		},
	];

	menuGroups.push({
		items: settingsItems,
	});

	const muteItem = {
		icon: isMuted ? (
			<BellIcon weight="fill" className={sharedStyles.icon} />
		) : (
			<BellSlashIcon weight="fill" className={sharedStyles.icon} />
		),
		label: isMuted ? t`Unmute Community` : t`Mute Community`,
		onClick: handleOpenMuteSheet,
	};

	const hideMutedChannelsItem = {
		label: t`Hide Muted Channels`,
		checked: hideMutedChannels,
		onChange: handleToggleHideMutedChannels,
	};

	menuGroups.push({
		items: [muteItem, hideMutedChannelsItem],
	});

	if (!guild.isOwner(AuthenticationStore.currentUserId)) {
		menuGroups.push({
			items: [
				{
					icon: <SignOutIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Leave Community`,
					onClick: handleLeaveCommunity,
					danger: true,
				},
			],
		});
	}

	const utilityItems = [
		{
			icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Copy Guild ID`,
			onClick: handleCopyGuildId,
		},
	];

	menuGroups.push({
		items: utilityItems,
	});

	const presenceCount = PresenceStore.getPresenceCount(guild.id);
	const memberCount = GuildMemberStore.getMemberCount(guild.id);

	const headerContent = (
		<div className={headerStyles.header}>
			<div className={headerStyles.avatarWrapper}>
				<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={headerStyles.icon} sizePx={48} />
			</div>
			<div className={headerStyles.text}>
				<span className={headerStyles.title}>{guild.name}</span>
				<div className={headerStyles.stats}>
					<div className={headerStyles.stat}>
						<div className={`${headerStyles.statDot} ${headerStyles.statDotOnline}`} />
						<span className={headerStyles.statText}>{t`${presenceCount} Online`}</span>
					</div>
					<div className={headerStyles.stat}>
						<div className={`${headerStyles.statDot} ${headerStyles.statDotMembers}`} />
						<span className={headerStyles.statText}>
							{memberCount === 1 ? t`${memberCount} Member` : t`${memberCount} Members`}
						</span>
					</div>
				</div>
			</div>
		</div>
	);

	return (
		<>
			<MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} headerContent={headerContent} />

			<Sheet.Root isOpen={muteSheetOpen} onClose={handleCloseMuteSheet} snapPoints={[0, 1]} initialSnap={1}>
				<Sheet.Handle />
				<Sheet.Header trailing={<Sheet.CloseButton onClick={handleCloseMuteSheet} />}>
					<Sheet.Title>{isMuted ? t`Unmute Community` : t`Mute Community`}</Sheet.Title>
				</Sheet.Header>
				<Sheet.Content padding="none">
					<div className={styles.muteSheetContainer}>
						<div className={styles.muteSheetContent}>
							{isMuted && mutedText ? (
								<>
									<div className={styles.muteStatusBanner}>
										<p className={styles.muteStatusText}>
											<Trans>Currently: {mutedText}</Trans>
										</p>
									</div>
									<div className={styles.muteOptionsContainer}>
										<button
											type="button"
											onClick={() => {
												UserGuildSettingsActionCreators.updateGuildSettings(guild.id, {
													muted: false,
													mute_config: null,
												});
												handleCloseMuteSheet();
											}}
											className={styles.muteOptionButton}
										>
											<span className={styles.muteOptionLabel}>
												<Trans>Unmute</Trans>
											</span>
										</button>
									</div>
								</>
							) : (
								<div className={styles.muteOptionsContainer}>
									{[
										{label: t`For 15 Minutes`, value: 15 * 60 * 1000},
										{label: t`For 1 Hour`, value: 60 * 60 * 1000},
										{label: t`For 3 Hours`, value: 3 * 60 * 60 * 1000},
										{label: t`For 8 Hours`, value: 8 * 60 * 60 * 1000},
										{label: t`For 24 Hours`, value: 24 * 60 * 60 * 1000},
										{label: t`Until I turn it back on`, value: null},
									].map((option, index, array) => {
										const isSelected =
											isMuted &&
											((option.value === null && !muteConfig?.end_time) ||
												(option.value !== null && muteConfig?.selected_time_window === option.value));

										return (
											<React.Fragment key={option.label}>
												<button
													type="button"
													onClick={() => {
														const newMuteConfig = option.value
															? {
																	selected_time_window: option.value,
																	end_time: new Date(Date.now() + option.value).toISOString(),
																}
															: null;

														UserGuildSettingsActionCreators.updateGuildSettings(guild.id, {
															muted: true,
															mute_config: newMuteConfig,
														});
														handleCloseMuteSheet();
													}}
													className={styles.muteOptionButton}
												>
													<span className={styles.muteOptionLabel}>{option.label}</span>
													{isSelected && <CheckIcon className={styles.iconMedium} weight="bold" />}
												</button>
												{index < array.length - 1 && <div className={styles.muteOptionDivider} />}
											</React.Fragment>
										);
									})}
								</div>
							)}
						</div>
					</div>
				</Sheet.Content>
			</Sheet.Root>
		</>
	);
});
