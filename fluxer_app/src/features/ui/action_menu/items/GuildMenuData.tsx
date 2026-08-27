// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Authentication from '@app/features/auth/state/Authentication';
import {CategoryCreateModal} from '@app/features/channel/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/features/channel/components/modals/ChannelCreateModal';
import Channels from '@app/features/channel/state/Channels';
import {DELETE_MY_MESSAGES_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {GuildDebugModal} from '@app/features/devtools/components/debug/GuildDebugModal';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '@app/features/guild/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '@app/features/guild/components/modals/GuildSettingsModal';
import {useDeleteMyMessagesInGuild} from '@app/features/guild/hooks/useDeleteMyMessagesInGuild';
import {useLeaveGuild} from '@app/features/guild/hooks/useLeaveGuild';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import {
	COPY_COMMUNITY_ID_DESCRIPTOR,
	CREATE_CATEGORY_DESCRIPTOR,
	CREATE_CHANNEL_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	LEAVE_COMMUNITY_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	MUTE_COMMUNITY_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	PRIVACY_SETTINGS_DESCRIPTOR,
	RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR,
	UNMUTE_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {REPORT_COMMUNITY_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {openReportGuildModal} from '@app/features/moderation/utils/ReportActionUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Permission from '@app/features/permissions/state/Permission';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {
	CopyIdIcon,
	CreateCategoryIcon,
	CreateChannelIcon,
	DebugChannelIcon,
	DeleteIcon,
	EditProfileIcon,
	InviteIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
	PrivacySettingsIcon,
	ReportUserIcon,
	SettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {
	MenuCheckboxType,
	MenuGroupType,
	MenuItemType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {
	GUILD_SETTINGS_LABEL_DESCRIPTOR,
	getGuildSettingsTabs,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const COMMUNITY_DEBUG_DESCRIPTOR = msg({
	message: 'Community debug',
	comment: 'Title of the developer-mode community debug modal.',
});
const INVITE_MEMBERS_DESCRIPTOR = msg({
	message: 'Invite members',
	comment: 'Action label that opens the invite flow.',
});
const EDIT_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Edit community profile',
	comment: 'Action that opens the community profile editor for the current member.',
});
const DEBUG_COMMUNITY_DESCRIPTOR = msg({
	message: 'Debug community',
	comment: 'Developer-mode action that opens the community debug modal.',
});

interface UseGuildMenuDataOptions {
	onClose: () => void;
	onOpenMuteSheet?: () => void;
	preserveInitialMarkAsReadVisibility?: boolean;
}

export interface GuildMenuHandlers {
	handleMarkAsRead: () => void;
	handleInviteMembers: () => void;
	handleCommunitySettings: () => void;
	handleCreateChannel: () => void;
	handleCreateCategory: () => void;
	handleNotificationSettings: () => void;
	handlePrivacySettings: () => void;
	handleEditCommunityProfile: () => void;
	handleLeaveCommunity: () => void;
	handleDeleteMyMessagesInCommunity: () => void;
	handleCopyGuildId: () => void;
	handleDebugGuild: () => void;
	handleResetMatureContentAgreeState: () => void;
	handleReportGuild: () => void;
	handleToggleHideMutedChannels: (checked: boolean) => void;
}

export interface GuildMenuPermissions {
	canManageGuild: boolean;
	canManageChannels: boolean;
	canInvite: boolean;
	canAccessGuildSettings: boolean;
	isOwner: boolean;
	hasGuildUnread: boolean;
	developerMode: boolean;
}

export interface GuildMenuData {
	groups: Array<MenuGroupType>;
	handlers: GuildMenuHandlers;
	permissions: GuildMenuPermissions;
	isMuted: boolean;
	mutedText: string | null;
	hideMutedChannels: boolean;
}

export function useGuildMenuData(guild: Guild, options: UseGuildMenuDataOptions): GuildMenuData {
	const {i18n} = useLingui();
	const {onClose, onOpenMuteSheet, preserveInitialMarkAsReadVisibility = false} = options;
	const leaveGuild = useLeaveGuild();
	const deleteMyMessagesInGuild = useDeleteMyMessagesInGuild();
	const channels = Channels.getGuildChannels(guild.id);
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: guild.id});
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);
	const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId: guild.id});
	const canViewAuditLog = Permission.can(Permissions.VIEW_AUDIT_LOG, {guildId: guild.id});
	const canManageWebhooks = Permission.can(Permissions.MANAGE_WEBHOOKS, {guildId: guild.id});
	const canManageEmojis = Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id});
	const canCreateExpressions = Permission.can(Permissions.CREATE_EXPRESSIONS, {guildId: guild.id});
	const canBanMembers = Permission.can(Permissions.BAN_MEMBERS, {guildId: guild.id});
	const canAccessGuildSettings =
		canManageGuild ||
		canManageRoles ||
		canViewAuditLog ||
		canManageWebhooks ||
		canManageEmojis ||
		canCreateExpressions ||
		canBanMembers;
	const isOwner = guild.isOwner(Authentication.currentUserId);
	const developerMode = UserSettings.developerMode;
	const canEditCommunityProfile = Users.getCurrentUser()?.isClaimed() ?? true;
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const hasCurrentGuildMatureContentGate =
		guild.nsfw || guild.contentWarningLevel === ContentWarningLevel.CONTENT_WARNING;
	const initialHasGuildUnread = useMemo(
		() => Channels.getGuildChannels(guild.id).some((channel) => ReadStates.hasUnread(channel.id)),
		[guild.id],
	);
	const hasGuildUnread = preserveInitialMarkAsReadVisibility
		? initialHasGuildUnread
		: channels.some((channel) => ReadStates.hasUnread(channel.id));
	const handlers = useMemo(
		() => ({
			handleMarkAsRead: () => {
				const channelIds = channels
					.filter((channel) => ReadStates.isUnreadOrMentioned(channel.id))
					.map((channel) => channel.id);
				if (channelIds.length > 0) {
					void ReadStateCommands.bulkAckChannels(channelIds);
				}
				onClose();
			},
			handleInviteMembers: () => {
				const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<InviteModal
							channelId={invitableChannelId ?? ''}
							data-flx="ui.action-menu.items.guild-menu-data.handle-invite-members.invite-modal"
						/>
					)),
				);
			},
			handleCommunitySettings: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<GuildSettingsModal
							guildId={guild.id}
							data-flx="ui.action-menu.items.guild-menu-data.handle-community-settings.guild-settings-modal"
						/>
					)),
				);
			},
			handleCreateChannel: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelCreateModal
							guildId={guild.id}
							data-flx="ui.action-menu.items.guild-menu-data.handle-create-channel.channel-create-modal"
						/>
					)),
				);
			},
			handleCreateCategory: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<CategoryCreateModal
							guildId={guild.id}
							data-flx="ui.action-menu.items.guild-menu-data.handle-create-category.category-create-modal"
						/>
					)),
				);
			},
			handleNotificationSettings: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<GuildNotificationSettingsModal
							guildId={guild.id}
							data-flx="ui.action-menu.items.guild-menu-data.handle-notification-settings.guild-notification-settings-modal"
						/>
					)),
				);
			},
			handlePrivacySettings: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<GuildPrivacySettingsModal
							guildId={guild.id}
							data-flx="ui.action-menu.items.guild-menu-data.handle-privacy-settings.guild-privacy-settings-modal"
						/>
					)),
				);
			},
			handleEditCommunityProfile: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<UserSettingsModal
							initialGuildId={guild.id}
							initialTab="my_profile"
							data-flx="ui.action-menu.items.guild-menu-data.handle-edit-community-profile.user-settings-modal"
						/>
					)),
				);
			},
			handleLeaveCommunity: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => leaveGuild(guild.id));
			},
			handleDeleteMyMessagesInCommunity: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => deleteMyMessagesInGuild(guild.id));
			},
			handleCopyGuildId: () => {
				void TextCopyCommands.copy(i18n, guild.id);
				onClose();
			},
			handleDebugGuild: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<GuildDebugModal
							title={i18n._(COMMUNITY_DEBUG_DESCRIPTOR)}
							guild={guild}
							data-flx="ui.action-menu.items.guild-menu-data.handle-debug-guild.guild-debug-modal"
						/>
					)),
				);
			},
			handleResetMatureContentAgreeState: () => {
				GuildMatureContentAgree.revokeGuild(guild.id);
				onClose();
			},
			handleReportGuild: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => openReportGuildModal({i18n, guild}));
			},
			handleToggleHideMutedChannels: (checked: boolean) => {
				const currentSettings = UserGuildSettings.getSettingsForScope(guild.id);
				const currentValue = currentSettings?.hide_muted_channels ?? false;
				if (checked === currentValue) return;
				UserGuildSettingsCommands.toggleHideMutedChannels(guild.id);
			},
		}),
		[channels, deleteMyMessagesInGuild, guild, i18n.locale, leaveGuild, onClose],
	);
	const permissions: GuildMenuPermissions = useMemo(
		() => ({
			canManageGuild,
			canManageChannels,
			canInvite,
			canAccessGuildSettings,
			isOwner,
			hasGuildUnread,
			developerMode,
		}),
		[canManageGuild, canManageChannels, canInvite, canAccessGuildSettings, isOwner, hasGuildUnread, developerMode],
	);
	const availableSettingsTabs = useMemo(() => {
		const allTabs = getGuildSettingsTabs(i18n);
		return allTabs.filter((tab) => {
			if (tab.permission) {
				const perms = Array.isArray(tab.permission) ? tab.permission : [tab.permission];
				if (!perms.some((p) => Permission.can(p, {guildId: guild.id}))) {
					return false;
				}
			}
			if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
				return false;
			}
			return true;
		});
	}, [guild.features, guild.id, i18n.locale]);
	const groups = useMemo(() => {
		const menuGroups: Array<MenuGroupType> = [];
		const quickActions: Array<MenuItemType | MenuSubmenuItemType> = [];
		if (hasGuildUnread) {
			quickActions.push({
				icon: <MarkAsReadIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.mark-as-read-icon" />,
				label: i18n._(MARK_AS_READ_DESCRIPTOR),
				onClick: handlers.handleMarkAsRead,
			});
		}
		if (canInvite) {
			quickActions.push({
				icon: <InviteIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.invite-icon" />,
				label: i18n._(INVITE_MEMBERS_DESCRIPTOR),
				onClick: handlers.handleInviteMembers,
			});
		}
		if (canAccessGuildSettings) {
			const settingsSubItems: Array<MenuItemType> = availableSettingsTabs.map((tab) => ({
				id: tab.type,
				label: tab.label,
				onClick: () => {
					if (tab.type === 'members') {
						ModalCommands.runAfterBottomSheetClose(onClose, () =>
							RouterUtils.transitionTo(Routes.guildMembers(guild.id)),
						);
						return;
					}
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<GuildSettingsModal
								guildId={guild.id}
								initialTab={tab.type}
								data-flx="ui.action-menu.items.guild-menu-data.on-click.guild-settings-modal"
							/>
						)),
					);
				},
			}));
			quickActions.push({
				icon: <SettingsIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.settings-icon" />,
				label: i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR),
				items: settingsSubItems,
				onTriggerSelect: handlers.handleCommunitySettings,
			});
		}
		if (canManageChannels) {
			quickActions.push({
				icon: (
					<CreateChannelIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.create-channel-icon" />
				),
				label: i18n._(CREATE_CHANNEL_DESCRIPTOR),
				onClick: handlers.handleCreateChannel,
			});
			quickActions.push({
				icon: (
					<CreateCategoryIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.create-category-icon" />
				),
				label: i18n._(CREATE_CATEGORY_DESCRIPTOR),
				onClick: handlers.handleCreateCategory,
			});
		}
		if (quickActions.length > 0) {
			menuGroups.push({items: quickActions});
		}
		const settingsItems: Array<MenuItemType> = [
			{
				icon: (
					<NotificationSettingsIcon
						size={20}
						data-flx="ui.action-menu.items.guild-menu-data.groups.notification-settings-icon"
					/>
				),
				label: i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR),
				onClick: handlers.handleNotificationSettings,
			},
			{
				icon: (
					<PrivacySettingsIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.privacy-settings-icon" />
				),
				label: i18n._(PRIVACY_SETTINGS_DESCRIPTOR),
				onClick: handlers.handlePrivacySettings,
			},
		];
		if (canEditCommunityProfile) {
			settingsItems.push({
				icon: <EditProfileIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.edit-profile-icon" />,
				label: i18n._(EDIT_COMMUNITY_PROFILE_DESCRIPTOR),
				onClick: handlers.handleEditCommunityProfile,
			});
		}
		menuGroups.push({items: settingsItems});
		const muteItems: Array<MenuItemType | MenuCheckboxType> = [];
		if (onOpenMuteSheet) {
			muteItems.push({
				icon: <MuteIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.mute-icon" />,
				label: isMuted ? i18n._(UNMUTE_COMMUNITY_DESCRIPTOR) : i18n._(MUTE_COMMUNITY_DESCRIPTOR),
				onClick: onOpenMuteSheet,
			});
		}
		const hideMutedChannelsItem: MenuCheckboxType = {
			label: i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR),
			checked: hideMutedChannels,
			onChange: handlers.handleToggleHideMutedChannels,
		};
		muteItems.push(hideMutedChannelsItem);
		menuGroups.push({items: muteItems});
		const dangerActions: Array<MenuItemType> = [
			{
				icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.delete-icon" />,
				label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
				onClick: handlers.handleDeleteMyMessagesInCommunity,
				danger: true,
			},
		];
		if (!isOwner) {
			if (!isStockCommunityGuild(guild.id)) {
				dangerActions.push({
					icon: <LeaveIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.leave-icon" />,
					label: i18n._(LEAVE_COMMUNITY_DESCRIPTOR),
					onClick: handlers.handleLeaveCommunity,
					danger: true,
				});
			}
			dangerActions.push({
				icon: <ReportUserIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.report-user-icon" />,
				label: i18n._(REPORT_COMMUNITY_DESCRIPTOR),
				onClick: handlers.handleReportGuild,
				danger: true,
			});
		}
		menuGroups.push({items: dangerActions});
		if (developerMode) {
			const debugItems: Array<MenuItemType> = [
				{
					icon: (
						<DebugChannelIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.debug-channel-icon" />
					),
					label: i18n._(DEBUG_COMMUNITY_DESCRIPTOR),
					onClick: handlers.handleDebugGuild,
				},
			];
			if (hasCurrentGuildMatureContentGate && GuildMatureContentAgree.hasAgreedToGuild(guild.id)) {
				debugItems.push({
					icon: (
						<DebugChannelIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.debug-channel-icon--2" />
					),
					label: i18n._(RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR),
					onClick: handlers.handleResetMatureContentAgreeState,
				});
			}
			menuGroups.push({items: debugItems});
		}
		const utilityItems: Array<MenuItemType> = [
			{
				icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.guild-menu-data.groups.copy-id-icon" />,
				label: i18n._(COPY_COMMUNITY_ID_DESCRIPTOR),
				onClick: handlers.handleCopyGuildId,
			},
		];
		menuGroups.push({items: utilityItems});
		return menuGroups;
	}, [
		hasGuildUnread,
		canInvite,
		canAccessGuildSettings,
		canManageChannels,
		canEditCommunityProfile,
		availableSettingsTabs,
		guild.id,
		isMuted,
		hideMutedChannels,
		hasCurrentGuildMatureContentGate,
		isOwner,
		developerMode,
		handlers,
		onClose,
		onOpenMuteSheet,
		i18n.locale,
	]);
	return {
		groups,
		handlers,
		permissions,
		isMuted,
		mutedText: mutedText ?? null,
		hideMutedChannels,
	};
}
