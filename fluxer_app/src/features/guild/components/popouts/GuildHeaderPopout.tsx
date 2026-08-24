// SPDX-License-Identifier: AGPL-3.0-or-later

import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import Authentication from '@app/features/auth/state/Authentication';
import {CategoryCreateModal} from '@app/features/channel/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/features/channel/components/modals/ChannelCreateModal';
import {DELETE_MY_MESSAGES_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '@app/features/guild/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '@app/features/guild/components/modals/GuildSettingsModal';
import styles from '@app/features/guild/components/popouts/GuildHeaderPopout.module.css';
import {useDeleteMyMessagesInGuild} from '@app/features/guild/hooks/useDeleteMyMessagesInGuild';
import {useLeaveGuild} from '@app/features/guild/hooks/useLeaveGuild';
import type {Guild} from '@app/features/guild/models/Guild';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import {
	CREATE_CATEGORY_DESCRIPTOR,
	CREATE_CHANNEL_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	LEAVE_COMMUNITY_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	PRIVACY_SETTINGS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {GUILD_SETTINGS_LABEL_DESCRIPTOR} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	BellIcon,
	FolderPlusIcon,
	GearIcon,
	type Icon,
	PlusCircleIcon,
	ShieldIcon,
	SignOutIcon,
	TrashIcon,
	UserCircleIcon,
	UserPlusIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const INVITE_MEMBERS_DESCRIPTOR = msg({
	message: 'Invite members',
	comment: 'Button or menu action label in the guild header popout. Keep it concise.',
});
const EDIT_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Edit community profile',
	comment: 'Button or menu action label in the guild header popout. Keep it concise.',
});
export const GuildHeaderPopoutItem = observer(
	(props: {title: string; icon: Icon; onClick?: () => void; danger?: boolean}) => {
		const handleSelect = useCallback(() => {
			PopoutCommands.close();
			props.onClick?.();
		}, [props]);
		const handleMouseEnter = useCallback((event: React.MouseEvent<HTMLButtonElement>) => {
			event.currentTarget.focus();
		}, []);
		return (
			<FocusRing offset={-2} data-flx="guild.guild-header-popout.guild-header-popout-item.focus-ring">
				<button
					type="button"
					role="menuitem"
					className={clsx(styles.itemButton, props.danger && styles.itemDanger)}
					onClick={handleSelect}
					onMouseEnter={handleMouseEnter}
					data-roving-focus="true"
					data-flx="guild.guild-header-popout.guild-header-popout-item.item-button.select"
				>
					<span className={styles.itemTitle} data-flx="guild.guild-header-popout.guild-header-popout-item.item-title">
						{props.title}
					</span>
					<props.icon
						className={styles.iconMedium}
						data-flx="guild.guild-header-popout.guild-header-popout-item.icon-medium"
					/>
				</button>
			</FocusRing>
		);
	},
);
export const GuildHeaderPopoutCheckboxItem = observer(
	(props: {title: string; checked: boolean; onChange: (checked: boolean) => void}) => {
		const handleChange = useCallback(
			(checked: boolean) => {
				props.onChange(checked);
			},
			[props],
		);
		const handleClick = useCallback(() => {
			props.onChange(!props.checked);
		}, [props]);
		return (
			<FocusRing offset={-2} data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.focus-ring">
				<div
					className={styles.itemButton}
					onMouseEnter={(event) => {
						event.currentTarget.focus();
					}}
					onClick={handleClick}
					onKeyDown={(e) => {
						if (isKeyboardActivationKey(e.key)) {
							e.preventDefault();
							handleClick();
						}
					}}
					role="menuitemcheckbox"
					aria-checked={props.checked}
					tabIndex={0}
					data-roving-focus="true"
					data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.item-button.click"
				>
					<span
						className={styles.itemTitle}
						data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.item-title"
					>
						{props.title}
					</span>
					<div
						className={styles.checkboxIcon}
						data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.checkbox-icon"
					>
						<Checkbox
							checked={props.checked}
							onChange={handleChange}
							noFocus
							size={18.75}
							aria-hidden={true}
							data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.checkbox.change"
						/>
					</div>
				</div>
			</FocusRing>
		);
	},
);
export const GuildHeaderPopout = observer(({guild}: {guild: Guild}) => {
	const {i18n} = useLingui();
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
	const canEditCommunityProfile = Users.getCurrentUser()?.isClaimed() ?? true;
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;
	const handleToggleHideMutedChannels = useCallback(
		(checked: boolean) => {
			const currentSettings = UserGuildSettings.getSettingsForScope(guild.id);
			const currentValue = currentSettings?.hide_muted_channels ?? false;
			if (checked === currentValue) return;
			UserGuildSettingsCommands.toggleHideMutedChannels(guild.id);
		},
		[guild.id],
	);
	const listRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: '[data-roving-focus="true"]',
		manageTabIndex: true,
	});
	const leaveGuild = useLeaveGuild();
	const deleteMyMessagesInGuild = useDeleteMyMessagesInGuild();
	return (
		<div
			className={styles.container}
			ref={listRef}
			role="menu"
			aria-orientation="vertical"
			tabIndex={-1}
			data-autofocus
			data-flx="guild.guild-header-popout.container"
		>
			{canInvite && (
				<GuildHeaderPopoutItem
					icon={UserPlusIcon}
					title={i18n._(INVITE_MEMBERS_DESCRIPTOR)}
					onClick={() => {
						ModalCommands.push(
							modal(() => (
								<InviteModal channelId={invitableChannelId ?? ''} data-flx="guild.guild-header-popout.invite-modal" />
							)),
						);
					}}
					data-flx="guild.guild-header-popout.guild-header-popout-item.push"
				/>
			)}
			{canAccessGuildSettings && (
				<GuildHeaderPopoutItem
					icon={GearIcon}
					title={i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)}
					onClick={() =>
						ModalCommands.push(
							modal(() => (
								<GuildSettingsModal guildId={guild.id} data-flx="guild.guild-header-popout.guild-settings-modal" />
							)),
						)
					}
					data-flx="guild.guild-header-popout.guild-header-popout-item.push--2"
				/>
			)}
			{canManageChannels && (
				<GuildHeaderPopoutItem
					icon={PlusCircleIcon}
					title={i18n._(CREATE_CHANNEL_DESCRIPTOR)}
					onClick={() =>
						ModalCommands.push(
							modal(() => (
								<ChannelCreateModal guildId={guild.id} data-flx="guild.guild-header-popout.channel-create-modal" />
							)),
						)
					}
					data-flx="guild.guild-header-popout.guild-header-popout-item.push--3"
				/>
			)}
			{canManageChannels && (
				<GuildHeaderPopoutItem
					icon={FolderPlusIcon}
					title={i18n._(CREATE_CATEGORY_DESCRIPTOR)}
					onClick={() =>
						ModalCommands.push(
							modal(() => (
								<CategoryCreateModal guildId={guild.id} data-flx="guild.guild-header-popout.category-create-modal" />
							)),
						)
					}
					data-flx="guild.guild-header-popout.guild-header-popout-item.push--4"
				/>
			)}
			<GuildHeaderPopoutItem
				icon={BellIcon}
				title={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
				onClick={() =>
					ModalCommands.push(
						modal(() => (
							<GuildNotificationSettingsModal
								guildId={guild.id}
								data-flx="guild.guild-header-popout.guild-notification-settings-modal"
							/>
						)),
					)
				}
				data-flx="guild.guild-header-popout.guild-header-popout-item.push--5"
			/>
			<GuildHeaderPopoutItem
				icon={ShieldIcon}
				title={i18n._(PRIVACY_SETTINGS_DESCRIPTOR)}
				onClick={() =>
					ModalCommands.push(
						modal(() => (
							<GuildPrivacySettingsModal
								guildId={guild.id}
								data-flx="guild.guild-header-popout.guild-privacy-settings-modal"
							/>
						)),
					)
				}
				data-flx="guild.guild-header-popout.guild-header-popout-item.push--6"
			/>
			{canEditCommunityProfile && (
				<GuildHeaderPopoutItem
					icon={UserCircleIcon}
					title={i18n._(EDIT_COMMUNITY_PROFILE_DESCRIPTOR)}
					onClick={() => {
						ModalCommands.push(
							modal(() => (
								<UserSettingsModal
									initialGuildId={guild.id}
									initialTab="my_profile"
									data-flx="guild.guild-header-popout.user-settings-modal"
								/>
							)),
						);
					}}
					data-flx="guild.guild-header-popout.guild-header-popout-item.push--7"
				/>
			)}
			<GuildHeaderPopoutCheckboxItem
				title={i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
				checked={hideMutedChannels}
				onChange={handleToggleHideMutedChannels}
				data-flx="guild.guild-header-popout.guild-header-popout-checkbox-item.toggle-hide-muted-channels"
			/>
			{!guild.isOwner(Authentication.currentUserId) && (
				<>
					<GuildHeaderPopoutItem
						danger={true}
						icon={TrashIcon}
						onClick={() => deleteMyMessagesInGuild(guild.id)}
						title={i18n._(DELETE_MY_MESSAGES_DESCRIPTOR)}
						data-flx="guild.guild-header-popout.guild-header-popout-item.delete-my-messages-in-guild"
					/>
					{!isStockCommunityGuild(guild.id) && (
						<GuildHeaderPopoutItem
							danger={true}
							icon={SignOutIcon}
							onClick={() => leaveGuild(guild.id)}
							title={i18n._(LEAVE_COMMUNITY_DESCRIPTOR)}
							data-flx="guild.guild-header-popout.guild-header-popout-item.leave-guild"
						/>
					)}
				</>
			)}
		</div>
	);
});
