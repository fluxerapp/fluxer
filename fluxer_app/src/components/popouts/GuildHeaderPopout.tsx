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

import {useLingui} from '@lingui/react/macro';
import {
	BellIcon,
	FolderPlusIcon,
	GearIcon,
	type Icon,
	PlusCircleIcon,
	ShieldIcon,
	SignOutIcon,
	UserCircleIcon,
	UserPlusIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {Permissions} from '~/Constants';
import {CategoryCreateModal} from '~/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '~/components/modals/ChannelCreateModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '~/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '~/components/modals/GuildSettingsModal';
import {InviteModal} from '~/components/modals/InviteModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useLeaveGuild} from '~/hooks/useLeaveGuild';
import {useRovingFocusList} from '~/hooks/useRovingFocusList';
import type {GuildRecord} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import PermissionStore from '~/stores/PermissionStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import * as InviteUtils from '~/utils/InviteUtils';
import styles from './GuildHeaderPopout.module.css';

const GuildHeaderPopoutItem = observer((props: {title: string; icon: Icon; onClick?: () => void; danger?: boolean}) => {
	const handleSelect = React.useCallback(() => {
		PopoutActionCreators.close();
		props.onClick?.();
	}, [props]);

	const handleMouseEnter = React.useCallback((event: React.MouseEvent<HTMLButtonElement>) => {
		event.currentTarget.focus();
	}, []);

	return (
		<FocusRing offset={-2}>
			<button
				type="button"
				className={clsx(styles.itemButton, props.danger && styles.itemDanger)}
				onClick={handleSelect}
				onMouseEnter={handleMouseEnter}
				data-roving-focus="true"
			>
				<span>{props.title}</span>
				<props.icon className={styles.iconMedium} />
			</button>
		</FocusRing>
	);
});

const GuildHeaderPopoutCheckboxItem = observer(
	(props: {title: string; checked: boolean; onChange: (checked: boolean) => void}) => {
		const [isHovered, setIsHovered] = React.useState(false);
		const [isFocused, setIsFocused] = React.useState(false);

		const handleChange = React.useCallback(
			(checked: boolean) => {
				props.onChange(checked);
			},
			[props],
		);

		const handleClick = React.useCallback(() => {
			props.onChange(!props.checked);
		}, [props]);

		return (
			<FocusRing offset={-2}>
				<div
					className={styles.itemButton}
					onMouseEnter={(event) => {
						setIsHovered(true);
						event.currentTarget.focus();
					}}
					onMouseLeave={() => setIsHovered(false)}
					onClick={handleClick}
					onFocus={() => setIsFocused(true)}
					onBlur={() => setIsFocused(false)}
					onKeyDown={(e) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
							handleClick();
						}
					}}
					role="button"
					tabIndex={0}
					data-roving-focus="true"
				>
					<span>{props.title}</span>
					<div className={styles.checkboxIcon}>
						<Checkbox
							checked={props.checked}
							onChange={handleChange}
							noFocus
							size={18.75}
							inverted={isHovered || isFocused}
							aria-hidden={true}
						/>
					</div>
				</div>
			</FocusRing>
		);
	},
);

export const GuildHeaderPopout = observer(({guild}: {guild: GuildRecord}) => {
	const {t} = useLingui();
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

	const handleToggleHideMutedChannels = React.useCallback(
		(checked: boolean) => {
			const currentSettings = UserGuildSettingsStore.getSettings(guild.id);
			const currentValue = currentSettings?.hide_muted_channels ?? false;
			if (checked === currentValue) return;
			UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
		},
		[guild.id],
	);

	const listRef = useRovingFocusList<HTMLDivElement>({
		autoFocusFirst: true,
		focusableSelector: '[data-roving-focus="true"]',
	});
	const leaveGuild = useLeaveGuild();

	return (
		<div className={styles.container} ref={listRef} style={{width: 'var(--layout-header-popout-width)'}}>
			{canInvite && (
				<GuildHeaderPopoutItem
					icon={UserPlusIcon}
					title={t`Invite Members`}
					onClick={() => {
						ModalActionCreators.push(modal(() => <InviteModal channelId={invitableChannelId ?? ''} />));
					}}
				/>
			)}
			{canAccessGuildSettings && (
				<GuildHeaderPopoutItem
					icon={GearIcon}
					title={t`Community Settings`}
					onClick={() => ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} />))}
				/>
			)}
			{canManageChannels && (
				<GuildHeaderPopoutItem
					icon={PlusCircleIcon}
					title={t`Create Channel`}
					onClick={() => ModalActionCreators.push(modal(() => <ChannelCreateModal guildId={guild.id} />))}
				/>
			)}
			{canManageChannels && (
				<GuildHeaderPopoutItem
					icon={FolderPlusIcon}
					title={t`Create Category`}
					onClick={() => ModalActionCreators.push(modal(() => <CategoryCreateModal guildId={guild.id} />))}
				/>
			)}
			<GuildHeaderPopoutItem
				icon={BellIcon}
				title={t`Notification Settings`}
				onClick={() => ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guild.id} />))}
			/>
			<GuildHeaderPopoutItem
				icon={ShieldIcon}
				title={t`Privacy Settings`}
				onClick={() => ModalActionCreators.push(modal(() => <GuildPrivacySettingsModal guildId={guild.id} />))}
			/>
			<GuildHeaderPopoutItem
				icon={UserCircleIcon}
				title={t`Edit Community Profile`}
				onClick={() => {
					ModalActionCreators.push(
						modal(() => <UserSettingsModal initialGuildId={guild.id} initialTab="my_profile" />),
					);
				}}
			/>
			<GuildHeaderPopoutCheckboxItem
				title={t`Hide Muted Channels`}
				checked={hideMutedChannels}
				onChange={handleToggleHideMutedChannels}
			/>
			{!guild.isOwner(AuthenticationStore.currentUserId) && (
				<GuildHeaderPopoutItem
					danger={true}
					icon={SignOutIcon}
					onClick={() => leaveGuild(guild.id)}
					title={t`Leave Community`}
				/>
			)}
		</div>
	);
});
