// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import {Avatar} from '@app/features/ui/components/Avatar';
import {CaretRightIcon} from '@app/features/ui/icons/DirectionalIcons';
import type {User} from '@app/features/user/models/User';
import {UsersIcon} from '@phosphor-icons/react';
import type React from 'react';

interface MobileOverrideRowProps {
	overwrite: PermissionOverwrite;
	name: string;
	color?: number;
	user: User | null;
	roleId: string | null;
	isEveryone: boolean;
	guildId: string;
	onClick: () => void;
}

export const MobileOverrideRow: React.FC<MobileOverrideRowProps> = ({
	overwrite,
	name,
	color,
	user,
	roleId,
	isEveryone,
	guildId,
	onClick,
}) => {
	let icon: React.ReactNode;
	if (overwrite.type === 0 && !isEveryone) {
		icon = (
			<div
				className={styles.roleDot}
				style={{
					backgroundColor: color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(color || 0),
				}}
				data-flx="channel.channel-tabs.channel-permissions-tab.role-dot"
			/>
		);
	} else if (overwrite.type === 1 && user) {
		icon = (
			<Avatar
				user={user}
				size={12}
				className={styles.overwriteIcon}
				guildId={guildId}
				data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-icon"
			/>
		);
	} else {
		icon = (
			<UsersIcon
				className={styles.overwriteIcon}
				data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-icon--2"
			/>
		);
	}
	return (
		<button
			type="button"
			className={styles.mobileOverrideItem}
			onClick={onClick}
			onContextMenu={roleId ? (event) => openRoleContextMenu(event, roleId) : undefined}
			data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-item.mobile-overwrite-select.button"
		>
			{icon}
			<span
				className={styles.mobileOverrideName}
				data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-name"
			>
				{name}
			</span>
			<CaretRightIcon
				className={styles.mobileOverrideChevron}
				size={remFromPx(20)}
				weight="bold"
				data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-chevron"
			/>
		</button>
	);
};
