// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import {OverwriteContextMenu} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/OverwriteContextMenu';
import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import type {User} from '@app/features/user/models/User';
import {UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

interface OverwriteItemProps {
	overwrite: PermissionOverwrite;
	name: string;
	color?: number;
	user?: User | null;
	roleId?: string | null;
	isSelected: boolean;
	isEveryone: boolean;
	canDelete: boolean;
	onDelete: (overwriteId: string) => void;
	onClick: () => void;
	guildId: string;
}

export const OverwriteItem: React.FC<OverwriteItemProps> = observer(
	({overwrite, name, color, user, roleId, isSelected, isEveryone, canDelete, onDelete, onClick, guildId}) => {
		const buttonRef = useRef<HTMLButtonElement>(null);
		const contextMenuOpen = useContextMenuHoverState(buttonRef);
		const handleContextMenu = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<OverwriteContextMenu
						overwrite={overwrite}
						roleId={roleId ?? null}
						user={user ?? null}
						canDelete={canDelete}
						onDelete={() => onDelete(overwrite.id)}
						onClose={onClose}
						data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.handle-context-menu.overwrite-context-menu"
					/>
				));
			},
			[overwrite, roleId, user, canDelete, onDelete],
		);
		return (
			<button
				ref={buttonRef}
				type="button"
				aria-pressed={isSelected}
				className={clsx(styles.overwriteItem, {
					[styles.overwriteItemSelected]: isSelected,
					[styles.overwriteItemContextMenuOpen]: contextMenuOpen && !isSelected,
				})}
				onClick={onClick}
				onContextMenu={handleContextMenu}
				data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.overwrite-item.click.button"
			>
				{overwrite.type === 0 && !isEveryone ? (
					<div
						className={styles.roleDot}
						style={{backgroundColor: color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(color || 0)}}
						data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.role-dot"
					/>
				) : overwrite.type === 1 && user ? (
					<Avatar
						user={user}
						size={12}
						className={styles.overwriteIcon}
						guildId={guildId}
						data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.overwrite-icon"
					/>
				) : (
					<UsersIcon
						className={styles.overwriteIcon}
						data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.overwrite-icon--2"
					/>
				)}
				<span
					className={styles.overwriteName}
					data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-item.overwrite-name"
				>
					{name}
				</span>
			</button>
		);
	},
);
