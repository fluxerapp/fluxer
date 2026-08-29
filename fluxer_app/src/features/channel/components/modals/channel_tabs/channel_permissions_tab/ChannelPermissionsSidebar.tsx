// SPDX-License-Identifier: AGPL-3.0-or-later

import {AddOverridePopout} from '@app/features/app/components/dialogs/shared/AddOverridePopout';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import {OverwriteItem} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/OverwriteItem';
import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import {Button} from '@app/features/ui/button/Button';
import type {User} from '@app/features/user/models/User';
import {type FloatingContext, FloatingFocusManager} from '@floating-ui/react';
import {Trans} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import type React from 'react';

interface ChannelPermissionsSidebarProps {
	guildId: string;
	overwritesWithUpdates: Array<PermissionOverwrite>;
	selectedOverwriteId: string | null;
	canManageChannels: boolean;
	canManageRoles: boolean;
	canManageOverwrites: boolean;
	onDeleteOverwrite: (overwriteId: string) => void;
	isAddOverrideOpen: boolean;
	setIsAddOverrideOpen: (open: boolean) => void;
	existingOverwriteIds: Set<string>;
	addOverrideContext: FloatingContext;
	addOverrideFloatingStyles: React.CSSProperties;
	addOverrideReferenceRef: (node: HTMLElement | null) => void;
	addOverrideFloatingRef: (node: HTMLElement | null) => void;
	addOverrideReferenceWidth: number | undefined;
	getAddOverrideReferenceProps: () => Record<string, unknown>;
	getAddOverrideFloatingProps: () => Record<string, unknown>;
	onAddOverride: (id: string, type: 0 | 1, name: string) => void;
	onSelectOverwrite: (overwriteId: string) => void;
	getOverwriteName: (overwrite: PermissionOverwrite) => string;
	getOverwriteColor: (overwrite: PermissionOverwrite) => number | undefined;
	getOverwriteUser: (overwrite: PermissionOverwrite) => User | null;
}

export const ChannelPermissionsSidebar: React.FC<ChannelPermissionsSidebarProps> = ({
	guildId,
	overwritesWithUpdates,
	selectedOverwriteId,
	canManageChannels,
	canManageRoles,
	canManageOverwrites,
	onDeleteOverwrite,
	isAddOverrideOpen,
	setIsAddOverrideOpen,
	existingOverwriteIds,
	addOverrideContext,
	addOverrideFloatingStyles,
	addOverrideReferenceRef,
	addOverrideFloatingRef,
	addOverrideReferenceWidth,
	getAddOverrideReferenceProps,
	getAddOverrideFloatingProps,
	onAddOverride,
	onSelectOverwrite,
	getOverwriteName,
	getOverwriteColor,
	getOverwriteUser,
}) => {
	return (
		<div data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.div">
			<div
				className={styles.leftTitle}
				style={{padding: '6px 8px'}}
				data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.left-title"
			>
				<Trans>Access overrides</Trans>
			</div>
			<div
				ref={addOverrideReferenceRef}
				data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.div--2"
				{...getAddOverrideReferenceProps()}
				style={{padding: '0 8px 8px 8px'}}
			>
				<Button
					variant="secondary"
					small={true}
					disabled={!canManageChannels || !canManageRoles}
					leftIcon={
						<PlusIcon
							size={18}
							weight="bold"
							data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.plus-icon"
						/>
					}
					onClick={() => setIsAddOverrideOpen(!isAddOverrideOpen)}
					data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.button.set-is-add-override-open"
				>
					<Trans>Add override</Trans>
				</Button>
			</div>
			{isAddOverrideOpen && (
				<FloatingFocusManager
					context={addOverrideContext}
					modal={true}
					data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.floating-focus-manager"
				>
					<div
						ref={addOverrideFloatingRef}
						style={{
							...addOverrideFloatingStyles,
							zIndex: 99999,
							width: addOverrideReferenceWidth ?? 'auto',
							left: addOverrideFloatingStyles.left,
						}}
						data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.div--3"
						{...getAddOverrideFloatingProps()}
					>
						<AddOverridePopout
							guildId={guildId}
							existingOverwriteIds={existingOverwriteIds}
							onSelect={onAddOverride}
							onClose={() => setIsAddOverrideOpen(false)}
							data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.add-override-popout"
						/>
					</div>
				</FloatingFocusManager>
			)}
			<div
				style={{padding: '0 8px 8px 8px'}}
				data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.div--4"
			>
				{overwritesWithUpdates.map((overwrite) => {
					const name = getOverwriteName(overwrite);
					const color = getOverwriteColor(overwrite);
					const user = getOverwriteUser(overwrite);
					const isSelected = selectedOverwriteId === overwrite.id;
					const isEveryone = overwrite.id === guildId;
					const roleId = overwrite.type === 0 ? overwrite.id : null;
					return (
						<OverwriteItem
							key={overwrite.id}
							overwrite={overwrite}
							name={name}
							color={color}
							user={user}
							roleId={roleId}
							isSelected={isSelected}
							isEveryone={isEveryone}
							canDelete={canManageOverwrites && !isEveryone}
							onDelete={onDeleteOverwrite}
							onClick={() => onSelectOverwrite(overwrite.id)}
							guildId={guildId}
							data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.overwrite-item.set-selected-overwrite-id"
						/>
					);
				})}
			</div>
		</div>
	);
};
