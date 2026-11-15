// SPDX-License-Identifier: AGPL-3.0-or-later

import {AddOverridePopout} from '@app/features/app/components/dialogs/shared/AddOverridePopout';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import {MobileOverrideRow} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/MobileOverrideRow';
import {SyncWithParentBanner} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/SyncWithParentBanner';
import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import type {Channel} from '@app/features/channel/models/Channel';
import {Button} from '@app/features/ui/button/Button';
import type {User} from '@app/features/user/models/User';
import {type FloatingContext, FloatingFocusManager} from '@floating-ui/react';
import {Trans} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface MobileOverrideListViewProps {
	guildId: string;
	parentChannel: Channel | null | undefined;
	isSyncedWithParent: boolean | null;
	canManageChannels: boolean;
	canManageRoles: boolean;
	onSyncWithParent: () => void;
	overwritesWithUpdates: Array<PermissionOverwrite>;
	isAddOverrideOpen: boolean;
	setIsAddOverrideOpen: (open: boolean) => void;
	addOverrideContext: FloatingContext;
	addOverrideFloatingStyles: React.CSSProperties;
	addOverrideFloatingRef: (node: HTMLElement | null) => void;
	getAddOverrideFloatingProps: () => Record<string, unknown>;
	existingOverwriteIds: Set<string>;
	onAddOverride: (id: string, type: 0 | 1, name: string) => void;
	getOverwriteName: (overwrite: PermissionOverwrite) => string;
	getOverwriteColor: (overwrite: PermissionOverwrite) => number | undefined;
	getOverwriteUser: (overwrite: PermissionOverwrite) => User | null;
	onMobileOverwriteSelect: (overwriteId: string) => void;
}

export const MobileOverrideListView: React.FC<MobileOverrideListViewProps> = observer(
	({
		guildId,
		parentChannel,
		isSyncedWithParent,
		canManageChannels,
		canManageRoles,
		onSyncWithParent,
		overwritesWithUpdates,
		isAddOverrideOpen,
		setIsAddOverrideOpen,
		addOverrideContext,
		addOverrideFloatingStyles,
		addOverrideFloatingRef,
		getAddOverrideFloatingProps,
		existingOverwriteIds,
		onAddOverride,
		getOverwriteName,
		getOverwriteColor,
		getOverwriteUser,
		onMobileOverwriteSelect,
	}) => {
		return (
			<div className={styles.container} data-flx="channel.channel-tabs.channel-permissions-tab.container">
				{isSyncedWithParent !== null && (
					<SyncWithParentBanner
						isSyncedWithParent={isSyncedWithParent}
						parentChannel={parentChannel}
						canManageChannels={canManageChannels}
						canManageRoles={canManageRoles}
						onSync={onSyncWithParent}
						variant="mobile"
						data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-list-view.sync-with-parent-banner"
					/>
				)}
				<div
					className={styles.mobileOverrideList}
					data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-list"
				>
					<div
						className={styles.mobileListHeader}
						data-flx="channel.channel-tabs.channel-permissions-tab.mobile-list-header"
					>
						<h2
							className={styles.mobileListTitle}
							data-flx="channel.channel-tabs.channel-permissions-tab.mobile-list-title"
						>
							<Trans>Access overrides</Trans>
						</h2>
						<Button
							variant="secondary"
							small={true}
							leftIcon={
								<PlusIcon size={18} weight="bold" data-flx="channel.channel-tabs.channel-permissions-tab.plus-icon" />
							}
							onClick={() => setIsAddOverrideOpen(!isAddOverrideOpen)}
							disabled={!canManageChannels || !canManageRoles}
							data-flx="channel.channel-tabs.channel-permissions-tab.button.set-is-add-override-open"
						>
							<Trans>Add override</Trans>
						</Button>
					</div>
					{isAddOverrideOpen && (
						<FloatingFocusManager
							context={addOverrideContext}
							modal={true}
							data-flx="channel.channel-tabs.channel-permissions-tab.floating-focus-manager"
						>
							<div
								ref={addOverrideFloatingRef}
								style={{
									...addOverrideFloatingStyles,
									zIndex: 99999,
								}}
								data-flx="channel.channel-tabs.channel-permissions-tab.div--2"
								{...getAddOverrideFloatingProps()}
							>
								<AddOverridePopout
									guildId={guildId}
									existingOverwriteIds={existingOverwriteIds}
									onSelect={onAddOverride}
									onClose={() => setIsAddOverrideOpen(false)}
									data-flx="channel.channel-tabs.channel-permissions-tab.add-override-popout"
								/>
							</div>
						</FloatingFocusManager>
					)}
					<div
						className={styles.mobileOverrides}
						data-flx="channel.channel-tabs.channel-permissions-tab.mobile-overrides"
					>
						{overwritesWithUpdates.map((overwrite) => {
							const name = getOverwriteName(overwrite);
							const color = getOverwriteColor(overwrite);
							const user = getOverwriteUser(overwrite);
							const isEveryone = overwrite.id === guildId;
							const roleId = overwrite.type === 0 ? overwrite.id : null;
							return (
								<MobileOverrideRow
									key={overwrite.id}
									overwrite={overwrite}
									name={name}
									color={color}
									user={user}
									roleId={roleId}
									isEveryone={isEveryone}
									guildId={guildId}
									onClick={() => onMobileOverwriteSelect(overwrite.id)}
									data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-list-view.mobile-override-row.mobile-overwrite-select"
								/>
							);
						})}
					</div>
				</div>
			</div>
		);
	},
);
