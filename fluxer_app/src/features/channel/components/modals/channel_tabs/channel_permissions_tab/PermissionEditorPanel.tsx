// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PermissionOverwriteCategory,
	type PermissionState,
	PermissionStateButtons,
} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import {LayoutToggleButtons} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/LayoutToggleButtons';
import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import type * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, TrashIcon} from '@phosphor-icons/react';
import type React from 'react';

const SEARCH_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Search permissions…',
	comment:
		'Placeholder text in the permissions search input on the channel permissions tab. Trailing ellipsis is intentional.',
});

interface PermissionEditorPanelProps {
	selectedOverwrite: PermissionOverwrite;
	isMobile: boolean;
	isEveryoneSelected: boolean;
	canManageChannels: boolean;
	canManageRoles: boolean;
	onMobileBack: () => void;
	getOverwriteName: (overwrite: PermissionOverwrite) => string;
	onDeleteOverride: () => void;
	allPermissionsState: PermissionState | undefined;
	onSetAllPermissions: (state: PermissionState) => void;
	permissionSearchQuery: string;
	setPermissionSearchQuery: (query: string) => void;
	filteredPermissionSpecs: Array<PermissionUtils.PermissionSpec>;
	onPermissionChange: (permission: bigint, state: PermissionState) => void;
	getPermissionDisabledReason: (permission: bigint) => string | undefined;
	getPermissionWarning: (permission: bigint) => string | undefined;
}

export const PermissionEditorPanel: React.FC<PermissionEditorPanelProps> = ({
	selectedOverwrite,
	isMobile,
	isEveryoneSelected,
	canManageChannels,
	canManageRoles,
	onMobileBack,
	getOverwriteName,
	onDeleteOverride,
	allPermissionsState,
	onSetAllPermissions,
	permissionSearchQuery,
	setPermissionSearchQuery,
	filteredPermissionSpecs,
	onPermissionChange,
	getPermissionDisabledReason,
	getPermissionWarning,
}) => {
	const {i18n} = useLingui();
	return (
		<>
			{isMobile && (
				<div className={styles.mobileBackRow} data-flx="channel.channel-tabs.channel-permissions-tab.mobile-back-row">
					<Button
						variant="secondary"
						small={true}
						onClick={onMobileBack}
						data-flx="channel.channel-tabs.channel-permissions-tab.button.mobile-back"
					>
						<Trans>Back to overrides</Trans>
					</Button>
				</div>
			)}
			<div className={styles.sectionRow} data-flx="channel.channel-tabs.channel-permissions-tab.section-row">
				<div className={styles.sectionHeader} data-flx="channel.channel-tabs.channel-permissions-tab.section-header">
					<h2 className={styles.sectionTitle} data-flx="channel.channel-tabs.channel-permissions-tab.section-title">
						<Trans>Edit access for {getOverwriteName(selectedOverwrite)}</Trans>
					</h2>
					<p className={styles.subtleText} data-flx="channel.channel-tabs.channel-permissions-tab.subtle-text">
						{isEveryoneSelected ? (
							<Trans>Configure base access for this channel</Trans>
						) : selectedOverwrite?.type === 0 ? (
							<Trans>Configure overrides for this role</Trans>
						) : (
							<Trans>Configure overrides for this member</Trans>
						)}
					</p>
				</div>
				{!isEveryoneSelected && (
					<Button
						variant="secondary"
						small={true}
						leftIcon={
							<TrashIcon size={remFromPx(18)} data-flx="channel.channel-tabs.channel-permissions-tab.trash-icon" />
						}
						onClick={onDeleteOverride}
						disabled={!canManageChannels || !canManageRoles}
						data-flx="channel.channel-tabs.channel-permissions-tab.button.delete-override"
					>
						<Trans>Remove override</Trans>
					</Button>
				)}
			</div>
			<div className={styles.sectionRow} data-flx="channel.channel-tabs.channel-permissions-tab.section-row--2">
				<div className={styles.permHeaderRow} data-flx="channel.channel-tabs.channel-permissions-tab.perm-header-row">
					<p className={styles.permHelp} data-flx="channel.channel-tabs.channel-permissions-tab.perm-help">
						<Trans>Use these buttons to quickly set all permissions.</Trans>
					</p>
					<PermissionStateButtons
						currentState={allPermissionsState}
						onStateChange={onSetAllPermissions}
						disabled={!canManageChannels || !canManageRoles}
						data-flx="channel.channel-tabs.channel-permissions-tab.permission-state-buttons"
					/>
				</div>
			</div>
			<div className={styles.permSearchRow} data-flx="channel.channel-tabs.channel-permissions-tab.perm-search-row">
				<Input
					type="text"
					placeholder={i18n._(SEARCH_PERMISSIONS_DESCRIPTOR)}
					value={permissionSearchQuery}
					onChange={(e) => setPermissionSearchQuery(e.target.value)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="channel.channel-tabs.channel-permissions-tab.magnifying-glass-icon"
						/>
					}
					className={styles.permSearchInput}
					data-flx="channel.channel-tabs.channel-permissions-tab.perm-search-input.set-permission-search-query.text"
				/>
				<LayoutToggleButtons data-flx="channel.channel-tabs.channel-permissions-tab.permission-editor-panel.layout-toggle-buttons" />
			</div>
			<div className={styles.permCategories} data-flx="channel.channel-tabs.channel-permissions-tab.perm-categories">
				{filteredPermissionSpecs.map((spec, index) => (
					<PermissionOverwriteCategory
						key={spec.title}
						spec={spec}
						allow={selectedOverwrite.allow}
						deny={selectedOverwrite.deny}
						onPermissionChange={onPermissionChange}
						disabled={!canManageChannels || !canManageRoles}
						getPermissionDisabledReason={getPermissionDisabledReason}
						getPermissionWarning={getPermissionWarning}
						isFirst={index === 0}
						data-flx="channel.channel-tabs.channel-permissions-tab.permission-overwrite-category"
					/>
				))}
				{filteredPermissionSpecs.length === 0 && permissionSearchQuery && (
					<div className={styles.emptyState} data-flx="channel.channel-tabs.channel-permissions-tab.empty-state">
						<Trans>No permissions found</Trans>
					</div>
				)}
			</div>
		</>
	);
};
