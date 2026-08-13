// SPDX-License-Identifier: AGPL-3.0-or-later

import {PermissionRoleCategory} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab.module.css';
import type {RoleUpdate} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import PermissionLayout from '@app/features/permissions/state/PermissionLayout';
import type * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {ColorPickerField} from '@app/features/ui/components/form/ColorPickerField';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GridFourIcon, ListIcon, MagnifyingGlassIcon, RowsIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const ROLE_NAME_DESCRIPTOR = msg({
	message: 'Role name',
	comment: 'Label of the role name input in the community role editor.',
});
const ROLE_COLOR_DESCRIPTOR = msg({
	message: 'Role color',
	comment: 'Label of the role color picker in the community role editor. American English spelling is canonical.',
});
const TYPE_A_COLOR_HEX_RGB_HSL_OR_NAME_DESCRIPTOR = msg({
	message: 'Type a color (hex, rgb(), hsl(), or name) or use the picker.',
	comment:
		'Helper text under the role color input. Lists accepted color formats; the token names "hex", "rgb()", "hsl()" are CSS-standard and should not be translated.',
});
const SHOW_THIS_ROLE_SEPARATELY_DESCRIPTOR = msg({
	message: 'Show this role separately',
	comment:
		'Switch label in the role editor (hoist toggle). When on, members with this role appear in their own member-list section.',
});
const LISTS_MEMBERS_WITH_THIS_ROLE_IN_THEIR_OWN_DESCRIPTOR = msg({
	message: 'Lists members with this role in their own section in the member list.',
	comment: 'Helper text under the Show this role separately switch in the role editor.',
});
const ALLOW_MENTIONS_FOR_THIS_ROLE_DESCRIPTOR = msg({
	message: 'Allow mentions for this role',
	comment: 'Switch label in the role editor (mentionable toggle). When on, any member can mention this role in chat.',
});
const MEMBERS_WITH_THE_PERMISSION_CAN_ALWAYS_MENTION_ROLES_DESCRIPTOR = msg({
	message:
		'Members with the "{mentionEveryonePermissionLabel}" permission can always mention roles, regardless of this setting.',
	comment:
		'Helper text under the Allow mentions switch in the role editor. {mentionEveryonePermissionLabel} is the localized name of the Mention Everyone permission and should match its label exactly.',
});
const SEARCH_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Search permissions',
	comment: 'Placeholder in the search input above the permission list in the role editor.',
});
const DENSE_LAYOUT_DESCRIPTOR = msg({
	message: 'Dense layout',
	comment: 'Status text or tooltip indicating the role editor permission list is currently in dense (compact) layout.',
});
const COMFY_LAYOUT_DESCRIPTOR = msg({
	message: 'Comfy layout',
	comment: 'Status text or tooltip indicating the role editor permission list is currently in comfy (spacious) layout.',
});
const SWITCH_TO_DENSE_LAYOUT_DESCRIPTOR = msg({
	message: 'Switch to dense layout',
	comment: 'Tooltip on the layout toggle button in the role editor that switches the permission list to dense layout.',
});
const SWITCH_TO_COMFY_LAYOUT_DESCRIPTOR = msg({
	message: 'Switch to comfy layout',
	comment: 'Tooltip on the layout toggle button in the role editor that switches the permission list to comfy layout.',
});
const SINGLE_COLUMN_DESCRIPTOR = msg({
	message: 'Single column',
	comment: 'Status text or tooltip indicating the role editor permission list is currently in single-column layout.',
});
const TWO_COLUMNS_DESCRIPTOR = msg({
	message: 'Two columns',
	comment: 'Status text or tooltip indicating the role editor permission list is currently in two-column layout.',
});
const SWITCH_TO_SINGLE_COLUMN_DESCRIPTOR = msg({
	message: 'Switch to single column',
	comment: 'Tooltip on the column toggle button in the role editor that switches the permission list to one column.',
});
const SWITCH_TO_TWO_COLUMNS_DESCRIPTOR = msg({
	message: 'Switch to two columns',
	comment: 'Tooltip on the column toggle button in the role editor that switches the permission list to two columns.',
});

interface RoleEditorProps {
	rolesScrollerKey: string;
	isMobile: boolean;
	selectedRole: GuildRole | null;
	selectedRoleWithUpdates: GuildRole | null;
	selectedRoleLocked: boolean;
	canManageRoles: boolean;
	mentionEveryonePermissionLabel: string;
	permissionSearchQuery: string;
	filteredPermissionSpecs: Array<PermissionUtils.PermissionSpec>;
	onMobileBack: () => void;
	onDeleteRole: () => void;
	onRoleUpdate: (roleId: string, updates: Partial<RoleUpdate>) => void;
	onClearPermissions: () => void;
	onPermissionToggle: (permission: bigint) => void;
	onPermissionSearchQueryChange: (query: string) => void;
	getPermissionDisabledReason: (permission: bigint) => string | undefined;
	getPermissionWarning: (permission: bigint) => string | undefined;
}

export const RoleEditor: React.FC<RoleEditorProps> = observer(
	({
		rolesScrollerKey,
		isMobile,
		selectedRole,
		selectedRoleWithUpdates,
		selectedRoleLocked,
		canManageRoles,
		mentionEveryonePermissionLabel,
		permissionSearchQuery,
		filteredPermissionSpecs,
		onMobileBack,
		onDeleteRole,
		onRoleUpdate,
		onClearPermissions,
		onPermissionToggle,
		onPermissionSearchQueryChange,
		getPermissionDisabledReason,
		getPermissionWarning,
	}) => {
		const {i18n} = useLingui();
		return (
			<div className={styles.container} data-flx="guild.guild-tabs.guild-roles-tab.container--2">
				<div className={styles.right} data-flx="guild.guild-tabs.guild-roles-tab.right">
					<div
						className={styles.rightScroller}
						key={rolesScrollerKey}
						data-flx="guild.guild-tabs.guild-roles-tab.right-scroller"
					>
						{selectedRoleWithUpdates && (
							<>
								{isMobile && (
									<div className={styles.mobileBackRow} data-flx="guild.guild-tabs.guild-roles-tab.mobile-back-row">
										<Button
											variant="secondary"
											small={true}
											onClick={onMobileBack}
											data-flx="guild.guild-tabs.guild-roles-tab.button.mobile-back"
										>
											<Trans>Back to roles</Trans>
										</Button>
									</div>
								)}
								<div className={styles.sectionRow} data-flx="guild.guild-tabs.guild-roles-tab.section-row">
									<div className={styles.sectionHeader} data-flx="guild.guild-tabs.guild-roles-tab.section-header">
										<h2 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-roles-tab.section-title">
											<Trans>Edit "{selectedRoleWithUpdates.name}"</Trans>
										</h2>
										<p className={styles.subtleText} data-flx="guild.guild-tabs.guild-roles-tab.subtle-text">
											<Trans>Configure role settings and permissions</Trans>
										</p>
									</div>
									{!selectedRoleWithUpdates.isEveryone && (
										<Button
											variant="secondary"
											small={true}
											leftIcon={
												<TrashIcon size={remFromPx(18)} data-flx="guild.guild-tabs.guild-roles-tab.trash-icon" />
											}
											onClick={onDeleteRole}
											disabled={selectedRoleLocked || !canManageRoles}
											data-flx="guild.guild-tabs.guild-roles-tab.button.delete-role"
										>
											<Trans>Delete role</Trans>
										</Button>
									)}
								</div>
								<div className={styles.sectionSubtitle} data-flx="guild.guild-tabs.guild-roles-tab.section-subtitle">
									<Trans>Display</Trans>
								</div>
								<div className={styles.displayRow} data-flx="guild.guild-tabs.guild-roles-tab.display-row">
									{!selectedRoleWithUpdates.isEveryone && (
										<div className={styles.nameField} data-flx="guild.guild-tabs.guild-roles-tab.name-field">
											<Input
												type="text"
												label={i18n._(ROLE_NAME_DESCRIPTOR)}
												value={selectedRoleWithUpdates.name}
												onChange={(e) => selectedRole && onRoleUpdate(selectedRole.id, {name: e.target.value})}
												disabled={selectedRoleLocked || !canManageRoles}
												maxLength={100}
												data-flx="guild.guild-tabs.guild-roles-tab.input.text"
											/>
										</div>
									)}
									<div className={styles.colorField} data-flx="guild.guild-tabs.guild-roles-tab.color-field">
										<ColorPickerField
											label={i18n._(ROLE_COLOR_DESCRIPTOR)}
											description={i18n._(TYPE_A_COLOR_HEX_RGB_HSL_OR_NAME_DESCRIPTOR)}
											value={selectedRoleWithUpdates.color === 0 ? 0x000000 : Number(selectedRoleWithUpdates.color)}
											onChange={(num) => {
												const clean = num >>> 0;
												if (selectedRole) {
													onRoleUpdate(selectedRole.id, {color: clean === 0 ? 0 : clean});
												}
											}}
											disabled={selectedRoleLocked || !canManageRoles}
											data-flx="guild.guild-tabs.guild-roles-tab.color-picker-field"
										/>
									</div>
								</div>
								{!selectedRoleWithUpdates.isEveryone && (
									<div className={styles.settingsGroup} data-flx="guild.guild-tabs.guild-roles-tab.settings-group">
										<Switch
											label={i18n._(SHOW_THIS_ROLE_SEPARATELY_DESCRIPTOR)}
											description={i18n._(LISTS_MEMBERS_WITH_THIS_ROLE_IN_THEIR_OWN_DESCRIPTOR)}
											value={selectedRoleWithUpdates.hoist}
											onChange={(value) => selectedRole && onRoleUpdate(selectedRole.id, {hoist: value})}
											disabled={selectedRoleLocked || !canManageRoles}
											data-flx="guild.guild-tabs.guild-roles-tab.switch"
										/>
										<Switch
											label={i18n._(ALLOW_MENTIONS_FOR_THIS_ROLE_DESCRIPTOR)}
											description={i18n._(MEMBERS_WITH_THE_PERMISSION_CAN_ALWAYS_MENTION_ROLES_DESCRIPTOR, {
												mentionEveryonePermissionLabel,
											})}
											value={selectedRoleWithUpdates.mentionable}
											onChange={(value) => selectedRole && onRoleUpdate(selectedRole.id, {mentionable: value})}
											disabled={selectedRoleLocked || !canManageRoles}
											data-flx="guild.guild-tabs.guild-roles-tab.switch--2"
										/>
									</div>
								)}
								<div className={styles.sectionRow} data-flx="guild.guild-tabs.guild-roles-tab.section-row--2">
									<div className={styles.permHeaderRow} data-flx="guild.guild-tabs.guild-roles-tab.perm-header-row">
										<p className={styles.permHelp} data-flx="guild.guild-tabs.guild-roles-tab.perm-help">
											<Trans>Use this button to quickly clear all permissions.</Trans>
										</p>
										<Button
											variant="secondary"
											small={true}
											onClick={onClearPermissions}
											disabled={selectedRoleLocked || !canManageRoles}
											data-flx="guild.guild-tabs.guild-roles-tab.button.clear-permissions"
										>
											<Trans>Clear permissions</Trans>
										</Button>
									</div>
								</div>
								<div className={styles.sectionSubtitle} data-flx="guild.guild-tabs.guild-roles-tab.section-subtitle--2">
									<Trans>Permissions</Trans>
								</div>
								<div className={styles.permSearchRow} data-flx="guild.guild-tabs.guild-roles-tab.perm-search-row">
									<Input
										type="text"
										placeholder={i18n._(SEARCH_PERMISSIONS_DESCRIPTOR)}
										value={permissionSearchQuery}
										onChange={(e) => onPermissionSearchQueryChange(e.target.value)}
										leftIcon={
											<MagnifyingGlassIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="guild.guild-tabs.guild-roles-tab.magnifying-glass-icon"
											/>
										}
										className={styles.permSearchInput}
										data-flx="guild.guild-tabs.guild-roles-tab.perm-search-input.set-permission-search-query.text"
									/>
									<div className={styles.layoutButtons} data-flx="guild.guild-tabs.guild-roles-tab.layout-buttons">
										<Tooltip
											text={
												PermissionLayout.isComfy ? i18n._(DENSE_LAYOUT_DESCRIPTOR) : i18n._(COMFY_LAYOUT_DESCRIPTOR)
											}
											data-flx="guild.guild-tabs.guild-roles-tab.tooltip"
										>
											<button
												type="button"
												className={styles.layoutButton}
												onClick={() => PermissionLayout.toggleLayoutMode()}
												aria-label={
													PermissionLayout.isComfy
														? i18n._(SWITCH_TO_DENSE_LAYOUT_DESCRIPTOR)
														: i18n._(SWITCH_TO_COMFY_LAYOUT_DESCRIPTOR)
												}
												data-flx="guild.guild-tabs.guild-roles-tab.layout-button.toggle-layout-mode"
											>
												{PermissionLayout.isComfy ? (
													<RowsIcon
														size={remFromPx(20)}
														weight="bold"
														data-flx="guild.guild-tabs.guild-roles-tab.rows-icon"
													/>
												) : (
													<ListIcon
														size={remFromPx(20)}
														weight="bold"
														data-flx="guild.guild-tabs.guild-roles-tab.list-icon"
													/>
												)}
											</button>
										</Tooltip>
										<Tooltip
											text={PermissionLayout.isGrid ? i18n._(SINGLE_COLUMN_DESCRIPTOR) : i18n._(TWO_COLUMNS_DESCRIPTOR)}
											data-flx="guild.guild-tabs.guild-roles-tab.tooltip--2"
										>
											<button
												type="button"
												className={styles.layoutButton}
												onClick={() => PermissionLayout.toggleGridMode()}
												aria-label={
													PermissionLayout.isGrid
														? i18n._(SWITCH_TO_SINGLE_COLUMN_DESCRIPTOR)
														: i18n._(SWITCH_TO_TWO_COLUMNS_DESCRIPTOR)
												}
												data-flx="guild.guild-tabs.guild-roles-tab.layout-button.toggle-grid-mode"
											>
												<GridFourIcon
													size={remFromPx(20)}
													weight={PermissionLayout.isGrid ? 'fill' : 'bold'}
													data-flx="guild.guild-tabs.guild-roles-tab.grid-four-icon"
												/>
											</button>
										</Tooltip>
									</div>
								</div>
								<div className={styles.permCategories} data-flx="guild.guild-tabs.guild-roles-tab.perm-categories">
									{filteredPermissionSpecs.map((spec, index) => (
										<PermissionRoleCategory
											key={spec.title}
											spec={spec}
											rolePermissions={selectedRoleWithUpdates.permissions}
											onPermissionToggle={onPermissionToggle}
											disabled={selectedRoleLocked || !canManageRoles}
											getPermissionDisabledReason={getPermissionDisabledReason}
											getPermissionWarning={getPermissionWarning}
											isFirst={index === 0}
											data-flx="guild.guild-tabs.guild-roles-tab.permission-role-category"
										/>
									))}
									{filteredPermissionSpecs.length === 0 && permissionSearchQuery && (
										<div className={styles.emptyState} data-flx="guild.guild-tabs.guild-roles-tab.empty-state">
											<Trans>No permissions found</Trans>
										</div>
									)}
								</div>
							</>
						)}
					</div>
				</div>
			</div>
		);
	},
);
