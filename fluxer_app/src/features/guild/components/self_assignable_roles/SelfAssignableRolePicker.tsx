// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {type GuildRoleOption, useGuildRoleOptions} from '@app/features/permissions/hooks/useGuildRoleOptions';
import {RoleCheckboxMenuItem, roleCheckboxSheetItem} from '@app/features/ui/action_menu/items/RoleMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const NO_ROLES_TO_ADD_DESCRIPTOR = msg({
	message: 'No roles to add',
	comment:
		'Shown in the role picker of the self-assignable role section editor when every role is already used or none exist.',
});

function usePickerOptions(guild: Guild | undefined, unavailableRoleIds: ReadonlySet<string>): Array<GuildRoleOption> {
	const options = useGuildRoleOptions(guild);
	return useMemo(() => options.filter(({role}) => !unavailableRoleIds.has(role.id)), [options, unavailableRoleIds]);
}

function isOptionLocked(
	option: GuildRoleOption,
	checked: boolean,
	selectedCount: number,
	allowUnmanageable: boolean,
): boolean {
	if (checked) return false;
	return (!allowUnmanageable && !option.canManage) || selectedCount >= MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION;
}

interface PickerProps {
	readonly guildId: string;
	readonly unavailableRoleIds: ReadonlySet<string>;
	readonly allowUnmanageable?: boolean;
	readonly onChange: (roleId: string, checked: boolean) => void;
}

const SelfAssignableRolePickerMenu: React.FC<PickerProps & {readonly initialSelectedRoleIds: ReadonlyArray<string>}> =
	observer(function SelfAssignableRolePickerMenu({
		guildId,
		unavailableRoleIds,
		allowUnmanageable = false,
		initialSelectedRoleIds,
		onChange,
	}) {
		const {i18n} = useLingui();
		const options = usePickerOptions(Guilds.getGuild(guildId), unavailableRoleIds);
		const [selected, setSelected] = useState<ReadonlySet<string>>(() => new Set(initialSelectedRoleIds));
		if (options.length === 0) {
			return (
				<MenuGroup data-flx="guild.self-assignable-roles.self-assignable-role-picker.self-assignable-role-picker-menu.menu-group">
					<MenuItem
						disabled={true}
						data-flx="guild.self-assignable-roles.self-assignable-role-picker.self-assignable-role-picker-menu.menu-item"
					>
						{i18n._(NO_ROLES_TO_ADD_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			);
		}
		return (
			<MenuGroup data-flx="guild.self-assignable-roles.self-assignable-role-picker.self-assignable-role-picker-menu.menu-group--2">
				{options.map((option) => {
					const checked = selected.has(option.role.id);
					const locked = isOptionLocked(option, checked, selected.size, allowUnmanageable);
					return (
						<RoleCheckboxMenuItem
							key={option.role.id}
							role={option.role}
							checked={checked}
							disabled={locked}
							onToggle={() => {
								const next = new Set(selected);
								if (checked) {
									next.delete(option.role.id);
								} else {
									next.add(option.role.id);
								}
								setSelected(next);
								onChange(option.role.id, !checked);
							}}
							data-flx="guild.self-assignable-roles.self-assignable-role-picker.self-assignable-role-picker-menu.checkbox-item"
						/>
					);
				})}
			</MenuGroup>
		);
	});

export function openSelfAssignableRolePickerMenu(
	event: React.MouseEvent,
	props: PickerProps & {readonly selectedRoleIds: ReadonlyArray<string>},
): void {
	ContextMenuCommands.openFromEvent(event, () => (
		<SelfAssignableRolePickerMenu
			guildId={props.guildId}
			unavailableRoleIds={props.unavailableRoleIds}
			allowUnmanageable={props.allowUnmanageable}
			initialSelectedRoleIds={props.selectedRoleIds}
			onChange={props.onChange}
			data-flx="guild.self-assignable-roles.self-assignable-role-picker.open-self-assignable-role-picker-menu.self-assignable-role-picker-menu.change"
		/>
	));
}

interface SelfAssignableRolePickerSheetProps extends PickerProps {
	readonly isOpen: boolean;
	readonly onClose: () => void;
	readonly selectedRoleIds: ReadonlyArray<string>;
}

export const SelfAssignableRolePickerSheet: React.FC<SelfAssignableRolePickerSheetProps> = observer(
	function SelfAssignableRolePickerSheet({
		isOpen,
		onClose,
		guildId,
		unavailableRoleIds,
		allowUnmanageable = false,
		selectedRoleIds,
		onChange,
	}) {
		const {i18n} = useLingui();
		const options = usePickerOptions(Guilds.getGuild(guildId), unavailableRoleIds);
		const groups: Array<MenuGroupType> = useMemo(() => {
			const selected = new Set(selectedRoleIds);
			if (options.length === 0) {
				return [{items: [{label: i18n._(NO_ROLES_TO_ADD_DESCRIPTOR), onClick: onClose, disabled: true}]}];
			}
			return [
				{
					items: options.map((option) => {
						const checked = selected.has(option.role.id);
						return roleCheckboxSheetItem({
							role: option.role,
							checked,
							disabled: isOptionLocked(option, checked, selected.size, allowUnmanageable),
							onChange: (value) => onChange(option.role.id, value),
						});
					}),
				},
			];
		}, [options, selectedRoleIds, allowUnmanageable, onChange, onClose, i18n.locale]);
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				title={i18n._(ROLES_DESCRIPTOR)}
				groups={groups}
				data-flx="guild.self-assignable-roles.self-assignable-role-picker.self-assignable-role-picker-sheet.menu-bottom-sheet"
			/>
		);
	},
);
