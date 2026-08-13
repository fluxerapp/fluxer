// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	type ActiveOverrideEntry,
	type LocalizedLabel,
	type RadioMenuOption,
	translateDescriptor,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import {useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {updateOption} from './ResetOptions';

interface DeveloperOptionCheckboxProps<K extends keyof DeveloperOptionsState> {
	optionKey: K;
	checked?: boolean;
	checkedValue?: DeveloperOptionsState[K];
	uncheckedValue?: DeveloperOptionsState[K];
	onAfterChange?: (checked: boolean) => void;
	children: React.ReactNode;
}

const DeveloperOptionCheckboxInner = <K extends keyof DeveloperOptionsState>({
	optionKey,
	checked,
	checkedValue = true as DeveloperOptionsState[K],
	uncheckedValue = false as DeveloperOptionsState[K],
	onAfterChange,
	children,
}: DeveloperOptionCheckboxProps<K>) => {
	const currentChecked = checked ?? Boolean(DeveloperOptions[optionKey]);
	return (
		<CheckboxItem
			checked={currentChecked}
			onCheckedChange={(nextChecked) => {
				updateOption(optionKey, nextChecked ? checkedValue : uncheckedValue);
				onAfterChange?.(nextChecked);
			}}
			data-flx="channel.channel-header-components.developer-tools-context-menu.developer-option-checkbox-inner.checkbox-item"
		>
			{children}
		</CheckboxItem>
	);
};
export const DeveloperOptionCheckbox = observer(DeveloperOptionCheckboxInner) as typeof DeveloperOptionCheckboxInner;

interface DeveloperOptionRadioItemsProps<K extends keyof DeveloperOptionsState> {
	optionKey: K;
	options: Array<RadioMenuOption<DeveloperOptionsState[K]>>;
	selectedValue?: DeveloperOptionsState[K];
}

const DeveloperOptionRadioItemsInner = <K extends keyof DeveloperOptionsState>({
	optionKey,
	options,
	selectedValue = DeveloperOptions[optionKey],
}: DeveloperOptionRadioItemsProps<K>) => {
	const {i18n} = useLingui();
	return (
		<>
			{options.map(({value, label, key}) => (
				<MenuItemRadio
					key={key ?? String(value)}
					label={translateDescriptor(i18n, label as LocalizedLabel)}
					selected={Object.is(selectedValue, value)}
					onSelect={() => updateOption(optionKey, value)}
					data-flx="channel.channel-header-components.developer-tools-context-menu.developer-option-radio-items-inner.menu-item-radio.update-option"
				/>
			))}
		</>
	);
};
export const DeveloperOptionRadioItems = observer(
	DeveloperOptionRadioItemsInner,
) as typeof DeveloperOptionRadioItemsInner;

interface DeveloperOptionRadioSubmenuProps<K extends keyof DeveloperOptionsState>
	extends DeveloperOptionRadioItemsProps<K> {
	label: string;
}

const DeveloperOptionRadioSubmenuInner = <K extends keyof DeveloperOptionsState>({
	label,
	optionKey,
	options,
	selectedValue,
}: DeveloperOptionRadioSubmenuProps<K>) => (
	<MenuItemSubmenu
		label={label}
		render={() => (
			<DeveloperOptionRadioItems
				optionKey={optionKey}
				options={options}
				selectedValue={selectedValue}
				data-flx="channel.channel-header-components.developer-tools-context-menu.developer-option-radio-submenu-inner.developer-option-radio-items"
			/>
		)}
		data-flx="channel.channel-header-components.developer-tools-context-menu.developer-option-radio-submenu-inner.menu-item-submenu"
	/>
);
export const DeveloperOptionRadioSubmenu = observer(
	DeveloperOptionRadioSubmenuInner,
) as typeof DeveloperOptionRadioSubmenuInner;

interface ActiveOverrideMenuGroupProps {
	entries: Array<ActiveOverrideEntry>;
	icon: () => React.ReactNode;
}

export const ActiveOverrideMenuGroup: React.FC<ActiveOverrideMenuGroupProps> = ({entries, icon}) => {
	if (entries.length === 0) return null;
	return (
		<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.active-override-menu-group.menu-group">
			{entries.map((entry) => (
				<MenuItem
					key={entry.key}
					icon={icon()}
					hint={entry.value}
					onClick={entry.reset}
					closeOnSelect={false}
					data-flx="channel.channel-header-components.developer-tools-context-menu.active-override-menu-group.menu-item.reset"
				>
					{entry.label}
				</MenuItem>
			))}
		</MenuGroup>
	);
};
export const BackendPremiumOverrideMenuGroup: React.FC<{label: string; hint: string}> = ({label, hint}) => (
	<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-override-menu-group.menu-group">
		<MenuItem
			icon={
				<CrownIcon
					size={remFromPx(16)}
					weight="fill"
					data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-override-menu-group.crown-icon"
				/>
			}
			hint={hint}
			onClick={() => {
				void UserCommands.update({premium_enabled_override: false});
			}}
			closeOnSelect={false}
			data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-override-menu-group.menu-item"
		>
			{label}
		</MenuItem>
	</MenuGroup>
);
export const BackendPremiumPerksDisabledMenuGroup: React.FC<{label: string; hint: string}> = ({label, hint}) => (
	<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-perks-disabled-menu-group.menu-group">
		<MenuItem
			icon={
				<CrownIcon
					size={remFromPx(16)}
					weight="fill"
					data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-perks-disabled-menu-group.crown-icon"
				/>
			}
			hint={hint}
			onClick={() => {
				void PremiumCommands.setPremiumPerksDisabled(false);
			}}
			closeOnSelect={false}
			data-flx="channel.channel-header-components.developer-tools-context-menu.backend-premium-perks-disabled-menu-group.menu-item"
		>
			{label}
		</MenuItem>
	</MenuGroup>
);

interface DangerConfirmationOptions {
	onClose: () => void;
	title: string;
	description: string;
	primaryText: string;
	onPrimary: () => void | Promise<void>;
}

export const openDangerConfirmation = ({
	onClose,
	title,
	description,
	primaryText,
	onPrimary,
}: DangerConfirmationOptions): void => {
	ModalCommands.pushAfterBottomSheetClose(
		onClose,
		ModalCommands.modal(() => (
			<ConfirmModal
				title={title}
				description={description}
				primaryText={primaryText}
				primaryVariant="danger"
				onPrimary={onPrimary}
				data-flx="channel.channel-header-components.developer-tools-context-menu.open-danger-confirmation.confirm-modal"
			/>
		)),
	);
};

interface PendingMenuItemProps {
	icon?: React.ReactNode;
	danger?: boolean;
	disabled?: boolean;
	isPending: boolean;
	label: string;
	pendingLabel: string;
	onClick: () => void | Promise<void>;
}

export const PendingMenuItem: React.FC<PendingMenuItemProps> = ({
	icon,
	danger = false,
	disabled = false,
	isPending,
	label,
	pendingLabel,
	onClick,
}) => (
	<MenuItem
		icon={icon}
		danger={danger}
		disabled={disabled || isPending}
		onClick={onClick}
		data-flx="channel.channel-header-components.developer-tools-context-menu.pending-menu-item.menu-item.click"
	>
		{isPending ? pendingLabel : label}
	</MenuItem>
);
