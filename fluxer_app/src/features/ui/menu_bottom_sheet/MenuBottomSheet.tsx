// SPDX-License-Identifier: AGPL-3.0-or-later

import {usePressable} from '@app/features/app/hooks/usePressable';
import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Slider} from '@app/features/ui/components/Slider';
import styles from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet.module.css';
import {useLingui} from '@lingui/react/macro';
import {CaretLeftIcon, CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

export type MenuActionEvent =
	| React.MouseEvent<Element>
	| React.KeyboardEvent<Element>
	| {
			readonly shiftKey?: boolean;
	  };
export type MenuActionHandler = {bivarianceHack(event?: MenuActionEvent): void}['bivarianceHack'];

export interface MenuItemType {
	id?: string;
	icon?: React.ReactNode;
	label: string;
	subtext?: string;
	onClick: MenuActionHandler;
	danger?: boolean;
	disabled?: boolean;
	hint?: string;
	shortcut?: React.ReactNode;
	closeOnSelect?: boolean;
}

export interface MenuSubmenuItemType {
	id?: string;
	icon?: React.ReactNode;
	label: string;
	items: Array<MenuSheetItem>;
	disabled?: boolean;
	onTriggerSelect?: () => void;
}

export interface MenuSliderType {
	label: string;
	value: number;
	minValue: number;
	maxValue: number;
	onChange: (value: number) => void;
	onFormat?: (value: number) => string;
	factoryDefaultValue?: number;
	step?: number;
}

export interface MenuCheckboxType {
	icon?: React.ReactNode;
	label: string;
	checked: boolean;
	onChange: (checked: boolean) => void;
	disabled?: boolean;
}

export interface MenuRadioType {
	label: string;
	subtext?: string;
	selected: boolean;
	onSelect: () => void;
	disabled?: boolean;
}

export type MenuLeafItem = MenuItemType | MenuSliderType | MenuCheckboxType | MenuRadioType;
export type MenuSheetItem = MenuLeafItem | MenuSubmenuItemType;

export interface MenuGroupType {
	items: Array<MenuSheetItem>;
}

export interface MenuBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	title?: string;
	groups: Array<MenuGroupType>;
	headerContent?: React.ReactNode;
	showCloseButton?: boolean;
}

const MenuCheckboxItem: React.FC<{item: MenuCheckboxType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				role="checkbox"
				aria-checked={item.checked}
				aria-label={item.label}
				onClick={() => item.onChange(!item.checked)}
				disabled={item.disabled}
				className={clsx(styles.menuItem, item.disabled && styles.disabled, isPressed && styles.pressed)}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.menu-item.change.button"
				{...pressableProps}
			>
				{item.icon && (
					<div
						className={styles.iconContainer}
						aria-hidden="true"
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.icon-container"
					>
						{item.icon}
					</div>
				)}
				<span className={styles.label} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.label">
					{item.label}
				</span>
				<div
					className={styles.checkboxContainer}
					aria-hidden="true"
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.checkbox-container"
				>
					<div
						className={clsx(styles.checkbox, item.checked && styles.checked)}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.checkbox"
					>
						{item.checked && (
							<svg
								className={styles.checkIcon}
								viewBox="0 0 12 12"
								fill="none"
								aria-hidden="true"
								data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.check-icon"
							>
								<path
									d="M10 3L4.5 8.5L2 6"
									stroke="currentColor"
									strokeWidth="2"
									strokeLinecap="round"
									strokeLinejoin="round"
									data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.path"
								/>
							</svg>
						)}
					</div>
				</div>
			</button>
			{!isLast && (
				<div className={styles.divider} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-checkbox-item.divider" />
			)}
		</>
	);
});
const MenuRadioItem: React.FC<{item: MenuRadioType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				role="radio"
				aria-checked={item.selected}
				aria-label={item.label}
				onClick={item.onSelect}
				disabled={item.disabled}
				className={clsx(styles.menuItem, item.disabled && styles.disabled, isPressed && styles.pressed)}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.menu-item.select.button"
				{...pressableProps}
			>
				<div
					className={styles.radioContainer}
					aria-hidden="true"
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.radio-container"
				>
					<div
						className={clsx(styles.radio, item.selected && styles.radioSelected)}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.radio"
					>
						{item.selected && (
							<div
								className={styles.radioInner}
								data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.radio-inner"
							/>
						)}
					</div>
				</div>
				<div
					className={styles.labelColumn}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.label-column"
				>
					<span className={styles.label} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.label">
						{item.label}
					</span>
					{item.subtext && (
						<span className={styles.subtext} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.subtext">
							{item.subtext}
						</span>
					)}
				</div>
			</button>
			{!isLast && (
				<div className={styles.divider} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-radio-item.divider" />
			)}
		</>
	);
});
const MenuActionItem: React.FC<{item: MenuItemType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				onClick={item.onClick}
				disabled={item.disabled}
				className={clsx(
					styles.menuItem,
					item.disabled && styles.disabled,
					item.danger && styles.danger,
					isPressed && styles.pressed,
					isPressed && item.danger && styles.pressedDanger,
				)}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.menu-item.click.button"
				{...pressableProps}
			>
				{item.icon && (
					<div
						className={styles.iconContainer}
						aria-hidden="true"
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.icon-container"
					>
						{item.icon}
					</div>
				)}
				{item.subtext ? (
					<div
						className={styles.labelColumn}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.label-column"
					>
						<span className={styles.label} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.label">
							{item.label}
						</span>
						<span className={styles.subtext} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.subtext">
							{item.subtext}
						</span>
					</div>
				) : (
					<span className={styles.label} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.label--2">
						{item.label}
					</span>
				)}
			</button>
			{!isLast && (
				<div className={styles.divider} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-action-item.divider" />
			)}
		</>
	);
});
const MenuSliderItem: React.FC<{item: MenuSliderType; isLast: boolean}> = observer(({item, isLast}) => {
	return (
		<>
			<div
				className={styles.sliderContainer}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-slider-item.slider-container"
			>
				<span
					className={styles.sliderLabel}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-slider-item.slider-label"
				>
					{item.label}
				</span>
				<Slider
					defaultValue={item.value}
					factoryDefaultValue={item.factoryDefaultValue ?? item.value}
					minValue={item.minValue}
					maxValue={item.maxValue}
					onValueChange={item.onChange}
					onValueRender={item.onFormat}
					value={item.value}
					mini={true}
					step={item.step ?? 1}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-slider-item.slider"
				/>
			</div>
			{!isLast && (
				<div className={styles.divider} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-slider-item.divider" />
			)}
		</>
	);
});
const MenuSubmenuItem: React.FC<{
	item: MenuSubmenuItemType;
	isLast: boolean;
	onExpand: (item: MenuSubmenuItemType) => void;
}> = observer(({item, isLast, onExpand}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	const handleClick = useCallback(() => {
		onExpand(item);
	}, [item, onExpand]);
	return (
		<>
			<button
				type="button"
				onClick={handleClick}
				disabled={item.disabled}
				className={clsx(styles.menuItem, item.disabled && styles.disabled, isPressed && styles.pressed)}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-submenu-item.menu-item.click.button"
				{...pressableProps}
			>
				{item.icon && (
					<div
						className={styles.iconContainer}
						aria-hidden="true"
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-submenu-item.icon-container"
					>
						{item.icon}
					</div>
				)}
				<span className={styles.label} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-submenu-item.label">
					{item.label}
				</span>
				<CaretRightIcon
					size={remFromPx(20)}
					className={styles.submenuChevron}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-submenu-item.submenu-chevron"
				/>
			</button>
			{!isLast && (
				<div className={styles.divider} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-submenu-item.divider" />
			)}
		</>
	);
});
const MenuItem: React.FC<{
	item: MenuItemType | MenuSliderType | MenuCheckboxType | MenuRadioType | MenuSubmenuItemType;
	isLast?: boolean;
	onExpandSubmenu?: (item: MenuSubmenuItemType) => void;
}> = observer(({item, isLast = false, onExpandSubmenu}) => {
	if ('checked' in item) {
		return (
			<MenuCheckboxItem
				item={item as MenuCheckboxType}
				isLast={isLast}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-item.menu-checkbox-item"
			/>
		);
	}
	if ('selected' in item) {
		return (
			<MenuRadioItem
				item={item as MenuRadioType}
				isLast={isLast}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-item.menu-radio-item"
			/>
		);
	}
	if ('items' in item && onExpandSubmenu) {
		return (
			<MenuSubmenuItem
				item={item as MenuSubmenuItemType}
				isLast={isLast}
				onExpand={onExpandSubmenu}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-item.menu-submenu-item"
			/>
		);
	}
	if ('onClick' in item) {
		return (
			<MenuActionItem
				item={item as MenuItemType}
				isLast={isLast}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-item.menu-action-item"
			/>
		);
	}
	return (
		<MenuSliderItem
			item={item as MenuSliderType}
			isLast={isLast}
			data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-item.menu-slider-item"
		/>
	);
});
const MenuGroup: React.FC<{
	group: MenuGroupType;
	isLast?: boolean;
	onExpandSubmenu?: (item: MenuSubmenuItemType) => void;
}> = observer(({group, isLast = false, onExpandSubmenu}) => {
	return (
		<>
			<div
				className={styles.groupContainer}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-group.group-container"
			>
				{group.items.map((item, index) => (
					<MenuItem
						key={`${'label' in item ? item.label : 'slider'}-${index}`}
						item={item}
						isLast={index === group.items.length - 1}
						onExpandSubmenu={onExpandSubmenu}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-group.menu-item"
					/>
				))}
			</div>
			{!isLast && (
				<div className={styles.groupSpacer} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-group.group-spacer" />
			)}
		</>
	);
});
export const MenuBottomSheet: React.FC<MenuBottomSheetProps> = observer(
	({isOpen, onClose, title, groups, headerContent, showCloseButton = false}) => {
		const {i18n} = useLingui();
		const [activeSubmenu, setActiveSubmenu] = useState<MenuSubmenuItemType | null>(null);
		const hasHeader = Boolean(title || headerContent);
		const handleExpandSubmenu = useCallback((item: MenuSubmenuItemType) => {
			setActiveSubmenu(item);
		}, []);
		const handleCloseSubmenu = useCallback(() => {
			setActiveSubmenu(null);
		}, []);
		const handleClose = useCallback(() => {
			setActiveSubmenu(null);
			onClose();
		}, [onClose]);
		if (activeSubmenu) {
			const submenuGroups: Array<MenuGroupType> = [{items: activeSubmenu.items}];
			const backButton = (
				<button
					type="button"
					onClick={handleCloseSubmenu}
					className={styles.backButton}
					aria-label={i18n._(GO_BACK_DESCRIPTOR)}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.back-button.close-submenu"
				>
					<CaretLeftIcon size={remFromPx(20)} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.caret-left-icon" />
				</button>
			);
			return (
				<BottomSheet
					isOpen={isOpen}
					onClose={handleClose}
					snapPoints={[0, 0.6, 1]}
					initialSnap={1}
					title={activeSubmenu.label}
					showCloseButton={false}
					leadingAction={backButton}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.bottom-sheet"
				>
					<div
						className={styles.bottomSheetContent}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.bottom-sheet-content"
					>
						<div className={styles.groupStack} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.group-stack">
							{submenuGroups.map((group, index) => (
								<MenuGroup
									key={index}
									group={group}
									isLast={index === submenuGroups.length - 1}
									data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-group"
								/>
							))}
						</div>
					</div>
				</BottomSheet>
			);
		}
		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={handleClose}
				snapPoints={[0, 0.6, 1]}
				initialSnap={1}
				title={title}
				showCloseButton={showCloseButton}
				disableDefaultHeader={!title && !showCloseButton}
				data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.bottom-sheet--2"
			>
				<div
					className={styles.bottomSheetContent}
					data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.bottom-sheet-content--2"
				>
					{headerContent && (
						<div className={styles.headerSlot} data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.header-slot">
							{headerContent}
						</div>
					)}
					<div
						className={clsx(styles.groupStack, hasHeader && styles.groupStackWithHeader)}
						data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.group-stack--2"
					>
						{groups.map((group, index) => (
							<MenuGroup
								key={index}
								group={group}
								isLast={index === groups.length - 1}
								onExpandSubmenu={handleExpandSubmenu}
								data-flx="ui.menu-bottom-sheet.menu-bottom-sheet.menu-group--2"
							/>
						))}
					</div>
				</div>
			</BottomSheet>
		);
	},
);

MenuBottomSheet.displayName = 'MenuBottomSheet';
