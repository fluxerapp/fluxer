// SPDX-License-Identifier: AGPL-3.0-or-later

import {CheckboxItem, SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';

import type {
	MenuCheckboxType,
	MenuGroupType,
	MenuItemType,
	MenuRadioType,
	MenuSliderType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

type MenuDataItem = MenuItemType | MenuSliderType | MenuCheckboxType | MenuRadioType | MenuSubmenuItemType;

interface DataMenuRendererProps {
	groups: Array<MenuGroupType>;
	excludeLabels?: Array<string>;
}

interface DataMenuItemsRendererProps {
	items: Array<MenuDataItem>;
	excludeLabels?: Array<string>;
}

function isSubmenuItem(item: MenuDataItem): item is MenuSubmenuItemType {
	return 'items' in item && Array.isArray((item as MenuSubmenuItemType).items);
}

function isCheckboxItem(item: MenuDataItem): item is MenuCheckboxType {
	return 'checked' in item && 'onChange' in item;
}

function isRadioItem(item: MenuDataItem): item is MenuRadioType {
	return 'selected' in item && 'onSelect' in item;
}

function isMenuItem(item: MenuDataItem): item is MenuItemType {
	return 'onClick' in item && 'label' in item && !isSubmenuItem(item);
}

function isSliderItem(item: MenuDataItem): item is MenuSliderType {
	return 'value' in item && 'minValue' in item && 'maxValue' in item && 'onChange' in item;
}

function filterMenuItems(items: Array<MenuDataItem>, excludeLabels: Array<string>): Array<MenuDataItem> {
	return items.filter((item) => {
		if ('label' in item && excludeLabels.includes(item.label)) {
			return false;
		}
		return true;
	});
}

export const DataMenuItemsRenderer: React.FC<DataMenuItemsRendererProps> = observer(({items, excludeLabels = []}) => {
	const filteredItems = useMemo(() => filterMenuItems(items, excludeLabels), [items, excludeLabels]);
	return (
		<>
			{filteredItems.map((item, itemIndex) => {
				const key = 'label' in item ? `${item.label}-${itemIndex}` : `item-${itemIndex}`;
				if (isSubmenuItem(item)) {
					return (
						<MenuItemSubmenu
							key={key}
							label={item.label}
							disabled={item.disabled}
							onTriggerSelect={item.onTriggerSelect}
							render={() => (
								<DataMenuRenderer
									groups={[{items: item.items}]}
									excludeLabels={excludeLabels}
									data-flx="ui.action-menu.data-menu-renderer.data-menu-renderer"
								/>
							)}
							data-flx="ui.action-menu.data-menu-renderer.menu-item-submenu"
						/>
					);
				}
				if (isCheckboxItem(item)) {
					return (
						<CheckboxItem
							key={key}
							checked={item.checked}
							onCheckedChange={item.onChange}
							data-flx="ui.action-menu.data-menu-renderer.checkbox-item"
						>
							{item.label}
						</CheckboxItem>
					);
				}
				if (isRadioItem(item)) {
					return (
						<MenuItemRadio
							key={key}
							selected={item.selected}
							onSelect={item.onSelect}
							disabled={item.disabled}
							data-flx="ui.action-menu.data-menu-renderer.menu-item-radio.select"
						>
							{item.label}
						</MenuItemRadio>
					);
				}
				if (isSliderItem(item)) {
					return (
						<MenuItemSlider
							key={key}
							label={item.label}
							value={item.value}
							minValue={item.minValue}
							maxValue={item.maxValue}
							onChange={item.onChange}
							onFormat={item.onFormat}
							step={item.step}
							data-flx="ui.action-menu.data-menu-renderer.menu-item-slider.change"
						/>
					);
				}
				if (isMenuItem(item)) {
					return (
						<MenuItem
							key={item.id ?? key}
							icon={item.icon}
							onClick={item.onClick}
							danger={item.danger}
							disabled={item.disabled}
							hint={item.hint}
							data-flx="ui.action-menu.data-menu-renderer.menu-item.click"
						>
							{item.label}
						</MenuItem>
					);
				}
				return null;
			})}
		</>
	);
});

export const DataMenuRenderer: React.FC<DataMenuRendererProps> = observer(({groups, excludeLabels = []}) => {
	const filteredGroups = useMemo(() => {
		return groups
			.map((group) => ({
				...group,
				items: filterMenuItems(group.items, excludeLabels),
			}))
			.filter((group) => group.items.length > 0);
	}, [groups, excludeLabels]);
	return (
		<>
			{filteredGroups.map((group, groupIndex) => (
				<MenuGroup key={groupIndex} data-flx="ui.action-menu.data-menu-renderer.menu-group">
					<DataMenuItemsRenderer
						items={group.items}
						excludeLabels={excludeLabels}
						data-flx="ui.action-menu.data-menu-renderer.data-menu-items-renderer"
					/>
				</MenuGroup>
			))}
		</>
	);
});
