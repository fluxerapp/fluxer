// SPDX-License-Identifier: AGPL-3.0-or-later

import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import type {
	SearchProviderMenuEngine,
	SearchProviderMenuState,
} from '@app/features/ui/action_menu/items/SearchProviderMenuUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const DEFAULT_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Option label representing the default value.',
});

interface SearchProviderContextMenuItemsProps<T extends SearchProviderMenuEngine> {
	state: SearchProviderMenuState<T>;
	defaultLabel: string;
	renderIcon: () => React.ReactNode;
	onDefaultSearch: () => void;
	onSearchWithEngine: (engine: T) => void;
}

export function SearchProviderContextMenuItems<T extends SearchProviderMenuEngine>({
	state,
	defaultLabel,
	renderIcon,
	onDefaultSearch,
	onSearchWithEngine,
}: SearchProviderContextMenuItemsProps<T>): React.ReactNode {
	const {i18n} = useLingui();
	if (state.enabledEngines.length === 0) {
		return null;
	}
	if (state.alternateEngines.length > 0) {
		const defaultEngine = state.defaultEngine;
		return (
			<MenuItemSubmenu
				label={defaultLabel}
				onTriggerSelect={onDefaultSearch}
				render={() => (
					<MenuGroup data-flx="ui.action-menu.items.search-provider-menu-items.search-provider-context-menu-items.menu-group">
						{defaultEngine && (
							<MenuItem
								hint={i18n._(DEFAULT_DESCRIPTOR)}
								onClick={() => onSearchWithEngine(defaultEngine)}
								data-flx="ui.action-menu.items.search-provider-menu-items.search-provider-context-menu-items.menu-item.search-with-engine"
							>
								{defaultEngine.name}
							</MenuItem>
						)}
						{state.alternateEngines.map((engine) => (
							<MenuItem
								key={engine.id}
								onClick={() => onSearchWithEngine(engine)}
								data-flx="ui.action-menu.items.search-provider-menu-items.search-provider-context-menu-items.menu-item.search-with-engine--2"
							>
								{engine.name}
							</MenuItem>
						))}
					</MenuGroup>
				)}
				data-flx="ui.action-menu.items.search-provider-menu-items.search-provider-context-menu-items.menu-item-submenu"
			/>
		);
	}
	return (
		<MenuItem
			icon={renderIcon()}
			onClick={onDefaultSearch}
			data-flx="ui.action-menu.items.search-provider-menu-items.search-provider-context-menu-items.menu-item.default-search"
		>
			{defaultLabel}
		</MenuItem>
	);
}
