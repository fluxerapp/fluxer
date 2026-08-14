// SPDX-License-Identifier: AGPL-3.0-or-later

import {getNagbarControls, type NagbarControlDefinition} from '@app/features/devtools/components/NagbarControls';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import Nagbar from '@app/features/ui/state/Nagbar';
import {Trans, useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {getNagbarActionItems, NAGBAR_OVERRIDES_DESCRIPTOR} from './NagbarControls';

const NagbarOverrideSubmenu: React.FC<{control: NagbarControlDefinition}> = observer(({control}) => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(control.label)}
			hint={i18n._(control.status(Nagbar))}
			render={() => (
				<>
					{getNagbarActionItems(control).map(({key, label, disabled, onClick}) => (
						<MenuItem
							key={key}
							disabled={disabled}
							onClick={onClick}
							closeOnSelect={false}
							data-flx="channel.channel-header-components.developer-tools-context-menu.nagbar-override-submenu.menu-item.click"
						>
							{i18n._(label)}
						</MenuItem>
					))}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.nagbar-override-submenu.menu-item-submenu"
		/>
	);
});
export const NagbarsMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const nagbarControls = getNagbarControls();
	return (
		<MenuItemSubmenu
			label={i18n._(NAGBAR_OVERRIDES_DESCRIPTOR)}
			render={() => (
				<>
					{nagbarControls.map((control) => (
						<NagbarOverrideSubmenu
							key={control.key}
							control={control}
							data-flx="channel.channel-header-components.developer-tools-context-menu.nagbars-menu.nagbar-override-submenu"
						/>
					))}
					<MenuItem
						icon={
							<TrashIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="channel.channel-header-components.developer-tools-context-menu.nagbars-menu.trash-icon"
							/>
						}
						onClick={() => NagbarCommands.resetAllNagbars()}
						closeOnSelect={false}
						data-flx="channel.channel-header-components.developer-tools-context-menu.nagbars-menu.menu-item.reset-all-nagbars"
					>
						<Trans>Reset all nagbars</Trans>
					</MenuItem>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.nagbars-menu.menu-item-submenu"
		/>
	);
});
