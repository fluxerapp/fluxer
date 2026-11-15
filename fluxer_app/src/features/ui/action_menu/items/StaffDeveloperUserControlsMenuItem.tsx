// SPDX-License-Identifier: AGPL-3.0-or-later

import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {SpammerOverrideMenuItems} from '@app/features/ui/action_menu/items/SpammerOverrideMenuItems';
import {StaffUserControlsMenuItems} from '@app/features/ui/action_menu/items/StaffUserControlsMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DEVELOPER_TOOLS_DESCRIPTOR = msg({
	message: 'Developer tools',
	comment: 'Section label for developer-only menu items.',
});
const STAFF_CONTROLS_DESCRIPTOR = msg({
	message: 'Staff controls',
	comment: 'Section label for staff-only menu items.',
});
const SPAM_CLASSIFICATION_DESCRIPTOR = msg({
	message: 'Spam classification',
	comment: 'Section label for spam classification debug controls.',
});

interface StaffDeveloperUserControlsMenuItemProps {
	user: User;
	showStaffControls: boolean;
	showSpammerOverrideControls: boolean;
	developerMode: boolean;
}

export const StaffDeveloperUserControlsMenuItem: React.FC<StaffDeveloperUserControlsMenuItemProps> = observer(
	({user, showStaffControls, showSpammerOverrideControls, developerMode}) => {
		const {i18n} = useLingui();
		if (!showStaffControls && !showSpammerOverrideControls) {
			return null;
		}
		return (
			<MenuItemSubmenu
				label={i18n._(DEVELOPER_TOOLS_DESCRIPTOR)}
				render={() => (
					<>
						{showStaffControls && (
							<MenuGroup data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.menu-group">
								<MenuItemSubmenu
									label={i18n._(STAFF_CONTROLS_DESCRIPTOR)}
									render={() => (
										<StaffUserControlsMenuItems
											user={user}
											data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.staff-user-controls-menu-items"
										/>
									)}
									data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.menu-item-submenu--2"
								/>
							</MenuGroup>
						)}
						{showSpammerOverrideControls && (
							<MenuGroup data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.menu-group--2">
								<MenuItemSubmenu
									label={i18n._(SPAM_CLASSIFICATION_DESCRIPTOR)}
									render={() => (
										<SpammerOverrideMenuItems
											user={user}
											developerMode={developerMode}
											data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.spammer-override-menu-items"
										/>
									)}
									data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.menu-item-submenu--3"
								/>
							</MenuGroup>
						)}
					</>
				)}
				data-flx="ui.action-menu.items.staff-developer-user-controls-menu-item.menu-item-submenu"
			/>
		);
	},
);
