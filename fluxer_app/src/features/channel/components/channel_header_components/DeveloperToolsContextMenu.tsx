// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountPremiumMenu} from '@app/features/channel/components/channel_header_components/developer_tools/AccountPremiumMenu';
import {GeneralDeveloperOptionsMenu} from '@app/features/channel/components/channel_header_components/developer_tools/GeneralOptionsMenu';
import {MockingMenu} from '@app/features/channel/components/channel_header_components/developer_tools/MockingMenu';
import {NagbarsMenu} from '@app/features/channel/components/channel_header_components/developer_tools/NagbarsMenu';
import {DeveloperStateSummaryMenu} from '@app/features/channel/components/channel_header_components/developer_tools/StateSummaryMenu';
import {ToolsMenu} from '@app/features/channel/components/channel_header_components/developer_tools/ToolsMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuGroups} from '@app/features/ui/action_menu/MenuGroups';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import Users from '@app/features/user/state/Users';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

export const DeveloperToolsContextMenu: React.FC<{onClose: () => void}> = observer(({onClose}) => {
	const currentUser = Users.currentUser;
	const hasRuntimeState = useMemo(() => Boolean(currentUser), [currentUser]);
	if (!hasRuntimeState) {
		return (
			<MenuGroups data-flx="channel.channel-header-components.developer-tools-context-menu.menu-groups">
				<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.menu-group">
					<MenuItem disabled data-flx="channel.channel-header-components.developer-tools-context-menu.menu-item">
						<Trans>Developer tools unavailable</Trans>
					</MenuItem>
				</MenuGroup>
			</MenuGroups>
		);
	}
	return (
		<MenuGroups data-flx="channel.channel-header-components.developer-tools-context-menu.menu-groups--2">
			<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.menu-group--2">
				<DeveloperStateSummaryMenu data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu" />
			</MenuGroup>
			<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.menu-group--3">
				<GeneralDeveloperOptionsMenu data-flx="channel.channel-header-components.developer-tools-context-menu.general-developer-options-menu" />
				<AccountPremiumMenu data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu" />
				<MockingMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu" />
			</MenuGroup>
			<MenuGroup data-flx="channel.channel-header-components.developer-tools-context-menu.menu-group--4">
				<NagbarsMenu data-flx="channel.channel-header-components.developer-tools-context-menu.nagbars-menu" />
				<ToolsMenu
					onClose={onClose}
					data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu"
				/>
			</MenuGroup>
		</MenuGroups>
	);
});
