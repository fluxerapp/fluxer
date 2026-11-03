// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import {
	MUTE_COMMUNITY_DESCRIPTOR,
	UNMUTE_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {useGuildMenuData} from '@app/features/ui/action_menu/items/GuildMenuData';
import {MuteCommunityMenuItem} from '@app/features/ui/action_menu/items/GuildMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface GuildContextMenuProps {
	guild: Guild;
	onClose: () => void;
}

export const GuildContextMenu: React.FC<GuildContextMenuProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const {groups} = useGuildMenuData(guild, {
		onClose,
		preserveInitialMarkAsReadVisibility: true,
	});
	const excludeLabels = useMemo(
		() => [i18n._(MUTE_COMMUNITY_DESCRIPTOR), i18n._(UNMUTE_COMMUNITY_DESCRIPTOR)],
		[i18n.locale],
	);
	return (
		<>
			<DataMenuRenderer
				groups={groups}
				excludeLabels={excludeLabels}
				data-flx="ui.action-menu.guild-context-menu.data-menu-renderer"
			/>
			{guild.visiting ? null : (
				<MenuGroup data-flx="ui.action-menu.guild-context-menu.menu-group">
					<MuteCommunityMenuItem
						guild={guild}
						onClose={onClose}
						data-flx="ui.action-menu.guild-context-menu.mute-community-menu-item"
					/>
				</MenuGroup>
			)}
		</>
	);
});
