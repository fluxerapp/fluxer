// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildListItemProps} from '@app/features/app/components/layout/sidebar_nav/GuildListItemProps';
import {
	DesktopGuildListItem,
	MobileGuildListItem,
} from '@app/features/app/components/layout/sidebar_nav/GuildListItemVariants';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {observer} from 'mobx-react-lite';

export const GuildListItem = observer((props: GuildListItemProps) => {
	const isMobileExperience = isMobileExperienceEnabled();
	if (MobileLayout.enabled) {
		return (
			<MobileGuildListItem
				data-flx="app.sidebar-nav.guild-list-item.mobile-guild-list-item"
				{...props}
				isMobileExperience={isMobileExperience}
			/>
		);
	}
	return (
		<DesktopGuildListItem
			data-flx="app.sidebar-nav.guild-list-item.desktop-guild-list-item"
			{...props}
			isMobileExperience={isMobileExperience}
		/>
	);
});
