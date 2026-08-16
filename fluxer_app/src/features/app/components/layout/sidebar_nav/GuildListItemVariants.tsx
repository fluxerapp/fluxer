// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildListItemContent} from '@app/features/app/components/layout/sidebar_nav/GuildListItemContent';
import type {GuildListItemProps} from '@app/features/app/components/layout/sidebar_nav/GuildListItemProps';
import {useGuildListItemDragAndDrop} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemDragAndDrop';
import {useRef} from 'react';

interface GuildListItemVariantProps extends GuildListItemProps {
	readonly isMobileExperience: boolean;
}

export function DesktopGuildListItem(props: GuildListItemVariantProps) {
	const itemRef = useRef<HTMLElement | null>(null);
	const dragAndDrop = useGuildListItemDragAndDrop({
		guildId: props.guild.id,
		insideFolderId: props.insideFolderId,
		isLastInsideFolder: props.isLastInsideFolder,
		disableDrag: props.disableDrag,
		itemRef,
		onGuildDrop: props.onGuildDrop,
		onDragStateChange: props.onDragStateChange,
	});
	return (
		<GuildListItemContent
			data-flx="app.sidebar-nav.guild-list-item-variants.desktop-guild-list-item.guild-list-item-content"
			{...props}
			isDesktopLayout
			itemRef={itemRef}
			dragAndDrop={dragAndDrop}
		/>
	);
}

export function MobileGuildListItem(props: GuildListItemVariantProps) {
	const itemRef = useRef<HTMLElement | null>(null);
	return (
		<GuildListItemContent
			data-flx="app.sidebar-nav.guild-list-item-variants.mobile-guild-list-item.guild-list-item-content"
			{...props}
			isDesktopLayout={false}
			itemRef={itemRef}
			dragAndDrop={null}
		/>
	);
}
