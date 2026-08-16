// SPDX-License-Identifier: AGPL-3.0-or-later

import {DirectSelectionSurface, markDirectSelection} from '@app/features/app/components/layout/DirectSelectionOrigin';
import {useGuildListItemPreload} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemPreload';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import type {Guild} from '@app/features/guild/models/Guild';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {GuildContextMenu} from '@app/features/ui/action_menu/GuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as AvatarSourceUtils from '@app/features/user/utils/AvatarSourceUtils';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

interface UseGuildListItemInteractionOptions {
	readonly guild: Guild;
	readonly isDesktopLayout: boolean;
	readonly isMobileExperience: boolean;
	readonly isSortingList: boolean;
	readonly itemRef: React.RefObject<HTMLElement | null>;
}

export interface GuildListItemInteraction {
	readonly hoverRef: React.RefCallback<HTMLElement>;
	readonly isHovering: boolean;
	readonly contextMenuOpen: boolean;
	readonly bottomSheetOpen: boolean;
	readonly backgroundImage: string | null;
	readonly handleSelect: () => void;
	readonly handleKeyDown: (event: React.KeyboardEvent) => void;
	readonly handleContextMenu: (event: React.MouseEvent) => void;
	readonly handleLongPress: () => void;
	readonly handleCloseBottomSheet: () => void;
}

interface DisplayedGuildIconURLQuery {
	readonly hoverIconURL: string;
	readonly iconURL: string;
	readonly shouldPlayAnimated: boolean;
}

function resolveDisplayedGuildIconURL(query: DisplayedGuildIconURLQuery): string {
	if (query.shouldPlayAnimated) {
		return query.hoverIconURL;
	}
	return query.iconURL;
}

function resolveGuildIconBackgroundImage(iconURL: string, displayedIconURL: string): string | null {
	if (iconURL.length === 0) {
		return null;
	}
	return `url(${displayedIconURL})`;
}

function resolveInitiallyLoadedImageURL(imageURL: string): string | null {
	if (ImageCacheUtils.hasImage(imageURL)) {
		return imageURL;
	}
	return null;
}

export function useGuildListItemInteraction({
	guild,
	isDesktopLayout,
	isMobileExperience,
	isSortingList,
	itemRef,
}: UseGuildListItemInteractionOptions): GuildListItemInteraction {
	const [hoverRef, isHovering] = useHover();
	const contextMenuOpen = useContextMenuHoverState(itemRef, isDesktopLayout);
	const {preloadChannelNow, selectedChannelId} = useGuildListItemPreload({
		guild,
		isHovering,
		isMobileExperience,
		isSortingList,
	});
	const iconURL = AvatarSourceUtils.getGuildIconURL(guild, false);
	const hoverIconURL = AvatarSourceUtils.getGuildIconURL(guild, true);
	const [loadedAnimatedURL, setLoadedAnimatedURL] = useState<string | null>(() =>
		resolveInitiallyLoadedImageURL(hoverIconURL),
	);
	const [bottomSheetOpen, setBottomSheetOpen] = useState(false);
	useEffect(() => ImageCacheUtils.pinImage(iconURL), [iconURL]);
	useEffect(() => {
		if (!isHovering && !contextMenuOpen) return;
		if (loadedAnimatedURL === hoverIconURL) return;
		return ImageCacheUtils.loadImage(hoverIconURL, () => setLoadedAnimatedURL(hoverIconURL));
	}, [contextMenuOpen, hoverIconURL, isHovering, loadedAnimatedURL]);
	const handleSelect = useCallback(() => {
		markDirectSelection(DirectSelectionSurface.GUILD_RAIL);
		preloadChannelNow();
		if (isMobileExperience || selectedChannelId == null) {
			NavigationCommands.selectGuild(guild.id);
			return;
		}
		NavigationCommands.selectGuild(guild.id, selectedChannelId);
	}, [guild.id, isMobileExperience, preloadChannelNow, selectedChannelId]);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			handleSelect();
		},
		[handleSelect],
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			if (isSortingList) return;
			event.preventDefault();
			event.stopPropagation();
			if (isMobileExperience) return;
			ContextMenuCommands.openFromEvent(event, (props) => (
				<GuildContextMenu
					guild={guild}
					onClose={props.onClose}
					data-flx="app.sidebar-nav.use-guild-list-item-interaction.handle-context-menu.guild-context-menu"
				/>
			));
		},
		[guild, isMobileExperience, isSortingList],
	);
	const handleLongPress = useCallback(() => {
		if (!isSortingList && isMobileExperience) setBottomSheetOpen(true);
	}, [isMobileExperience, isSortingList]);
	const handleCloseBottomSheet = useCallback(() => setBottomSheetOpen(false), []);
	const shouldPlayAnimated = (isHovering || contextMenuOpen) && loadedAnimatedURL === hoverIconURL;
	const displayedIconURL = resolveDisplayedGuildIconURL({hoverIconURL, iconURL, shouldPlayAnimated});
	const backgroundImage = resolveGuildIconBackgroundImage(iconURL, displayedIconURL);
	return {
		hoverRef,
		isHovering,
		contextMenuOpen,
		bottomSheetOpen,
		backgroundImage,
		handleSelect,
		handleKeyDown,
		handleContextMenu,
		handleLongPress,
		handleCloseBottomSheet,
	};
}
