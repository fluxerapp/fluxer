// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type RememberedSkeletonGuildRailItem,
	type RememberedSkeletonGuildRailLayout,
	SKELETON_GUILD_RAIL_FALLBACK_ITEMS,
	SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {
	skeletonDirectMessagesDisabled,
	skeletonDiscoveryAvailable,
	skeletonSingleCommunityEnabled,
} from '@app/features/app/components/skeleton/SkeletonRuntimeConfig';
import {Platform} from '@app/features/platform/types/Platform';

const SHOWS_DOWNLOAD_ACTION = !Platform.isElectron && !Platform.isPWA;

export interface SkeletonRailProjection {
	readonly fluxerVisible: boolean;
	readonly favoritesVisible: boolean;
	readonly inlineDmUnreadFlags: ReadonlyArray<boolean>;
	readonly selectedInlineDmRowIndex: number;
	readonly outageVisible: boolean;
	readonly organizedItems: ReadonlyArray<RememberedSkeletonGuildRailItem>;
	readonly selectedItemIndex: number;
	readonly discoveryVisible: boolean;
	readonly addGuildVisible: boolean;
	readonly downloadVisible: boolean;
	readonly helpVisible: boolean;
	readonly scrollTopPx: number;
}

export function resolveSkeletonRailProjection(
	layout: RememberedSkeletonGuildRailLayout | null,
): SkeletonRailProjection {
	const communityActionsAvailable = !skeletonSingleCommunityEnabled();
	const inlineDmsVisible = !skeletonDirectMessagesDisabled();
	const inlineDmRowCount = inlineDmsVisible ? (layout?.inlineDmRowCount ?? 0) : 0;
	const rememberedUnread = layout?.inlineDmUnreadFlags ?? [];
	const inlineDmUnreadFlags = Array.from(
		{length: inlineDmRowCount},
		(_unused, index) => rememberedUnread[index] === true,
	);
	return {
		fluxerVisible: layout?.fluxerVisible ?? true,
		favoritesVisible: layout?.favoritesVisible ?? true,
		inlineDmUnreadFlags,
		selectedInlineDmRowIndex: inlineDmsVisible
			? (layout?.selectedInlineDmRowIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX)
			: SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		outageVisible: layout?.outageVisible ?? false,
		organizedItems: layout?.organizedItems ?? SKELETON_GUILD_RAIL_FALLBACK_ITEMS,
		selectedItemIndex: layout?.selectedItemIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		discoveryVisible: skeletonDiscoveryAvailable() && (layout?.discoveryVisible ?? true),
		addGuildVisible: communityActionsAvailable && (layout?.addGuildVisible ?? true),
		downloadVisible: layout?.downloadVisible ?? SHOWS_DOWNLOAD_ACTION,
		helpVisible: layout?.helpVisible ?? true,
		scrollTopPx: layout?.scrollTopPx ?? 0,
	};
}
