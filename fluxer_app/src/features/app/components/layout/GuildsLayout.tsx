// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useActiveNagbars, useNagbarConditions} from '@app/features/app/components/layout/app_layout/AppLayoutHooks';
import {type NagbarState, NagbarType} from '@app/features/app/components/layout/app_layout/AppLayoutTypes';
import {NagbarContainer} from '@app/features/app/components/layout/app_layout/NagbarContainer';
import {MAINTENANCE_NAGBAR_TONE_KINDS} from '@app/features/app/components/layout/app_layout/nagbars/ScheduledMaintenanceNagbar';
import {TopNagbarContext} from '@app/features/app/components/layout/app_layout/TopNagbarContext';
import {
	consumeDirectSelection,
	DirectSelectionSurface,
} from '@app/features/app/components/layout/DirectSelectionOrigin';
import type {
	FloatingUnreadTarget,
	FloatingUnreadTargetBounds,
} from '@app/features/app/components/layout/FloatingUnreadEdges';
import {
	FloatingUnreadIndicators,
	type FloatingUnreadTargetRegistry,
	useFloatingUnreadTargetRegistry,
} from '@app/features/app/components/layout/FloatingUnreadIndicators';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {
	DM_LIST_REMOVAL_DELAY_MS,
	DMListAnimatedRow,
	type DMListRow,
	useFrameBatchedDMListRows,
} from '@app/features/app/components/layout/guilds_layout/GuildListDMAnimation';
import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorStateMachine';
import {AddGuildButton} from '@app/features/app/components/layout/sidebar_nav/AddGuildButton';
import {DiscoveryButton} from '@app/features/app/components/layout/sidebar_nav/DiscoveryButton';
import {DownloadButton} from '@app/features/app/components/layout/sidebar_nav/DownloadButton';
import {FavoritesButton} from '@app/features/app/components/layout/sidebar_nav/FavoritesButton';
import {FluxerButton} from '@app/features/app/components/layout/sidebar_nav/FluxerButton';
import {GuildFolderItem} from '@app/features/app/components/layout/sidebar_nav/GuildFolderItem';
import {resolveDMListItemUnreadState} from '@app/features/app/components/layout/sidebar_nav/GuildListDMItem';
import {GuildListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListItem';
import {HelpButton} from '@app/features/app/components/layout/sidebar_nav/HelpButton';
import {
	DragItemType,
	DropPlacement,
	type GuildDragItem,
	type GuildDropPosition,
	type GuildDropResult,
} from '@app/features/app/components/layout/types/DndTypes';
import {UserArea} from '@app/features/app/components/layout/UserArea';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {
	getRememberedSkeletonGuildRailScrollTopPx,
	type RememberedSkeletonGuildRailItem,
	type RememberedSkeletonGuildRailLayout,
	type RememberedSkeletonNagbarRow,
	registerSkeletonLayoutMemoryPreFlush,
	reportSkeletonGuildRailLayout,
	reportSkeletonGuildRailScrollTop,
	reportSkeletonNagbarLayout,
	SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT,
	SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT,
	SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
	SkeletonGuildRailItemIndicator,
	SkeletonGuildRailItemKind,
	SkeletonNagbarTone,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {WHATS_NEW_ENTRIES} from '@app/features/app/components/whats_new/WhatsNewEntries';
import {openWhatsNewModal} from '@app/features/app/components/whats_new/WhatsNewModal';
import {ConnectionNoticeTone, resolveConnectionNoticeShape} from '@app/features/app/hooks/useConnectionNotice';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {
	NavigationAlignment,
	type NavigationRow,
	type NavigationRowBounds,
	resolveNavigationAlignment,
	useNavigationList,
} from '@app/features/app/hooks/useNavigationList';
import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import Initialization from '@app/features/app/state/Initialization';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildFolderExpanded from '@app/features/guild/state/GuildFolderExpanded';
import GuildListState, {type OrganizedItem} from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import HiddenGuildListButtons from '@app/features/guild/state/HiddenGuildListButtons';
import {PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openMacPermissionsModal} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Platform} from '@app/features/platform/types/Platform';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {AxisOrientation, type VerticalEdge} from '@app/features/ui/AxisOrientation';
import * as DimensionCommands from '@app/features/ui/commands/DimensionCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {useHoverDeferredOrderedItems} from '@app/features/ui/hooks/UseHoverDeferredOrderedItems';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import {RelativePosition} from '@app/features/ui/RelativePosition';
import Dimension from '@app/features/ui/state/Dimension';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Nagbar from '@app/features/ui/state/Nagbar';
import SidebarPreferences from '@app/features/ui/state/SidebarPreferences';
import SidebarWidth from '@app/features/ui/state/SidebarWidth';
import WhatsNew from '@app/features/ui/state/WhatsNew';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import StatusPage from '@app/features/user/state/StatusPage';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings, {type GuildFolder} from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallState from '@app/features/voice/state/CallState';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {flxElementClassName} from '@app/lib/react';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	GuildFolderFlags,
	UNCATEGORIZED_FOLDER_ID,
} from '@fluxer/constants/src/UserConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ExclamationMarkIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun, untracked} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {useDrop} from 'react-dnd';

const NEW_DESCRIPTOR = msg({
	message: 'New',
	context: 'navigation-badge',
	comment: 'Small badge on a new or recently added app navigation item.',
});
const isSelectedPath = (pathname: string, path: string) => {
	return pathname.startsWith(path);
};
const UNAVAILABLE_INDICATOR_DEBOUNCE_MS = 1500;
const GUILD_LIST_FOCUSABLE_SELECTOR = '[data-guild-list-focus-item="true"]';
const WHEEL_LINE_HEIGHT_PX = 16;
const DOM_DELTA_LINE = 1;
const DOM_DELTA_PAGE = 2;
const getChannelId = (channel: Channel): string => channel.id;
const EMPTY_CHANNELS: ReadonlyArray<Channel> = Object.freeze([]);
const ESTIMATED_GUILD_ROW_HEIGHT = 54;
const GUILD_BOTTOM_DROP_ZONE_HEIGHT = 24;
const GUILD_LIST_SCROLLER_IDENTITY = 'guild-list-scroller';
const GUILD_LIST_SCROLL_PERSIST_IDLE_MS = 150;
const GUILD_ROW_STYLE: React.CSSProperties = Object.freeze({width: '100%', paddingBottom: 0});
const GUILD_ROW_STYLE_WITH_GAP: React.CSSProperties = Object.freeze({
	width: '100%',
	paddingBottom: 'var(--guild-list-item-gap)',
});

interface GuildsLayoutSidebarStyle extends React.CSSProperties {
	'--layout-sidebar-width': string;
}

function resolveGuildsLayoutSidebarStyle(
	mobileEnabled: boolean,
	sidebarWidth: string | null,
): React.CSSProperties | undefined {
	if (mobileEnabled || sidebarWidth == null || sidebarWidth === '') return undefined;
	const style: GuildsLayoutSidebarStyle = {'--layout-sidebar-width': sidebarWidth};
	return style;
}

function getResizeObserverEntryBlockSize(entry: ResizeObserverEntry): number {
	const borderBoxSize = entry.borderBoxSize;
	const firstBorderBoxSize = Array.isArray(borderBoxSize) ? borderBoxSize[0] : borderBoxSize;
	return firstBorderBoxSize?.blockSize ?? entry.contentRect.height;
}

function resolveGuildFolderId(folderId: number | null): number {
	if (folderId != null) return folderId;
	return UNCATEGORIZED_FOLDER_ID;
}

function resolveGuildListScrollerNode(scroller: ScrollerHandle | null): HTMLElement | null {
	if (scroller == null) return null;
	return scroller.getScrollerNode();
}

type GuildNavigationRow =
	| (NavigationRow & {readonly kind: 'fluxer'})
	| (NavigationRow & {readonly kind: 'favorites'})
	| (NavigationRow & {readonly kind: 'dm'; readonly row: DMListRow; readonly isLast: boolean})
	| (NavigationRow & {readonly kind: 'divider'})
	| (NavigationRow & {readonly kind: 'outage'})
	| (NavigationRow & {readonly kind: 'organized-item'; readonly item: OrganizedItem})
	| (NavigationRow & {readonly kind: 'bottom-drop-zone'})
	| (NavigationRow & {readonly kind: 'discovery'})
	| (NavigationRow & {readonly kind: 'add-guild'})
	| (NavigationRow & {readonly kind: 'download'})
	| (NavigationRow & {readonly kind: 'help'});

interface GuildNavigationVisibility {
	readonly fluxerVisible: boolean;
	readonly favoritesVisible: boolean;
	readonly discoveryVisible: boolean;
	readonly addGuildVisible: boolean;
	readonly downloadVisible: boolean;
	readonly helpVisible: boolean;
}

function useGuildNavigationVisibility(): GuildNavigationVisibility {
	const communityActionsVisible = !RuntimeConfig.singleCommunityEnabled;
	const fluxerVisible = !RuntimeConfig.directMessagesDisabled;
	const favoritesVisible = Accessibility.showFavorites;
	const downloadVisible = !Platform.isElectron && !Platform.isPWA && !HiddenGuildListButtons.downloadButtonHidden;
	const helpVisible = !HiddenGuildListButtons.helpButtonHidden;
	return useMemo(
		() =>
			Object.freeze({
				fluxerVisible,
				favoritesVisible,
				discoveryVisible: communityActionsVisible,
				addGuildVisible: communityActionsVisible,
				downloadVisible,
				helpVisible,
			}),
		[communityActionsVisible, downloadVisible, favoritesVisible, fluxerVisible, helpVisible],
	);
}

interface CreateGuildNavigationRowsRequest {
	readonly hasUnavailableGuilds: boolean;
	readonly isDragging: boolean;
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly shouldRenderGuildListItems: boolean;
	readonly visibleDMListRows: ReadonlyArray<DMListRow>;
	readonly visibility: GuildNavigationVisibility;
}

interface ResolveSelectedGuildNavigationKeyRequest {
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly pathname: string;
	readonly visibleDMListRows: ReadonlyArray<DMListRow>;
	readonly visibility: GuildNavigationVisibility;
}

function createGuildNavigationRows({
	hasUnavailableGuilds,
	isDragging,
	organizedItems,
	shouldRenderGuildListItems,
	visibleDMListRows,
	visibility,
}: CreateGuildNavigationRowsRequest): Array<GuildNavigationRow> {
	const rows: Array<GuildNavigationRow> = [];
	if (visibility.fluxerVisible) {
		rows.push({
			kind: 'fluxer',
			key: 'fluxer',
			focusable: true,
			focusTargetIdentity: 'fluxer',
		});
	}
	if (visibility.favoritesVisible) {
		rows.push({
			kind: 'favorites',
			key: 'favorites',
			focusable: true,
			focusTargetIdentity: 'favorites',
		});
	}
	for (let index = 0; index < visibleDMListRows.length; index++) {
		const row = visibleDMListRows[index];
		rows.push({
			kind: 'dm',
			key: `dm:${row.channel.id}`,
			focusable: true,
			focusTargetIdentity: row,
			row,
			isLast: index === visibleDMListRows.length - 1,
		});
	}
	rows.push({
		kind: 'divider',
		key: 'divider',
		focusable: false,
		focusTargetIdentity: 'divider',
	});
	if (shouldRenderGuildListItems) {
		if (hasUnavailableGuilds) {
			rows.push({
				kind: 'outage',
				key: 'outage',
				focusable: false,
				focusTargetIdentity: 'outage',
			});
		}
		for (const item of organizedItems) {
			rows.push({
				kind: 'organized-item',
				key: `organized:${getOrganizedItemKey(item)}`,
				focusable: true,
				focusTargetIdentity: item,
				item,
			});
		}
		if (isDragging && organizedItems.length > 0) {
			rows.push({
				kind: 'bottom-drop-zone',
				key: 'bottom-drop-zone',
				focusable: false,
				focusTargetIdentity: 'bottom-drop-zone',
			});
		}
	}
	if (visibility.discoveryVisible) {
		rows.push({
			kind: 'discovery',
			key: 'discovery',
			focusable: true,
			focusTargetIdentity: 'discovery',
		});
	}
	if (visibility.addGuildVisible) {
		rows.push({
			kind: 'add-guild',
			key: 'add-guild',
			focusable: true,
			focusTargetIdentity: 'add-guild',
		});
	}
	if (visibility.downloadVisible) {
		rows.push({
			kind: 'download',
			key: 'download',
			focusable: true,
			focusTargetIdentity: 'download',
		});
	}
	if (visibility.helpVisible) {
		rows.push({
			kind: 'help',
			key: 'help',
			focusable: true,
			focusTargetIdentity: 'help',
		});
	}
	return rows;
}

function hasGapAfterGuildNavigationRow(row: GuildNavigationRow, nextRow: GuildNavigationRow | null): boolean {
	switch (row.kind) {
		case 'fluxer':
		case 'discovery':
		case 'add-guild':
		case 'download':
			return true;
		case 'favorites':
			if (nextRow == null) {
				return false;
			}
			return nextRow.kind === 'dm';
		case 'organized-item':
			if (nextRow == null) {
				return true;
			}
			return nextRow.kind !== 'organized-item';
		default:
			return false;
	}
}

interface OrganizedItemPathQuery {
	readonly item: OrganizedItem;
	readonly pathname: string;
}

function guildNavigationItemContainsSelectedGuild({item, pathname}: OrganizedItemPathQuery): boolean {
	if (item.type === 'guild') return isSelectedPath(pathname, Routes.guildChannel(item.guild.id));
	return item.guilds.some((guild) => isSelectedPath(pathname, Routes.guildChannel(guild.id)));
}

function resolveSelectedGuildNavigationKey({
	organizedItems,
	pathname,
	visibleDMListRows,
	visibility,
}: ResolveSelectedGuildNavigationKeyRequest): string | null {
	for (const row of visibleDMListRows) {
		if (isSelectedPath(pathname, Routes.dmChannel(row.channel.id))) return `dm:${row.channel.id}`;
	}
	for (const item of organizedItems) {
		if (guildNavigationItemContainsSelectedGuild({item, pathname})) {
			return `organized:${getOrganizedItemKey(item)}`;
		}
	}
	if (visibility.favoritesVisible && pathname === Routes.FAVORITES) return 'favorites';
	if (visibility.discoveryVisible && Routes.isDiscoverRoute(pathname)) return 'discovery';
	if (visibility.fluxerVisible && pathname === Routes.ME) return 'fluxer';
	return null;
}

const getUnreadDMChannels = (dmChannels: ReadonlyArray<Channel>): Array<Channel> => {
	const out: Array<Channel> = [];
	for (let i = 0; i < dmChannels.length; i++) {
		if (ReadStates.hasUnreadPrivateChannel(dmChannels[i].id)) out.push(dmChannels[i]);
	}
	return out;
};

function resolvePinnedCallChannel(directMessagesDisabled: boolean): Channel | null {
	if (directMessagesDisabled) return null;
	if (!MediaEngine.connected) return null;
	const channelId = MediaEngine.channelId;
	if (channelId == null || channelId === '') return null;
	const channel = Channels.getChannel(channelId);
	if (channel == null) return null;
	if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) return null;
	if (!CallState.hasActiveCall(channel.id)) return null;
	return channel;
}

interface DMChannelVisibilityControllerState {
	readonly removalDelayMs: number;
	readonly removalTimers: React.MutableRefObject<Map<string, number>>;
	readonly orderedUnreadChannels: ReadonlyArray<Channel>;
	readonly setVisibleChannels: React.Dispatch<React.SetStateAction<ReadonlyArray<Channel>>>;
	readonly unreadChannels: ReadonlyArray<Channel>;
	readonly unreadIdsRef: React.MutableRefObject<ReadonlySet<string>>;
}

interface ProjectVisibleDMChannelsRequest {
	readonly currentChannels: ReadonlyArray<Channel>;
	readonly unreadIds: ReadonlySet<string>;
}

class DMChannelVisibilityController {
	private readonly removalDelayMs: number;
	private readonly removalTimers: React.MutableRefObject<Map<string, number>>;
	private readonly orderedUnreadChannels: ReadonlyArray<Channel>;
	private readonly setVisibleChannels: React.Dispatch<React.SetStateAction<ReadonlyArray<Channel>>>;
	private readonly unreadChannels: ReadonlyArray<Channel>;
	private readonly unreadIdsRef: React.MutableRefObject<ReadonlySet<string>>;

	public constructor({
		removalDelayMs,
		removalTimers,
		orderedUnreadChannels,
		setVisibleChannels,
		unreadChannels,
		unreadIdsRef,
	}: DMChannelVisibilityControllerState) {
		this.removalDelayMs = removalDelayMs;
		this.removalTimers = removalTimers;
		this.orderedUnreadChannels = orderedUnreadChannels;
		this.setVisibleChannels = setVisibleChannels;
		this.unreadChannels = unreadChannels;
		this.unreadIdsRef = unreadIdsRef;
	}

	public synchronize(): void {
		this.setVisibleChannels((currentChannels) =>
			this.projectVisibleChannels({currentChannels, unreadIds: this.unreadIdsRef.current}),
		);
		for (const channel of this.unreadChannels) this.cancelRemoval(channel.id);
	}

	private projectVisibleChannels({currentChannels, unreadIds}: ProjectVisibleDMChannelsRequest): Array<Channel> {
		const visibleChannelIds = new Set(this.orderedUnreadChannels.map(getChannelId));
		const leavingChannels = currentChannels.filter((channel) => {
			if (unreadIds.has(channel.id) || visibleChannelIds.has(channel.id)) return false;
			visibleChannelIds.add(channel.id);
			return true;
		});
		for (const channel of leavingChannels) this.scheduleRemoval(channel.id);
		return [...this.orderedUnreadChannels, ...leavingChannels];
	}

	private scheduleRemoval(channelId: string): void {
		if (this.removalTimers.current.has(channelId)) return;
		const timer = window.setTimeout(() => this.removeChannel(channelId), this.removalDelayMs);
		this.removalTimers.current.set(channelId, timer);
	}

	private removeChannel(channelId: string): void {
		this.cancelRemoval(channelId);
		if (this.unreadIdsRef.current.has(channelId)) return;
		this.setVisibleChannels((channels) => channels.filter((channel) => channel.id !== channelId));
	}

	public completeRemoval(channelId: string): void {
		this.removeChannel(channelId);
	}

	private cancelRemoval(channelId: string): void {
		const timer = this.removalTimers.current.get(channelId);
		if (timer == null) return;
		window.clearTimeout(timer);
		this.removalTimers.current.delete(channelId);
	}
}

interface ClearHoveredChannelRequest {
	readonly currentChannelId: string | null;
	readonly endedChannelId: string;
}

function clearHoveredChannelId({currentChannelId, endedChannelId}: ClearHoveredChannelRequest): string | null {
	if (currentChannelId === endedChannelId) return null;
	return currentChannelId;
}

function getOrganizedItemKey(item: OrganizedItem): string {
	if (item.type === 'folder') {
		return `folder-${item.folder.id}`;
	}
	return item.guild.id;
}

interface GuildListTargetRow {
	readonly rowKey: string;
	readonly nestedIndex: number | null;
}

interface CreateGuildTargetRowsRequest {
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly visibleDMListRows: ReadonlyArray<DMListRow>;
}

interface RegisterFolderTargetRowsRequest {
	readonly folder: GuildFolder;
	readonly folderGuilds: ReadonlyArray<{id: string}>;
	readonly rowKey: string;
	readonly targetRows: Map<string, GuildListTargetRow>;
}

interface ResolveGuildTargetBoundsRequest {
	readonly getRowBounds: (keys: ReadonlyArray<string>) => ReadonlyMap<string, NavigationRowBounds>;
	readonly scrollTargetRegistry: FloatingUnreadTargetRegistry;
	readonly scrollNode: HTMLElement | null;
	readonly targetRows: ReadonlyMap<string, GuildListTargetRow>;
	readonly targets: ReadonlyArray<FloatingUnreadTarget>;
}

interface ResolveMeasuredGuildTargetBoundsRequest {
	readonly containerRect: DOMRect | null;
	readonly node: HTMLElement | null;
	readonly scrollNode: HTMLElement | null;
}

interface ResolveEstimatedGuildTargetBoundsRequest {
	readonly rowBounds: ReadonlyMap<string, NavigationRowBounds>;
	readonly targetRow: GuildListTargetRow;
}

function registerGuildFolderTargetRows({
	folder,
	folderGuilds,
	rowKey,
	targetRows,
}: RegisterFolderTargetRowsRequest): void {
	const folderId = resolveGuildFolderId(folder.id);
	if (!GuildFolderExpanded.isExpanded(folderId)) {
		targetRows.set(`folder-${folder.id}`, {rowKey, nestedIndex: null});
		return;
	}
	for (let index = 0; index < folderGuilds.length; index++) {
		targetRows.set(`guild-${folderGuilds[index].id}`, {rowKey, nestedIndex: index + 1});
	}
}

function createGuildTargetRows({
	organizedItems,
	visibleDMListRows,
}: CreateGuildTargetRowsRequest): ReadonlyMap<string, GuildListTargetRow> {
	const targetRows = new Map<string, GuildListTargetRow>();
	for (const row of visibleDMListRows) {
		targetRows.set(`dm-${row.channel.id}`, {rowKey: `dm:${row.channel.id}`, nestedIndex: null});
	}
	for (const item of organizedItems) {
		const rowKey = `organized:${getOrganizedItemKey(item)}`;
		if (item.type === 'folder') {
			registerGuildFolderTargetRows({
				folder: item.folder,
				folderGuilds: item.guilds,
				rowKey,
				targetRows,
			});
			continue;
		}
		targetRows.set(`guild-${item.guild.id}`, {rowKey, nestedIndex: null});
	}
	return targetRows;
}

function resolveMeasuredGuildTargetBounds({
	containerRect,
	node,
	scrollNode,
}: ResolveMeasuredGuildTargetBoundsRequest): NavigationRowBounds | null {
	if (scrollNode == null) return null;
	if (containerRect == null) return null;
	if (node == null) return null;
	if (!node.isConnected) return null;
	if (!scrollNode.contains(node)) return null;
	const rect = node.getBoundingClientRect();
	return {
		top: scrollNode.scrollTop + rect.top - containerRect.top,
		bottom: scrollNode.scrollTop + rect.bottom - containerRect.top,
	};
}

function resolveEstimatedGuildTargetBounds({
	rowBounds,
	targetRow,
}: ResolveEstimatedGuildTargetBoundsRequest): NavigationRowBounds | null {
	const bounds = rowBounds.get(targetRow.rowKey);
	if (bounds == null) return null;
	if (targetRow.nestedIndex == null) return bounds;
	const top = Math.min(bounds.bottom, bounds.top + targetRow.nestedIndex * ESTIMATED_GUILD_ROW_HEIGHT);
	return {top, bottom: Math.min(bounds.bottom, top + ESTIMATED_GUILD_ROW_HEIGHT)};
}

function resolveGuildTargetBounds({
	getRowBounds,
	scrollTargetRegistry,
	scrollNode,
	targetRows,
	targets,
}: ResolveGuildTargetBoundsRequest): ReadonlyMap<string, FloatingUnreadTargetBounds> {
	const targetBounds = new Map<string, NavigationRowBounds>();
	if (targets.length === 0) return targetBounds;
	if (scrollNode == null) return targetBounds;
	const containerRect = scrollNode.getBoundingClientRect();
	const unmountedTargetRows: Array<GuildListTargetRow> = [];
	const unmountedTargetIds: Array<string> = [];
	for (const target of targets) {
		const node = scrollTargetRegistry.getTargetNode(target.id);
		const measuredBounds = resolveMeasuredGuildTargetBounds({containerRect, node, scrollNode});
		if (measuredBounds != null) {
			targetBounds.set(target.id, measuredBounds);
			continue;
		}
		const targetRow = targetRows.get(target.id);
		if (targetRow == null) continue;
		unmountedTargetRows.push(targetRow);
		unmountedTargetIds.push(target.id);
	}
	if (unmountedTargetRows.length === 0) return targetBounds;
	const rowKeys: Array<string> = [];
	for (const targetRow of unmountedTargetRows) {
		rowKeys.push(targetRow.rowKey);
	}
	const rowBounds = getRowBounds(rowKeys);
	for (let index = 0; index < unmountedTargetRows.length; index += 1) {
		const estimatedBounds = resolveEstimatedGuildTargetBounds({rowBounds, targetRow: unmountedTargetRows[index]});
		if (estimatedBounds != null) targetBounds.set(unmountedTargetIds[index], estimatedBounds);
	}
	return targetBounds;
}

function getDMScrollIndicatorSeverity(channelId: string): ScrollIndicatorSeverity | null {
	const mentionCount = ReadStates.getPrivateChannelMentionCount(channelId);
	const unreadState = getChannelUnreadState({
		unreadCount: ReadStates.getPrivateChannelUnreadCount(channelId),
		mentionCount,
		isMuted: UserGuildSettings.isChannelMuted(null, channelId),
		showFadedUnreadOnMutedChannels: Accessibility.showFadedUnreadOnMutedChannels,
	});
	if (mentionCount > 0) return 'mention';
	if (unreadState.shouldShowUnreadIndicator) return 'unread';
	return null;
}

function getGuildScrollIndicatorSeverity(guildId: string): ScrollIndicatorSeverity | null {
	if (GuildReadState.getMentionCount(guildId) > 0) return 'mention';
	if (GuildReadState.hasUnread(guildId)) return 'unread';
	return null;
}

function getFolderScrollIndicatorSeverity(guilds: ReadonlyArray<{id: string}>): ScrollIndicatorSeverity | null {
	let hasUnread = false;
	for (const guild of guilds) {
		const severity = getGuildScrollIndicatorSeverity(guild.id);
		if (severity === 'mention') return 'mention';
		if (severity === 'unread') hasUnread = true;
	}
	if (hasUnread) return 'unread';
	return null;
}

function pushScrollIndicatorTarget(
	targets: Array<FloatingUnreadTarget>,
	id: string,
	severity: ScrollIndicatorSeverity | null,
): void {
	if (severity == null) return;
	targets.push({id, severity});
}

interface AppendFolderScrollIndicatorTargetsRequest {
	readonly folder: GuildFolder;
	readonly folderGuilds: ReadonlyArray<{id: string}>;
	readonly targets: Array<FloatingUnreadTarget>;
}

function appendFolderScrollIndicatorTargets({
	folder,
	folderGuilds,
	targets,
}: AppendFolderScrollIndicatorTargetsRequest): void {
	const folderId = resolveGuildFolderId(folder.id);
	if (!GuildFolderExpanded.isExpanded(folderId)) {
		pushScrollIndicatorTarget(targets, `folder-${folder.id}`, getFolderScrollIndicatorSeverity(folderGuilds));
		return;
	}
	for (const guild of folderGuilds) {
		pushScrollIndicatorTarget(targets, `guild-${guild.id}`, getGuildScrollIndicatorSeverity(guild.id));
	}
}

interface BuildGuildScrollIndicatorTargetsRequest {
	readonly dmListRows: ReadonlyArray<DMListRow>;
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
}

function buildGuildScrollIndicatorTargets({
	dmListRows,
	organizedItems,
}: BuildGuildScrollIndicatorTargetsRequest): Array<FloatingUnreadTarget> {
	const targets: Array<FloatingUnreadTarget> = [];
	for (const row of dmListRows) {
		pushScrollIndicatorTarget(targets, `dm-${row.channel.id}`, getDMScrollIndicatorSeverity(row.channel.id));
	}
	for (const item of organizedItems) {
		if (item.type === 'folder') {
			appendFolderScrollIndicatorTargets({folder: item.folder, folderGuilds: item.guilds, targets});
			continue;
		}
		pushScrollIndicatorTarget(targets, `guild-${item.guild.id}`, getGuildScrollIndicatorSeverity(item.guild.id));
	}
	return targets;
}

function areScrollIndicatorTargetsEqual(
	left: ReadonlyArray<FloatingUnreadTarget>,
	right: ReadonlyArray<FloatingUnreadTarget>,
): boolean {
	if (left.length !== right.length) return false;
	for (let index = 0; index < left.length; index++) {
		if (left[index].id !== right[index].id) return false;
		if (left[index].severity !== right[index].severity) return false;
	}
	return true;
}

function useStableScrollIndicatorTargets(
	targets: ReadonlyArray<FloatingUnreadTarget>,
): ReadonlyArray<FloatingUnreadTarget> {
	const stableTargetsRef = useRef(targets);
	const stableTargets = stableTargetsRef.current;
	if (stableTargets !== targets && areScrollIndicatorTargetsEqual(stableTargets, targets)) {
		return stableTargets;
	}
	stableTargetsRef.current = targets;
	return targets;
}

interface GuildScrollIndicatorsProps {
	readonly dmListRows: ReadonlyArray<DMListRow>;
	readonly getTargetBounds: (
		targets: ReadonlyArray<FloatingUnreadTarget>,
	) => ReadonlyMap<string, FloatingUnreadTargetBounds>;
	readonly getTargetNode: (id: string) => HTMLElement | null;
	readonly label: string;
	readonly measurementRevision: number;
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly scrollToTarget: (id: string, direction: VerticalEdge) => boolean;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
}

const GuildScrollIndicators = observer(
	({
		dmListRows,
		getTargetBounds,
		getTargetNode,
		label,
		measurementRevision,
		organizedItems,
		scrollToTarget,
		scrollerRef,
	}: GuildScrollIndicatorsProps) => {
		const targets = useStableScrollIndicatorTargets(buildGuildScrollIndicatorTargets({dmListRows, organizedItems}));
		return (
			<FloatingUnreadIndicators
				label={label}
				scrollerRef={scrollerRef}
				scrollerIdentity={GUILD_LIST_SCROLLER_IDENTITY}
				targets={targets}
				getTargetNode={getTargetNode}
				getTargetBounds={getTargetBounds}
				scrollToTarget={scrollToTarget}
				measurementRevision={measurementRevision}
				data-flx="app.guilds-layout.guild-scroll-indicators.floating-unread-indicators"
			/>
		);
	},
);

function buildGuildNavigationIndexMap(organizedItems: ReadonlyArray<OrganizedItem>): Map<string, number> {
	const guildNavigationIndexes = new Map<string, number>();
	let guildIndex = 0;
	for (const item of organizedItems) {
		if (item.type === 'guild') {
			guildNavigationIndexes.set(item.guild.id, guildIndex++);
			continue;
		}
		for (const guild of item.guilds) {
			guildNavigationIndexes.set(guild.id, guildIndex++);
		}
	}
	return guildNavigationIndexes;
}

function getSelectedGuildNavigationIndex(
	pathname: string,
	guildNavigationIndexes: ReadonlyMap<string, number>,
): number | null {
	for (const [guildId, guildIndex] of guildNavigationIndexes) {
		if (isSelectedPath(pathname, Routes.guildChannel(guildId))) {
			return guildIndex;
		}
	}
	return null;
}

function getWheelScrollDeltaY(event: WheelEvent | React.WheelEvent<HTMLDivElement>, viewportHeight: number): number {
	if (event.deltaMode === DOM_DELTA_LINE) {
		return event.deltaY * WHEEL_LINE_HEIGHT_PX;
	}
	if (event.deltaMode === DOM_DELTA_PAGE) {
		return event.deltaY * viewportHeight;
	}
	return event.deltaY;
}

function isWheelEventOverElement(event: WheelEvent, element: HTMLElement): boolean {
	const ownerDocument = element.ownerDocument;
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow != null && event.target instanceof ownerWindow.Node && element.contains(event.target)) {
		return true;
	}
	const pointElement = ownerDocument.elementFromPoint(event.clientX, event.clientY);
	return pointElement !== null && element.contains(pointElement);
}

interface TopLevelGuildItem {
	type: 'guild';
	guildId: string;
}

interface TopLevelGuildFolderItem {
	type: 'folder';
	folder: GuildFolder;
}

type TopLevelItem = TopLevelGuildItem | TopLevelGuildFolderItem;

function cloneGuildFolder(folder: GuildFolder): GuildFolder {
	return {
		id: folder.id,
		name: folder.name,
		color: folder.color,
		flags: folder.flags,
		icon: folder.icon,
		guildIds: [...folder.guildIds],
	};
}

function getFolderIdFromKey(itemKey: string): number | null {
	if (!itemKey.startsWith('folder-')) return null;
	const folderIdRaw = itemKey.slice('folder-'.length);
	if (folderIdRaw === 'null') return null;
	const parsedFolderId = Number(folderIdRaw);
	if (Number.isNaN(parsedFolderId)) return null;
	return parsedFolderId;
}

function getNextFolderId(guildFolders: ReadonlyArray<GuildFolder>): number {
	let maxId = 0;
	for (const folder of guildFolders) {
		if (folder.id !== null && folder.id > maxId) {
			maxId = folder.id;
		}
	}
	return maxId + 1;
}

function buildTopLevelItems(guildFolders: ReadonlyArray<GuildFolder>): Array<TopLevelItem> {
	const topLevelItems: Array<TopLevelItem> = [];
	for (const folder of guildFolders) {
		if (folder.id === UNCATEGORIZED_FOLDER_ID) {
			for (const guildId of folder.guildIds) {
				topLevelItems.push({
					type: 'guild',
					guildId,
				});
			}
			continue;
		}
		if (folder.guildIds.length === 0) {
			continue;
		}
		topLevelItems.push({
			type: 'folder',
			folder: cloneGuildFolder(folder),
		});
	}
	return topLevelItems;
}

function buildGuildFoldersFromTopLevelItems(topLevelItems: ReadonlyArray<TopLevelItem>): Array<GuildFolder> {
	const guildFolders: Array<GuildFolder> = [];
	let pendingUncategorizedGuildIds: Array<string> = [];
	function flushUncategorized(): void {
		if (pendingUncategorizedGuildIds.length === 0) return;
		guildFolders.push({
			id: UNCATEGORIZED_FOLDER_ID,
			name: null,
			color: null,
			flags: 0,
			icon: DEFAULT_GUILD_FOLDER_ICON,
			guildIds: pendingUncategorizedGuildIds,
		});
		pendingUncategorizedGuildIds = [];
	}
	for (const topLevelItem of topLevelItems) {
		if (topLevelItem.type === 'guild') {
			pendingUncategorizedGuildIds.push(topLevelItem.guildId);
			continue;
		}
		flushUncategorized();
		if (topLevelItem.folder.guildIds.length === 0) {
			continue;
		}
		guildFolders.push(cloneGuildFolder(topLevelItem.folder));
	}
	flushUncategorized();
	return guildFolders;
}

function removeGuildIdsFromGuildFolders(
	guildFolders: ReadonlyArray<GuildFolder>,
	guildIdsToRemove: ReadonlySet<string>,
): Array<GuildFolder> {
	return guildFolders
		.map((folder) => {
			const filteredGuildIds = folder.guildIds.filter((guildId) => !guildIdsToRemove.has(guildId));
			return {
				id: folder.id,
				name: folder.name,
				color: folder.color,
				flags: folder.flags,
				icon: folder.icon,
				guildIds: filteredGuildIds,
			};
		})
		.filter((folder) => folder.guildIds.length > 0);
}

function isRelativeGuildDropPosition(position: GuildDropPosition): boolean {
	return position === RelativePosition.BEFORE || position === RelativePosition.AFTER;
}

interface ResolveGuildInsertIndexRequest {
	readonly position: GuildDropPosition;
	readonly targetIndex: number;
}

function resolveGuildInsertIndex({position, targetIndex}: ResolveGuildInsertIndexRequest): number {
	if (position === RelativePosition.AFTER) return targetIndex + 1;
	return targetIndex;
}

interface MoveGuildIntoFolderRequest {
	readonly sourceGuildId: string;
	readonly targetKey: string;
}

function moveGuildIntoFolder({sourceGuildId, targetKey}: MoveGuildIntoFolderRequest): Array<GuildFolder> {
	const targetFolderId = getFolderIdFromKey(targetKey);
	return removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId])).map((folder) => {
		if (folder.id !== targetFolderId) return folder;
		return {...folder, guildIds: [...folder.guildIds, sourceGuildId]};
	});
}

interface MoveGuildWithinFolderRequest {
	readonly position: GuildDropPosition;
	readonly sourceGuildId: string;
	readonly targetFolderId: number;
	readonly targetGuildId: string;
}

function moveGuildWithinFolder({
	position,
	sourceGuildId,
	targetFolderId,
	targetGuildId,
}: MoveGuildWithinFolderRequest): Array<GuildFolder> {
	return removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId])).map((folder) => {
		if (folder.id !== targetFolderId) return folder;
		const guildIds = [...folder.guildIds];
		const targetIndex = guildIds.indexOf(targetGuildId);
		if (targetIndex === -1) {
			guildIds.push(sourceGuildId);
			return {...folder, guildIds};
		}
		guildIds.splice(resolveGuildInsertIndex({position, targetIndex}), 0, sourceGuildId);
		return {...folder, guildIds};
	});
}

interface CombineGuildsRequest {
	readonly sourceGuildId: string;
	readonly targetGuildId: string;
}

function combineGuildsIntoFolder({sourceGuildId, targetGuildId}: CombineGuildsRequest): Array<GuildFolder> | null {
	const cleanedFolders = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
	const topLevelItems = buildTopLevelItems(cleanedFolders);
	const targetIndex = topLevelItems.findIndex(
		(topLevelItem) => topLevelItem.type === 'guild' && topLevelItem.guildId === targetGuildId,
	);
	if (targetIndex === -1) return null;
	const folder: GuildFolder = {
		id: getNextFolderId(UserSettings.guildFolders),
		name: null,
		color: null,
		flags: 0,
		icon: DEFAULT_GUILD_FOLDER_ICON,
		guildIds: [targetGuildId, sourceGuildId],
	};
	topLevelItems[targetIndex] = {type: 'folder', folder};
	return buildGuildFoldersFromTopLevelItems(topLevelItems);
}

interface MoveGuildBesideFolderRequest {
	readonly position: GuildDropPosition;
	readonly sourceGuildId: string;
	readonly targetKey: string;
}

function moveGuildBesideFolder({
	position,
	sourceGuildId,
	targetKey,
}: MoveGuildBesideFolderRequest): Array<GuildFolder> | null {
	const targetFolderId = getFolderIdFromKey(targetKey);
	const originalItems = buildTopLevelItems(UserSettings.guildFolders);
	const originalTargetIndex = originalItems.findIndex(
		(item) => item.type === 'folder' && item.folder.id === targetFolderId,
	);
	if (originalTargetIndex === -1) return null;
	const cleanedFolders = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
	const topLevelItems = buildTopLevelItems(cleanedFolders);
	let targetIndex = topLevelItems.findIndex((item) => item.type === 'folder' && item.folder.id === targetFolderId);
	if (targetIndex === -1) targetIndex = Math.min(originalTargetIndex, topLevelItems.length);
	const insertIndex = resolveGuildInsertIndex({position, targetIndex});
	topLevelItems.splice(Math.min(insertIndex, topLevelItems.length), 0, {type: 'guild', guildId: sourceGuildId});
	return buildGuildFoldersFromTopLevelItems(topLevelItems);
}

interface MoveGuildBesideGuildRequest {
	readonly position: GuildDropPosition;
	readonly sourceGuildId: string;
	readonly targetGuildId: string;
}

function moveGuildBesideGuild({
	position,
	sourceGuildId,
	targetGuildId,
}: MoveGuildBesideGuildRequest): Array<GuildFolder> | null {
	const cleanedFolders = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
	const topLevelItems = buildTopLevelItems(cleanedFolders);
	const targetIndex = topLevelItems.findIndex((item) => item.type === 'guild' && item.guildId === targetGuildId);
	if (targetIndex === -1) return null;
	topLevelItems.splice(resolveGuildInsertIndex({position, targetIndex}), 0, {type: 'guild', guildId: sourceGuildId});
	return buildGuildFoldersFromTopLevelItems(topLevelItems);
}

interface ReorderOrganizedGuildItemRequest {
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly position: GuildDropPosition;
	readonly sourceKey: string;
	readonly targetKey: string;
}

function reorderOrganizedGuildItem({
	organizedItems,
	position,
	sourceKey,
	targetKey,
}: ReorderOrganizedGuildItemRequest): Array<GuildFolder> | null {
	const oldIndex = organizedItems.findIndex((item) => getOrganizedItemKey(item) === sourceKey);
	const targetIndex = organizedItems.findIndex((item) => getOrganizedItemKey(item) === targetKey);
	if (oldIndex === -1 || targetIndex === -1) return null;
	let targetIndexAfterRemoval = targetIndex;
	if (oldIndex < targetIndex) targetIndexAfterRemoval = targetIndex - 1;
	const newIndex = resolveGuildInsertIndex({position, targetIndex: targetIndexAfterRemoval});
	const reorderedItems = [...organizedItems];
	const [movedItem] = reorderedItems.splice(oldIndex, 1);
	reorderedItems.splice(newIndex, 0, movedItem);
	const topLevelItems: Array<TopLevelItem> = reorderedItems.map((item) => {
		if (item.type === 'folder') {
			return {type: 'folder', folder: cloneGuildFolder(item.folder)};
		}
		return {type: 'guild', guildId: item.guild.id};
	});
	return buildGuildFoldersFromTopLevelItems(topLevelItems);
}

interface ApplyGuildDropRequest {
	readonly item: GuildDragItem;
	readonly organizedItems: ReadonlyArray<OrganizedItem>;
	readonly result: GuildDropResult;
}

function applyGuildFolderDrop({item, organizedItems, result}: ApplyGuildDropRequest): Array<GuildFolder> | null {
	const sourceKey = item.id;
	const targetKey = result.targetId;
	if (sourceKey === targetKey) return null;
	const {position, targetIsFolder, targetFolderId} = result;
	if (position === DropPlacement.INSIDE && targetIsFolder && !item.isFolder) {
		return moveGuildIntoFolder({sourceGuildId: item.id, targetKey});
	}
	if (
		targetFolderId != null &&
		targetFolderId !== UNCATEGORIZED_FOLDER_ID &&
		!item.isFolder &&
		isRelativeGuildDropPosition(position)
	) {
		return moveGuildWithinFolder({
			position,
			sourceGuildId: item.id,
			targetFolderId,
			targetGuildId: targetKey,
		});
	}
	if (position === DropPlacement.COMBINE && !targetIsFolder && !item.isFolder && targetFolderId == null) {
		return combineGuildsIntoFolder({sourceGuildId: item.id, targetGuildId: targetKey});
	}
	if (
		!item.isFolder &&
		item.folderId != null &&
		item.folderId !== UNCATEGORIZED_FOLDER_ID &&
		targetIsFolder &&
		isRelativeGuildDropPosition(position)
	) {
		return moveGuildBesideFolder({position, sourceGuildId: item.id, targetKey});
	}
	if (
		!item.isFolder &&
		item.folderId != null &&
		item.folderId !== UNCATEGORIZED_FOLDER_ID &&
		!targetIsFolder &&
		targetFolderId == null &&
		isRelativeGuildDropPosition(position)
	) {
		return moveGuildBesideGuild({position, sourceGuildId: item.id, targetGuildId: targetKey});
	}
	return reorderOrganizedGuildItem({organizedItems, position, sourceKey, targetKey});
}

interface BottomDropZoneProps {
	readonly onGuildDrop: (item: GuildDragItem, result: GuildDropResult) => void;
	readonly lastItemKey: string;
	readonly lastItemIsFolder: boolean;
	readonly isDragging: boolean;
}

function BottomDropZone({onGuildDrop, lastItemKey, lastItemIsFolder, isDragging}: BottomDropZoneProps) {
	const [{isOver, canDrop}, dropRef] = useDrop(
		() => ({
			accept: [DragItemType.GUILD_ITEM, DragItemType.GUILD_FOLDER],
			canDrop: (item: GuildDragItem) => item.id !== lastItemKey,
			drop: (item: GuildDragItem, monitor): GuildDropResult | undefined => {
				if (!monitor.canDrop()) return;
				const result: GuildDropResult = {
					targetId: lastItemKey,
					position: RelativePosition.AFTER,
					targetIsFolder: lastItemIsFolder,
					targetFolderId: null,
				};
				onGuildDrop(item, result);
				return result;
			},
			collect: (monitor) => ({
				isOver: monitor.isOver(),
				canDrop: monitor.canDrop(),
			}),
		}),
		[onGuildDrop, lastItemKey, lastItemIsFolder],
	);
	const isActive = isOver && canDrop;
	const setRef = useCallback(
		(node: HTMLElement | null) => {
			dropRef(node);
		},
		[dropRef],
	);
	if (!isDragging) return null;
	return (
		<flx-app-guild-list-bottom-drop-zone
			ref={setRef}
			className={flxElementClassName(
				styles.guildListDropZone,
				styles.guildListDropZoneBottom,
				isDragging && styles.guildListDropZoneEnabled,
				isActive && styles.guildListDropZoneActive,
			)}
			data-flx="app.guilds-layout.bottom-drop-zone.guild-list-drop-zone"
		/>
	);
}

interface GuildListDividerProps {
	readonly nextRowExtended: boolean;
}

function GuildListDivider({nextRowExtended}: GuildListDividerProps): React.ReactNode {
	const divider = (
		<flx-app-guild-list-divider
			className={flxElementClassName(styles.guildDivider)}
			data-flx="app.guilds-layout.guild-list-divider.guild-divider"
		/>
	);
	if (nextRowExtended) {
		return (
			<flx-app-guild-list-divider-slot
				className={flxElementClassName(styles.guildDividerSlot)}
				data-next-row-extended="true"
				data-flx="app.guilds-layout.guild-list-divider.guild-divider-slot"
			>
				{divider}
			</flx-app-guild-list-divider-slot>
		);
	}
	return (
		<flx-app-guild-list-divider-slot
			className={flxElementClassName(styles.guildDividerSlot)}
			data-flx="app.guilds-layout.guild-list-divider.guild-divider-slot--2"
		>
			{divider}
		</flx-app-guild-list-divider-slot>
	);
}

interface SkeletonNagbarRowShape {
	readonly tone: SkeletonNagbarTone;
	readonly hasActions: boolean;
}

const SKELETON_NAGBAR_ROW_SHAPES: Record<NagbarType, SkeletonNagbarRowShape> = {
	[NagbarType.BUILD_ENVIRONMENT]: {tone: SkeletonNagbarTone.DEVELOPMENT, hasActions: false},
	[NagbarType.CONNECTION]: {tone: SkeletonNagbarTone.NEUTRAL, hasActions: false},
	[NagbarType.CORRUPTED_INSTALLATION]: {tone: SkeletonNagbarTone.CRITICAL, hasActions: true},
	[NagbarType.SCHEDULED_MAINTENANCE]: {tone: SkeletonNagbarTone.MAINTENANCE_SCHEDULED, hasActions: true},
	[NagbarType.UNCLAIMED_ACCOUNT]: {tone: SkeletonNagbarTone.ALERT, hasActions: true},
	[NagbarType.EMAIL_VERIFICATION]: {tone: SkeletonNagbarTone.ALERT, hasActions: true},
	[NagbarType.DESKTOP_NOTIFICATION]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.PREMIUM_GRACE_PERIOD]: {tone: SkeletonNagbarTone.PREMIUM, hasActions: true},
	[NagbarType.PREMIUM_EXPIRED]: {tone: SkeletonNagbarTone.DANGER, hasActions: true},
	[NagbarType.PREMIUM_ONBOARDING]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.GIFT_INVENTORY]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.DESKTOP_DOWNLOAD]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.GUILD_MEMBERSHIP_CTA]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.VISIONARY_MFA]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.VOICE_SESSION_RESTORE]: {tone: SkeletonNagbarTone.VOICE, hasActions: true},
	[NagbarType.TERMS_ACCEPTANCE]: {tone: SkeletonNagbarTone.LEGAL, hasActions: true},
	[NagbarType.LINUX_INPUT_ACCESS]: {tone: SkeletonNagbarTone.BRAND, hasActions: true},
	[NagbarType.SOFTWARE_ENCODER]: {tone: SkeletonNagbarTone.ENCODER, hasActions: true},
	[NagbarType.STREAMER_MODE]: {tone: SkeletonNagbarTone.STREAMER, hasActions: true},
};

const CONNECTION_SKELETON_NAGBAR_TONES: Record<ConnectionNoticeTone, SkeletonNagbarTone> = {
	[ConnectionNoticeTone.NEUTRAL]: SkeletonNagbarTone.NEUTRAL,
	[ConnectionNoticeTone.MAINTENANCE]: SkeletonNagbarTone.MAINTENANCE,
};

function createSkeletonNagbarRow(nagbar: NagbarState): RememberedSkeletonNagbarRow {
	const shape = SKELETON_NAGBAR_ROW_SHAPES[nagbar.type];
	if (nagbar.type === NagbarType.CONNECTION) {
		const notice = resolveConnectionNoticeShape();
		return {
			tone: CONNECTION_SKELETON_NAGBAR_TONES[notice.tone],
			hasActions: notice.hasActions,
			dismissible: nagbar.dismissible,
		};
	}
	if (nagbar.type === NagbarType.SCHEDULED_MAINTENANCE) {
		const scheduledMaintenance = StatusPage.scheduledMaintenance;
		if (scheduledMaintenance != null) {
			return {
				tone: MAINTENANCE_NAGBAR_TONE_KINDS[scheduledMaintenance.status],
				hasActions: shape.hasActions,
				dismissible: nagbar.dismissible,
			};
		}
	}
	return {tone: shape.tone, hasActions: shape.hasActions, dismissible: nagbar.dismissible};
}

const GUILD_RAIL_INDICATOR_TOKENS: Record<SkeletonGuildRailItemIndicator, string> = {
	[SkeletonGuildRailItemIndicator.NONE]: 'n',
	[SkeletonGuildRailItemIndicator.UNREAD]: 'u',
	[SkeletonGuildRailItemIndicator.MENTION]: 'm',
};
const GUILD_RAIL_INDICATORS_BY_TOKEN: Record<string, SkeletonGuildRailItemIndicator> = {
	n: SkeletonGuildRailItemIndicator.NONE,
	u: SkeletonGuildRailItemIndicator.UNREAD,
	m: SkeletonGuildRailItemIndicator.MENTION,
};

function resolveGuildRailIndicator(guildIds: ReadonlyArray<string>): SkeletonGuildRailItemIndicator {
	let hasUnread = false;
	for (const guildId of guildIds) {
		if (GuildReadState.getMentionCount(guildId) > 0) {
			return SkeletonGuildRailItemIndicator.MENTION;
		}
		if (GuildReadState.hasUnread(guildId)) {
			hasUnread = true;
		}
	}
	if (hasUnread) {
		return SkeletonGuildRailItemIndicator.UNREAD;
	}
	return SkeletonGuildRailItemIndicator.NONE;
}

function parseGuildRailIndicatorToken(token: string): SkeletonGuildRailItemIndicator {
	const indicator = GUILD_RAIL_INDICATORS_BY_TOKEN[token];
	if (indicator == null) {
		throw new Error(`Invalid guild rail indicator token: ${token}`);
	}
	return indicator;
}

function createGuildRailItemProjection(organizedItems: ReadonlyArray<OrganizedItem>, pathname: string): string {
	const expandedFolderIds = new Set(GuildFolderExpanded.expandedFolderIds);
	let projection = '';
	let remainingVisualRows = SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT;
	for (const item of organizedItems) {
		if (remainingVisualRows === 0) {
			break;
		}
		if (item.type !== 'folder') {
			projection += `g${GUILD_RAIL_INDICATOR_TOKENS[resolveGuildRailIndicator([item.guild.id])]};`;
			remainingVisualRows -= 1;
			continue;
		}
		const folderId = resolveGuildFolderId(item.folder.id);
		const isExpanded = expandedFolderIds.has(folderId);
		if (!isExpanded) {
			const childCount = Math.min(item.guilds.length, SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT);
			const showIconWhenCollapsed =
				(item.folder.flags & GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED) === GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED;
			const indicatorToken =
				GUILD_RAIL_INDICATOR_TOKENS[resolveGuildRailIndicator(item.guilds.map((guild) => guild.id))];
			projection += `c${childCount}${showIconWhenCollapsed ? '1' : '0'}${indicatorToken};`;
			remainingVisualRows -= 1;
			continue;
		}
		const childCount = Math.min(item.guilds.length, remainingVisualRows - 1);
		let childTokens = '';
		for (const guild of item.guilds.slice(0, childCount)) {
			const childToken = GUILD_RAIL_INDICATOR_TOKENS[resolveGuildRailIndicator([guild.id])];
			if (isSelectedPath(pathname, Routes.guildChannel(guild.id))) {
				childTokens += childToken.toUpperCase();
			} else {
				childTokens += childToken;
			}
		}
		projection += `e${childTokens};`;
		remainingVisualRows -= childCount + 1;
	}
	return projection;
}

interface CreateGuildRailSkeletonLayoutRequest {
	readonly guildRailItemProjection: string;
	readonly hasUnavailableGuilds: boolean;
	readonly inlineDmChannelIds: ReadonlyArray<string>;
	readonly pathname: string;
	readonly selectedOrganizedItemIndex: number;
	readonly visibility: GuildNavigationVisibility;
}

function createGuildRailSkeletonLayout({
	guildRailItemProjection,
	hasUnavailableGuilds,
	inlineDmChannelIds,
	pathname,
	selectedOrganizedItemIndex,
	visibility,
}: CreateGuildRailSkeletonLayoutRequest): Omit<RememberedSkeletonGuildRailLayout, 'scrollTopPx'> {
	const projectedItems: Array<RememberedSkeletonGuildRailItem> = [];
	for (const token of guildRailItemProjection.split(';')) {
		if (token === '') {
			continue;
		}
		if (token.startsWith('g')) {
			if (token.length !== 2) {
				throw new Error(`Invalid guild projection token: ${token}`);
			}
			projectedItems.push(
				Object.freeze({
					kind: SkeletonGuildRailItemKind.GUILD,
					indicator: parseGuildRailIndicatorToken(token.slice(1)),
				}),
			);
			continue;
		}
		if (token.startsWith('c')) {
			const childCount = Number(token.slice(1, 2));
			const showIconWhenCollapsed = token.slice(2, 3) === '1';
			if (
				token.length !== 4 ||
				!Number.isSafeInteger(childCount) ||
				childCount < 0 ||
				childCount > SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT ||
				(token.slice(2, 3) !== '0' && token.slice(2, 3) !== '1')
			) {
				throw new Error(`Invalid collapsed guild folder projection token: ${token}`);
			}
			projectedItems.push(
				Object.freeze({
					kind: SkeletonGuildRailItemKind.COLLAPSED_FOLDER,
					indicator: parseGuildRailIndicatorToken(token.slice(3)),
					childCount,
					showIconWhenCollapsed,
				}),
			);
			continue;
		}
		if (!token.startsWith('e')) {
			throw new Error(`Invalid guild rail item projection token: ${token}`);
		}
		const childTokens = token.slice(1);
		if (childTokens.length > SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT - 1) {
			throw new Error(`Invalid expanded guild folder projection token: ${token}`);
		}
		const childIndicators: Array<SkeletonGuildRailItemIndicator> = [];
		let selectedChildIndex = SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
		for (let childIndex = 0; childIndex < childTokens.length; childIndex += 1) {
			const childToken = childTokens[childIndex];
			if (childToken !== childToken.toLowerCase()) {
				selectedChildIndex = childIndex;
			}
			childIndicators.push(parseGuildRailIndicatorToken(childToken.toLowerCase()));
		}
		projectedItems.push(
			Object.freeze({
				kind: SkeletonGuildRailItemKind.EXPANDED_FOLDER,
				indicator: SkeletonGuildRailItemIndicator.NONE,
				childCount: childTokens.length,
				childIndicators: Object.freeze(childIndicators),
				selectedChildIndex,
			}),
		);
	}
	let selectedItemIndex = SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
	if (selectedOrganizedItemIndex >= 0 && selectedOrganizedItemIndex < projectedItems.length) {
		selectedItemIndex = selectedOrganizedItemIndex;
	}
	return Object.freeze({
		inlineDmRowCount: inlineDmChannelIds.length,
		inlineDmUnreadFlags: Object.freeze(
			inlineDmChannelIds.map((channelId) => resolveDMListItemUnreadState(channelId).shouldShowUnreadIndicator),
		),
		selectedInlineDmRowIndex: inlineDmChannelIds.findIndex((channelId) =>
			isSelectedPath(pathname, Routes.dmChannel(channelId)),
		),
		outageVisible: hasUnavailableGuilds,
		fluxerVisible: visibility.fluxerVisible,
		favoritesVisible: visibility.favoritesVisible,
		discoveryVisible: visibility.discoveryVisible,
		addGuildVisible: visibility.addGuildVisible,
		downloadVisible: visibility.downloadVisible,
		helpVisible: visibility.helpVisible,
		selectedItemIndex,
		organizedItems: Object.freeze(projectedItems),
	});
}

const GuildList = observer(() => {
	const {i18n} = useLingui();
	const [activeDragItem, setActiveDragItem] = useState<GuildDragItem | null>(null);
	const isDragging = activeDragItem != null;
	const guildNavigationVisibility = useGuildNavigationVisibility();
	const organizedItems = GuildListState.getOrganizedGuildList();
	const guildNavigationIndexes = useMemo(() => buildGuildNavigationIndexMap(organizedItems), [organizedItems]);
	const unavailableCount = GuildAvailability.totalUnavailableGuilds;
	const privateReadStateVersion = ReadStates.privateChannelVersion;
	const dmChannels = Channels.dmChannels;
	const unreadDMChannels = useMemo(() => getUnreadDMChannels(dmChannels), [dmChannels, privateReadStateVersion]);
	const unreadDMChannelIdSet = useMemo(() => {
		const ids = new Set<string>();
		for (let index = 0; index < unreadDMChannels.length; index++) {
			ids.add(unreadDMChannels[index].id);
		}
		return ids;
	}, [unreadDMChannels]);
	const unreadDMChannelIdSetRef = useRef<ReadonlySet<string>>(unreadDMChannelIdSet);
	unreadDMChannelIdSetRef.current = unreadDMChannelIdSet;
	const [hoveredInlineDMChannelId, setHoveredInlineDMChannelId] = useState<string | null>(null);
	const orderedUnreadDMChannels = useHoverDeferredOrderedItems({
		items: unreadDMChannels,
		getKey: getChannelId,
		isHoveringDynamicItem: hoveredInlineDMChannelId != null,
		releaseToken: privateReadStateVersion,
	});
	const scrollRef = useRef<ScrollerHandle | null>(null);
	const pendingScrollTopRef = useRef<number | null>(null);
	const armedScrollTopRef = useRef<number | null>(null);
	const scrollPersistTimerRef = useRef<number | null>(null);
	const location = useLocation();
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	const [visibleUnavailableCount, setVisibleUnavailableCount] = useState(unavailableCount);
	const unavailableIndicatorHideTimer = useRef<NodeJS.Timeout | null>(null);
	const hasUnavailableGuilds = visibleUnavailableCount > 0;
	const scrollTargetRegistry = useFloatingUnreadTargetRegistry();
	const scrollToMountedTargetFrameRef = useRef<number | null>(null);
	const getGuildScrollContainer = useCallback(() => resolveGuildListScrollerNode(scrollRef.current), []);
	useDragAutoScroll({active: isDragging, getScrollElement: getGuildScrollContainer});
	const scrollGuildListByWheel = useCallback((event: WheelEvent | React.WheelEvent<HTMLDivElement>) => {
		const scrollNode = resolveGuildListScrollerNode(scrollRef.current);
		if (scrollNode == null) return false;
		const maxScrollTop = Math.max(0, scrollNode.scrollHeight - scrollNode.clientHeight);
		if (maxScrollTop === 0) return false;
		const deltaY = getWheelScrollDeltaY(event, scrollNode.clientHeight);
		if (deltaY === 0) return false;
		const scrollTop = scrollNode.scrollTop;
		const nextScrollTop = Math.min(maxScrollTop, Math.max(0, scrollTop + deltaY));
		if (nextScrollTop === scrollTop) return false;
		scrollNode.scrollTop = nextScrollTop;
		return true;
	}, []);
	const [visibleDMChannels, setVisibleDMChannels] = useState<ReadonlyArray<Channel>>(orderedUnreadDMChannels);
	const directMessagesDisabled = RuntimeConfig.directMessagesDisabled;
	const pinnedCallChannel = resolvePinnedCallChannel(directMessagesDisabled);
	const inlineDmsCollapsed = SidebarPreferences.inlineDmsCollapsed;
	let baseDMChannels: ReadonlyArray<Channel>;
	if (inlineDmsCollapsed || directMessagesDisabled) {
		baseDMChannels = EMPTY_CHANNELS;
	} else {
		baseDMChannels = visibleDMChannels;
	}
	const filteredDMChannels = useMemo(() => {
		if (pinnedCallChannel != null) {
			return baseDMChannels.filter((channel) => channel.id !== pinnedCallChannel.id);
		}
		return baseDMChannels;
	}, [baseDMChannels, pinnedCallChannel]);
	const targetDMListRows = useMemo<Array<DMListRow>>(() => {
		const rows: Array<DMListRow> = [];
		if (pinnedCallChannel != null) {
			rows.push({type: 'channel', channel: pinnedCallChannel, voiceCallActive: true, pendingRemoval: false});
		}
		for (let index = 0; index < filteredDMChannels.length; index++) {
			const channel = filteredDMChannels[index];
			rows.push({
				type: 'channel',
				channel,
				voiceCallActive: false,
				pendingRemoval: !unreadDMChannelIdSet.has(channel.id),
			});
		}
		return rows;
	}, [filteredDMChannels, pinnedCallChannel, unreadDMChannelIdSet]);
	const visibleDMListRows = useFrameBatchedDMListRows(targetDMListRows);
	const shouldRenderGuildListItems = hasUnavailableGuilds || organizedItems.length > 0;
	const initialGuildListScrollTop = useMemo(() => {
		const liveScrollTop = untracked(() => Dimension.getGuildListDimensions().scrollTop);
		if (liveScrollTop > 0) {
			return liveScrollTop;
		}
		return Math.round(getRememberedSkeletonGuildRailScrollTopPx() * getRemScaleForDocument(document));
	}, []);
	const skeletonGuildRailChrome = useMemo(
		() => ({
			hasUnavailableGuilds,
			inlineDmChannelIds: visibleDMListRows.map((row) => row.channel.id),
			pathname: location.pathname,
			visibility: guildNavigationVisibility,
		}),
		[hasUnavailableGuilds, visibleDMListRows, location.pathname, guildNavigationVisibility],
	);
	useEffect(() => {
		let reportedProjection: string | null = null;
		const disposeSkeletonProjection = autorun(() => {
			const projectedOrganizedItems = GuildListState.getOrganizedGuildList();
			const guildRailItemProjection = createGuildRailItemProjection(
				projectedOrganizedItems,
				skeletonGuildRailChrome.pathname,
			);
			const skeletonGuildRailLayout = createGuildRailSkeletonLayout({
				guildRailItemProjection,
				hasUnavailableGuilds: skeletonGuildRailChrome.hasUnavailableGuilds,
				inlineDmChannelIds: skeletonGuildRailChrome.inlineDmChannelIds,
				pathname: skeletonGuildRailChrome.pathname,
				selectedOrganizedItemIndex: projectedOrganizedItems.findIndex((item) =>
					guildNavigationItemContainsSelectedGuild({item, pathname: skeletonGuildRailChrome.pathname}),
				),
				visibility: skeletonGuildRailChrome.visibility,
			});
			const projection = [
				guildRailItemProjection,
				skeletonGuildRailLayout.inlineDmUnreadFlags.join(''),
				skeletonGuildRailLayout.selectedInlineDmRowIndex,
			].join('|');
			if (projection === reportedProjection) return;
			reportedProjection = projection;
			untracked(() => reportSkeletonGuildRailLayout(skeletonGuildRailLayout));
		});
		return () => disposeSkeletonProjection();
	}, [skeletonGuildRailChrome]);
	const selectedGuildIndex = useMemo(
		() => getSelectedGuildNavigationIndex(location.pathname, guildNavigationIndexes),
		[location.pathname, guildNavigationIndexes],
	);
	const removalTimers = useRef<Map<string, number>>(new Map());
	const dmVisibilityControllerRef = useRef<DMChannelVisibilityController | null>(null);
	let dmRemovalDelayMs = DM_LIST_REMOVAL_DELAY_MS;
	if (Accessibility.useReducedMotion) dmRemovalDelayMs = 0;
	const handleInlineDMHoverStart = useCallback((channelId: string) => {
		setHoveredInlineDMChannelId(channelId);
	}, []);
	const handleInlineDMHoverEnd = useCallback((channelId: string) => {
		setHoveredInlineDMChannelId((currentChannelId) =>
			clearHoveredChannelId({currentChannelId, endedChannelId: channelId}),
		);
	}, []);
	const handleInlineDMRemovalAnimationComplete = useCallback((channelId: string) => {
		const controller = dmVisibilityControllerRef.current;
		if (controller != null) controller.completeRemoval(channelId);
	}, []);
	const guildListNavigationRef = useRovingFocusList<HTMLElement>({
		focusableSelector: GUILD_LIST_FOCUSABLE_SELECTOR,
		orientation: AxisOrientation.VERTICAL,
		loop: true,
		enabled: keyboardModeEnabled,
		restoreFocusOnWindowFocus: false,
		manageTabIndex: true,
	});
	useEffect(() => {
		const controller = new DMChannelVisibilityController({
			removalDelayMs: dmRemovalDelayMs,
			removalTimers,
			orderedUnreadChannels: orderedUnreadDMChannels,
			setVisibleChannels: setVisibleDMChannels,
			unreadChannels: unreadDMChannels,
			unreadIdsRef: unreadDMChannelIdSetRef,
		});
		dmVisibilityControllerRef.current = controller;
		controller.synchronize();
	}, [dmRemovalDelayMs, orderedUnreadDMChannels, unreadDMChannels]);
	useEffect(() => {
		if (hoveredInlineDMChannelId == null) return;
		const hoveredRow = visibleDMListRows.find((row) => row.channel.id === hoveredInlineDMChannelId);
		if (hoveredRow == null || hoveredRow.pendingRemoval) setHoveredInlineDMChannelId(null);
	}, [hoveredInlineDMChannelId, visibleDMListRows]);
	useEffect(() => {
		if (unavailableCount > 0) {
			if (unavailableIndicatorHideTimer.current) {
				clearTimeout(unavailableIndicatorHideTimer.current);
				unavailableIndicatorHideTimer.current = null;
			}
			setVisibleUnavailableCount(unavailableCount);
			return;
		}
		if (unavailableIndicatorHideTimer.current) return;
		unavailableIndicatorHideTimer.current = setTimeout(() => {
			unavailableIndicatorHideTimer.current = null;
			setVisibleUnavailableCount(0);
		}, UNAVAILABLE_INDICATOR_DEBOUNCE_MS);
	}, [unavailableCount]);
	useEffect(() => {
		return () => {
			dmVisibilityControllerRef.current = null;
			if (unavailableIndicatorHideTimer.current) {
				clearTimeout(unavailableIndicatorHideTimer.current);
				unavailableIndicatorHideTimer.current = null;
			}
			removalTimers.current.forEach((timer) => window.clearTimeout(timer));
			removalTimers.current.clear();
		};
	}, []);
	const handleGuildDrop = useCallback(
		(item: GuildDragItem, result: GuildDropResult) => {
			const guildFolders = applyGuildFolderDrop({item, organizedItems, result});
			if (guildFolders == null) return;
			UserSettingsCommands.update({guildFolders});
		},
		[organizedItems],
	);
	const handleDragStateChange = useCallback((item: GuildDragItem | null) => {
		setActiveDragItem(item);
	}, []);
	const guildNavigationRows = useMemo(
		() =>
			createGuildNavigationRows({
				hasUnavailableGuilds,
				isDragging,
				organizedItems,
				shouldRenderGuildListItems,
				visibleDMListRows,
				visibility: guildNavigationVisibility,
			}),
		[
			hasUnavailableGuilds,
			isDragging,
			organizedItems,
			shouldRenderGuildListItems,
			visibleDMListRows,
			guildNavigationVisibility,
		],
	);
	const selectedGuildNavigationKey = useMemo(
		() =>
			resolveSelectedGuildNavigationKey({
				organizedItems,
				pathname: location.pathname,
				visibleDMListRows,
				visibility: guildNavigationVisibility,
			}),
		[organizedItems, location.pathname, visibleDMListRows, guildNavigationVisibility],
	);
	const {
		navigationRef: guildNavigationListRef,
		onKeyDownCapture: handleGuildNavigationKeyDownCapture,
		onFocusCapture: handleGuildNavigationFocusCapture,
		onBlurCapture: handleGuildNavigationBlurCapture,
		getRowListPosition: getGuildRowListPosition,
		getRowBounds: getGuildRowBounds,
		layoutRevision: guildLayoutRevision,
		scrollToKey: scrollToGuildKey,
	} = useNavigationList({
		scrollerRef: scrollRef,
		rows: guildNavigationRows,
		focusableSelector: GUILD_LIST_FOCUSABLE_SELECTOR,
		keyboardNavigationEnabled: keyboardModeEnabled,
		preferredActiveKey: selectedGuildNavigationKey,
		resolveMissingActiveKey: null,
	});
	const mergedGuildNavigationRef = useMergeRefs<HTMLElement>([guildListNavigationRef, guildNavigationListRef]);
	const previousSelectedGuildNavigationKeyRef = useRef(selectedGuildNavigationKey);
	const previousGuildNavigationPathnameRef = useRef(location.pathname);
	useEffect(() => {
		const previousSelectedGuildNavigationKey = previousSelectedGuildNavigationKeyRef.current;
		const previousGuildNavigationPathname = previousGuildNavigationPathnameRef.current;
		previousSelectedGuildNavigationKeyRef.current = selectedGuildNavigationKey;
		previousGuildNavigationPathnameRef.current = location.pathname;
		const selectedFromRail = consumeDirectSelection(DirectSelectionSurface.GUILD_RAIL);
		if (selectedGuildNavigationKey == null || selectedGuildNavigationKey === previousSelectedGuildNavigationKey) {
			return;
		}
		if (location.pathname === previousGuildNavigationPathname) {
			return;
		}
		if (selectedFromRail) {
			return;
		}
		scrollToGuildKey(selectedGuildNavigationKey, NavigationAlignment.AUTO);
	}, [location.pathname, scrollToGuildKey, selectedGuildNavigationKey]);
	const persistGuildListScrollTop = useCallback(() => {
		const pendingScrollTop = pendingScrollTopRef.current;
		if (pendingScrollTop == null) return;
		pendingScrollTopRef.current = null;
		armedScrollTopRef.current = null;
		DimensionCommands.updateGuildListScroll(pendingScrollTop);
		reportSkeletonGuildRailScrollTop(pendingScrollTop);
	}, []);
	const armGuildListScrollPersist = useCallback(
		function armGuildListScrollPersistTimer(): void {
			if (scrollPersistTimerRef.current != null) return;
			armedScrollTopRef.current = pendingScrollTopRef.current;
			scrollPersistTimerRef.current = window.setTimeout(() => {
				scrollPersistTimerRef.current = null;
				if (pendingScrollTopRef.current !== armedScrollTopRef.current) {
					armGuildListScrollPersistTimer();
					return;
				}
				persistGuildListScrollTop();
			}, GUILD_LIST_SCROLL_PERSIST_IDLE_MS);
		},
		[persistGuildListScrollTop],
	);
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			pendingScrollTopRef.current = event.currentTarget.scrollTop;
			armGuildListScrollPersist();
		},
		[armGuildListScrollPersist],
	);
	const handleWheel = useCallback(
		(event: React.WheelEvent<HTMLDivElement>) => {
			if (!isDragging || event.defaultPrevented) return;
			if (!scrollGuildListByWheel(event)) return;
			if (event.cancelable) {
				event.preventDefault();
			}
		},
		[isDragging, scrollGuildListByWheel],
	);
	useEffect(() => {
		if (!isDragging) return;
		const scrollNode = getGuildScrollContainer();
		if (scrollNode == null) return;
		const ownerWindow = scrollNode.ownerDocument.defaultView;
		if (ownerWindow == null) return;
		const handleWindowWheel = (event: WheelEvent) => {
			const currentScrollNode = getGuildScrollContainer();
			if (currentScrollNode == null || !isWheelEventOverElement(event, currentScrollNode)) return;
			if (!scrollGuildListByWheel(event)) return;
			if (event.cancelable) {
				event.preventDefault();
			}
		};
		ownerWindow.addEventListener('wheel', handleWindowWheel, {capture: true, passive: false});
		return () => {
			ownerWindow.removeEventListener('wheel', handleWindowWheel, true);
		};
	}, [getGuildScrollContainer, isDragging, scrollGuildListByWheel]);
	const expandedGuildFolderKey = GuildFolderExpanded.expandedFolderIds.join(',');
	const targetGuildRows = useMemo(
		() => createGuildTargetRows({organizedItems, visibleDMListRows}),
		[organizedItems, visibleDMListRows, expandedGuildFolderKey],
	);
	const getGuildTargetBounds = useCallback(
		(targets: ReadonlyArray<FloatingUnreadTarget>) => {
			const scrollNode = resolveGuildListScrollerNode(scrollRef.current);
			return resolveGuildTargetBounds({
				getRowBounds: getGuildRowBounds,
				scrollTargetRegistry,
				scrollNode,
				targetRows: targetGuildRows,
				targets,
			});
		},
		[getGuildRowBounds, scrollTargetRegistry, targetGuildRows],
	);
	const scrollToGuildTarget = useCallback(
		(targetId: string, direction: VerticalEdge) => {
			const alignment = resolveNavigationAlignment(direction);
			const scrollToMountedTarget = () => {
				const node = scrollTargetRegistry.getTargetNode(targetId);
				const scroller = scrollRef.current;
				if (node == null || scroller == null) return false;
				scroller.scrollIntoViewNode({
					node,
					alignment,
					animate: Accessibility.useSmoothScrolling,
				});
				return true;
			};
			if (scrollToMountedTarget()) return true;
			const targetRow = targetGuildRows.get(targetId);
			if (targetRow == null || !scrollToGuildKey(targetRow.rowKey, alignment)) return false;
			if (scrollToMountedTargetFrameRef.current != null) {
				window.cancelAnimationFrame(scrollToMountedTargetFrameRef.current);
			}
			scrollToMountedTargetFrameRef.current = window.requestAnimationFrame(() => {
				scrollToMountedTargetFrameRef.current = null;
				scrollToMountedTarget();
			});
			return true;
		},
		[scrollTargetRegistry, scrollToGuildKey, targetGuildRows],
	);
	useEffect(() => {
		return registerSkeletonLayoutMemoryPreFlush(persistGuildListScrollTop);
	}, [persistGuildListScrollTop]);
	useEffect(() => {
		return () => {
			if (scrollToMountedTargetFrameRef.current != null) {
				window.cancelAnimationFrame(scrollToMountedTargetFrameRef.current);
				scrollToMountedTargetFrameRef.current = null;
			}
			if (scrollPersistTimerRef.current != null) {
				window.clearTimeout(scrollPersistTimerRef.current);
				scrollPersistTimerRef.current = null;
			}
			persistGuildListScrollTop();
		};
	}, [persistGuildListScrollTop]);
	useEffect(() => {
		const scrollNode = resolveGuildListScrollerNode(scrollRef.current);
		if (initialGuildListScrollTop > 0 && scrollNode != null) {
			scrollNode.scrollTop = initialGuildListScrollTop;
			DimensionCommands.updateGuildListScroll(initialGuildListScrollTop);
		}
	}, [initialGuildListScrollTop]);
	const renderGuildNavigationRow = (row: GuildNavigationRow): React.ReactNode => {
		switch (row.kind) {
			case 'fluxer':
				return <FluxerButton data-flx="app.guilds-layout.render-guild-navigation-row.fluxer-button" />;
			case 'favorites':
				return <FavoritesButton data-flx="app.guilds-layout.render-guild-navigation-row.favorites-button" />;
			case 'dm': {
				const {channel, pendingRemoval, voiceCallActive} = row.row;
				return (
					<DMListAnimatedRow
						channel={channel}
						isLast={row.isLast}
						isSelected={isSelectedPath(location.pathname, Routes.dmChannel(channel.id))}
						pendingRemoval={pendingRemoval}
						reducedMotion={Accessibility.useReducedMotion}
						onHoverStart={handleInlineDMHoverStart}
						onHoverEnd={handleInlineDMHoverEnd}
						onRemovalAnimationComplete={handleInlineDMRemovalAnimationComplete}
						scrollTargetRef={scrollTargetRegistry.register(`dm-${channel.id}`)}
						voiceCallActive={voiceCallActive}
						data-flx="app.guilds-layout.render-guild-navigation-row.dm-list-animated-row"
					/>
				);
			}
			case 'divider':
				return (
					<GuildListDivider
						nextRowExtended={shouldRenderGuildListItems}
						data-flx="app.guilds-layout.render-guild-navigation-row.guild-list-divider"
					/>
				);
			case 'outage':
				return (
					<flx-app-guild-list-item-slot
						className={flxElementClassName(styles.guildListItemSlot)}
						data-flx="app.guilds-layout.render-guild-navigation-row.guild-list-item-slot"
					>
						<Tooltip
							position="right"
							type={'error'}
							maxWidth="xl"
							size="large"
							text={() =>
								plural(
									{count: visibleUnavailableCount},
									{
										one: '# community is temporarily unavailable due to a flux capacitor malfunction.',
										other: '# communities are temporarily unavailable due to a flux capacitor malfunction.',
									},
								)
							}
							data-flx="app.guilds-layout.render-guild-navigation-row.tooltip"
						>
							<flx-app-guild-list-unavailable
								className={flxElementClassName(styles.unavailableContainer)}
								data-flx="app.guilds-layout.render-guild-navigation-row.unavailable-container"
							>
								<flx-app-guild-list-unavailable-badge
									className={flxElementClassName(styles.unavailableBadge)}
									data-flx="app.guilds-layout.render-guild-navigation-row.unavailable-badge"
								>
									<ExclamationMarkIcon
										weight="regular"
										className={styles.unavailableIcon}
										data-flx="app.guilds-layout.render-guild-navigation-row.unavailable-icon"
									/>
								</flx-app-guild-list-unavailable-badge>
							</flx-app-guild-list-unavailable>
						</Tooltip>
					</flx-app-guild-list-item-slot>
				);
			case 'organized-item': {
				const {item} = row;
				if (item.type === 'folder') {
					const isFolderSelected = item.guilds.some((guild) =>
						isSelectedPath(location.pathname, Routes.guildChannel(guild.id)),
					);
					return (
						<flx-app-guild-list-item-slot
							className={flxElementClassName(styles.guildListItemSlot)}
							data-flx="app.guilds-layout.render-guild-navigation-row.guild-list-item-slot--2"
						>
							<GuildFolderItem
								folder={item.folder}
								guilds={item.guilds}
								isSelected={isFolderSelected}
								isSortingList={isDragging}
								onGuildDrop={handleGuildDrop}
								onDragStateChange={handleDragStateChange}
								guildNavigationIndexes={guildNavigationIndexes}
								selectedGuildIndex={selectedGuildIndex}
								registerScrollTarget={scrollTargetRegistry.register}
								data-flx="app.guilds-layout.render-guild-navigation-row.guild-folder-item"
							/>
						</flx-app-guild-list-item-slot>
					);
				}
				let guildIndex: number | null = null;
				const navigationIndex = guildNavigationIndexes.get(item.guild.id);
				if (navigationIndex != null) {
					guildIndex = navigationIndex;
				}
				return (
					<flx-app-guild-list-item-slot
						className={flxElementClassName(styles.guildListItemSlot)}
						data-flx="app.guilds-layout.render-guild-navigation-row.guild-list-item-slot--3"
					>
						<GuildListItem
							isSortingList={isDragging}
							guild={item.guild}
							isSelected={isSelectedPath(location.pathname, Routes.guildChannel(item.guild.id))}
							guildIndex={guildIndex}
							selectedGuildIndex={selectedGuildIndex}
							onGuildDrop={handleGuildDrop}
							onDragStateChange={handleDragStateChange}
							disableDrag={false}
							insideFolderId={null}
							isLastInsideFolder={false}
							scrollTargetRef={scrollTargetRegistry.register(`guild-${item.guild.id}`)}
							data-flx="app.guilds-layout.render-guild-navigation-row.guild-list-item"
						/>
					</flx-app-guild-list-item-slot>
				);
			}
			case 'bottom-drop-zone':
				return (
					<flx-app-guild-list-drop-zone
						style={{position: 'relative', display: 'block', height: GUILD_BOTTOM_DROP_ZONE_HEIGHT}}
						data-flx="app.guilds-layout.render-guild-navigation-row.flx-app-guild-list-drop-zone"
					>
						<BottomDropZone
							onGuildDrop={handleGuildDrop}
							lastItemKey={getOrganizedItemKey(organizedItems[organizedItems.length - 1])}
							lastItemIsFolder={organizedItems[organizedItems.length - 1].type === 'folder'}
							isDragging={isDragging}
							data-flx="app.guilds-layout.render-guild-navigation-row.bottom-drop-zone"
						/>
					</flx-app-guild-list-drop-zone>
				);
			case 'discovery':
				return <DiscoveryButton data-flx="app.guilds-layout.render-guild-navigation-row.discovery-button" />;
			case 'add-guild':
				return <AddGuildButton data-flx="app.guilds-layout.render-guild-navigation-row.add-guild-button" />;
			case 'download':
				return <DownloadButton data-flx="app.guilds-layout.render-guild-navigation-row.download-button" />;
			case 'help':
				return <HelpButton data-flx="app.guilds-layout.render-guild-navigation-row.help-button" />;
		}
	};
	const renderGuildNavigationListItem = (row: GuildNavigationRow, index: number): React.ReactNode => {
		const indexedNextRow = guildNavigationRows[index + 1];
		let nextRow: GuildNavigationRow | null = null;
		if (indexedNextRow != null) {
			nextRow = indexedNextRow;
		}
		let style = GUILD_ROW_STYLE;
		if (hasGapAfterGuildNavigationRow(row, nextRow)) {
			style = GUILD_ROW_STYLE_WITH_GAP;
		}
		const listPosition = getGuildRowListPosition(index);
		if (listPosition == null) {
			return (
				<div
					key={row.key}
					data-navigation-index={index}
					role="presentation"
					style={style}
					data-flx="app.guilds-layout.render-guild-navigation-list-item.presentation"
				>
					{renderGuildNavigationRow(row)}
				</div>
			);
		}
		return (
			<div
				key={row.key}
				data-navigation-index={index}
				role="listitem"
				aria-posinset={listPosition.position}
				aria-setsize={listPosition.setSize}
				style={style}
				data-flx="app.guilds-layout.render-guild-navigation-list-item.listitem"
			>
				{renderGuildNavigationRow(row)}
			</div>
		);
	};
	return (
		<nav
			className={styles.guildListScrollerWrapper}
			aria-label={i18n._(PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR)}
			data-flx="app.guilds-layout.guild-list.guild-list-scroller-wrapper"
		>
			<Scroller
				ref={scrollRef}
				className={styles.guildListScrollContainer}
				showTrack={false}
				onScroll={handleScroll}
				onWheel={handleWheel}
				key={GUILD_LIST_SCROLLER_IDENTITY}
				data-flx="app.guilds-layout.guild-list.guild-list-scroll-container"
			>
				<div
					className={styles.guildListContent}
					ref={mergedGuildNavigationRef}
					onKeyDownCapture={handleGuildNavigationKeyDownCapture}
					onFocusCapture={handleGuildNavigationFocusCapture}
					onBlurCapture={handleGuildNavigationBlurCapture}
					role="list"
					data-flx="app.guilds-layout.guild-list.guild-list-content"
				>
					{guildNavigationRows.map(renderGuildNavigationListItem)}
				</div>
			</Scroller>
			<GuildScrollIndicators
				dmListRows={visibleDMListRows}
				getTargetBounds={getGuildTargetBounds}
				getTargetNode={scrollTargetRegistry.getTargetNode}
				label={i18n._(NEW_DESCRIPTOR)}
				measurementRevision={guildLayoutRevision}
				organizedItems={organizedItems}
				scrollToTarget={scrollToGuildTarget}
				scrollerRef={scrollRef}
				data-flx="app.guilds-layout.guild-list.guild-scroll-indicators"
			/>
		</nav>
	);
});
export const GuildsLayout = observer(({children}: {children: React.ReactNode}) => {
	const mobileLayout = MobileLayout;
	const user = Users.currentUser;
	const location = useLocation();
	const isVoiceCallFullscreenActive = VoiceCallFullscreen.isActive;
	const shouldReserveUserAreaSpace = !!user && !mobileLayout.enabled && !isVoiceCallFullscreenActive;
	const layoutRef = useRef<HTMLDivElement | null>(null);
	const userAreaWrapperRef = useRef<HTMLDivElement | null>(null);
	const showGuildListOnMobile =
		!isVoiceCallFullscreenActive &&
		mobileLayout.enabled &&
		(location.pathname === Routes.ME ||
			Routes.isDiscoverRoute(location.pathname) ||
			(Routes.isChannelRoute(location.pathname) && location.pathname.split('/').length === 3));
	const showBottomNav =
		!isVoiceCallFullscreenActive &&
		mobileLayout.enabled &&
		(location.pathname === Routes.ME ||
			location.pathname === Routes.FAVORITES ||
			Routes.isDiscoverRoute(location.pathname) ||
			location.pathname === Routes.NOTIFICATIONS ||
			location.pathname === Routes.YOU ||
			(Routes.isGuildChannelRoute(location.pathname) && location.pathname.split('/').length === 3));
	const nagbarConditions = useNagbarConditions();
	const activeNagbars = useActiveNagbars(nagbarConditions);
	const skeletonNagbarRows = useMemo(() => activeNagbars.map(createSkeletonNagbarRow), [activeNagbars]);
	const skeletonNagbarRowKey = useMemo(
		() => skeletonNagbarRows.map((row) => `${row.tone}:${row.hasActions}:${row.dismissible}`).join(','),
		[skeletonNagbarRows],
	);
	const prevNagbarCount = useRef(activeNagbars.length);
	const isReady = Initialization.isReady;
	useEffect(() => {
		if (prevNagbarCount.current !== activeNagbars.length) {
			prevNagbarCount.current = activeNagbars.length;
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
	}, [activeNagbars.length]);
	useEffect(() => {
		if (isVoiceCallFullscreenActive) {
			return;
		}
		reportSkeletonNagbarLayout(skeletonNagbarRows);
	}, [skeletonNagbarRowKey, skeletonNagbarRows, isVoiceCallFullscreenActive]);
	useEffect(() => {
		const layoutElement = layoutRef.current;
		if (!layoutElement) return;
		const clearOverlayHeight = () => {
			layoutElement.style.removeProperty('--layout-user-area-overlay-height');
		};
		if (!shouldReserveUserAreaSpace) {
			clearOverlayHeight();
			return;
		}
		const userAreaWrapperElement = userAreaWrapperRef.current;
		if (!userAreaWrapperElement || typeof ResizeObserver === 'undefined') {
			clearOverlayHeight();
			return;
		}
		let currentOverlayHeight: number | null = null;
		const applyOverlayHeight = (height: number) => {
			const roundedHeight = Math.ceil(height);
			if (roundedHeight > 0) {
				if (currentOverlayHeight === roundedHeight) return;
				currentOverlayHeight = roundedHeight;
				layoutElement.style.setProperty('--layout-user-area-overlay-height', `${roundedHeight}px`);
			} else {
				currentOverlayHeight = null;
				clearOverlayHeight();
			}
		};
		applyOverlayHeight(userAreaWrapperElement.getBoundingClientRect().height);
		const observer = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (!entry) return;
			applyOverlayHeight(getResizeObserverEntryBlockSize(entry));
		});
		observer.observe(userAreaWrapperElement);
		return () => {
			observer.disconnect();
			clearOverlayHeight();
		};
	}, [shouldReserveUserAreaSpace]);
	const THIRTY_MINUTES_MS = 30 * 60 * 1000;
	useEffect(() => {
		if (!isReady) return;
		if (!user) return;
		if (Nagbar.claimAccountModalShownThisSession) return;
		if (user.isClaimed()) return;
		const accountAgeMs = SnowflakeUtils.age(user.id);
		if (accountAgeMs < THIRTY_MINUTES_MS) return;
		Nagbar.markClaimAccountModalShown();
		openClaimAccountModal();
	}, [isReady, user, location.pathname]);
	useEffect(() => {
		if (!isReady) return;
		if (!user) return;
		if (RuntimeConfig.isSelfHosted()) return;
		const latestEntry = WHATS_NEW_ENTRIES[0];
		if (!latestEntry) return;
		if (!WhatsNew.shouldShow(latestEntry.id, latestEntry.date, user.createdAt)) return;
		openWhatsNewModal();
	}, [isReady, user]);
	useEffect(() => {
		if (!isReady) return;
		if (!user) return;
		if (!MacPermissions.shouldShowOnboarding) return;
		if (MacPermissions.onboardingOpenedThisSession) return;
		MacPermissions.markOnboardingOpenedThisSession();
		openMacPermissionsModal();
	}, [isReady, user]);
	const shouldShowSidebarDivider = !mobileLayout.enabled;
	return (
		<div
			ref={layoutRef}
			className={clsx(
				styles.guildsLayoutContainer,
				isVoiceCallFullscreenActive && styles.guildsLayoutFullscreen,
				mobileLayout.enabled && !showGuildListOnMobile && styles.guildsLayoutContainerMobile,
				shouldReserveUserAreaSpace && styles.guildsLayoutReserveSpace,
				showBottomNav && styles.guildsLayoutReserveMobileBottomNav,
			)}
			style={resolveGuildsLayoutSidebarStyle(mobileLayout.enabled, SidebarWidth.cssValue)}
			data-flx="app.guilds-layout.guilds-layout"
		>
			{!isVoiceCallFullscreenActive && (!mobileLayout.enabled || showGuildListOnMobile) && (
				<GuildList key="guild-list" data-flx="app.guilds-layout.guild-list" />
			)}
			<div
				key="content"
				className={clsx(
					styles.contentContainer,
					isVoiceCallFullscreenActive && styles.contentContainerFullscreen,
					mobileLayout.enabled && !showGuildListOnMobile && styles.contentContainerMobile,
				)}
				data-flx="app.guilds-layout.content-container"
			>
				<TopNagbarContext.Provider value={activeNagbars.length}>
					<OutlineFrame
						className={clsx(styles.outlineFrame, isVoiceCallFullscreenActive && styles.outlineFrameFullscreen)}
						sidebarDivider={!isVoiceCallFullscreenActive && shouldShowSidebarDivider}
						sidebarResizeHandle={!isVoiceCallFullscreenActive && shouldShowSidebarDivider}
						nagbar={
							!isVoiceCallFullscreenActive && activeNagbars.length > 0 ? (
								<div className={styles.nagbarStack} data-flx="app.guilds-layout.nagbar-stack">
									<NagbarContainer nagbars={activeNagbars} data-flx="app.guilds-layout.nagbar-container" />
								</div>
							) : null
						}
						data-flx="app.guilds-layout.outline-frame"
					>
						<div
							id="main-content"
							className={clsx(styles.contentInner, isVoiceCallFullscreenActive && styles.contentInnerFullscreen)}
							tabIndex={-1}
							data-flx="app.guilds-layout.main-content"
						>
							{children}
						</div>
					</OutlineFrame>
				</TopNagbarContext.Provider>
			</div>
			{!isVoiceCallFullscreenActive && !mobileLayout.enabled && user && (
				<div ref={userAreaWrapperRef} className={styles.userAreaWrapper} data-flx="app.guilds-layout.user-area-wrapper">
					<UserArea user={user} data-flx="app.guilds-layout.user-area" />
				</div>
			)}
		</div>
	);
});
