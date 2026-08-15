// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useActiveNagbars, useNagbarConditions} from '@app/features/app/components/layout/app_layout/AppLayoutHooks';
import {NagbarContainer} from '@app/features/app/components/layout/app_layout/NagbarContainer';
import {TopNagbarContext} from '@app/features/app/components/layout/app_layout/TopNagbarContext';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {
	DM_LIST_REMOVAL_DELAY_MS,
	DMListAnimatedRow,
	type DMListRow,
	useFrameBatchedDMListRows,
} from '@app/features/app/components/layout/guilds_layout/GuildListDMAnimation';
import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import {ScrollIndicatorOverlay} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import {AddGuildButton} from '@app/features/app/components/layout/sidebar_nav/AddGuildButton';
import {DiscoveryButton} from '@app/features/app/components/layout/sidebar_nav/DiscoveryButton';
import {DownloadButton} from '@app/features/app/components/layout/sidebar_nav/DownloadButton';
import {FavoritesButton} from '@app/features/app/components/layout/sidebar_nav/FavoritesButton';
import {FluxerButton} from '@app/features/app/components/layout/sidebar_nav/FluxerButton';
import {GuildFolderItem} from '@app/features/app/components/layout/sidebar_nav/GuildFolderItem';
import {GuildListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListItem';
import {HelpButton} from '@app/features/app/components/layout/sidebar_nav/HelpButton';
import {DND_TYPES, type GuildDragItem, type GuildDropResult} from '@app/features/app/components/layout/types/DndTypes';
import {UserArea} from '@app/features/app/components/layout/UserArea';
import {WHATS_NEW_ENTRIES} from '@app/features/app/components/whats_new/WhatsNewEntries';
import {openWhatsNewModal} from '@app/features/app/components/whats_new/WhatsNewModal';
import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import Initialization from '@app/features/app/state/Initialization';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildListState, {type OrganizedItem} from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openMacPermissionsModal} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Platform} from '@app/features/platform/types/Platform';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as DimensionCommands from '@app/features/ui/commands/DimensionCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {useHoverDeferredOrderedItems} from '@app/features/ui/hooks/UseHoverDeferredOrderedItems';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import {useListScrollAnchor} from '@app/features/ui/hooks/useScrollAnchor';
import Dimension from '@app/features/ui/state/Dimension';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Nagbar from '@app/features/ui/state/Nagbar';
import SidebarPreferences from '@app/features/ui/state/SidebarPreferences';
import SidebarWidth from '@app/features/ui/state/SidebarWidth';
import WhatsNew from '@app/features/ui/state/WhatsNew';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings, {type GuildFolder} from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallState from '@app/features/voice/state/CallState';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {DEFAULT_GUILD_FOLDER_ICON, UNCATEGORIZED_FOLDER_ID} from '@fluxer/constants/src/UserConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ExclamationMarkIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';
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

const getUnreadDMChannels = (dmChannels: ReadonlyArray<Channel>): Array<Channel> => {
	const out: Array<Channel> = [];
	for (let i = 0; i < dmChannels.length; i++) {
		if (ReadStates.hasUnread(dmChannels[i].id)) out.push(dmChannels[i]);
	}
	return out;
};

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

function getGuildListAnchorKeyForItem(item: OrganizedItem): string {
	if (item.type === 'folder') {
		return `folder-${item.folder.id}`;
	}
	return `guild-${item.guild.id}`;
}

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
): number | undefined {
	for (const [guildId, guildIndex] of guildNavigationIndexes) {
		if (isSelectedPath(pathname, Routes.guildChannel(guildId))) {
			return guildIndex;
		}
	}
	return undefined;
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

interface BottomDropZoneProps {
	onGuildDrop: (item: GuildDragItem, result: GuildDropResult) => void;
	lastItemKey: string;
	lastItemIsFolder: boolean;
	isDragging: boolean;
}

function BottomDropZone({onGuildDrop, lastItemKey, lastItemIsFolder, isDragging}: BottomDropZoneProps) {
	const [{isOver, canDrop}, dropRef] = useDrop(
		() => ({
			accept: [DND_TYPES.GUILD_ITEM, DND_TYPES.GUILD_FOLDER],
			canDrop: (item: GuildDragItem) => item.id !== lastItemKey,
			drop: (item: GuildDragItem, monitor): GuildDropResult | undefined => {
				if (!monitor.canDrop()) return;
				const result: GuildDropResult = {
					targetId: lastItemKey,
					position: 'after',
					targetIsFolder: lastItemIsFolder,
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
		(node: HTMLDivElement | null) => {
			dropRef(node);
		},
		[dropRef],
	);
	if (!isDragging) return null;
	return (
		<div
			ref={setRef}
			className={clsx(
				styles.guildListDropZone,
				styles.guildListDropZoneBottom,
				isDragging && styles.guildListDropZoneEnabled,
				isActive && styles.guildListDropZoneActive,
			)}
			data-flx="app.guilds-layout.bottom-drop-zone.guild-list-drop-zone"
		/>
	);
}

const GuildList = observer(() => {
	const {i18n} = useLingui();
	const [isDragging, setIsDragging] = useState(false);
	const guilds = GuildListState.guilds;
	const organizedItems = GuildListState.getOrganizedGuildList();
	const guildNavigationIndexes = useMemo(() => buildGuildNavigationIndexMap(organizedItems), [organizedItems]);
	const unavailableGuilds = GuildAvailability.unavailableGuilds;
	const unavailableCount = unavailableGuilds.size;
	const readVersion = ReadStates.version;
	const dmChannels = Channels.dmChannels;
	const unreadDMChannels = useMemo(() => getUnreadDMChannels(dmChannels), [dmChannels, readVersion]);
	const unreadDMChannelIdSet = useMemo(() => {
		const ids = new Set<string>();
		for (let index = 0; index < unreadDMChannels.length; index++) {
			ids.add(unreadDMChannels[index].id);
		}
		return ids;
	}, [unreadDMChannels]);
	const unreadDMChannelIdSetRef = useRef<ReadonlySet<string>>(unreadDMChannelIdSet);
	unreadDMChannelIdSetRef.current = unreadDMChannelIdSet;
	const unreadDMChannelIds = useMemo(() => unreadDMChannels.map(getChannelId).join(','), [unreadDMChannels]);
	const [hoveredInlineDMChannelId, setHoveredInlineDMChannelId] = useState<string | null>(null);
	const orderedUnreadDMChannels = useHoverDeferredOrderedItems({
		items: unreadDMChannels,
		getKey: getChannelId,
		isHoveringDynamicItem: hoveredInlineDMChannelId != null,
		releaseToken: readVersion,
	});
	const scrollRef = useRef<ScrollerHandle>(null);
	const pendingScrollTopRef = useRef<number | null>(null);
	const scrollPersistRafRef = useRef<number | null>(null);
	const location = useLocation();
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	const [visibleUnavailableCount, setVisibleUnavailableCount] = useState(unavailableCount);
	const unavailableIndicatorHideTimer = useRef<NodeJS.Timeout | null>(null);
	const hasUnavailableGuilds = visibleUnavailableCount > 0;
	const guildReadVersion = GuildReadState.version;
	const guildIndicatorDependencies = useMemo(
		() => [guilds.length, guildReadVersion, readVersion, unreadDMChannelIds],
		[guilds.length, guildReadVersion, readVersion, unreadDMChannelIds],
	);
	const getGuildScrollContainer = useCallback(() => scrollRef.current?.getScrollerNode() ?? null, []);
	useDragAutoScroll({active: isDragging, getScrollElement: getGuildScrollContainer});
	const scrollGuildListByWheel = useCallback((event: WheelEvent | React.WheelEvent<HTMLDivElement>) => {
		const scrollNode = scrollRef.current?.getScrollerNode();
		if (!scrollNode) return false;
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
	let pinnedCallChannel: Channel | null = null;
	if (!directMessagesDisabled && MediaEngine.connected) {
		const engineChannelId = MediaEngine.channelId;
		if (engineChannelId) {
			const candidate = Channels.getChannel(engineChannelId);
			if (
				candidate &&
				(candidate.type === ChannelTypes.DM || candidate.type === ChannelTypes.GROUP_DM) &&
				CallState.hasActiveCall(candidate.id)
			) {
				pinnedCallChannel = candidate;
			}
		}
	}
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
	const {
		getAnchorRef: getGuildListAnchorRef,
		handleResize: handleGuildListAnchorResize,
		handleScroll: handleGuildListAnchorScroll,
		stabilize: stabilizeGuildListAnchor,
	} = useListScrollAnchor({scrollerRef: scrollRef});
	const hasVisibleDMChannels = targetDMListRows.length > 0 || visibleDMListRows.length > 0;
	const shouldShowTopDivider = (guilds.length > 0 || hasUnavailableGuilds) && !hasVisibleDMChannels;
	const shouldShowEmptyStateDivider = !hasVisibleDMChannels && !hasUnavailableGuilds && guilds.length === 0;
	const shouldRenderGuildListItems = hasUnavailableGuilds || organizedItems.length > 0;
	useLayoutEffect(() => {
		stabilizeGuildListAnchor();
	}, [
		hasUnavailableGuilds,
		hasVisibleDMChannels,
		organizedItems,
		shouldRenderGuildListItems,
		stabilizeGuildListAnchor,
		visibleDMListRows,
		visibleUnavailableCount,
	]);
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
	const guildListNavigationRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: GUILD_LIST_FOCUSABLE_SELECTOR,
		orientation: 'vertical',
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
			const sourceKey = item.id;
			const targetKey = result.targetId;
			if (sourceKey === targetKey) return;
			const {position, targetIsFolder, targetFolderId} = result;
			if (position === 'inside' && targetIsFolder && !item.isFolder) {
				const sourceGuildId = item.id;
				const targetFolderId = getFolderIdFromKey(targetKey);
				const cleaned = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
				const newGuildFolders = cleaned.map((folder) => {
					if (folder.id !== targetFolderId) return folder;
					return {
						id: folder.id,
						name: folder.name,
						color: folder.color,
						flags: folder.flags,
						icon: folder.icon,
						guildIds: [...folder.guildIds, sourceGuildId],
					};
				});
				UserSettingsCommands.update({guildFolders: newGuildFolders});
				return;
			}
			if (
				targetFolderId != null &&
				targetFolderId !== UNCATEGORIZED_FOLDER_ID &&
				!item.isFolder &&
				(position === 'before' || position === 'after')
			) {
				const sourceGuildId = item.id;
				const targetGuildId = targetKey;
				const cleaned = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
				const newGuildFolders = cleaned.map((folder) => {
					if (folder.id !== targetFolderId) return folder;
					const guildIds = [...folder.guildIds];
					const targetIdx = guildIds.indexOf(targetGuildId);
					if (targetIdx !== -1) {
						const insertIdx = position === 'after' ? targetIdx + 1 : targetIdx;
						guildIds.splice(insertIdx, 0, sourceGuildId);
					} else {
						guildIds.push(sourceGuildId);
					}
					return {
						id: folder.id,
						name: folder.name,
						color: folder.color,
						flags: folder.flags,
						icon: folder.icon,
						guildIds,
					};
				});
				UserSettingsCommands.update({guildFolders: newGuildFolders});
				return;
			}
			if (position === 'combine' && !targetIsFolder && !item.isFolder && result.targetFolderId == null) {
				const sourceGuildId = item.id;
				const targetGuildId = targetKey;
				const cleaned = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
				const topLevelItems = buildTopLevelItems(cleaned);
				const targetIdx = topLevelItems.findIndex((tli) => tli.type === 'guild' && tli.guildId === targetGuildId);
				if (targetIdx === -1) return;
				const newFolder: GuildFolder = {
					id: getNextFolderId(UserSettings.guildFolders),
					name: null,
					color: null,
					flags: 0,
					icon: DEFAULT_GUILD_FOLDER_ICON,
					guildIds: [targetGuildId, sourceGuildId],
				};
				topLevelItems[targetIdx] = {type: 'folder', folder: newFolder};
				const newGuildFolders = buildGuildFoldersFromTopLevelItems(topLevelItems);
				UserSettingsCommands.update({guildFolders: newGuildFolders});
				return;
			}
			if (
				!item.isFolder &&
				item.folderId != null &&
				item.folderId !== UNCATEGORIZED_FOLDER_ID &&
				targetIsFolder &&
				(position === 'before' || position === 'after')
			) {
				const sourceGuildId = item.id;
				const parsedTargetFolderId = getFolderIdFromKey(targetKey);
				const originalTopLevelItems = buildTopLevelItems(UserSettings.guildFolders);
				const originalTargetIdx = originalTopLevelItems.findIndex(
					(tli) => tli.type === 'folder' && tli.folder.id === parsedTargetFolderId,
				);
				if (originalTargetIdx === -1) return;
				const cleaned = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
				const topLevelItems = buildTopLevelItems(cleaned);
				let targetIdx = topLevelItems.findIndex(
					(tli) => tli.type === 'folder' && tli.folder.id === parsedTargetFolderId,
				);
				if (targetIdx === -1) {
					targetIdx = Math.min(originalTargetIdx, topLevelItems.length);
				}
				const newItem: TopLevelGuildItem = {type: 'guild', guildId: sourceGuildId};
				const insertIdx = position === 'after' ? targetIdx + 1 : targetIdx;
				topLevelItems.splice(Math.min(insertIdx, topLevelItems.length), 0, newItem);
				const newGuildFolders = buildGuildFoldersFromTopLevelItems(topLevelItems);
				UserSettingsCommands.update({guildFolders: newGuildFolders});
				return;
			}
			if (
				!item.isFolder &&
				item.folderId != null &&
				item.folderId !== UNCATEGORIZED_FOLDER_ID &&
				!targetIsFolder &&
				result.targetFolderId == null &&
				(position === 'before' || position === 'after')
			) {
				const sourceGuildId = item.id;
				const targetGuildId = targetKey;
				const cleaned = removeGuildIdsFromGuildFolders(UserSettings.guildFolders, new Set([sourceGuildId]));
				const topLevelItems = buildTopLevelItems(cleaned);
				const targetIdx = topLevelItems.findIndex((tli) => tli.type === 'guild' && tli.guildId === targetGuildId);
				if (targetIdx === -1) return;
				const newItem: TopLevelGuildItem = {type: 'guild', guildId: sourceGuildId};
				const insertIdx = position === 'after' ? targetIdx + 1 : targetIdx;
				topLevelItems.splice(insertIdx, 0, newItem);
				const newGuildFolders = buildGuildFoldersFromTopLevelItems(topLevelItems);
				UserSettingsCommands.update({guildFolders: newGuildFolders});
				return;
			}
			const oldIndex = organizedItems.findIndex((i) => getOrganizedItemKey(i) === sourceKey);
			const targetIndex = organizedItems.findIndex((i) => getOrganizedItemKey(i) === targetKey);
			if (oldIndex === -1 || targetIndex === -1) return;
			const targetIndexAfterRemoval = oldIndex < targetIndex ? targetIndex - 1 : targetIndex;
			const newIndex = position === 'after' ? targetIndexAfterRemoval + 1 : targetIndexAfterRemoval;
			const newOrganizedItems = [...organizedItems];
			const [movedItem] = newOrganizedItems.splice(oldIndex, 1);
			newOrganizedItems.splice(newIndex, 0, movedItem);
			const topLevelItems: Array<TopLevelItem> = newOrganizedItems.map((oi) => {
				if (oi.type === 'folder') {
					return {type: 'folder' as const, folder: cloneGuildFolder(oi.folder)};
				}
				return {type: 'guild' as const, guildId: oi.guild.id};
			});
			const newGuildFolders = buildGuildFoldersFromTopLevelItems(topLevelItems);
			UserSettingsCommands.update({guildFolders: newGuildFolders});
		},
		[organizedItems],
	);
	const handleDragStateChange = useCallback((item: GuildDragItem | null) => {
		setIsDragging(item !== null);
	}, []);
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			handleGuildListAnchorScroll(event.currentTarget);
			const scrollTop = event.currentTarget.scrollTop;
			pendingScrollTopRef.current = scrollTop;
			if (scrollPersistRafRef.current != null) return;
			scrollPersistRafRef.current = requestAnimationFrame(() => {
				scrollPersistRafRef.current = null;
				const pendingScrollTop = pendingScrollTopRef.current;
				if (pendingScrollTop == null) return;
				DimensionCommands.updateGuildListScroll(pendingScrollTop);
			});
		},
		[handleGuildListAnchorScroll],
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
	useEffect(() => {
		return () => {
			if (scrollPersistRafRef.current != null) {
				cancelAnimationFrame(scrollPersistRafRef.current);
				scrollPersistRafRef.current = null;
			}
		};
	}, []);
	useEffect(() => {
		const scrollTop = Dimension.getGuildListDimensions().scrollTop;
		const scrollNode = scrollRef.current?.getScrollerNode();
		if (scrollTop > 0 && scrollNode) {
			scrollNode.scrollTop = scrollTop;
		}
	}, []);
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
				onResize={handleGuildListAnchorResize}
				onWheel={handleWheel}
				key="guild-list-scroller"
				data-flx="app.guilds-layout.guild-list.guild-list-scroll-container"
			>
				<div
					className={styles.guildListContent}
					ref={guildListNavigationRef}
					data-flx="app.guilds-layout.guild-list.guild-list-content"
				>
					<div className={styles.guildListTopSection} data-flx="app.guilds-layout.guild-list.guild-list-top-section">
						<FluxerButton data-flx="app.guilds-layout.guild-list.fluxer-button" />
						<FavoritesButton data-flx="app.guilds-layout.guild-list.favorites-button" />
						<div className={styles.dmListSection} data-flx="app.guilds-layout.guild-list.dm-list-section">
							{visibleDMListRows.map((row, index) => (
								<DMListAnimatedRow
									key={row.channel.id}
									channel={row.channel}
									isLast={index === visibleDMListRows.length - 1}
									isSelected={isSelectedPath(location.pathname, Routes.dmChannel(row.channel.id))}
									pendingRemoval={row.pendingRemoval}
									reducedMotion={Accessibility.useReducedMotion}
									onHoverStart={handleInlineDMHoverStart}
									onHoverEnd={handleInlineDMHoverEnd}
									onRemovalAnimationComplete={handleInlineDMRemovalAnimationComplete}
									scrollAnchorRef={getGuildListAnchorRef(`dm-${row.channel.id}`)}
									voiceCallActive={row.voiceCallActive}
								/>
							))}
						</div>
						{hasVisibleDMChannels && (
							<div className={styles.guildDivider} data-flx="app.guilds-layout.guild-list.guild-divider" />
						)}
					</div>
					<div
						ref={getGuildListAnchorRef('guild-section')}
						className={styles.guildListGuildsSection}
						data-flx="app.guilds-layout.guild-list.guild-list-guilds-section"
					>
						{shouldShowTopDivider && (
							<div
								ref={getGuildListAnchorRef('guild-divider')}
								className={styles.guildDivider}
								data-flx="app.guilds-layout.guild-list.guild-divider--2"
							/>
						)}
						{shouldRenderGuildListItems && (
							<div className={styles.guildListItems} data-flx="app.guilds-layout.guild-list.guild-list-items">
								{hasUnavailableGuilds && (
									<div
										ref={getGuildListAnchorRef('guild-outage-indicator')}
										className={styles.guildListItemSlot}
										key="guild-outage-indicator"
										data-flx="app.guilds-layout.guild-list.guild-list-item-slot"
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
											data-flx="app.guilds-layout.guild-list.tooltip"
										>
											<div
												className={styles.unavailableContainer}
												data-flx="app.guilds-layout.guild-list.unavailable-container"
											>
												<div
													className={styles.unavailableBadge}
													data-flx="app.guilds-layout.guild-list.unavailable-badge"
												>
													<ExclamationMarkIcon
														weight="regular"
														className={styles.unavailableIcon}
														data-flx="app.guilds-layout.guild-list.unavailable-icon"
													/>
												</div>
											</div>
										</Tooltip>
									</div>
								)}
								{organizedItems.length > 0 &&
									(() => {
										return organizedItems.map((item) => {
											if (item.type === 'folder') {
												const isFolderSelected = item.guilds.some((guild) =>
													isSelectedPath(location.pathname, Routes.guildChannel(guild.id)),
												);
												return (
													<div
														ref={getGuildListAnchorRef(getGuildListAnchorKeyForItem(item))}
														className={styles.guildListItemSlot}
														key={getOrganizedItemKey(item)}
														data-flx="app.guilds-layout.guild-list.guild-list-item-slot--2"
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
															data-flx="app.guilds-layout.guild-list.guild-folder-item"
														/>
													</div>
												);
											}
											return (
												<div
													ref={getGuildListAnchorRef(getGuildListAnchorKeyForItem(item))}
													className={styles.guildListItemSlot}
													key={item.guild.id}
													data-flx="app.guilds-layout.guild-list.guild-list-item-slot--3"
												>
													<GuildListItem
														isSortingList={isDragging}
														guild={item.guild}
														isSelected={isSelectedPath(location.pathname, Routes.guildChannel(item.guild.id))}
														guildIndex={guildNavigationIndexes.get(item.guild.id)}
														selectedGuildIndex={selectedGuildIndex}
														onGuildDrop={handleGuildDrop}
														onDragStateChange={handleDragStateChange}
														data-flx="app.guilds-layout.guild-list.guild-list-item"
													/>
												</div>
											);
										});
									})()}
								{organizedItems.length > 0 && (
									<BottomDropZone
										onGuildDrop={handleGuildDrop}
										lastItemKey={getOrganizedItemKey(organizedItems[organizedItems.length - 1])}
										lastItemIsFolder={organizedItems[organizedItems.length - 1].type === 'folder'}
										isDragging={isDragging}
										data-flx="app.guilds-layout.guild-list.bottom-drop-zone"
									/>
								)}
							</div>
						)}
						{shouldShowEmptyStateDivider && (
							<div className={styles.guildDivider} data-flx="app.guilds-layout.guild-list.guild-divider--3" />
						)}
						<DiscoveryButton data-flx="app.guilds-layout.guild-list.discovery-button" />
						<AddGuildButton data-flx="app.guilds-layout.guild-list.add-guild-button" />
						{!Platform.isElectron && !Platform.isPWA && (
							<DownloadButton data-flx="app.guilds-layout.guild-list.download-button" />
						)}
						<HelpButton data-flx="app.guilds-layout.guild-list.help-button" />
					</div>
				</div>
			</Scroller>
			<ScrollIndicatorOverlay
				getScrollContainer={getGuildScrollContainer}
				scrollContainerIdentity="guild-list"
				dependencies={guildIndicatorDependencies}
				label={i18n._(NEW_DESCRIPTOR)}
				data-flx="app.guilds-layout.guild-list.scroll-indicator-overlay"
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
	const prevNagbarCount = useRef(activeNagbars.length);
	const isReady = Initialization.isReady;
	useEffect(() => {
		if (prevNagbarCount.current !== activeNagbars.length) {
			prevNagbarCount.current = activeNagbars.length;
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
	}, [activeNagbars.length]);
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
