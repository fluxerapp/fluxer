// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	type GuildFolderReorderIndicator,
	GuildReorderStateMachine,
	type GuildReorderTarget,
	GuildReorderTargetKind,
} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import {useDragTargetRect} from '@app/features/app/components/layout/dnd/useDragTargetRect';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildFolderItem.module.css';
import {resolveGuildListIndicatorBarTarget} from '@app/features/app/components/layout/sidebar_nav/GuildListIndicator';
import {GuildListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListItem';
import {VoiceBadge, type VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {
	DragItemType,
	DropPlacement,
	type GuildDragItem,
	type GuildDropResult,
} from '@app/features/app/components/layout/types/DndTypes';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildFolderExpanded from '@app/features/guild/state/GuildFolderExpanded';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {getInitialsFromName, truncateInitials} from '@app/features/guild/utils/GuildInitialsUtils';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import Theme from '@app/features/theme/state/Theme';
import {Edge} from '@app/features/ui/AxisOrientation';
import {GuildFolderContextMenu} from '@app/features/ui/action_menu/GuildFolderContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {AnimePresence, AnimeSpan, createAnimeFlxElement} from '@app/features/ui/motion/AnimeElement';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarSourceUtils from '@app/features/user/utils/AvatarSourceUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {flxElementClassName} from '@app/lib/react';
import {
	GuildFolderFlags,
	type GuildFolderIcon,
	GuildFolderIcons,
	ThemeTypes,
	UNCATEGORIZED_FOLDER_ID,
} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	BookmarkSimpleIcon,
	FolderIcon,
	GameControllerIcon,
	HeartIcon,
	MusicNoteIcon,
	ShieldIcon,
	StarIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, ReactNode} from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const ExpandedFolderBackground = createAnimeFlxElement('flx-app-guild-folder-backdrop');
const ExpandedFolderGuilds = createAnimeFlxElement('flx-app-guild-folder-guilds');

const FOLDER_DESCRIPTOR = msg({
	message: 'Folder',
	comment: 'Short label in the sidebar navigation guild folder item.',
});
const FOLDER_ARIA_NAME_DESCRIPTOR = msg({
	message: '{folderName} folder',
	comment: 'Short label in the sidebar navigation guild folder item. Preserve {folderName}; it is inserted by code.',
});
const EXPANDED_DESCRIPTOR = msg({
	message: 'expanded',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild folder item.',
});
const COLLAPSED_DESCRIPTOR = msg({
	message: 'collapsed',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild folder item.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild folder item.',
});
const VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'voice activity',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild folder item.',
});
const COLLAPSE_DESCRIPTOR = msg({
	message: 'Collapse {folderName}',
	comment: 'Short label in the sidebar navigation guild folder item. Preserve {folderName}; it is inserted by code.',
});

interface GuildFolder {
	id: number | null;
	name: string | null;
	color: number | null;
	flags: number;
	icon: GuildFolderIcon;
	guildIds: Array<string>;
}

interface GuildFolderItemProps {
	readonly folder: GuildFolder;
	readonly guilds: Array<Guild>;
	readonly isSelected: boolean;
	readonly isSortingList: boolean;
	readonly onGuildDrop: ((item: GuildDragItem, result: GuildDropResult) => void) | null;
	readonly onDragStateChange: ((item: GuildDragItem | null) => void) | null;
	readonly guildNavigationIndexes: ReadonlyMap<string, number>;
	readonly selectedGuildIndex: number | null;
	readonly registerScrollTarget: ((id: string) => React.RefCallback<HTMLElement>) | null;
}

interface FolderVoiceActivity {
	readonly hasVoice: boolean;
	readonly hasScreenshare: boolean;
	readonly hasVideo: boolean;
}

const FOLDER_BACKGROUND_FADE_TARGET = Object.freeze({opacity: 0});
const FOLDER_GUILDS_COLLAPSE_TARGET = Object.freeze({opacity: 0, translateY: '-0.5rem'});

interface FolderStateAttributesRequest {
	readonly isExpanded: boolean;
	readonly isSelected: boolean;
	readonly shouldShowHoverState: boolean;
}

function buildFolderStateAttributes({
	isExpanded,
	isSelected,
	shouldShowHoverState,
}: FolderStateAttributesRequest): Record<string, 'true'> {
	const attributes: Record<string, 'true'> = {};
	if (isExpanded) {
		attributes['data-expanded'] = 'true';
	}
	if (isSelected) {
		attributes['data-selected'] = 'true';
	}
	if (shouldShowHoverState) {
		attributes['data-hovered'] = 'true';
	}
	return attributes;
}

function resolveFolderDragCursorStyle(isDragging: boolean): CSSProperties {
	if (!isDragging) {
		return {};
	}
	return {cursor: 'grabbing'};
}

function resolveMotionTarget<Target>(prefersReducedMotion: boolean, target: Target): Target | false {
	if (prefersReducedMotion) {
		return false;
	}
	return target;
}

function resolveMotionLeave<Target>(
	prefersReducedMotion: boolean,
	target: Target,
): {leave: Target} | Record<string, never> {
	if (prefersReducedMotion) {
		return {};
	}
	return {leave: target};
}

function folderIndicatorLeaveOpacity(prefersReducedMotion: boolean): {opacity: number} {
	if (prefersReducedMotion) {
		return {opacity: 1};
	}
	return {opacity: 0};
}

function deriveGuildFolderName(guilds: ReadonlyArray<Guild>): string {
	const names: Array<string> = [];
	for (let index = 0; index < guilds.length && index < 3; index++) {
		names.push(guilds[index].name);
	}
	return names.join(', ');
}

function getFolderColor(color: number | null, isLightTheme: boolean): string {
	if (color == null || color === 0) {
		if (isLightTheme) {
			return 'var(--brand-primary)';
		}
		return 'var(--brand-primary-light)';
	}
	return `#${color.toString(16).padStart(6, '0')}`;
}

function shouldShowCollapsedFolderIcon(flags: number): boolean {
	return (flags & GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED) === GuildFolderFlags.SHOW_ICON_WHEN_COLLAPSED;
}

function renderCollapsedFolderIcon(icon: GuildFolderIcon) {
	switch (icon) {
		case GuildFolderIcons.STAR:
			return (
				<StarIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon"
				/>
			);
		case GuildFolderIcons.HEART:
			return (
				<HeartIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--2"
				/>
			);
		case GuildFolderIcons.BOOKMARK:
			return (
				<BookmarkSimpleIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--3"
				/>
			);
		case GuildFolderIcons.GAME_CONTROLLER:
			return (
				<GameControllerIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--4"
				/>
			);
		case GuildFolderIcons.SHIELD:
			return (
				<ShieldIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--5"
				/>
			);
		case GuildFolderIcons.MUSIC_NOTE:
			return (
				<MusicNoteIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--6"
				/>
			);
		default:
			return (
				<FolderIcon
					weight="fill"
					className={styles.folderIcon}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-icon.folder-icon--7"
				/>
			);
	}
}

function resolveFolderExpansionId(folderId: number | null): number {
	if (folderId == null) {
		return UNCATEGORIZED_FOLDER_ID;
	}
	return folderId;
}

export const GuildFolderItem = observer((props: GuildFolderItemProps) => {
	const {
		folder,
		guilds,
		isSelected,
		isSortingList,
		onGuildDrop,
		onDragStateChange,
		guildNavigationIndexes,
		selectedGuildIndex,
		registerScrollTarget,
	} = props;
	const {i18n} = useLingui();
	const location = useLocation();
	const isExpanded = GuildFolderExpanded.isExpanded(resolveFolderExpansionId(folder.id));
	const [hoverRef, isHovering] = useHover();
	const focusableRef = useRef<HTMLElement | null>(null);
	const focusRingTargetRef = useRef<HTMLElement | null>(null);
	const itemRef = useRef<HTMLElement | null>(null);
	const mobileLayout = MobileLayout;
	const contextMenuOpen = useContextMenuHoverState(itemRef, !mobileLayout.enabled);
	const [dropIndicator, setDropIndicator] = useState<GuildFolderReorderIndicator | null>(null);
	const getDropTargetRect = useDragTargetRect(itemRef);
	const setFolderDropIndicator = useCallback((indicator: GuildFolderReorderIndicator | null) => {
		setDropIndicator(indicator);
	}, []);
	const resetFolderDropIndicator = useCallback(() => {
		setFolderDropIndicator(null);
	}, [setFolderDropIndicator]);
	let folderName = folder.name;
	if (folderName == null || folderName === '') folderName = deriveGuildFolderName(guilds);
	if (folderName === '') folderName = i18n._(FOLDER_DESCRIPTOR);
	const isLightTheme = Theme.effectiveTheme === ThemeTypes.LIGHT;
	const folderColor = getFolderColor(folder.color, isLightTheme);
	const folderId = `folder-${folder.id}`;
	const folderAccentStyle = useMemo<CSSProperties>(
		() =>
			({
				'--folder-accent': folderColor,
			}) as CSSProperties,
		[folderColor],
	);
	const hasUnreadMessages = guilds.some((guild) => GuildReadState.hasUnread(guild.id));
	const totalMentionCount = guilds.reduce((sum, guild) => sum + GuildReadState.getMentionCount(guild.id), 0);
	let folderScrollTargetRef: React.RefCallback<HTMLElement> | null = null;
	if (registerScrollTarget != null) {
		folderScrollTargetRef = registerScrollTarget(folderId);
	}
	function resolveFolderVoiceActivity(): FolderVoiceActivity {
		let hasVoice = false;
		let hasScreenshare = false;
		let hasVideo = false;
		for (const guild of guilds) {
			const guildVoiceStates = MediaEngine.getAllVoiceStatesInGuild(guild.id);
			if (guildVoiceStates == null) continue;
			for (const channelId in guildVoiceStates) {
				const channelStates = guildVoiceStates[channelId];
				if (channelStates == null) continue;
				for (const connectionId in channelStates) {
					const voiceState = channelStates[connectionId];
					if (voiceState == null) continue;
					hasVoice = true;
					if (voiceState.self_stream === true) {
						hasScreenshare = true;
					}
					if (voiceState.self_video === true) {
						hasVideo = true;
					}
					if (hasScreenshare && hasVideo) {
						return {hasVoice, hasScreenshare, hasVideo};
					}
				}
			}
		}
		return {hasVoice, hasScreenshare, hasVideo};
	}
	const folderVoiceActivity = resolveFolderVoiceActivity();
	const folderActivityType = useMemo<VoiceBadgeActivity | null>(() => {
		if (!folderVoiceActivity.hasVoice) return null;
		if (folderVoiceActivity.hasScreenshare) return 'screenshare';
		if (folderVoiceActivity.hasVideo) return 'video';
		return 'voice';
	}, [folderVoiceActivity.hasScreenshare, folderVoiceActivity.hasVideo, folderVoiceActivity.hasVoice]);
	const folderARIALabel = useMemo(() => {
		let expansionLabel = i18n._(COLLAPSED_DESCRIPTOR);
		if (isExpanded) {
			expansionLabel = i18n._(EXPANDED_DESCRIPTOR);
		}
		const parts = [i18n._(FOLDER_ARIA_NAME_DESCRIPTOR, {folderName}), expansionLabel];
		if (totalMentionCount > 0) {
			parts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount: totalMentionCount}));
		} else if (hasUnreadMessages) parts.push(i18n._(UNREAD_DESCRIPTOR));
		if (folderVoiceActivity.hasVoice) parts.push(i18n._(VOICE_ACTIVITY_DESCRIPTOR));
		return parts.join(', ');
	}, [folderName, folderVoiceActivity.hasVoice, hasUnreadMessages, isExpanded, totalMentionCount, i18n.locale]);
	const dragItemData = useMemo<GuildDragItem>(
		() => ({
			type: DragItemType.GUILD_FOLDER,
			id: folderId,
			isFolder: true,
			folderId: folder.id,
		}),
		[folderId, folder.id],
	);
	const dropTargetData = useMemo<GuildReorderTarget>(
		() => ({
			id: folderId,
			kind: GuildReorderTargetKind.FOLDER,
		}),
		[folderId],
	);
	const [{isDragging}, dragRef, preview] = useDrag(
		() => ({
			type: DragItemType.GUILD_FOLDER,
			item: () => {
				if (onDragStateChange != null) {
					onDragStateChange(dragItemData);
				}
				return dragItemData;
			},
			canDrag: !mobileLayout.enabled,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: () => {
				if (onDragStateChange != null) {
					onDragStateChange(null);
				}
				resetFolderDropIndicator();
			},
		}),
		[dragItemData, mobileLayout.enabled, onDragStateChange, resetFolderDropIndicator],
	);
	const [{isOver}, dropRef] = useDrop(
		() => ({
			accept: [DragItemType.GUILD_ITEM, DragItemType.GUILD_FOLDER],
			canDrop: (item: GuildDragItem) => GuildReorderStateMachine.canDrop({item, target: dropTargetData}),
			hover: (item: GuildDragItem, monitor) => {
				if (!GuildReorderStateMachine.canDrop({item, target: dropTargetData})) {
					resetFolderDropIndicator();
					return;
				}
				const clientOffset = monitor.getClientOffset();
				if (clientOffset == null) return;
				const boundingRect = getDropTargetRect();
				if (boundingRect == null) return;
				const intent = GuildReorderStateMachine.selectIntent({
					item,
					target: dropTargetData,
					clientOffset,
					targetRect: boundingRect,
				});
				if (intent == null || intent.indicator === DropPlacement.COMBINE) {
					resetFolderDropIndicator();
					return;
				}
				setFolderDropIndicator(intent.indicator);
			},
			drop: (item: GuildDragItem, monitor): GuildDropResult | undefined => {
				if (!monitor.canDrop()) {
					resetFolderDropIndicator();
					return;
				}
				const node = itemRef.current;
				if (node == null) return;
				const clientOffset = monitor.getClientOffset();
				if (clientOffset == null) return;
				const boundingRect = node.getBoundingClientRect();
				const intent = GuildReorderStateMachine.selectIntent({
					item,
					target: dropTargetData,
					clientOffset,
					targetRect: boundingRect,
				});
				if (intent == null) {
					resetFolderDropIndicator();
					return;
				}
				const result = intent.result;
				if (onGuildDrop != null) {
					onGuildDrop(item, result);
				}
				resetFolderDropIndicator();
				return result;
			},
			collect: (monitor) => ({
				isOver: monitor.isOver({shallow: true}),
			}),
		}),
		[dropTargetData, getDropTargetRect, onGuildDrop, resetFolderDropIndicator, setFolderDropIndicator],
	);
	useEffect(() => {
		if (!isOver) resetFolderDropIndicator();
	}, [isOver, resetFolderDropIndicator]);
	useEffect(() => {
		preview(getEmptyImage(), {captureDraggingState: true});
	}, [preview]);
	const dragConnectorRef = useCallback(
		(node: ConnectableElement | null) => {
			dragRef(node);
		},
		[dragRef],
	);
	const dropConnectorRef = useCallback(
		(node: ConnectableElement | null) => {
			dropRef(node);
		},
		[dropRef],
	);
	const mergedRef = useMergeRefs([
		dragConnectorRef,
		dropConnectorRef,
		hoverRef,
		focusableRef,
		itemRef,
		folderScrollTargetRef,
	]);
	const handleToggleExpanded = useCallback(() => {
		GuildFolderExpanded.toggleExpanded(resolveFolderExpansionId(folder.id));
	}, [folder.id]);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			if (isKeyboardActivationKey(event.key)) {
				event.preventDefault();
				handleToggleExpanded();
			}
		},
		[handleToggleExpanded],
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, (contextMenuProps) => (
				<GuildFolderContextMenu
					folder={folder}
					guilds={guilds}
					onClose={contextMenuProps.onClose}
					data-flx="app.sidebar-nav.guild-folder-item.handle-context-menu.guild-folder-context-menu"
				/>
			));
		},
		[folder, guilds],
	);
	const shouldShowHoverState = isHovering || contextMenuOpen;
	let showFolderIndicator = hasUnreadMessages;
	if (isSelected) showFolderIndicator = true;
	if (shouldShowHoverState) showFolderIndicator = true;
	const indicatorTarget = resolveGuildListIndicatorBarTarget({isSelected, showHoverState: shouldShowHoverState});
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const firstFourGuilds = guilds.slice(0, 4);
	const showCollapsedIcon = shouldShowCollapsedFolderIcon(folder.flags);
	const tooltipText = useMemo(() => {
		if (isExpanded) {
			return i18n._(COLLAPSE_DESCRIPTOR, {folderName});
		}
		return folderName;
	}, [isExpanded, folderName, i18n.locale]);
	const expandTransition = useMemo(() => {
		if (prefersReducedMotion) {
			return {duration: 0};
		}
		return {duration: 0.2, ease: [0.25, 0.1, 0.25, 1] as const};
	}, [prefersReducedMotion]);
	let indicatorTweenDuration = 0.2;
	if (prefersReducedMotion) {
		indicatorTweenDuration = 0;
	}
	function renderExpandedFolderBackground(): ReactNode {
		if (!isExpanded) return null;
		return (
			<ExpandedFolderBackground
				className={styles.expandedFolderBackground}
				from={resolveMotionTarget(prefersReducedMotion, FOLDER_BACKGROUND_FADE_TARGET)}
				to={{opacity: 1}}
				tween={expandTransition}
				data-flx="app.sidebar-nav.guild-folder-item.render-expanded-folder-background.expanded-folder-background"
				{...resolveMotionLeave(prefersReducedMotion, FOLDER_BACKGROUND_FADE_TARGET)}
			/>
		);
	}
	function renderCollapsedFolderIndicator(): ReactNode {
		if (isExpanded) return null;
		if (isSortingList) return null;
		if (!showFolderIndicator) return null;
		return (
			<flx-app-guild-folder-indicator
				className={flxElementClassName(guildStyles.guildIndicator)}
				data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-indicator.flx-app-guild-folder-indicator"
			>
				<AnimeSpan
					className={guildStyles.guildIndicatorBar}
					from={false}
					to={indicatorTarget}
					leave={folderIndicatorLeaveOpacity(prefersReducedMotion)}
					tween={{duration: indicatorTweenDuration, ease: [0.25, 0.1, 0.25, 1]}}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder-indicator.anime-span"
				/>
			</flx-app-guild-folder-indicator>
		);
	}
	function renderExpandedFolderIcon(): ReactNode {
		if (!isExpanded) return null;
		return (
			<flx-app-guild-folder-item-icon
				className={flxElementClassName(styles.folderHeaderButton)}
				data-flx="app.sidebar-nav.guild-folder-item.render-expanded-folder-icon.folder-header-button"
			>
				{renderCollapsedFolderIcon(folder.icon)}
			</flx-app-guild-folder-item-icon>
		);
	}
	function renderMiniGuildIcon(guild: Guild): ReactNode {
		return (
			<MiniGuildIcon
				key={guild.id}
				guild={guild}
				data-flx="app.sidebar-nav.guild-folder-item.render-mini-guild-icon.mini-guild-icon"
			/>
		);
	}
	function renderCollapsedFolder(): ReactNode {
		if (isExpanded) return null;
		let folderVisual: ReactNode;
		if (showCollapsedIcon) {
			folderVisual = (
				<flx-app-guild-folder-item-icon
					className={flxElementClassName(styles.folderHeaderButton)}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.folder-header-button"
				>
					{renderCollapsedFolderIcon(folder.icon)}
				</flx-app-guild-folder-item-icon>
			);
		} else {
			folderVisual = (
				<flx-app-guild-folder-item-grid
					className={flxElementClassName(styles.collapsedFolder)}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.collapsed-folder"
				>
					{firstFourGuilds.map(renderMiniGuildIcon)}
				</flx-app-guild-folder-item-grid>
			);
		}
		return (
			<>
				<flx-app-guild-folder-item-backdrop
					className={flxElementClassName(styles.collapsedFolderBackground)}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.collapsed-folder-background"
				/>
				{folderVisual}
				{folderActivityType != null && (
					<VoiceBadge
						activity={folderActivityType}
						data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.voice-badge"
					/>
				)}
				{/* biome-ignore lint/a11y/noAriaHiddenOnFocusable: decorative badge, not focusable */}
				<flx-app-guild-folder-item-badge
					aria-hidden="true"
					className={flxElementClassName(styles.folderBadge, totalMentionCount > 0 && styles.folderBadgeActive)}
					data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.folder-badge"
				>
					<MentionBadgeAnimated
						mentionCount={totalMentionCount}
						size="small"
						data-flx="app.sidebar-nav.guild-folder-item.render-collapsed-folder.mention-badge-animated"
					/>
				</flx-app-guild-folder-item-badge>
			</>
		);
	}
	function renderExpandedGuild(guild: Guild, index: number): ReactNode {
		const isGuildSelected = location.pathname.startsWith(Routes.guildChannel(guild.id));
		let scrollTargetRef: React.RefCallback<HTMLElement> | null = null;
		if (registerScrollTarget != null) {
			scrollTargetRef = registerScrollTarget(`guild-${guild.id}`);
		}
		let guildIndex: number | null = null;
		const navigationIndex = guildNavigationIndexes.get(guild.id);
		if (navigationIndex != null) {
			guildIndex = navigationIndex;
		}
		return (
			<GuildListItem
				key={guild.id}
				guild={guild}
				isSelected={isGuildSelected}
				guildIndex={guildIndex}
				selectedGuildIndex={selectedGuildIndex}
				onGuildDrop={onGuildDrop}
				onDragStateChange={onDragStateChange}
				insideFolderId={folder.id}
				isLastInsideFolder={index === guilds.length - 1}
				isSortingList={isSortingList}
				disableDrag={false}
				scrollTargetRef={scrollTargetRef}
				data-flx="app.sidebar-nav.guild-folder-item.render-expanded-guild.guild-list-item"
			/>
		);
	}
	function renderExpandedGuilds(): ReactNode {
		if (!isExpanded) return null;
		return (
			<ExpandedFolderGuilds
				className={styles.expandedGuilds}
				from={resolveMotionTarget(prefersReducedMotion, FOLDER_GUILDS_COLLAPSE_TARGET)}
				to={{opacity: 1, translateY: '0rem'}}
				tween={expandTransition}
				data-flx="app.sidebar-nav.guild-folder-item.render-expanded-guilds.expanded-guilds"
				{...resolveMotionLeave(prefersReducedMotion, FOLDER_GUILDS_COLLAPSE_TARGET)}
			>
				{guilds.map(renderExpandedGuild)}
			</ExpandedFolderGuilds>
		);
	}
	return (
		<flx-app-guild-folder-item
			className={flxElementClassName(styles.folderContainer)}
			style={folderAccentStyle}
			data-flx="app.sidebar-nav.guild-folder-item.folder-container"
			{...buildFolderStateAttributes({isExpanded, isSelected, shouldShowHoverState})}
		>
			<AnimePresence enterOnMount={false} data-flx="app.sidebar-nav.guild-folder-item.anime-presence">
				{renderExpandedFolderBackground()}
			</AnimePresence>
			<Tooltip
				position="right"
				maxWidth="xl"
				size="large"
				text={() => (
					<flx-app-guild-folder-item-tooltip
						className={flxElementClassName(styles.folderTooltipContainer)}
						data-flx="app.sidebar-nav.guild-folder-item.folder-tooltip-container"
					>
						<span className={styles.folderTooltipName} data-flx="app.sidebar-nav.guild-folder-item.folder-tooltip-name">
							{tooltipText}
						</span>
					</flx-app-guild-folder-item-tooltip>
				)}
				data-flx="app.sidebar-nav.guild-folder-item.tooltip"
			>
				<FocusRing
					focusTarget={focusableRef}
					ringTarget={focusRingTargetRef}
					offset={-2}
					data-flx="app.sidebar-nav.guild-folder-item.focus-ring"
				>
					<flx-app-guild-folder-item-toggle
						className={flxElementClassName(
							styles.folderHeader,
							dropIndicator === Edge.TOP && styles.dropIndicatorTop,
							dropIndicator === Edge.BOTTOM && styles.dropIndicatorBottom,
							dropIndicator === DropPlacement.INSIDE && styles.dropIndicatorInside,
						)}
						ref={mergedRef}
						role="button"
						tabIndex={0}
						data-guild-list-focus-item="true"
						aria-label={folderARIALabel}
						aria-expanded={isExpanded}
						onClick={handleToggleExpanded}
						onContextMenu={handleContextMenu}
						onKeyDown={handleKeyDown}
						style={resolveFolderDragCursorStyle(isDragging)}
						data-flx="app.sidebar-nav.guild-folder-item.folder-header.toggle-expanded"
					>
						<AnimePresence data-flx="app.sidebar-nav.guild-folder-item.anime-presence--2">
							{renderCollapsedFolderIndicator()}
						</AnimePresence>
						<flx-app-guild-folder-item-frame
							className={flxElementClassName(styles.relative)}
							ref={focusRingTargetRef}
							data-flx="app.sidebar-nav.guild-folder-item.relative"
						>
							{renderExpandedFolderIcon()}
							{renderCollapsedFolder()}
						</flx-app-guild-folder-item-frame>
					</flx-app-guild-folder-item-toggle>
				</FocusRing>
			</Tooltip>
			<AnimePresence enterOnMount={false} data-flx="app.sidebar-nav.guild-folder-item.anime-presence--3">
				{renderExpandedGuilds()}
			</AnimePresence>
		</flx-app-guild-folder-item>
	);
});

interface MiniGuildIconProps {
	readonly guild: Guild;
}

const MiniGuildIcon = observer(({guild}: MiniGuildIconProps) => {
	const iconURL = AvatarSourceUtils.getGuildIconURL(guild, false);
	const initials = getInitialsFromName(guild.name);
	const displayInitials = truncateInitials(initials, 2);
	if (iconURL !== '') {
		return (
			<flx-app-mini-guild-icon
				className={flxElementClassName(styles.miniGuildIcon)}
				style={{backgroundImage: `url(${iconURL})`}}
				data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-icon"
			/>
		);
	}
	return (
		<flx-app-mini-guild-icon
			className={flxElementClassName(styles.miniGuildIcon, styles.miniGuildIconWithInitials)}
			data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-icon--2"
		>
			<span
				className={styles.miniGuildInitials}
				data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-initials"
			>
				{displayInitials}
			</span>
		</flx-app-mini-guild-icon>
	);
});
