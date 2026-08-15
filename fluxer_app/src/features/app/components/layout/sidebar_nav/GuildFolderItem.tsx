// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	canGuildDropOnTarget,
	type GuildReorderTarget,
	selectGuildReorderIntent,
} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import {useDragTargetRect} from '@app/features/app/components/layout/dnd/useDragTargetRect';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildFolderItem.module.css';
import {GuildListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListItem';
import {VoiceBadge, type VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {DND_TYPES, type GuildDragItem, type GuildDropResult} from '@app/features/app/components/layout/types/DndTypes';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildFolderExpanded from '@app/features/guild/state/GuildFolderExpanded';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {truncateInitials} from '@app/features/guild/utils/GuildInitialsUtils';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import Theme from '@app/features/theme/state/Theme';
import {GuildFolderContextMenu} from '@app/features/ui/action_menu/GuildFolderContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useVoiceGatewayStateVersion} from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import * as StringUtils from '@app/lib/strings';
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
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {CSSProperties} from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const FOLDER_DESCRIPTOR = msg({
	message: 'Folder',
	comment: 'Short label in the sidebar navigation guild folder item.',
});
const FOLDER_2_DESCRIPTOR = msg({
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
const MENTIONS_DESCRIPTOR = msg({
	message: '{totalMentionCount} mentions',
	comment:
		'Short label in the sidebar navigation guild folder item. Preserve {totalMentionCount}; it is inserted by code.',
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
	folder: GuildFolder;
	guilds: Array<Guild>;
	isSelected: boolean;
	isSortingList?: boolean;
	onGuildDrop?: (item: GuildDragItem, result: GuildDropResult) => void;
	onDragStateChange?: (item: GuildDragItem | null) => void;
	guildNavigationIndexes: ReadonlyMap<string, number>;
	selectedGuildIndex?: number;
}

function getFolderColor(color: number | null, isLightTheme: boolean): string {
	if (color === null || color === 0) {
		return isLightTheme ? 'var(--brand-primary)' : 'var(--brand-primary-light)';
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

export const GuildFolderItem = observer((props: GuildFolderItemProps) => {
	const {
		folder,
		guilds,
		isSelected,
		isSortingList = false,
		onGuildDrop,
		onDragStateChange,
		guildNavigationIndexes,
		selectedGuildIndex,
	} = props;
	const {i18n} = useLingui();
	useVoiceGatewayStateVersion();
	const location = useLocation();
	const isExpanded = GuildFolderExpanded.isExpanded(folder.id ?? UNCATEGORIZED_FOLDER_ID);
	const [hoverRef, isHovering] = useHover();
	const focusableRef = useRef<HTMLDivElement | null>(null);
	const focusRingTargetRef = useRef<HTMLDivElement | null>(null);
	const itemRef = useRef<HTMLElement | null>(null);
	const mobileLayout = MobileLayout;
	const [dropIndicator, setDropIndicator] = useState<'top' | 'bottom' | 'inside' | null>(null);
	const getDropTargetRect = useDragTargetRect(itemRef);
	const setFolderDropIndicator = useCallback((indicator: 'top' | 'bottom' | 'inside' | null) => {
		setDropIndicator((current) => (current === indicator ? current : indicator));
	}, []);
	const resetFolderDropIndicator = useCallback(() => {
		setFolderDropIndicator(null);
	}, [setFolderDropIndicator]);
	const derivedFolderName = useMemo(() => {
		return guilds
			.slice(0, 3)
			.map((guild) => guild.name)
			.join(', ');
	}, [guilds]);
	const folderName = folder.name || derivedFolderName || i18n._(FOLDER_DESCRIPTOR);
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
	const folderScrollSeverity: ScrollIndicatorSeverity | undefined = (() => {
		if (isExpanded) return undefined;
		if (totalMentionCount > 0) return 'mention';
		if (hasUnreadMessages) return 'unread';
		return undefined;
	})();
	const folderVoiceActivity = (() => {
		let hasVoice = false;
		let hasScreenshare = false;
		let hasVideo = false;
		for (const guild of guilds) {
			const guildVoiceStates = MediaEngine.getAllVoiceStatesInGuild(guild.id);
			if (!guildVoiceStates) continue;
			for (const channelId in guildVoiceStates) {
				const channelStates = guildVoiceStates[channelId];
				if (!channelStates) continue;
				for (const connectionId in channelStates) {
					const voiceState = channelStates[connectionId];
					if (!voiceState) continue;
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
	})();
	const folderActivityType = useMemo<VoiceBadgeActivity | null>(() => {
		if (!folderVoiceActivity.hasVoice) return null;
		if (folderVoiceActivity.hasScreenshare) return 'screenshare';
		if (folderVoiceActivity.hasVideo) return 'video';
		return 'voice';
	}, [folderVoiceActivity.hasScreenshare, folderVoiceActivity.hasVideo, folderVoiceActivity.hasVoice]);
	const folderAriaLabel = useMemo(() => {
		const parts = [
			i18n._(FOLDER_2_DESCRIPTOR, {folderName}),
			isExpanded ? i18n._(EXPANDED_DESCRIPTOR) : i18n._(COLLAPSED_DESCRIPTOR),
		];
		if (totalMentionCount > 0) parts.push(i18n._(MENTIONS_DESCRIPTOR, {totalMentionCount}));
		else if (hasUnreadMessages) parts.push(i18n._(UNREAD_DESCRIPTOR));
		if (folderVoiceActivity.hasVoice) parts.push(i18n._(VOICE_ACTIVITY_DESCRIPTOR));
		return parts.join(', ');
	}, [folderName, folderVoiceActivity.hasVoice, hasUnreadMessages, isExpanded, totalMentionCount, i18n.locale]);
	const dragItemData = useMemo<GuildDragItem>(
		() => ({
			type: DND_TYPES.GUILD_FOLDER,
			id: folderId,
			isFolder: true,
			folderId: folder.id,
		}),
		[folderId, folder.id],
	);
	const dropTargetData = useMemo<GuildReorderTarget>(
		() => ({
			id: folderId,
			kind: 'folder',
		}),
		[folderId],
	);
	const [{isDragging}, dragRef, preview] = useDrag(
		() => ({
			type: DND_TYPES.GUILD_FOLDER,
			item: () => {
				onDragStateChange?.(dragItemData);
				return dragItemData;
			},
			canDrag: !mobileLayout.enabled,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: () => {
				onDragStateChange?.(null);
				resetFolderDropIndicator();
			},
		}),
		[dragItemData, mobileLayout.enabled, onDragStateChange, resetFolderDropIndicator],
	);
	const [{isOver}, dropRef] = useDrop(
		() => ({
			accept: [DND_TYPES.GUILD_ITEM, DND_TYPES.GUILD_FOLDER],
			canDrop: (item: GuildDragItem) => canGuildDropOnTarget(item, dropTargetData),
			hover: (item: GuildDragItem, monitor) => {
				if (!canGuildDropOnTarget(item, dropTargetData)) {
					resetFolderDropIndicator();
					return;
				}
				const clientOffset = monitor.getClientOffset();
				if (!clientOffset) return;
				const boundingRect = getDropTargetRect();
				if (!boundingRect) return;
				const intent = selectGuildReorderIntent(item, dropTargetData, clientOffset, boundingRect);
				if (!intent || intent.indicator === 'combine') {
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
				if (!node) return;
				const clientOffset = monitor.getClientOffset();
				if (!clientOffset) return;
				const boundingRect = node.getBoundingClientRect();
				const intent = selectGuildReorderIntent(item, dropTargetData, clientOffset, boundingRect);
				if (!intent) {
					resetFolderDropIndicator();
					return;
				}
				const result = intent.result;
				onGuildDrop?.(item, result);
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
	const mergedRef = useMergeRefs([dragConnectorRef, dropConnectorRef, hoverRef, focusableRef, itemRef]);
	const handleToggleExpanded = useCallback(() => {
		GuildFolderExpanded.toggleExpanded(folder.id ?? UNCATEGORIZED_FOLDER_ID);
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
			ContextMenuCommands.openFromEvent(event, (props) => (
				<GuildFolderContextMenu
					folder={folder}
					guilds={guilds}
					onClose={props.onClose}
					data-flx="app.sidebar-nav.guild-folder-item.handle-context-menu.guild-folder-context-menu"
				/>
			));
		},
		[folder, guilds],
	);
	const shouldShowHoverState = isHovering;
	const indicatorHeight =
		(() => {
			if (isSelected) return 40;
			if (shouldShowHoverState) return 20;
			return 8;
		})() * getAppZoomFactor();
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const firstFourGuilds = guilds.slice(0, 4);
	const showCollapsedIcon = shouldShowCollapsedFolderIcon(folder.flags);
	const tooltipText = useMemo(() => {
		return isExpanded ? i18n._(COLLAPSE_DESCRIPTOR, {folderName}) : folderName;
	}, [isExpanded, folderName, i18n.locale]);
	const expandTransition = useMemo(
		() => (prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: [0.25, 0.1, 0.25, 1] as const}),
		[prefersReducedMotion],
	);
	return (
		<div
			className={styles.folderContainer}
			style={folderAccentStyle}
			data-expanded={isExpanded ? 'true' : undefined}
			data-selected={isSelected ? 'true' : undefined}
			data-hovered={shouldShowHoverState ? 'true' : undefined}
			data-flx="app.sidebar-nav.guild-folder-item.folder-container"
		>
			<AnimatePresence initial={false} data-flx="app.sidebar-nav.guild-folder-item.animate-presence">
				{isExpanded && (
					<motion.div
						className={styles.expandedFolderBackground}
						initial={prefersReducedMotion ? false : {opacity: 0}}
						animate={{opacity: 1}}
						exit={prefersReducedMotion ? undefined : {opacity: 0}}
						transition={expandTransition}
						data-flx="app.sidebar-nav.guild-folder-item.expanded-folder-background"
					/>
				)}
			</AnimatePresence>
			<Tooltip
				position="right"
				maxWidth="xl"
				size="large"
				text={() => (
					<div
						className={styles.folderTooltipContainer}
						data-flx="app.sidebar-nav.guild-folder-item.folder-tooltip-container"
					>
						<span className={styles.folderTooltipName} data-flx="app.sidebar-nav.guild-folder-item.folder-tooltip-name">
							{tooltipText}
						</span>
					</div>
				)}
				data-flx="app.sidebar-nav.guild-folder-item.tooltip"
			>
				<FocusRing
					focusTarget={focusableRef}
					ringTarget={focusRingTargetRef}
					offset={-2}
					data-flx="app.sidebar-nav.guild-folder-item.focus-ring"
				>
					<div
						className={clsx(
							styles.folderHeader,
							dropIndicator === 'top' && styles.dropIndicatorTop,
							dropIndicator === 'bottom' && styles.dropIndicatorBottom,
							dropIndicator === 'inside' && styles.dropIndicatorInside,
						)}
						ref={mergedRef}
						role="button"
						tabIndex={0}
						data-guild-list-focus-item="true"
						aria-label={folderAriaLabel}
						aria-expanded={isExpanded}
						onClick={handleToggleExpanded}
						onContextMenu={handleContextMenu}
						onKeyDown={handleKeyDown}
						style={{cursor: isDragging ? 'grabbing' : undefined}}
						data-scroll-indicator={folderScrollSeverity}
						data-scroll-id={folderId}
						data-flx="app.sidebar-nav.guild-folder-item.folder-header.toggle-expanded"
					>
						{!isExpanded && (
							<AnimatePresence data-flx="app.sidebar-nav.guild-folder-item.animate-presence--2">
								{!isSortingList && (hasUnreadMessages || isSelected || shouldShowHoverState) && (
									<motion.div
										className={styles.folderIndicator}
										initial={false}
										animate={{opacity: 1}}
										exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
										transition={expandTransition}
										data-flx="app.sidebar-nav.guild-folder-item.folder-indicator"
									>
										<motion.span
											className={styles.folderIndicatorBar}
											initial={false}
											animate={{opacity: 1, scale: 1, height: indicatorHeight}}
											transition={{duration: prefersReducedMotion ? 0 : 0.2, ease: [0.25, 0.1, 0.25, 1]}}
											data-flx="app.sidebar-nav.guild-folder-item.folder-indicator-bar"
										/>
									</motion.div>
								)}
							</AnimatePresence>
						)}
						<div
							className={styles.relative}
							ref={focusRingTargetRef}
							data-flx="app.sidebar-nav.guild-folder-item.relative"
						>
							{isExpanded ? (
								<div
									className={styles.folderHeaderButton}
									data-flx="app.sidebar-nav.guild-folder-item.folder-header-button"
								>
									{renderCollapsedFolderIcon(folder.icon)}
								</div>
							) : (
								<>
									{showCollapsedIcon ? (
										<>
											<div
												className={styles.collapsedFolderBackground}
												data-flx="app.sidebar-nav.guild-folder-item.collapsed-folder-background"
											/>
											<div
												className={styles.folderHeaderButton}
												data-flx="app.sidebar-nav.guild-folder-item.folder-header-button--2"
											>
												{renderCollapsedFolderIcon(folder.icon)}
											</div>
										</>
									) : (
										<>
											<div
												className={styles.collapsedFolderBackground}
												data-flx="app.sidebar-nav.guild-folder-item.collapsed-folder-background--2"
											/>
											<div
												className={styles.collapsedFolder}
												data-flx="app.sidebar-nav.guild-folder-item.collapsed-folder"
											>
												{firstFourGuilds.map((guild) => (
													<MiniGuildIcon
														key={guild.id}
														guild={guild}
														data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon"
													/>
												))}
											</div>
										</>
									)}
									{folderActivityType && (
										<VoiceBadge
											activity={folderActivityType}
											data-flx="app.sidebar-nav.guild-folder-item.voice-badge"
										/>
									)}
									<div
										aria-hidden="true"
										className={clsx(styles.folderBadge, totalMentionCount > 0 && styles.folderBadgeActive)}
										data-flx="app.sidebar-nav.guild-folder-item.folder-badge"
									>
										<MentionBadgeAnimated
											mentionCount={totalMentionCount}
											size="small"
											data-flx="app.sidebar-nav.guild-folder-item.mention-badge-animated"
										/>
									</div>
								</>
							)}
						</div>
					</div>
				</FocusRing>
			</Tooltip>
			<AnimatePresence initial={false} data-flx="app.sidebar-nav.guild-folder-item.animate-presence--3">
				{isExpanded && (
					<motion.div
						className={styles.expandedGuilds}
						initial={prefersReducedMotion ? false : {height: 0, opacity: 0}}
						animate={{height: 'auto', opacity: 1}}
						exit={prefersReducedMotion ? undefined : {height: 0, opacity: 0}}
						transition={expandTransition}
						style={{overflow: 'hidden'}}
						data-flx="app.sidebar-nav.guild-folder-item.expanded-guilds"
					>
						{guilds.map((guild, index) => {
							const isGuildSelected = location.pathname.startsWith(Routes.guildChannel(guild.id));
							return (
								<GuildListItem
									key={guild.id}
									guild={guild}
									isSelected={isGuildSelected}
									guildIndex={guildNavigationIndexes.get(guild.id)}
									selectedGuildIndex={selectedGuildIndex}
									onGuildDrop={onGuildDrop}
									onDragStateChange={onDragStateChange}
									insideFolderId={folder.id}
									isLastInsideFolder={index === guilds.length - 1}
									isSortingList={isSortingList}
									data-flx="app.sidebar-nav.guild-folder-item.guild-list-item"
								/>
							);
						})}
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
});

interface MiniGuildIconProps {
	guild: Guild;
}

const MiniGuildIcon = observer(({guild}: MiniGuildIconProps) => {
	const iconUrl = AvatarUtils.getGuildIconURL(guild, false);
	const initials = StringUtils.getInitialsFromName(guild.name);
	const displayInitials = truncateInitials(initials, 2);
	if (iconUrl) {
		return (
			<div
				className={styles.miniGuildIcon}
				style={{backgroundImage: `url(${iconUrl})`}}
				data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-icon"
			/>
		);
	}
	return (
		<div
			className={clsx(styles.miniGuildIcon, styles.miniGuildIconWithInitials)}
			data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-icon--2"
		>
			<span
				className={styles.miniGuildInitials}
				data-flx="app.sidebar-nav.guild-folder-item.mini-guild-icon.mini-guild-initials"
			>
				{displayInitials}
			</span>
		</div>
	);
});
