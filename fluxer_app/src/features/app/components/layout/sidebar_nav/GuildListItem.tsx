// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {
	canGuildDropOnTarget,
	type GuildReorderTarget,
	selectGuildReorderIntent,
} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import {useDragTargetRect} from '@app/features/app/components/layout/dnd/useDragTargetRect';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import {VoiceBadge, type VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {DND_TYPES, type GuildDragItem, type GuildDropResult} from '@app/features/app/components/layout/types/DndTypes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useChannelHoverPreload} from '@app/features/app/hooks/useChannelHoverPreload';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import Channels from '@app/features/channel/state/Channels';
import {GuildHeaderBottomSheet} from '@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildCount from '@app/features/guild/state/GuildCount';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import {
	getGuildIconDisplayInitials,
	getInitialsLength,
	truncateInitials,
} from '@app/features/guild/utils/GuildInitialsUtils';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {filterViewableChannels} from '@app/features/messaging/utils/ChannelShared';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {GuildContextMenu} from '@app/features/ui/action_menu/GuildContextMenu';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useVoiceGatewayStateVersion} from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import * as StringUtils from '@app/lib/strings';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BellSlashIcon, ExclamationMarkIcon, MonitorPlayIcon, PauseIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const MUTED_UNTIL_DESCRIPTOR = msg({
	message: 'Muted until {dateUtilsGetFormattedDateTimeNewDateMuteConfigEndTime}',
	comment: 'Community sidebar tooltip showing when the per-community mute will expire. Date/time is interpolated.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	comment: 'Short label in the sidebar navigation guild list item.',
});
const SELECTED_DESCRIPTOR = msg({
	message: 'selected',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const ONLY_ACCESSIBLE_TO_STAFF_DESCRIPTOR = msg({
	message: 'Only accessible to {productName} staff',
	comment: 'Community sidebar tooltip shown when a community is only accessible to Fluxer staff.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const MUTED_2_DESCRIPTOR = msg({
	message: 'muted',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const TEMPORARILY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'temporarily unavailable',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'voice activity',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const INVITES_ARE_PAUSED_BECAUSE_DETECTED_A_POTENTIAL_RAID_DESCRIPTOR = msg({
	message: 'Invites are paused because {productName} detected a potential raid',
	comment: 'Community sidebar tooltip shown when raid protection has paused invites. Product name is interpolated.',
});
const INVITES_ARE_CURRENTLY_PAUSED_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Invites are currently paused in this community',
	comment: 'Short label in the sidebar navigation guild list item.',
});
const ONLINE_MEMBER_COUNT_DESCRIPTOR = msg({
	message: '{onlineCount} online',
	comment: 'Community tooltip stat showing how many members are currently online.',
});
const ONLINE_COUNT_LOADING_LABEL_DESCRIPTOR = msg({
	message: 'online',
	comment: 'Community tooltip stat label shown next to a loading skeleton while the online count is loading.',
});
const TOTAL_MEMBER_COUNT_DESCRIPTOR = msg({
	message: '{memberCount} {rawMemberCount, plural, one {member} other {members}}',
	comment:
		'Community tooltip stat showing the total member count. memberCount is already localized for display; rawMemberCount controls plural grammar.',
});
const MEMBER_COUNT_LOADING_LABEL_DESCRIPTOR = msg({
	message: 'members',
	comment: 'Community tooltip stat label shown next to a loading skeleton while the member count is loading.',
});

interface GuildListItemProps {
	guild: Guild;
	isSortingList?: boolean;
	isSelected: boolean;
	guildIndex?: number;
	selectedGuildIndex?: number;
	onGuildDrop?: (item: GuildDragItem, result: GuildDropResult) => void;
	onDragStateChange?: (item: GuildDragItem | null) => void;
	disableDrag?: boolean;
	insideFolderId?: number | null;
	isLastInsideFolder?: boolean;
}

interface VoiceRow {
	key: 'voice' | 'screenshare';
	users: Array<User>;
}

interface GuildVoiceSummary {
	voiceUsers: Array<User>;
	streamingUsers: Array<User>;
	hasScreenshare: boolean;
	hasVideo: boolean;
}

interface CombinePreviewProps {
	targetGuild: Guild;
	sourceGuildId: string;
}

function CombinePreview({targetGuild, sourceGuildId}: CombinePreviewProps) {
	const sourceGuild = Guilds.getGuild(sourceGuildId);
	if (!sourceGuild) return null;
	const targetIcon = AvatarUtils.getGuildIconURL(targetGuild, false);
	const sourceIcon = AvatarUtils.getGuildIconURL(sourceGuild, false);
	const targetInitials = StringUtils.getInitialsFromName(targetGuild.name);
	const sourceInitials = StringUtils.getInitialsFromName(sourceGuild.name);
	const targetDisplayInitials = truncateInitials(targetInitials, 2);
	const sourceDisplayInitials = truncateInitials(sourceInitials, 2);
	return (
		<div className={styles.combinePreview} data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview">
			<div
				className={styles.combinePreviewGrid}
				data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview-grid"
			>
				<div
					className={clsx(styles.combinePreviewIcon, !targetIcon && styles.combinePreviewIconInitials)}
					style={targetIcon ? {backgroundImage: `url(${targetIcon})`} : undefined}
					data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview-icon"
				>
					{!targetIcon && (
						<span data-flx="app.sidebar-nav.guild-list-item.combine-preview.span">{targetDisplayInitials}</span>
					)}
				</div>
				<div
					className={clsx(styles.combinePreviewIcon, !sourceIcon && styles.combinePreviewIconInitials)}
					style={sourceIcon ? {backgroundImage: `url(${sourceIcon})`} : undefined}
					data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview-icon--2"
				>
					{!sourceIcon && (
						<span data-flx="app.sidebar-nav.guild-list-item.combine-preview.span--2">{sourceDisplayInitials}</span>
					)}
				</div>
				<div
					className={clsx(styles.combinePreviewIcon, styles.combinePreviewIconEmpty)}
					data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview-icon--3"
				/>
				<div
					className={clsx(styles.combinePreviewIcon, styles.combinePreviewIconEmpty)}
					data-flx="app.sidebar-nav.guild-list-item.combine-preview.combine-preview-icon--4"
				/>
			</div>
		</div>
	);
}

export const GuildListItem = observer(
	({
		guild,
		isSortingList = false,
		isSelected,
		guildIndex,
		selectedGuildIndex,
		onGuildDrop,
		onDragStateChange,
		disableDrag = false,
		insideFolderId,
		isLastInsideFolder = false,
	}: GuildListItemProps) => {
		const {i18n} = useLingui();
		useVoiceGatewayStateVersion();
		const rawInitials = StringUtils.getInitialsFromName(guild.name);
		const initials = getGuildIconDisplayInitials(rawInitials);
		const initialsLength = rawInitials ? getInitialsLength(rawInitials) : null;
		const [hoverRef, isHovering] = useHover();
		const [bottomSheetOpen, setBottomSheetOpen] = useState(false);
		const isMobileExperience = isMobileExperienceEnabled();
		const mobileLayout = MobileLayout;
		const targetRadius = '50%';
		const guildReadSentinel = GuildReadState.getGuildChangeSentinel(guild.id);
		const hasUnreadMessages = GuildReadState.hasUnread(guild.id);
		const mentionCount = GuildReadState.getMentionCount(guild.id);
		const guildScrollSeverity: ScrollIndicatorSeverity | undefined = (() => {
			if (mentionCount > 0) return 'mention';
			if (hasUnreadMessages) return 'unread';
			return undefined;
		})();
		const guildScrollId = `guild-${guild.id}`;
		const selectedChannel = SelectedChannel.selectedChannelIds.get(guild.id);
		const preloadTargetChannel = useMemo(() => {
			const selected = selectedChannel ? Channels.getChannel(selectedChannel) : undefined;
			if (
				selected &&
				selected.guildId === guild.id &&
				selected.type !== ChannelTypes.GUILD_CATEGORY &&
				selected.type !== ChannelTypes.GUILD_LINK
			) {
				return selected;
			}
			return filterViewableChannels(Channels.getGuildChannels(guild.id))[0] ?? null;
		}, [guild.id, selectedChannel]);
		const {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow} = useChannelHoverPreload({
			channel: preloadTargetChannel,
			guild,
			defaultHiddenForChannel: preloadTargetChannel?.type === ChannelTypes.GUILD_VOICE,
			enabled: !guild.unavailable && !isSortingList,
		});
		const guildSettings = UserGuildSettings.getSettings(guild.id);
		const isMuted = guildSettings?.muted || false;
		const muteConfig = guildSettings?.mute_config;
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, guild);
		const voiceUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const streamingUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const guildVoiceStates = MediaEngine.getAllVoiceStatesInGuild(guild.id);
		const voiceSummary = useMemo<GuildVoiceSummary>(() => {
			if (!guildVoiceStates) {
				return {
					voiceUsers: [],
					streamingUsers: [],
					hasScreenshare: false,
					hasVideo: false,
				};
			}
			const voiceUsers: Array<User> = [];
			const streamingUsers: Array<User> = [];
			const seen = new Set<string>();
			let hasScreenshare = false;
			let hasVideo = false;
			for (const channelId in guildVoiceStates) {
				const channelStates = guildVoiceStates[channelId];
				if (!channelStates) continue;
				for (const connectionId in channelStates) {
					const voiceState = channelStates[connectionId];
					if (!voiceState) continue;
					const isScreensharing = voiceState.self_stream === true;
					const isVideo = voiceState.self_video === true;
					if (isScreensharing) {
						hasScreenshare = true;
					}
					if (isVideo) {
						hasVideo = true;
					}
					if (seen.has(voiceState.user_id)) continue;
					const user = Users.getUser(voiceState.user_id);
					if (!user) continue;
					if (isScreensharing) {
						streamingUsers.push(user);
					} else {
						voiceUsers.push(user);
					}
					seen.add(user.id);
				}
			}
			const sortedVoiceUsers =
				voiceUsers.length === 0
					? voiceUsers
					: sortVoiceParticipantItemsWithSnapshot(voiceUsers, {
							snapshot: voiceUserSortSnapshotRef.current,
							getParticipantKey: (user) => user.id,
							getUserId: (user) => user.id,
							guildId: guild.id,
						});
			const sortedStreamingUsers =
				streamingUsers.length === 0
					? streamingUsers
					: sortVoiceParticipantItemsWithSnapshot(streamingUsers, {
							snapshot: streamingUserSortSnapshotRef.current,
							getParticipantKey: (user) => user.id,
							getUserId: (user) => user.id,
							guildId: guild.id,
						});
			return {
				voiceUsers: sortedVoiceUsers,
				streamingUsers: sortedStreamingUsers,
				hasScreenshare,
				hasVideo,
			};
		}, [guildVoiceStates, guild.id]);
		const voiceRows = useMemo<Array<VoiceRow>>(() => {
			const rows: Array<VoiceRow> = [];
			if (voiceSummary.voiceUsers.length > 0) {
				rows.push({key: 'voice', users: voiceSummary.voiceUsers});
			}
			if (voiceSummary.streamingUsers.length > 0) {
				rows.push({key: 'screenshare', users: voiceSummary.streamingUsers});
			}
			return rows;
		}, [voiceSummary.streamingUsers, voiceSummary.voiceUsers]);
		const hasVoiceActivity = voiceSummary.voiceUsers.length > 0 || voiceSummary.streamingUsers.length > 0;
		const voiceBadgeActivity: VoiceBadgeActivity | null = !hasVoiceActivity
			? null
			: voiceSummary.hasScreenshare
				? 'screenshare'
				: voiceSummary.hasVideo
					? 'video'
					: 'voice';
		const iconUrl = AvatarUtils.getGuildIconURL(guild, false);
		const hoverIconUrl = AvatarUtils.getGuildIconURL(guild, true);
		const [loadedIconUrl, setLoadedIconUrl] = useState<string | null>(() =>
			ImageCacheUtils.hasImage(iconUrl) ? iconUrl : null,
		);
		const [loadedHoverIconUrl, setLoadedHoverIconUrl] = useState<string | null>(() =>
			ImageCacheUtils.hasImage(hoverIconUrl) ? hoverIconUrl : null,
		);
		const isStaticLoaded = iconUrl.length > 0 && loadedIconUrl === iconUrl;
		const isAnimatedLoaded = hoverIconUrl.length > 0 && loadedHoverIconUrl === hoverIconUrl;
		const [dropIndicator, setDropIndicator] = useState<'top' | 'bottom' | 'combine' | null>(null);
		const [combineSourceGuildId, setCombineSourceGuildId] = useState<string | null>(null);
		const itemRef = useRef<HTMLElement | null>(null);
		const getDropTargetRect = useDragTargetRect(itemRef);
		const setGuildDropState = useCallback(
			(indicator: 'top' | 'bottom' | 'combine' | null, sourceGuildId: string | null) => {
				setDropIndicator((current) => (current === indicator ? current : indicator));
				setCombineSourceGuildId((current) => (current === sourceGuildId ? current : sourceGuildId));
			},
			[],
		);
		const resetGuildDropState = useCallback(() => {
			setGuildDropState(null, null);
		}, [setGuildDropState]);
		const dragItemData = useMemo<GuildDragItem>(
			() => ({
				type: DND_TYPES.GUILD_ITEM,
				id: guild.id,
				isFolder: false,
				folderId: insideFolderId,
			}),
			[guild.id, insideFolderId],
		);
		const dropTargetData = useMemo<GuildReorderTarget>(
			() => ({
				id: guild.id,
				kind: 'guild',
				folderId: insideFolderId,
				isTerminal: insideFolderId != null && isLastInsideFolder,
			}),
			[guild.id, insideFolderId, isLastInsideFolder],
		);
		const dndEnabled = !mobileLayout.enabled && !disableDrag;
		const [{isDragging}, dragRef, preview] = useDrag(
			() => ({
				type: DND_TYPES.GUILD_ITEM,
				item: () => {
					onDragStateChange?.(dragItemData);
					return dragItemData;
				},
				canDrag: dndEnabled,
				collect: (monitor) => ({isDragging: monitor.isDragging()}),
				end: () => {
					onDragStateChange?.(null);
					resetGuildDropState();
				},
			}),
			[dragItemData, dndEnabled, onDragStateChange, resetGuildDropState],
		);
		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: [DND_TYPES.GUILD_ITEM, DND_TYPES.GUILD_FOLDER],
				canDrop: (item: GuildDragItem) => canGuildDropOnTarget(item, dropTargetData),
				hover: (item: GuildDragItem, monitor) => {
					if (!canGuildDropOnTarget(item, dropTargetData)) {
						resetGuildDropState();
						return;
					}
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const boundingRect = getDropTargetRect();
					if (!boundingRect) return;
					const intent = selectGuildReorderIntent(item, dropTargetData, clientOffset, boundingRect);
					if (!intent || intent.indicator === 'inside') {
						resetGuildDropState();
						return;
					}
					setGuildDropState(intent.indicator, intent.combineSourceGuildId);
				},
				drop: (item: GuildDragItem, monitor): GuildDropResult | undefined => {
					if (!monitor.canDrop()) {
						resetGuildDropState();
						return;
					}
					const node = itemRef.current;
					if (!node) return;
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const boundingRect = node.getBoundingClientRect();
					const intent = selectGuildReorderIntent(item, dropTargetData, clientOffset, boundingRect);
					if (!intent) {
						resetGuildDropState();
						return;
					}
					const result = intent.result;
					onGuildDrop?.(item, result);
					resetGuildDropState();
					return result;
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
				}),
			}),
			[dropTargetData, getDropTargetRect, onGuildDrop, resetGuildDropState, setGuildDropState],
		);
		useEffect(() => {
			if (!isOver) {
				resetGuildDropState();
			}
		}, [isOver, resetGuildDropState]);
		useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);
		const contextMenuOpen = useContextMenuHoverState(itemRef, !mobileLayout.enabled);
		useEffect(() => {
			let active = true;
			let cleanupIconLoad: () => void = () => {};
			if (iconUrl.length > 0 && !isStaticLoaded) {
				cleanupIconLoad = ImageCacheUtils.loadImage(iconUrl, () => {
					if (active) setLoadedIconUrl(iconUrl);
				});
			}
			let cleanupHoverIconLoad: () => void = () => {};
			if ((isHovering || contextMenuOpen) && hoverIconUrl.length > 0 && !isAnimatedLoaded) {
				cleanupHoverIconLoad = ImageCacheUtils.loadImage(hoverIconUrl, () => {
					if (active) setLoadedHoverIconUrl(hoverIconUrl);
				});
			}
			return () => {
				active = false;
				cleanupIconLoad();
				cleanupHoverIconLoad();
			};
		}, [iconUrl, hoverIconUrl, isHovering, contextMenuOpen]);
		const shouldPlayAnimated = (isHovering || contextMenuOpen) && isAnimatedLoaded;
		const handleSelect = () => {
			preloadChannelNow();
			NavigationCommands.selectGuild(guild.id, isMobileExperience ? undefined : selectedChannel);
		};
		const handleContextMenu = useCallback(
			(event: React.MouseEvent) => {
				if (isSortingList) return;
				event.preventDefault();
				event.stopPropagation();
				if (isMobileExperience) {
					return;
				}
				ContextMenuCommands.openFromEvent(event, (props) => (
					<GuildContextMenu
						guild={guild}
						onClose={props.onClose}
						data-flx="app.sidebar-nav.guild-list-item.handle-context-menu.guild-context-menu"
					/>
				));
			},
			[guild, isSortingList, isMobileExperience],
		);
		const handleOpenBottomSheet = useCallback(() => {
			setBottomSheetOpen(true);
		}, []);
		const handleCloseBottomSheet = useCallback(() => {
			setBottomSheetOpen(false);
		}, []);
		const handleLongPress = useCallback(() => {
			if (isSortingList) return;
			if (isMobileExperience) {
				handleOpenBottomSheet();
			}
		}, [handleOpenBottomSheet, isMobileExperience, isSortingList]);
		const shouldShowHoverState = isHovering || contextMenuOpen;
		const indicatorHeight =
			(() => {
				if (isSelected) return 40;
				if (shouldShowHoverState) return 20;
				return 8;
			})() * getAppZoomFactor();
		const isActive = isSelected || shouldShowHoverState;
		const focusableRef = useRef<HTMLDivElement | null>(null);
		const focusRingTargetRef = useRef<HTMLDivElement | null>(null);
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
		useEffect(() => {
			if (isSelected) {
				focusableRef.current?.scrollIntoView({block: 'nearest'});
			}
		}, [isSelected]);
		const getMutedText = () => {
			if (!isMuted) return null;
			const now = Date.now();
			if (muteConfig?.end_time && new Date(muteConfig.end_time).getTime() <= now) {
				return null;
			}
			if (muteConfig?.end_time) {
				return i18n._(MUTED_UNTIL_DESCRIPTOR, {
					dateUtilsGetFormattedDateTimeNewDateMuteConfigEndTime: DateUtils.getFormattedDateTime(
						new Date(muteConfig.end_time),
					),
				});
			}
			return i18n._(MUTED_DESCRIPTOR);
		};
		const getNavigationKeybind = () => {
			if (guildIndex === undefined || selectedGuildIndex === undefined || selectedGuildIndex === -1) {
				return null;
			}
			if (guildIndex < selectedGuildIndex) {
				return Keybind.getByAction('nav_guild_prev').combo;
			}
			if (guildIndex > selectedGuildIndex) {
				return Keybind.getByAction('nav_guild_next').combo;
			}
			return null;
		};
		const navigationKeybind = getNavigationKeybind();
		const guildCounts = GuildCount.getCounts(guild.id);
		const hasGuildCounts = guildCounts != null;
		const currentLocale = getCurrentLocale();
		const formattedOnlineCount = guildCounts ? formatNumber(guildCounts.onlineCount, currentLocale) : '';
		const formattedMemberCount = guildCounts ? formatNumber(guildCounts.memberCount, currentLocale) : '';
		const guildAriaParts = [guild.name];
		if (isSelected) guildAriaParts.push(i18n._(SELECTED_DESCRIPTOR));
		if (mentionCount > 0) guildAriaParts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
		else if (hasUnreadMessages) guildAriaParts.push(i18n._(UNREAD_DESCRIPTOR));
		if (isMuted) guildAriaParts.push(i18n._(MUTED_2_DESCRIPTOR));
		if (guild.unavailable) guildAriaParts.push(i18n._(TEMPORARILY_UNAVAILABLE_DESCRIPTOR));
		if (hasVoiceActivity) guildAriaParts.push(i18n._(VOICE_ACTIVITY_DESCRIPTOR));
		const guildAriaLabel = guildAriaParts.join(', ');
		useEffect(() => {
			if (isMobileExperience || isSortingList || !isHovering) return;
			const timeoutId = window.setTimeout(() => {
				GuildCount.requestCounts(guild.id);
			}, 250);
			return () => window.clearTimeout(timeoutId);
		}, [guild.id, isHovering, isMobileExperience, isSortingList]);
		useEffect(() => {
			if (isMobileExperience || isSortingList || !isHovering) {
				cancelChannelPreload();
				return;
			}
			scheduleChannelPreload();
			return cancelChannelPreload;
		}, [cancelChannelPreload, isHovering, isMobileExperience, isSortingList, scheduleChannelPreload]);
		return (
			<>
				<Tooltip
					position="right"
					maxWidth="xl"
					size="large"
					text={() =>
						!isSortingList && (
							<div
								className={styles.guildTooltipContainer}
								data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-container"
							>
								<div
									className={styles.guildTooltipHeader}
									data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-header"
								>
									<GuildBadge
										features={guild.features}
										showTooltip={false}
										onLightSurface
										data-flx="app.sidebar-nav.guild-list-item.guild-badge"
									/>
									<span
										className={styles.guildTooltipName}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-name"
									>
										{guild.name}
									</span>
								</div>
								{guild.unavailable && (
									<span
										className={styles.guildTooltipMessage}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-message"
									>
										<Trans>Something went wrong. We're working on it.</Trans>
									</span>
								)}
								{guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF) && (
									<span
										className={styles.guildTooltipError}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-error"
									>
										{i18n._(ONLY_ACCESSIBLE_TO_STAFF_DESCRIPTOR, {productName: PRODUCT_NAME})}
									</span>
								)}
								{canManageGuild && guild.features.has(GuildFeatures.INVITES_DISABLED) && (
									<span
										className={styles.guildTooltipMessage}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-message--2"
									>
										{guild.features.has(GuildFeatures.RAID_DETECTED)
											? i18n._(INVITES_ARE_PAUSED_BECAUSE_DETECTED_A_POTENTIAL_RAID_DESCRIPTOR, {
													productName: PRODUCT_NAME,
												})
											: i18n._(INVITES_ARE_CURRENTLY_PAUSED_IN_THIS_COMMUNITY_DESCRIPTOR)}
									</span>
								)}
								{isMuted && (
									<div className={styles.guildMutedInfo} data-flx="app.sidebar-nav.guild-list-item.guild-muted-info">
										<BellSlashIcon
											weight="fill"
											className={styles.guildMutedIcon}
											data-flx="app.sidebar-nav.guild-list-item.guild-muted-icon"
										/>
										<span className={styles.guildMutedText} data-flx="app.sidebar-nav.guild-list-item.guild-muted-text">
											{getMutedText()}
										</span>
									</div>
								)}
								<div
									className={styles.guildTooltipStats}
									aria-busy={!hasGuildCounts}
									data-loading={!hasGuildCounts ? 'true' : undefined}
									data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stats"
								>
									<div
										className={clsx(styles.guildTooltipStat, styles.guildTooltipStatOnline)}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat"
									>
										<div
											className={`${styles.guildTooltipStatDot} ${styles.guildTooltipStatDotOnline}`}
											data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-dot"
										/>
										{guildCounts ? (
											<span
												className={styles.guildTooltipStatText}
												data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-text"
											>
												{i18n._(ONLINE_MEMBER_COUNT_DESCRIPTOR, {
													onlineCount: formattedOnlineCount,
												})}
											</span>
										) : (
											<span
												className={styles.guildTooltipStatText}
												aria-hidden="true"
												data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-loading-text"
											>
												<span
													className={styles.guildTooltipStatSkeleton}
													data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-skeleton"
												/>
												<span data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-loading-label">
													{i18n._(ONLINE_COUNT_LOADING_LABEL_DESCRIPTOR)}
												</span>
											</span>
										)}
									</div>
									<div
										className={clsx(styles.guildTooltipStat, styles.guildTooltipStatMembers)}
										data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat--2"
									>
										<div
											className={`${styles.guildTooltipStatDot} ${styles.guildTooltipStatDotMembers}`}
											data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-dot--2"
										/>
										{guildCounts ? (
											<span
												className={styles.guildTooltipStatText}
												data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-text--2"
											>
												{i18n._(TOTAL_MEMBER_COUNT_DESCRIPTOR, {
													memberCount: formattedMemberCount,
													rawMemberCount: guildCounts.memberCount,
												})}
											</span>
										) : (
											<span
												className={styles.guildTooltipStatText}
												aria-hidden="true"
												data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-loading-text--2"
											>
												<span
													className={styles.guildTooltipStatSkeleton}
													data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-skeleton--2"
												/>
												<span data-flx="app.sidebar-nav.guild-list-item.guild-tooltip-stat-loading-label--2">
													{i18n._(MEMBER_COUNT_LOADING_LABEL_DESCRIPTOR)}
												</span>
											</span>
										)}
									</div>
								</div>
								{voiceRows.map((row) => (
									<div
										key={row.key}
										className={styles.guildVoiceInfo}
										data-flx="app.sidebar-nav.guild-list-item.guild-voice-info"
									>
										{row.key === 'screenshare' ? (
											<MonitorPlayIcon
												weight="fill"
												className={styles.guildVoiceIcon}
												data-flx="app.sidebar-nav.guild-list-item.guild-voice-icon"
											/>
										) : (
											<SpeakerHighIcon
												className={styles.guildVoiceIcon}
												data-flx="app.sidebar-nav.guild-list-item.guild-voice-icon--2"
											/>
										)}
										<AvatarStack
											className={styles.guildVoiceAvatarStack}
											size={28}
											maxVisible={3}
											users={row.users}
											guildId={guild.id}
											data-flx="app.sidebar-nav.guild-list-item.avatar-stack"
										/>
									</div>
								))}
								{navigationKeybind && (
									<KeybindHint combo={navigationKeybind} data-flx="app.sidebar-nav.guild-list-item.keybind-hint" />
								)}
							</div>
						)
					}
					data-flx="app.sidebar-nav.guild-list-item.tooltip"
				>
					<FocusRing
						focusTarget={focusableRef}
						ringTarget={focusRingTargetRef}
						offset={-2}
						data-flx="app.sidebar-nav.guild-list-item.focus-ring"
					>
						<LongPressable
							className={clsx(
								styles.guildListItem,
								styles.guildListReorderTarget,
								contextMenuOpen && styles.contextMenuHover,
								dropIndicator === 'top' && styles.dropIndicatorTop,
								dropIndicator === 'bottom' && styles.dropIndicatorBottom,
								dropIndicator === 'combine' && styles.dropIndicatorCombine,
							)}
							aria-label={guildAriaLabel}
							aria-current={isSelected ? 'page' : undefined}
							onClick={handleSelect}
							onContextMenu={handleContextMenu}
							onKeyDown={(event) => {
								if (!isKeyboardActivationKey(event.key)) return;
								event.preventDefault();
								handleSelect();
							}}
							ref={mergedRef as React.Ref<HTMLDivElement>}
							role="button"
							tabIndex={0}
							data-guild-list-focus-item="true"
							data-scroll-indicator={guildScrollSeverity}
							data-scroll-id={guildScrollId}
							data-guild-read-sentinel={guildReadSentinel}
							onLongPress={handleLongPress}
							data-flx="app.sidebar-nav.guild-list-item.guild-list-item.select"
						>
							<AnimatePresence data-flx="app.sidebar-nav.guild-list-item.animate-presence">
								{!isSortingList && (hasUnreadMessages || isSelected || shouldShowHoverState) && (
									<motion.div
										className={styles.guildIndicator}
										initial={false}
										animate={{opacity: 1}}
										exit={Accessibility.useReducedMotion ? {opacity: 1} : {opacity: 0}}
										transition={{
											duration: Accessibility.useReducedMotion ? 0 : 0.2,
											ease: [0.25, 0.1, 0.25, 1],
										}}
										data-flx="app.sidebar-nav.guild-list-item.guild-indicator"
									>
										<motion.span
											className={styles.guildIndicatorBar}
											initial={false}
											animate={{opacity: 1, scale: 1, height: indicatorHeight}}
											transition={{
												duration: Accessibility.useReducedMotion ? 0 : 0.2,
												ease: [0.25, 0.1, 0.25, 1],
											}}
											data-flx="app.sidebar-nav.guild-list-item.guild-indicator-bar"
										/>
									</motion.div>
								)}
							</AnimatePresence>
							<div className={styles.relative} data-flx="app.sidebar-nav.guild-list-item.relative">
								<motion.div
									ref={focusRingTargetRef}
									tabIndex={-1}
									className={clsx(
										styles.guildIcon,
										!guild.icon && styles.guildIconNoImage,
										isSelected && styles.guildIconSelected,
									)}
									animate={{borderRadius: isActive ? '30%' : targetRadius}}
									initial={false}
									transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
									data-initials-length={initialsLength}
									style={{
										backgroundImage: isStaticLoaded
											? `url(${shouldPlayAnimated && isAnimatedLoaded ? hoverIconUrl : iconUrl})`
											: undefined,
										cursor: isDragging ? 'grabbing' : undefined,
									}}
									data-flx="app.sidebar-nav.guild-list-item.guild-icon"
								>
									{!guild.icon && (
										<span
											className={styles.guildIconInitials}
											data-flx="app.sidebar-nav.guild-list-item.guild-icon-initials"
										>
											{initials}
										</span>
									)}
								</motion.div>
								{!guild.unavailable && (
									<div
										className={clsx(styles.guildBadge, mentionCount > 0 && styles.guildBadgeActive)}
										data-flx="app.sidebar-nav.guild-list-item.guild-badge--2"
									>
										<MentionBadgeAnimated
											mentionCount={mentionCount}
											size="small"
											data-flx="app.sidebar-nav.guild-list-item.mention-badge-animated"
										/>
									</div>
								)}
								{voiceBadgeActivity && (
									<VoiceBadge activity={voiceBadgeActivity} data-flx="app.sidebar-nav.guild-list-item.voice-badge" />
								)}
								{canManageGuild &&
									guild.features.has(GuildFeatures.INVITES_DISABLED) &&
									mentionCount === 0 &&
									!hasVoiceActivity && (
										<div
											className={styles.guildInvitesPausedBadge}
											data-flx="app.sidebar-nav.guild-list-item.guild-invites-paused-badge"
										>
											<div
												className={styles.guildInvitesPausedBadgeInner}
												data-flx="app.sidebar-nav.guild-list-item.guild-invites-paused-badge-inner"
											>
												<PauseIcon
													weight="fill"
													className={styles.guildInvitesPausedIcon}
													data-flx="app.sidebar-nav.guild-list-item.guild-invites-paused-icon"
												/>
											</div>
										</div>
									)}
								{(guild.unavailable || guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF)) && (
									<div className={styles.guildErrorBadge} data-flx="app.sidebar-nav.guild-list-item.guild-error-badge">
										<div
											className={styles.guildErrorBadgeInner}
											data-flx="app.sidebar-nav.guild-list-item.guild-error-badge-inner"
										>
											<ExclamationMarkIcon
												weight="regular"
												className={styles.guildErrorIcon}
												data-flx="app.sidebar-nav.guild-list-item.guild-error-icon"
											/>
										</div>
									</div>
								)}
							</div>
							{dropIndicator === 'combine' && combineSourceGuildId && (
								<CombinePreview
									targetGuild={guild}
									sourceGuildId={combineSourceGuildId}
									data-flx="app.sidebar-nav.guild-list-item.combine-preview"
								/>
							)}
						</LongPressable>
					</FocusRing>
				</Tooltip>
				{isMobileExperience && (
					<GuildHeaderBottomSheet
						isOpen={bottomSheetOpen}
						onClose={handleCloseBottomSheet}
						guild={guild}
						data-flx="app.sidebar-nav.guild-list-item.guild-header-bottom-sheet"
					/>
				)}
			</>
		);
	},
);
