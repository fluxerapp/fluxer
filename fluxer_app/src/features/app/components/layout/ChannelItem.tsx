// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility, {ChannelTypingIndicatorMode} from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/layout/ChannelItem.module.css';
import {ChannelItemContent} from '@app/features/app/components/layout/ChannelItemContent';
import {ChannelItemIcon} from '@app/features/app/components/layout/ChannelItemIcon';
import channelItemSurfaceStyles from '@app/features/app/components/layout/ChannelItemSurface.module.css';
import {
	DirectSelectionSurface,
	markDirectSelection,
	peekDirectSelection,
} from '@app/features/app/components/layout/DirectSelectionOrigin';
import {
	type ChannelReorderTarget,
	canChannelDropOnTarget,
	selectChannelReorderResolution,
} from '@app/features/app/components/layout/dnd/ChannelReorderStateMachine';
import {GenericChannelItem} from '@app/features/app/components/layout/GenericChannelItem';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import {type DragItem, DragItemType, type DropResult} from '@app/features/app/components/layout/types/DndTypes';
import {isCategory, isTextChannel} from '@app/features/app/components/layout/utils/ChannelOrganization';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {VoiceChannelUserCount} from '@app/features/app/components/layout/VoiceChannelUserCount';
import {useChannelHoverPreload} from '@app/features/app/hooks/useChannelHoverPreload';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {useTextOverflow} from '@app/features/app/hooks/useTextOverflow';
import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import {CategoryBottomSheet} from '@app/features/channel/components/bottomsheets/CategoryBottomSheet';
import {ChannelBottomSheet} from '@app/features/channel/components/bottomsheets/ChannelBottomSheet';
import {Typing} from '@app/features/channel/components/ChannelTyping';
import {ChannelCreateModal} from '@app/features/channel/components/modals/ChannelCreateModal';
import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import {getTypingText, usePresentableTypingUsers} from '@app/features/channel/components/TypingUsers';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {Guild} from '@app/features/guild/models/Guild';
import {
	CREATE_CHANNEL_DESCRIPTOR,
	MENTION_COUNT_ARIA_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey, stopPropagationOnEnterSpace} from '@app/features/input/utils/KeyboardUtils';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Autocomplete from '@app/features/search/state/Autocomplete';
import {CategoryContextMenu} from '@app/features/ui/action_menu/CategoryContextMenu';
import {ChannelContextMenu} from '@app/features/ui/action_menu/ChannelContextMenu';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {CHANNEL_SETTINGS_LABEL_DESCRIPTOR} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import {computeChannelE2EEStatus} from '@app/features/voice/state/ChannelE2EEStatus';
import CompactVoiceCallHeight, {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, ChatTeardropIcon, GearIcon, PlusIcon, UserPlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const CATEGORY_DESCRIPTOR = msg({
	message: 'category',
	comment: 'Lowercase channel type label used inside channel-list accessible text.',
});
const VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'voice channel',
	comment: 'Lowercase channel type label used inside channel-list accessible text.',
});
const CHANNEL_DESCRIPTOR = msg({
	message: 'channel',
	comment: 'Lowercase generic channel type label used inside channel-list accessible text.',
});
const SELECTED_DESCRIPTOR = msg({
	message: 'selected',
	comment: 'Lowercase state label used inside channel-list accessible text for the selected channel.',
});
const COLLAPSED_DESCRIPTOR = msg({
	message: 'collapsed',
	comment: 'Lowercase state label used inside channel-list accessible text for a collapsed category.',
});
const EXPANDED_DESCRIPTOR = msg({
	message: 'expanded',
	comment: 'Lowercase state label used inside channel-list accessible text for an expanded category.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Lowercase state label used inside channel-list accessible text when a channel has unread messages.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'muted',
	comment: 'Lowercase state label used inside channel-list accessible text when a channel is muted.',
});
const CONNECTED_DESCRIPTOR = msg({
	message: 'connected',
	comment: 'Lowercase state label used inside channel-list accessible text for the current voice channel.',
});
const EDIT_CATEGORY_DESCRIPTOR = msg({
	message: 'Edit category',
	comment: 'Tooltip and accessible label for the category settings button in the channel list.',
});
const INVITE_MEMBERS_DESCRIPTOR = msg({
	message: 'Invite members',
	comment: 'Tooltip and accessible label for the invite button shown beside a channel in the channel list.',
});
const OPEN_CHAT_DESCRIPTOR = msg({
	message: 'Open chat',
	comment: 'Tooltip and accessible label for the chat icon shown beside a voice channel in the channel list.',
});
const EMPTY_TYPING_USERS: ReadonlyArray<User> = Object.freeze([]);

interface ChannelItemCoreProps {
	channel: {
		name: string;
		type: number;
	};
	isSelected?: boolean;
	forceHover?: boolean;
	typingIndicator?: React.ReactNode;
	className?: string;
}

export const ChannelItemCore: React.FC<ChannelItemCoreProps> = observer(
	({channel, isSelected = false, forceHover = false, typingIndicator, className}) => {
		const channelLabelRef = useRef<HTMLSpanElement>(null);
		const isChannelNameOverflowing = useTextOverflow(channelLabelRef);
		return (
			<div
				className={clsx(
					styles.channelItemCore,
					isSelected ? styles.channelItemCoreSelected : styles.channelItemCoreUnselected,
					!isSelected && forceHover && styles.channelItemCoreHovered,
					className,
				)}
				data-flx="app.channel-item.channel-item-core.channel-item-core"
			>
				<Tooltip text={channel.name} data-flx="app.channel-item.channel-item-core.tooltip">
					<div data-flx="app.channel-item.channel-item-core.div">
						{ChannelUtils.getIcon(channel, {
							className: clsx(
								styles.channelItemIcon,
								isSelected ? styles.channelItemIconSelected : styles.channelItemIconUnselected,
							),
						})}
					</div>
				</Tooltip>
				<Tooltip
					text={isChannelNameOverflowing ? channel.name : ''}
					data-flx="app.channel-item.channel-item-core.tooltip--2"
				>
					<span
						ref={channelLabelRef}
						className={styles.channelItemLabel}
						data-flx="app.channel-item.channel-item-core.channel-item-label"
					>
						{channel.name}
					</span>
				</Tooltip>
				<div className={styles.channelItemActions} data-flx="app.channel-item.channel-item-core.channel-item-actions">
					{typingIndicator}
				</div>
			</div>
		);
	},
);

export interface ChannelItemProps {
	guild: Guild;
	channel: Channel;
	isCollapsed?: boolean;
	onToggle?: () => void;
	isDraggingAnything: boolean;
	activeDragItem?: DragItem | null;
	onChannelDrop?: (item: DragItem, result: DropResult) => void;
	onDragStateChange?: (item: DragItem | null) => void;
	isSelectedByPath?: boolean;
	isOnMembersRoute?: boolean;
}

export const ChannelItem = observer(
	({
		guild,
		channel,
		isCollapsed,
		onToggle,
		isDraggingAnything,
		activeDragItem,
		onChannelDrop,
		onDragStateChange,
		isSelectedByPath = false,
		isOnMembersRoute = false,
	}: ChannelItemProps) => {
		const {i18n} = useLingui();
		const elementRef = useRef<HTMLDivElement | null>(null);
		const dropTargetRef = useRef<HTMLDivElement | null>(null);
		const contextMenuOpen = useContextMenuHoverState(elementRef, !MobileLayout.enabled);
		const channelType = channel.type;
		const channelIsCategory = isCategory(channel);
		const channelIsVoice = channelType === ChannelTypes.GUILD_VOICE;
		const channelIsText = isTextChannel(channel);
		const {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow} = useChannelHoverPreload({
			channel,
			guild,
			defaultHiddenForChannel: channelIsVoice,
			enabled: !channelIsCategory,
		});
		const draggingChannel = activeDragItem?.type === DragItemType.CHANNEL ? activeDragItem : null;
		const isVoiceDragActive = draggingChannel?.channelType === ChannelTypes.GUILD_VOICE;
		const shouldDimForVoiceDrag = Boolean(isVoiceDragActive && channelIsText && channel.parentId !== null);
		const unreadCount = ReadStates.getUnreadCount(channel.id);
		const hasUnread = ReadStates.hasUnread(channel.id);
		const connectedVoiceGuildId = channelIsVoice ? MediaEngine.guildId : null;
		const connectedVoiceChannelId = channelIsVoice ? MediaEngine.channelId : null;
		const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, channel);
		const canUpdateRtcRegion = channelIsVoice && Permission.can(Permissions.UPDATE_RTC_REGION, channel);
		const canEditChannel = canManageChannels || canUpdateRtcRegion;
		const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);
		const mobileLayout = MobileLayout;
		const isMuted = UserGuildSettings.isGuildOrChannelMuted(guild.id, channel.id);
		const isChannelDirectlyMuted = UserGuildSettings.isChannelDirectlyMuted(guild.id, channel.id);
		const currentUserCount =
			channelIsVoice && channel.userLimit != null && channel.userLimit > 0
				? Object.keys(MediaEngine.getAllVoiceStatesInChannel(guild.id, channel.id)).length
				: 0;
		const isMobileLayout = mobileLayout.enabled;
		const allowHoverAffordances = !isMobileLayout;
		const [menuOpen, setMenuOpen] = useState(false);
		const [categoryMenuOpen, setCategoryMenuOpen] = useState(false);
		const [isFocused, setIsFocused] = useState(false);
		const [actionModalOpen, setActionModalOpen] = useState(false);
		const {keyboardModeEnabled} = KeyboardMode;
		const showKeyboardAffordances = keyboardModeEnabled && isFocused;
		const revealActionButtons = showKeyboardAffordances || actionModalOpen;
		const actionButtonTabIndex = revealActionButtons ? 0 : -1;
		const isVoiceSelected =
			channelIsVoice && connectedVoiceGuildId === guild.id && connectedVoiceChannelId === channel.id;
		const channelIsE2EEEncrypted = isVoiceSelected && computeChannelE2EEStatus(guild.id, channel.id) === 'encrypted';
		const isSelected = isVoiceSelected || (isSelectedByPath && !isOnMembersRoute);
		const mentionCount = ReadStates.getMentionCount(channel.id);
		const unreadBadgesLevel = UserGuildSettings.resolvedUnreadBadgesLevel({
			id: channel.id,
			guildId: channel.guildId ?? undefined,
			parentId: channel.parentId ?? undefined,
			type: channel.type,
		});
		const unreadState = getChannelUnreadState({
			hasUnread,
			unreadCount,
			mentionCount,
			isMuted: isChannelDirectlyMuted,
			showFadedUnreadOnMutedChannels: Accessibility.showFadedUnreadOnMutedChannels,
			unreadBadgesLevel,
		});
		const hasUnreadMessages = unreadState.hasUnreadMessages;
		const hasMentions = unreadState.hasMentions;
		const isHighlight = unreadState.isHighlight;
		let scrollIndicatorSeverity: ScrollIndicatorSeverity | undefined;
		if (!channelIsCategory) {
			if (hasMentions) scrollIndicatorSeverity = 'mention';
			else if (unreadState.shouldShowUnreadIndicator) scrollIndicatorSeverity = 'unread';
		}
		const scrollIndicatorId = `channel-${channel.id}`;
		const isAutocompleteHighlight = Autocomplete.highlightChannelId === channel.id;
		const channelTypingIndicatorMode = Accessibility.channelTypingIndicatorMode;
		const typingIndicatorEligible =
			!channelIsCategory && channelTypingIndicatorMode !== ChannelTypingIndicatorMode.HIDDEN;
		const typingUsers = typingIndicatorEligible ? usePresentableTypingUsers(channel) : EMPTY_TYPING_USERS;
		const [dropIndicator, setDropIndicator] = useState<{position: 'top' | 'bottom'; isValid: boolean} | null>(null);
		const dragItemData = useMemo<DragItem>(
			() => ({
				type: channelIsCategory ? DragItemType.CATEGORY : DragItemType.CHANNEL,
				id: channel.id,
				channelType: channel.type,
				parentId: channel.parentId,
				guildId: guild.id,
			}),
			[channelIsCategory, channel.id, channel.type, channel.parentId, guild.id],
		);
		const dropTargetData = useMemo<ChannelReorderTarget>(
			() => ({
				id: channel.id,
				channelType: channel.type,
				parentId: channel.parentId,
				guildId: guild.id,
			}),
			[channel.id, channel.type, channel.parentId, guild.id],
		);
		const dndEnabled = !mobileLayout.enabled;
		const [{isDragging}, dragRef, preview] = useDrag(
			() => ({
				type: dragItemData.type,
				item: () => {
					onDragStateChange?.(dragItemData);
					return dragItemData;
				},
				canDrag: dndEnabled && canManageChannels,
				collect: (monitor) => ({isDragging: monitor.isDragging()}),
				end: () => {
					onDragStateChange?.(null);
					setDropIndicator(null);
				},
			}),
			[dragItemData, canManageChannels, dndEnabled, onDragStateChange],
		);
		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: [DragItemType.CHANNEL, DragItemType.CATEGORY, DragItemType.VOICE_PARTICIPANT],
				canDrop: (item: DragItem) => canChannelDropOnTarget(item, dropTargetData),
				hover: (item: DragItem, monitor) => {
					const node = dropTargetRef.current;
					if (!node) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const resolution = selectChannelReorderResolution(item, dropTargetData, clientOffset, hoverBoundingRect);
					setDropIndicator(resolution.indicator);
				},
				drop: (item: DragItem, monitor): DropResult | undefined => {
					if (!monitor.canDrop()) {
						setDropIndicator(null);
						return;
					}
					if (item.type === DragItemType.VOICE_PARTICIPANT && channelIsVoice) {
						const canMove = Permission.can(Permissions.MOVE_MEMBERS, {guildId: guild.id});
						if (!canMove || item.currentChannelId === channel.id) {
							setDropIndicator(null);
							return;
						}
						const currentUserId = Users.getCurrentUser()?.id ?? null;
						if (currentUserId && item.userId === currentUserId) {
							void MediaEngine.connectToVoiceChannel(guild.id, channel.id);
							setDropIndicator(null);
							return;
						}
						const targetChannel = Channels.getChannel(channel.id);
						if (targetChannel) {
							const canTargetConnect = PermissionUtils.can(Permissions.CONNECT, item.userId!, targetChannel.toJSON());
							if (!canTargetConnect) {
								setDropIndicator(null);
								return;
							}
						}
						void GuildMemberCommands.update(guild.id, item.userId!, {channel_id: channel.id});
						setDropIndicator(null);
						return;
					}
					const node = dropTargetRef.current;
					if (!node) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const result = selectChannelReorderResolution(item, dropTargetData, clientOffset, hoverBoundingRect).intent
						?.result;
					if (!result) {
						setDropIndicator(null);
						return;
					}
					onChannelDrop?.(item, result);
					setDropIndicator(null);
					return result;
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
					canDrop: monitor.canDrop(),
				}),
			}),
			[dropTargetData, guild.id, channelIsVoice, onChannelDrop],
		);
		useEffect(() => {
			if (!isOver) setDropIndicator(null);
		}, [isOver]);
		useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);
		const {startConnection: startVoiceConnection} = usePendingVoiceConnection({
			guildId: guild.id,
			channelId: channel.id,
		});
		const singleClickConnectsToVoice =
			channelIsVoice && !Accessibility.voiceChannelJoinRequiresDoubleClick && !isVoiceSelected;
		const navigateToChannel = useCallback(() => {
			preloadChannelNow();
			NavigationCommands.selectChannel(guild.id, channel.id);
			if (MobileLayout.isMobileLayout()) {
				LayoutCommands.updateMobileLayoutState(false, true);
			}
		}, [guild.id, channel.id, preloadChannelNow]);
		const collapseVoiceCallView = useCallback(() => {
			if (!channelIsVoice) return;
			CompactVoiceCallHeight.setExpandedForKey(getGuildVoiceCallExpansionKey(channel.id), false);
		}, [channelIsVoice, channel.id]);
		const handleSelect = useCallback(() => {
			markDirectSelection(DirectSelectionSurface.CHANNEL_LIST);
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				onToggle?.();
				return;
			}
			if (LinkChannelCommands.openLinkChannel(channel)) {
				return;
			}
			if (singleClickConnectsToVoice && Permission.can(Permissions.CONNECT, channel)) {
				startVoiceConnection();
				return;
			}
			navigateToChannel();
		}, [channel, onToggle, singleClickConnectsToVoice, startVoiceConnection, navigateToChannel]);
		const handleDoubleClick = useCallback(() => {
			if (!channelIsVoice || isVoiceSelected) return;
			if (!Permission.can(Permissions.CONNECT, channel)) return;
			startVoiceConnection({skipConfirm: true});
		}, [channelIsVoice, isVoiceSelected, channel, startVoiceConnection]);
		const handleContextMenu = useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();
				if (isMobileLayout) {
					return;
				}
				ContextMenuCommands.openFromEvent(event, ({onClose}) =>
					channelIsCategory ? (
						<CategoryContextMenu
							category={channel}
							onClose={onClose}
							data-flx="app.channel-item.handle-context-menu.category-context-menu"
						/>
					) : (
						<ChannelContextMenu
							channel={channel}
							onClose={onClose}
							data-flx="app.channel-item.handle-context-menu.channel-context-menu"
						/>
					),
				);
			},
			[channel, channelIsCategory, isMobileLayout],
		);
		const dragConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dragRef(node);
			},
			[dragRef],
		);
		const dropConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dropRef(node);
				dropTargetRef.current = node as HTMLDivElement | null;
			},
			[dropRef],
		);
		const mergedRef = useMergeRefs([dragConnectorRef, elementRef]);
		const shouldShowSelectedState = !channelIsCategory && isSelected && (!channelIsVoice || isSelectedByPath);
		const hasMountedRef = useRef(false);
		useEffect(() => {
			const selectedFromThisRow = peekDirectSelection(DirectSelectionSurface.CHANNEL_LIST);
			if (shouldShowSelectedState && hasMountedRef.current && !selectedFromThisRow) {
				elementRef.current?.scrollIntoView({block: 'nearest'});
			}
			hasMountedRef.current = true;
		}, [shouldShowSelectedState]);
		const extraContent = unreadState.shouldShowUnreadIndicator ? (
			<div
				className={clsx(styles.unreadIndicator, unreadState.isUnreadIndicatorMuted && styles.unreadIndicatorMuted)}
				data-flx="app.channel-item.unread-indicator"
			/>
		) : null;
		const channelKindLabel = channelIsCategory
			? i18n._(CATEGORY_DESCRIPTOR)
			: channelIsVoice
				? i18n._(VOICE_CHANNEL_DESCRIPTOR)
				: i18n._(CHANNEL_DESCRIPTOR);
		const ariaLabelParts = [channel.name, channelKindLabel];
		if (shouldShowSelectedState) ariaLabelParts.push(i18n._(SELECTED_DESCRIPTOR));
		if (channelIsCategory)
			ariaLabelParts.push(isCollapsed ? i18n._(COLLAPSED_DESCRIPTOR) : i18n._(EXPANDED_DESCRIPTOR));
		if (!channelIsCategory) {
			if (mentionCount > 0) ariaLabelParts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
			else if (hasUnreadMessages) ariaLabelParts.push(i18n._(UNREAD_DESCRIPTOR));
			if (isMuted) ariaLabelParts.push(i18n._(MUTED_DESCRIPTOR));
			if (isVoiceSelected) ariaLabelParts.push(i18n._(CONNECTED_DESCRIPTOR));
		}
		const ariaLabel = ariaLabelParts.join(', ');
		const [isPointerHovered, setIsPointerHovered] = useState(false);
		const handleMouseEnter = useCallback(() => {
			setIsPointerHovered(true);
			scheduleChannelPreload();
		}, [scheduleChannelPreload]);
		const handleMouseLeave = useCallback(() => {
			setIsPointerHovered(false);
			cancelChannelPreload();
		}, [cancelChannelPreload]);
		const hoverAffordancesActive =
			allowHoverAffordances &&
			(contextMenuOpen || showKeyboardAffordances || shouldShowSelectedState || isPointerHovered);
		const hasVoiceUserLimit = channelIsVoice && channel.userLimit != null && channel.userLimit > 0;
		const showChatAffordance =
			allowHoverAffordances && channelIsVoice && !Accessibility.voiceChannelJoinRequiresDoubleClick;
		const hasVoiceHoverAffordances =
			allowHoverAffordances &&
			!isDraggingAnything &&
			!channelIsCategory &&
			(canInvite || canEditChannel || showChatAffordance);
		const shouldShowVoiceUserCount = hasVoiceUserLimit && !(hasVoiceHoverAffordances && hoverAffordancesActive);
		const showMentionBadge = !isSelected && hasMentions && !hoverAffordancesActive;
		const channelItemClassName = clsx(
			styles.channelItem,
			channelItemSurfaceStyles.channelItemSurface,
			shouldShowSelectedState && channelItemSurfaceStyles.channelItemSurfaceSelected,
			isAutocompleteHighlight && styles.channelItemAutocompleteHighlight,
			channelIsCategory ? styles.channelItemCategory : styles.channelItemRegular,
			!channelIsCategory && isHighlight && !shouldShowSelectedState && styles.channelItemHighlight,
			!channelIsCategory && !(isHighlight || isSelected || isVoiceSelected) && styles.channelItemMuted,
			shouldShowSelectedState && styles.channelItemSelected,
			shouldShowSelectedState && isHighlight && styles.channelItemSelectedWithUnread,
			!channelIsCategory && (!isSelected || (channelIsVoice && !isSelectedByPath)) && styles.channelItemHoverable,
			isOver && styles.channelItemOver,
			contextMenuOpen && !isSelected && !channelIsCategory && styles.channelItemContextMenu,
			contextMenuOpen && channelIsCategory && styles.channelItemCategoryContextMenu,
			isDragging && styles.channelItemDragging,
			shouldDimForVoiceDrag && !isSelected && styles.channelItemDimmed,
			isChannelDirectlyMuted && styles.channelItemMutedState,
			contextMenuOpen && styles.contextMenuOpen,
			revealActionButtons && styles.keyboardFocus,
			channelIsVoice && styles.channelItemVoice,
			hoverAffordancesActive && styles.channelItemHoverAffordancesActive,
		);
		const handleKeyDown = useCallback(
			(e: React.KeyboardEvent) => {
				if (!isKeyboardActivationKey(e.key)) return;
				e.preventDefault();
				handleSelect();
			},
			[handleSelect],
		);
		const handleFocus = useCallback(() => {
			setIsFocused(true);
			setActionModalOpen(false);
		}, []);
		const handleBlur = useCallback((e: React.FocusEvent<HTMLElement>) => {
			if (e.currentTarget.contains(e.relatedTarget as Node | null)) return;
			setIsFocused(false);
		}, []);
		const armActionModalReturn = useCallback(() => {
			if (KeyboardMode.keyboardModeEnabled) {
				setActionModalOpen(true);
			}
		}, []);
		const handleLongPress = useCallback(() => {
			if (isMobileLayout) {
				if (channelIsCategory) {
					setCategoryMenuOpen(true);
				} else {
					setMenuOpen(true);
				}
			}
		}, [isMobileLayout, channelIsCategory]);
		const handleInviteClick = useCallback(() => {
			armActionModalReturn();
			ModalCommands.push(
				modal(() => (
					<InviteModal channelId={channel.id} data-flx="app.channel-item.handle-invite-click.invite-modal" />
				)),
			);
		}, [channel.id, armActionModalReturn]);
		const handleOpenChatClick = useCallback(() => {
			collapseVoiceCallView();
			navigateToChannel();
		}, [collapseVoiceCallView, navigateToChannel]);
		const handleCreateChannelClick = useCallback(
			(e: React.MouseEvent) => {
				e.stopPropagation();
				armActionModalReturn();
				ModalCommands.push(
					modal(() => (
						<ChannelCreateModal
							guildId={guild.id}
							parentId={channel.id}
							data-flx="app.channel-item.handle-create-channel-click.channel-create-modal"
						/>
					)),
				);
			},
			[guild.id, channel.id, armActionModalReturn],
		);
		const handleChannelSettingsClick = useCallback(() => {
			armActionModalReturn();
			ModalCommands.push(
				modal(() => (
					<ChannelSettingsModal
						channelId={channel.id}
						data-flx="app.channel-item.handle-channel-settings-click.channel-settings-modal"
					/>
				)),
			);
		}, [channel.id, armActionModalReturn]);
		const channelSettingsLabel = channelIsCategory
			? i18n._(EDIT_CATEGORY_DESCRIPTOR)
			: i18n._(CHANNEL_SETTINGS_LABEL_DESCRIPTOR);
		const voiceLocked = channelIsVoice && !Permission.can(Permissions.CONNECT, channel);
		const channelIconNode = channelIsCategory
			? null
			: ChannelUtils.getIcon(
					channel,
					{
						className: clsx(
							styles.channelItemIcon,
							shouldShowSelectedState || (isHighlight && isSelected)
								? styles.channelItemIconSelected
								: isVoiceSelected && channel.type === ChannelTypes.GUILD_VOICE
									? styles.channelItemHighlight
									: isHighlight && !isSelected
										? styles.channelItemIconHighlight
										: styles.channelItemIconUnselected,
						),
					},
					voiceLocked ? {locked: true} : channelIsE2EEEncrypted ? {e2eeEncrypted: true} : undefined,
				);
		const typingIndicatorNode =
			!channelIsCategory &&
			typingUsers.length > 0 &&
			channelTypingIndicatorMode !== ChannelTypingIndicatorMode.HIDDEN &&
			!isSelected ? (
				<Tooltip
					text={() => (
						<span className={styles.typingTooltip} data-flx="app.channel-item.typing-tooltip">
							{getTypingText(i18n, typingUsers, channel)}
						</span>
					)}
					data-flx="app.channel-item.tooltip"
				>
					<div className={styles.channelTypingIndicator} data-flx="app.channel-item.channel-typing-indicator">
						<Typing
							className={styles.typingIndicatorIcon}
							size={20}
							data-flx="app.channel-item.typing-indicator-icon"
						/>
						{channelTypingIndicatorMode === ChannelTypingIndicatorMode.AVATARS && (
							<AvatarStack
								size={12}
								maxVisible={5}
								className={styles.typingAvatars}
								users={typingUsers}
								guildId={channel.guildId}
								channelId={channel.id}
								data-flx="app.channel-item.typing-avatars"
							/>
						)}
					</div>
				</Tooltip>
			) : null;
		const categoryCaretNode = channelIsCategory ? (
			<CaretDownIcon
				weight="bold"
				className={styles.categoryIcon}
				style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`}}
				data-flx="app.channel-item.category-icon"
			/>
		) : null;
		const channelActionsNode = (
			<>
				{typingIndicatorNode}
				{!isDraggingAnything && (
					<>
						{!channelIsCategory && showMentionBadge && (
							<MentionBadge mentionCount={mentionCount} size="small" data-flx="app.channel-item.mention-badge" />
						)}
						{shouldShowVoiceUserCount && channel.userLimit != null && (
							<div className={styles.voiceUserCount} data-flx="app.channel-item.voice-user-count">
								<VoiceChannelUserCount
									currentUserCount={currentUserCount}
									userLimit={channel.userLimit}
									data-flx="app.channel-item.voice-channel-user-count"
								/>
							</div>
						)}
						{showChatAffordance && (
							<div className={styles.hoverAffordance} data-flx="app.channel-item.hover-affordance--chat">
								<ChannelItemIcon
									icon={ChatTeardropIcon}
									label={i18n._(OPEN_CHAT_DESCRIPTOR)}
									selected={shouldShowSelectedState}
									onClick={handleOpenChatClick}
									tabIndex={actionButtonTabIndex}
									data-flx="app.channel-item.channel-item-icon.open-chat-click"
								/>
							</div>
						)}
						{allowHoverAffordances && canInvite && !channelIsCategory && (
							<div className={styles.hoverAffordance} data-flx="app.channel-item.hover-affordance">
								<ChannelItemIcon
									icon={UserPlusIcon}
									label={i18n._(INVITE_MEMBERS_DESCRIPTOR)}
									selected={shouldShowSelectedState}
									onClick={handleInviteClick}
									tabIndex={actionButtonTabIndex}
									data-flx="app.channel-item.channel-item-icon.invite-click"
								/>
							</div>
						)}
						{allowHoverAffordances && channelIsCategory && canManageChannels && (
							<div className={styles.hoverAffordance} data-flx="app.channel-item.hover-affordance--2">
								<Tooltip text={i18n._(CREATE_CHANNEL_DESCRIPTOR)} data-flx="app.channel-item.tooltip--2">
									<FocusRing offset={-2} data-flx="app.channel-item.focus-ring">
										<button
											type="button"
											tabIndex={actionButtonTabIndex}
											className={styles.createChannelButton}
											aria-label={i18n._(CREATE_CHANNEL_DESCRIPTOR)}
											onClick={handleCreateChannelClick}
											onKeyDown={stopPropagationOnEnterSpace}
											data-flx="app.channel-item.create-channel-button.create-channel-click"
										>
											<PlusIcon
												weight="bold"
												className={styles.createChannelIcon}
												data-flx="app.channel-item.create-channel-icon"
											/>
										</button>
									</FocusRing>
								</Tooltip>
							</div>
						)}
						{allowHoverAffordances && canEditChannel && (
							<div className={styles.hoverAffordance} data-flx="app.channel-item.hover-affordance--3">
								<ChannelItemIcon
									icon={GearIcon}
									label={channelSettingsLabel}
									selected={shouldShowSelectedState}
									onClick={handleChannelSettingsClick}
									tabIndex={actionButtonTabIndex}
									data-flx="app.channel-item.channel-item-icon.channel-settings-click"
								/>
							</div>
						)}
					</>
				)}
				{categoryCaretNode}
			</>
		);
		const channelItem = (
			<GenericChannelItem
				innerRef={mergedRef}
				ref={dropConnectorRef as React.Ref<HTMLDivElement>}
				containerClassName={clsx(
					styles.container,
					styles.channelReorderTarget,
					channelIsCategory && styles.channelCategoryReorderTarget,
				)}
				extraContent={extraContent}
				isOver={isOver}
				dropIndicator={dropIndicator}
				disabled={!isMobileLayout}
				data-dnd-name={channel.name}
				dataScrollIndicator={scrollIndicatorSeverity}
				dataScrollId={scrollIndicatorId}
				aria-label={ariaLabel}
				aria-current={shouldShowSelectedState ? 'page' : undefined}
				aria-expanded={channelIsCategory ? !isCollapsed : undefined}
				className={channelItemClassName}
				onClick={handleSelect}
				onDoubleClick={handleDoubleClick}
				onContextMenu={handleContextMenu}
				onKeyDown={handleKeyDown}
				onFocus={handleFocus}
				onBlur={handleBlur}
				onLongPress={handleLongPress}
				onMouseEnter={handleMouseEnter}
				onMouseLeave={handleMouseLeave}
				data-flx="app.channel-item.generic-channel-item.select"
			>
				<ChannelItemContent
					icon={channelIconNode}
					name={channel.name ?? ''}
					actions={channelActionsNode}
					isCategory={channelIsCategory}
					data-flx="app.channel-item.channel-item-content"
				/>
			</GenericChannelItem>
		);
		const handleCloseMenu = useCallback(() => setMenuOpen(false), []);
		const handleCloseCategoryMenu = useCallback(() => setCategoryMenuOpen(false), []);
		return (
			<>
				{channelItem}
				{isMobileLayout && !channelIsCategory && (
					<ChannelBottomSheet
						isOpen={menuOpen}
						onClose={handleCloseMenu}
						channel={channel}
						guild={guild}
						data-flx="app.channel-item.channel-bottom-sheet"
					/>
				)}
				{isMobileLayout && channelIsCategory && (
					<CategoryBottomSheet
						isOpen={categoryMenuOpen}
						onClose={handleCloseCategoryMenu}
						category={channel}
						data-flx="app.channel-item.category-bottom-sheet"
					/>
				)}
			</>
		);
	},
);
