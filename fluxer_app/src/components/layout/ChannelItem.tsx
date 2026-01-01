/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, GearIcon, PlusIcon, UserPlusIcon} from '@phosphor-icons/react';

import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as LayoutActionCreators from '~/actions/LayoutActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';

import {ChannelTypes, Permissions} from '~/Constants';

import {ChannelBottomSheet} from '~/components/bottomsheets/ChannelBottomSheet';
import {VoiceLobbyBottomSheet} from '~/components/bottomsheets/VoiceLobbyBottomSheet';
import {Typing} from '~/components/channel/Typing';
import {getTypingText, usePresentableTypingUsers} from '~/components/channel/TypingUsers';
import {GenericChannelItem} from '~/components/layout/GenericChannelItem';
import {ChannelCreateModal} from '~/components/modals/ChannelCreateModal';
import {ChannelSettingsModal} from '~/components/modals/ChannelSettingsModal';
import {ExternalLinkWarningModal} from '~/components/modals/ExternalLinkWarningModal';
import {InviteModal} from '~/components/modals/InviteModal';
import {Avatar} from '~/components/uikit/Avatar';
import {AvatarStack} from '~/components/uikit/avatars/AvatarStack';
import {CategoryContextMenu} from '~/components/uikit/ContextMenu/CategoryContextMenu';
import {ChannelContextMenu} from '~/components/uikit/ContextMenu/ChannelContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MentionBadge} from '~/components/uikit/MentionBadge';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';

import {useConnectedVoiceSession} from '~/hooks/useConnectedVoiceSession';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {useTextOverflow} from '~/hooks/useTextOverflow';

import {useLocation} from '~/lib/router';

import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';

import AccessibilityStore, {ChannelTypingIndicatorMode} from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import AutocompleteStore from '~/stores/AutocompleteStore';
import ChannelStore from '~/stores/ChannelStore';
import ContextMenuStore, {isContextMenuNodeTarget} from '~/stores/ContextMenuStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import TrustedDomainStore from '~/stores/TrustedDomainStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

import * as ChannelUtils from '~/utils/ChannelUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {stopPropagationOnEnterSpace} from '~/utils/KeyboardUtils';
import {openExternalUrl} from '~/utils/NativeUtils';
import * as PermissionUtils from '~/utils/PermissionUtils';
import * as RouterUtils from '~/utils/RouterUtils';

import styles from './ChannelItem.module.css';
import {ChannelItemIcon} from './ChannelItemIcon';
import channelItemSurfaceStyles from './ChannelItemSurface.module.css';
import type {ScrollIndicatorSeverity} from './ScrollIndicatorOverlay';
import {DND_TYPES, type DragItem, type DropResult} from './types/dnd';
import {isCategory, isTextChannel} from './utils/channelOrganization';
import {VoiceChannelUserCount} from './VoiceChannelUserCount';

export interface ChannelItemCoreProps {
	channel: {
		name: string;
		type: number;
	};
	isSelected?: boolean;
	typingIndicator?: React.ReactNode;
	className?: string;
}

export const ChannelItemCore: React.FC<ChannelItemCoreProps> = observer(
	({channel, isSelected = false, typingIndicator, className}) => {
		const channelLabelRef = React.useRef<HTMLSpanElement>(null);
		const isChannelNameOverflowing = useTextOverflow(channelLabelRef);

		return (
			<div
				className={clsx(
					styles.channelItemCore,
					isSelected ? styles.channelItemCoreSelected : styles.channelItemCoreUnselected,
					className,
				)}
			>
				<Tooltip text={channel.name}>
					<div>
						{ChannelUtils.getIcon(channel, {
							className: clsx(
								styles.channelItemIcon,
								isSelected ? styles.channelItemIconSelected : styles.channelItemIconUnselected,
							),
						})}
					</div>
				</Tooltip>
				<Tooltip text={isChannelNameOverflowing ? channel.name : ''}>
					<span ref={channelLabelRef} className={styles.channelItemLabel}>
						{channel.name}
					</span>
				</Tooltip>
				<div className={styles.channelItemActions}>{typingIndicator}</div>
			</div>
		);
	},
);

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
	}: {
		guild: GuildRecord;
		channel: ChannelRecord;
		isCollapsed?: boolean;
		onToggle?: () => void;
		isDraggingAnything: boolean;
		activeDragItem?: DragItem | null;
		onChannelDrop?: (item: DragItem, result: DropResult) => void;
		onDragStateChange?: (item: DragItem | null) => void;
	}) => {
		const {t} = useLingui();
		const elementRef = React.useRef<HTMLDivElement | null>(null);
		const [contextMenuOpen, setContextMenuOpen] = React.useState(false);
		const categoryNameRef = React.useRef<HTMLSpanElement>(null);
		const channelNameRef = React.useRef<HTMLSpanElement>(null);

		const isCategoryNameOverflowing = useTextOverflow(categoryNameRef);
		const isChannelNameOverflowing = useTextOverflow(channelNameRef);

		const channelIsCategory = isCategory(channel);
		const channelIsVoice = channel.type === ChannelTypes.GUILD_VOICE;
		const channelIsText = isTextChannel(channel);
		const draggingChannel = activeDragItem?.type === DND_TYPES.CHANNEL ? activeDragItem : null;
		const isVoiceDragActive = draggingChannel?.channelType === ChannelTypes.GUILD_VOICE;
		const shouldDimForVoiceDrag = Boolean(isVoiceDragActive && channelIsText && channel.parentId !== null);
		const location = useLocation();
		const channelPath = `/channels/${guild.id}/${channel.id}`;
		const unreadCount = ReadStateStore.getUnreadCount(channel.id);
		const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guild.id);
		const {guildId: connectedVoiceGuildId, channelId: connectedVoiceChannelId} = useConnectedVoiceSession();
		const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, channel);
		const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);
		const mobileLayout = MobileLayoutStore;
		const isMuted = UserGuildSettingsStore.isChannelMuted(guild.id, channel.id);
		const voiceStatesInChannel = MediaEngineStore.getAllVoiceStatesInChannel(guild.id, channel.id);
		const currentUserCount = Object.keys(voiceStatesInChannel).length;
		const isMobileLayout = MobileLayoutStore.isMobileLayout();
		const allowHoverAffordances = !isMobileLayout;
		const [menuOpen, setMenuOpen] = useState(false);
		const [voiceLobbyOpen, setVoiceLobbyOpen] = useState(false);
		const lastClickTime = React.useRef<number>(0);
		const voiceChannelJoinRequiresDoubleClick = AccessibilityStore.voiceChannelJoinRequiresDoubleClick;
		const [isFocused, setIsFocused] = useState(false);
		const {keyboardModeEnabled} = KeyboardModeStore;

		const showKeyboardAffordances = keyboardModeEnabled && isFocused;
		const currentUserId = AuthenticationStore.currentUserId;
		const currentMember = currentUserId ? GuildMemberStore.getMember(guild.id, currentUserId) : null;
		const isCurrentUserTimedOut = Boolean(currentMember?.isTimedOut());
		const voiceTooltipText =
			channelIsVoice && isCurrentUserTimedOut ? t`You can't join while you're on timeout.` : undefined;

		const isVoiceSelected =
			channel.type === ChannelTypes.GUILD_VOICE &&
			connectedVoiceGuildId === guild.id &&
			connectedVoiceChannelId === channel.id;
		const isSelected = isVoiceSelected || location.pathname.startsWith(channelPath) || selectedChannelId === channel.id;
		const mentionCount = ReadStateStore.getMentionCount(channel.id);
		const hasUnreadMessages = unreadCount > 0;
		const isHighlight = mentionCount > 0 || hasUnreadMessages;
		const scrollIndicatorSeverity: ScrollIndicatorSeverity | undefined = channelIsCategory
			? undefined
			: mentionCount > 0
				? 'mention'
				: hasUnreadMessages
					? 'unread'
					: undefined;
		const scrollIndicatorId = `channel-${channel.id}`;
		const isAutocompleteHighlight = AutocompleteStore.highlightChannelId === channel.id;
		const typingUsers = usePresentableTypingUsers(channel);
		const channelTypingIndicatorMode = AccessibilityStore.channelTypingIndicatorMode;
		const showSelectedChannelTypingIndicator = AccessibilityStore.showSelectedChannelTypingIndicator;

		const [dropIndicator, setDropIndicator] = React.useState<{position: 'top' | 'bottom'; isValid: boolean} | null>(
			null,
		);

		const dragItemData = React.useMemo<DragItem>(
			() => ({
				type: channelIsCategory ? DND_TYPES.CATEGORY : DND_TYPES.CHANNEL,
				id: channel.id,
				channelType: channel.type,
				parentId: channel.parentId,
				guildId: guild.id,
			}),
			[channelIsCategory, channel.id, channel.type, channel.parentId, guild.id],
		);

		const [{isDragging}, dragRef, preview] = useDrag(
			() => ({
				type: dragItemData.type,
				item: () => {
					onDragStateChange?.(dragItemData);
					return dragItemData;
				},
				canDrag: canManageChannels && !mobileLayout.enabled,
				collect: (monitor) => ({isDragging: monitor.isDragging()}),
				end: () => {
					onDragStateChange?.(null);
					setDropIndicator(null);
				},
			}),
			[dragItemData, canManageChannels, mobileLayout.enabled, onDragStateChange],
		);

		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: [DND_TYPES.CHANNEL, DND_TYPES.CATEGORY, DND_TYPES.VOICE_PARTICIPANT],
				canDrop: (item: DragItem) => {
					if (item.id === channel.id) return false;
					if (channelIsCategory && item.type === DND_TYPES.CHANNEL && item.parentId !== null) return false;
					if (item.type === DND_TYPES.VOICE_PARTICIPANT) return channelIsVoice;
					if (item.type === DND_TYPES.CHANNEL) {
						if (item.channelType === ChannelTypes.GUILD_VOICE) {
							if (!channelIsCategory && !channelIsVoice && !channelIsText) return false;
						}
						if (item.channelType !== ChannelTypes.GUILD_VOICE && channelIsVoice) return false;
					}
					if (item.type === DND_TYPES.CATEGORY && channel.parentId !== null && !channelIsCategory) return false;
					return true;
				},
				hover: (_item: DragItem, monitor) => {
					const node = elementRef.current;
					if (!node) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
					const hoverClientY = clientOffset.y - hoverBoundingRect.top;
					setDropIndicator({
						position: hoverClientY < hoverMiddleY ? 'top' : 'bottom',
						isValid: monitor.canDrop(),
					});
				},
				drop: (item: DragItem, monitor): DropResult | undefined => {
					if (!monitor.canDrop()) {
						setDropIndicator(null);
						return;
					}
					if (item.type === DND_TYPES.VOICE_PARTICIPANT && channelIsVoice) {
						const canMove = PermissionStore.can(Permissions.MOVE_MEMBERS, {guildId: guild.id});
						if (!canMove || item.currentChannelId === channel.id) {
							setDropIndicator(null);
							return;
						}
						const targetChannel = ChannelStore.getChannel(channel.id);
						if (targetChannel) {
							const canTargetConnect = PermissionUtils.can(Permissions.CONNECT, item.userId!, targetChannel.toJSON());
							if (!canTargetConnect) {
								setDropIndicator(null);
								return;
							}
						}
						void GuildMemberActionCreators.update(guild.id, item.userId!, {channel_id: channel.id});
						setDropIndicator(null);
						return;
					}
					const node = elementRef.current;
					if (!node) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
					const hoverClientY = clientOffset.y - hoverBoundingRect.top;
					let result: DropResult;
					if (channelIsCategory) {
						result = {
							targetId: channel.id,
							position: hoverClientY < hoverMiddleY ? 'before' : 'inside',
							targetParentId: hoverClientY < hoverMiddleY ? channel.parentId : channel.id,
						};
					} else {
						result = {
							targetId: channel.id,
							position: hoverClientY < hoverMiddleY ? 'before' : 'after',
							targetParentId: channel.parentId,
						};
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
			[channel.id, channel.type, channel.parentId, guild.id, channelIsCategory, channelIsVoice, onChannelDrop],
		);

		React.useEffect(() => {
			if (!isOver) setDropIndicator(null);
		}, [isOver]);

		React.useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);

		React.useEffect(() => {
			const disposer = autorun(() => {
				const contextMenu = ContextMenuStore.contextMenu;
				const contextMenuTarget = contextMenu?.target?.target ?? null;
				const element = elementRef.current;
				const isOpen =
					Boolean(contextMenu) &&
					isContextMenuNodeTarget(contextMenuTarget) &&
					Boolean(element?.contains(contextMenuTarget));
				setContextMenuOpen(!!isOpen);
			});
			return () => disposer();
		}, []);

		const handleSelect = useCallback(() => {
			if (channel.type === ChannelTypes.GUILD_VOICE && isCurrentUserTimedOut) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`You can't join while you're on timeout.`,
				});
				return;
			}
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				onToggle?.();
				return;
			}
			if (channel.type === ChannelTypes.GUILD_LINK && channel.url) {
				try {
					const parsed = new URL(channel.url);
					const isTrusted = TrustedDomainStore.isTrustedDomain(parsed.hostname);
					if (!isTrusted) {
						ModalActionCreators.push(
							modal(() => <ExternalLinkWarningModal url={channel.url!} hostname={parsed.hostname} />),
						);
					} else {
						void openExternalUrl(channel.url);
					}
				} catch {}
				return;
			}
			if (channel.type === ChannelTypes.GUILD_VOICE) {
				if (isMobileLayout) {
					setVoiceLobbyOpen(true);
					return;
				}
				if (isVoiceSelected) {
					RouterUtils.transitionTo(channelPath);
					if (MobileLayoutStore.isMobileLayout()) {
						LayoutActionCreators.updateMobileLayoutState(false, true);
					}
				} else {
					if (voiceChannelJoinRequiresDoubleClick) {
						const now = Date.now();
						const timeSinceLastClick = now - lastClickTime.current;
						lastClickTime.current = now;

						if (timeSinceLastClick < 500) {
							void MediaEngineStore.connectToVoiceChannel(guild.id, channel.id);
						} else {
							RouterUtils.transitionTo(channelPath);
							if (MobileLayoutStore.isMobileLayout()) {
								LayoutActionCreators.updateMobileLayoutState(false, true);
							}
						}
					} else {
						void MediaEngineStore.connectToVoiceChannel(guild.id, channel.id);
					}
				}
				return;
			}
			RouterUtils.transitionTo(channelPath);
			if (MobileLayoutStore.isMobileLayout()) {
				LayoutActionCreators.updateMobileLayoutState(false, true);
			}
		}, [
			channel,
			channelPath,
			guild.id,
			isVoiceSelected,
			onToggle,
			isMobileLayout,
			voiceChannelJoinRequiresDoubleClick,
		]);

		const handleContextMenu = useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();

				if (isMobileLayout) {
					return;
				}

				ContextMenuActionCreators.openFromEvent(event, ({onClose}) =>
					channelIsCategory ? (
						<CategoryContextMenu category={channel} onClose={onClose} />
					) : (
						<ChannelContextMenu channel={channel} onClose={onClose} />
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
			},
			[dropRef],
		);
		const mergedRef = useMergeRefs([dragConnectorRef, dropConnectorRef, elementRef]);

		const shouldShowSelectedState =
			!channelIsCategory &&
			isSelected &&
			(channel.type !== ChannelTypes.GUILD_VOICE || location.pathname.startsWith(channelPath));

		const hasMountedRef = React.useRef(false);

		React.useEffect(() => {
			if (shouldShowSelectedState && hasMountedRef.current) {
				elementRef.current?.scrollIntoView({block: 'nearest'});
			}
			hasMountedRef.current = true;
		}, [shouldShowSelectedState]);

		const channelItem = (
			<GenericChannelItem
				innerRef={mergedRef}
				containerClassName={styles.container}
				extraContent={hasUnreadMessages && <div className={styles.unreadIndicator} />}
				isOver={isOver}
				dropIndicator={dropIndicator}
				disabled={!isMobileLayout}
				data-dnd-name={channel.name}
				dataScrollIndicator={scrollIndicatorSeverity}
				dataScrollId={scrollIndicatorId}
				aria-label={`${channel.name} ${channelIsCategory ? 'category' : 'channel'}`}
				className={clsx(
					styles.channelItem,
					channelItemSurfaceStyles.channelItemSurface,
					shouldShowSelectedState && channelItemSurfaceStyles.channelItemSurfaceSelected,
					isAutocompleteHighlight && styles.channelItemAutocompleteHighlight,
					channelIsCategory ? styles.channelItemCategory : styles.channelItemRegular,
					!channelIsCategory && isHighlight && !shouldShowSelectedState && styles.channelItemHighlight,
					!channelIsCategory && !(isHighlight || isSelected || isVoiceSelected) && styles.channelItemMuted,
					shouldShowSelectedState && styles.channelItemSelected,
					shouldShowSelectedState && isHighlight && styles.channelItemSelectedWithUnread,
					!channelIsCategory &&
						(!isSelected ||
							(channel.type === ChannelTypes.GUILD_VOICE && !location.pathname.startsWith(channelPath))) &&
						styles.channelItemHoverable,
					isOver && styles.channelItemOver,
					contextMenuOpen && !isSelected && !channelIsCategory && styles.channelItemContextMenu,
					contextMenuOpen && channelIsCategory && styles.channelItemCategoryContextMenu,
					isDragging && styles.channelItemDragging,
					shouldDimForVoiceDrag && !isSelected && styles.channelItemDimmed,
					isMuted && styles.channelItemMutedState,
					contextMenuOpen && styles.contextMenuOpen,
					showKeyboardAffordances && styles.keyboardFocus,
					channelIsVoice && styles.channelItemVoice,
				)}
				onClick={handleSelect}
				onContextMenu={handleContextMenu}
				onKeyDown={(e) => e.key === 'Enter' && handleSelect()}
				onFocus={() => setIsFocused(true)}
				onBlur={() => setIsFocused(false)}
				onLongPress={() => {
					if (isMobileLayout) setMenuOpen(true);
				}}
			>
				{!channelIsCategory && (
					<Tooltip text={ChannelUtils.getName(channel)}>
						<div>
							{ChannelUtils.getIcon(channel, {
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
							})}
						</div>
					</Tooltip>
				)}
				{channelIsCategory ? (
					<div className={styles.categoryContent}>
						<Tooltip text={isCategoryNameOverflowing && channel.name ? channel.name : ''}>
							<span ref={categoryNameRef} className={styles.categoryName}>
								{channel.name ?? ''}
							</span>
						</Tooltip>
						<CaretDownIcon
							weight="bold"
							className={styles.categoryIcon}
							style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`}}
						/>
					</div>
				) : (
					<Tooltip text={isChannelNameOverflowing && channel.name ? channel.name : ''}>
						<span ref={channelNameRef} className={styles.channelName}>
							{channel.name ?? ''}
						</span>
					</Tooltip>
				)}
				{!isDraggingAnything && (
					<div className={styles.channelItemActions}>
						{!channelIsCategory && channel.type !== ChannelTypes.GUILD_VOICE && (
							<>
								{typingUsers.length > 0 &&
									channelTypingIndicatorMode !== ChannelTypingIndicatorMode.HIDDEN &&
									(showSelectedChannelTypingIndicator || !isSelected) && (
										<Tooltip
											text={() => (
												<span className={styles.typingTooltip}>{getTypingText(t, typingUsers, channel)}</span>
											)}
										>
											<div className={styles.channelTypingIndicator}>
												<Typing className={styles.typingIndicatorIcon} size={20} />
												{channelTypingIndicatorMode === ChannelTypingIndicatorMode.AVATARS && (
													<AvatarStack size={12} maxVisible={5} className={styles.typingAvatars}>
														{typingUsers.map((user) => (
															<Avatar key={user.id} user={user} size={12} guildId={channel.guildId} />
														))}
													</AvatarStack>
												)}
											</div>
										</Tooltip>
									)}
								{!isSelected && <MentionBadge mentionCount={mentionCount} size="small" />}
							</>
						)}
						{channelIsVoice && channel.userLimit != null && channel.userLimit > 0 && (
							<div className={styles.voiceUserCount}>
								<VoiceChannelUserCount currentUserCount={currentUserCount} userLimit={channel.userLimit} />
							</div>
						)}
						{allowHoverAffordances && canInvite && !channelIsCategory && (
							<div className={styles.hoverAffordance}>
								<ChannelItemIcon
									icon={UserPlusIcon}
									label={t`Invite Members`}
									selected={shouldShowSelectedState}
									onClick={() => ModalActionCreators.push(modal(() => <InviteModal channelId={channel.id} />))}
								/>
							</div>
						)}
						{allowHoverAffordances && channelIsCategory && canManageChannels && (
							<div className={styles.hoverAffordance}>
								<Tooltip text={t`Create Channel`}>
									<FocusRing offset={-2}>
										<button
											type="button"
											className={styles.createChannelButton}
											onClick={(e) => {
												e.stopPropagation();
												ModalActionCreators.push(
													modal(() => <ChannelCreateModal guildId={guild.id} parentId={channel.id} />),
												);
											}}
											onKeyDown={stopPropagationOnEnterSpace}
										>
											<PlusIcon weight="bold" className={styles.createChannelIcon} />
										</button>
									</FocusRing>
								</Tooltip>
							</div>
						)}
						{allowHoverAffordances && canManageChannels && (
							<div className={styles.hoverAffordance}>
								<ChannelItemIcon
									icon={GearIcon}
									label={channelIsCategory ? t`Edit Category` : t`Channel Settings`}
									selected={shouldShowSelectedState}
									onClick={() => ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={channel.id} />))}
								/>
							</div>
						)}
					</div>
				)}
			</GenericChannelItem>
		);
		const channelWrapper = voiceTooltipText ? (
			<Tooltip text={voiceTooltipText} position="top">
				{channelItem}
			</Tooltip>
		) : (
			channelItem
		);

		return (
			<>
				{channelWrapper}
				{isMobileLayout && (
					<ChannelBottomSheet isOpen={menuOpen} onClose={() => setMenuOpen(false)} channel={channel} guild={guild} />
				)}
				{isMobileLayout && channel.type === ChannelTypes.GUILD_VOICE && (
					<VoiceLobbyBottomSheet
						isOpen={voiceLobbyOpen}
						onClose={() => setVoiceLobbyOpen(false)}
						channel={channel}
						guild={guild}
					/>
				)}
			</>
		);
	},
);
