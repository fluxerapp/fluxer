// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import channelItemStyles from '@app/features/app/components/layout/ChannelItem.module.css';
import {ChannelItemIcon} from '@app/features/app/components/layout/ChannelItemIcon';
import channelItemSurfaceStyles from '@app/features/app/components/layout/ChannelItemSurface.module.css';
import styles from '@app/features/app/components/layout/ChannelListContent.module.css';
import {
	DirectSelectionSurface,
	markDirectSelection,
	peekDirectSelection,
} from '@app/features/app/components/layout/DirectSelectionOrigin';
import {computeVerticalDropPosition} from '@app/features/app/components/layout/dnd/DndDropPosition';
import {useDragTargetRect} from '@app/features/app/components/layout/dnd/useDragTargetRect';
import favoritesChannelListStyles from '@app/features/app/components/layout/FavoritesChannelListContent.module.css';
import {GenericChannelItem} from '@app/features/app/components/layout/GenericChannelItem';
import {DragItemType} from '@app/features/app/components/layout/types/DndTypes';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {GuildChannelListSkeleton} from '@app/features/app/components/skeleton/GuildSidebarSkeleton';
import {useChannelHoverPreload} from '@app/features/app/hooks/useChannelHoverPreload';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {AddFavoriteChannelModal} from '@app/features/expressions/components/modals/AddFavoriteChannelModal';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {ADD_CHANNEL_DESCRIPTOR, INVITE_PEOPLE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Favorites, {type FavoriteChannel} from '@app/features/messaging/state/Favorites';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import ReadStates from '@app/features/read_state/state/ReadStates';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {FavoritesCategoryContextMenu} from '@app/features/ui/action_menu/FavoritesCategoryContextMenu';
import {FavoritesChannelContextMenu} from '@app/features/ui/action_menu/FavoritesChannelContextMenu';
import {FavoritesChannelListContextMenu} from '@app/features/ui/action_menu/FavoritesChannelListContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, PlusIcon, UserPlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDragLayer, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const EMPTY_FAVORITES_DESCRIPTOR = msg({
	message: 'Empty favorites',
	comment: 'Short label in the app layout favorites channel list content.',
});
type ChannelDragItem = {
	type: typeof DragItemType.FAVORITES_CHANNEL;
	channelId: string;
	parentId: string | null;
};
type CategoryDragItem = {
	type: typeof DragItemType.FAVORITES_CATEGORY;
	categoryId: string;
};
type DragItem = ChannelDragItem | CategoryDragItem;

const isCategoryDragItem = (item: DragItem): item is CategoryDragItem => item.type === DragItemType.FAVORITES_CATEGORY;
const isChannelDragItem = (item: DragItem): item is ChannelDragItem => item.type === DragItemType.FAVORITES_CHANNEL;

type FavoriteDropIndicator = {position: 'top' | 'bottom'; isValid: boolean};

interface FavoriteChannelGroup {
	category: {id: string; name: string} | null;
	channels: Array<{
		favoriteChannel: FavoriteChannel;
		channel: Channel;
		guild: Guild | null;
	}>;
}

const FavoriteChannelResolvedItem = observer(
	({favoriteChannel, channel, guild}: {favoriteChannel: FavoriteChannel; channel: Channel; guild: Guild | null}) => {
		const {i18n} = useLingui();
		const elementRef = useRef<HTMLDivElement | null>(null);
		const getDropTargetRect = useDragTargetRect(elementRef);
		const [dropIndicator, setDropIndicator] = useState<FavoriteDropIndicator | null>(null);
		const setFavoriteDropIndicator = useCallback((indicator: FavoriteDropIndicator | null) => {
			setDropIndicator((current) => {
				if (
					current?.position === indicator?.position &&
					current?.isValid === indicator?.isValid &&
					(current === null) === (indicator === null)
				) {
					return current;
				}
				return indicator;
			});
		}, []);
		const resetFavoriteDropIndicator = useCallback(() => {
			setFavoriteDropIndicator(null);
		}, [setFavoriteDropIndicator]);
		const location = useLocation();
		const isSelected = location.pathname === Routes.favoritesChannel(favoriteChannel.channelId);
		const shouldShowSelectedState = isSelected;
		useEffect(() => {
			const selectedFromThisRow = peekDirectSelection(DirectSelectionSurface.FAVORITES_LIST);
			if (isSelected && !selectedFromThisRow) {
				elementRef.current?.scrollIntoView({block: 'nearest'});
			}
		}, [isSelected]);
		const [isFocused, setIsFocused] = useState(false);
		const {keyboardModeEnabled} = KeyboardMode;
		const showKeyboardAffordances = keyboardModeEnabled && isFocused;
		const [{isDragging}, dragRef, preview] = useDrag<ChannelDragItem, unknown, {isDragging: boolean}>({
			type: DragItemType.FAVORITES_CHANNEL,
			item: {
				type: DragItemType.FAVORITES_CHANNEL,
				channelId: favoriteChannel.channelId,
				parentId: favoriteChannel.parentId,
			},
			collect: (monitor) => ({
				isDragging: monitor.isDragging(),
			}),
			end: resetFavoriteDropIndicator,
		});
		const [{isOver}, dropRef] = useDrop<ChannelDragItem, unknown, {isOver: boolean}>({
			accept: DragItemType.FAVORITES_CHANNEL,
			hover: (_item, monitor) => {
				const clientOffset = monitor.getClientOffset();
				if (!clientOffset) return;
				const hoverBoundingRect = getDropTargetRect();
				if (!hoverBoundingRect) return;
				const dropPos = computeVerticalDropPosition(clientOffset, hoverBoundingRect);
				setFavoriteDropIndicator({
					position: dropPos === 'before' ? 'top' : 'bottom',
					isValid: true,
				});
			},
			drop: (item, monitor) => {
				resetFavoriteDropIndicator();
				if (item.channelId === favoriteChannel.channelId) return;
				const node = elementRef.current;
				if (!node) return;
				const hoverBoundingRect = node.getBoundingClientRect();
				const clientOffset = monitor.getClientOffset();
				if (!clientOffset) return;
				const dropPos = computeVerticalDropPosition(clientOffset, hoverBoundingRect);
				const position = dropPos === 'center' ? 'after' : dropPos;
				const channels = Favorites.getChannelsInCategory(favoriteChannel.parentId);
				let targetIndex = channels.findIndex((ch) => ch.channelId === favoriteChannel.channelId);
				if (targetIndex !== -1) {
					if (position === 'after') {
						targetIndex += 1;
					}
					Favorites.moveChannel(item.channelId, favoriteChannel.parentId, targetIndex);
				}
			},
			collect: (monitor) => ({
				isOver: monitor.isOver(),
			}),
		});
		useEffect(() => {
			if (!isOver) resetFavoriteDropIndicator();
		}, [isOver, resetFavoriteDropIndicator]);
		useEffect(() => {
			preview(getEmptyImage());
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
		const refs = useMergeRefs([dragConnectorRef, dropConnectorRef, elementRef]);
		const unreadCount = ReadStates.getUnreadCount(channel.id);
		const mentionCount = ReadStates.getMentionCount(channel.id);
		const isGroupDM = channel.isGroupDM();
		const isDM = channel.isDM();
		const recipientId = isDM ? (channel.recipientIds[0] ?? '') : '';
		const recipient = recipientId ? (Users.getUser(recipientId) ?? null) : null;
		const isTyping = recipientId ? TypingIndicator.isTyping(channel.id, recipientId) : false;
		const channelDisplayName = channel.isPrivate() ? ChannelUtils.getDMDisplayName(channel) : channel.name;
		const displayName = favoriteChannel.nickname || channelDisplayName || i18n._(UNKNOWN_CHANNEL_DESCRIPTOR);
		const isChannelDirectlyMuted = channel.guildId
			? UserGuildSettings.isChannelMuted(channel.guildId, channel.id)
			: false;
		const isMuted =
			isChannelDirectlyMuted ||
			(channel.guildId ? UserGuildSettings.isCategoryMuted(channel.guildId, channel.id) : false);
		const unreadBadgesLevel = channel.guildId
			? UserGuildSettings.resolvedUnreadBadgesLevel({
					id: channel.id,
					guildId: channel.guildId,
					parentId: channel.parentId ?? undefined,
					type: channel.type,
				})
			: null;
		const unreadState = getChannelUnreadState({
			unreadCount,
			mentionCount,
			isMuted,
			showFadedUnreadOnMutedChannels: Accessibility.showFadedUnreadOnMutedChannels,
			unreadBadgesLevel,
		});
		const {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow} = useChannelHoverPreload({
			channel,
			guild,
			defaultHiddenForChannel: channel.type === ChannelTypes.GUILD_VOICE,
		});
		const handleClick = () => {
			if (LinkChannelCommands.openLinkChannel(channel)) {
				return;
			}
			markDirectSelection(DirectSelectionSurface.FAVORITES_LIST);
			preloadChannelNow();
			NavigationCommands.selectChannel(FAVORITES_GUILD_ID, favoriteChannel.channelId);
		};
		const handleContextMenu = (event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<FavoritesChannelContextMenu
					favoriteChannel={favoriteChannel}
					channel={channel}
					guild={guild}
					onClose={onClose}
					data-flx="app.favorites-channel-list-content.handle-context-menu.favorites-channel-context-menu"
				/>
			));
		};
		const handleInvite = () => {
			ModalCommands.push(
				modal(() => (
					<InviteModal
						channelId={channel.id}
						data-flx="app.favorites-channel-list-content.handle-invite.invite-modal"
					/>
				)),
			);
		};
		const canInvite = channel.guildId && InviteUtils.canInviteToChannel(channel.id, channel.guildId);
		return (
			<GenericChannelItem
				ref={refs}
				containerClassName={favoritesChannelListStyles.favoriteItemContainer}
				style={{opacity: isDragging ? 0.5 : 1}}
				isOver={isOver}
				dropIndicator={dropIndicator}
				extraContent={
					unreadState.shouldShowUnreadIndicator ? (
						<div
							className={clsx(
								channelItemStyles.unreadIndicator,
								unreadState.isUnreadIndicatorMuted && channelItemStyles.unreadIndicatorMuted,
							)}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.div--2"
						/>
					) : null
				}
				className={clsx(
					favoritesChannelListStyles.favoriteItem,
					shouldShowSelectedState && favoritesChannelListStyles.favoriteItemSelected,
					!shouldShowSelectedState && favoritesChannelListStyles.favoriteItemDefault,
					isOver && favoritesChannelListStyles.favoriteItemOver,
					isChannelDirectlyMuted && favoritesChannelListStyles.favoriteItemMuted,
					showKeyboardAffordances && favoritesChannelListStyles.keyboardFocus,
				)}
				isSelected={shouldShowSelectedState}
				onClick={handleClick}
				onContextMenu={handleContextMenu}
				onKeyDown={(e) => {
					if (!isKeyboardActivationKey(e.key)) return;
					e.preventDefault();
					handleClick();
				}}
				onFocus={() => setIsFocused(true)}
				onBlur={() => setIsFocused(false)}
				onLongPress={() => {}}
				onMouseEnter={scheduleChannelPreload}
				onMouseLeave={cancelChannelPreload}
				data-flx="app.favorites-channel-list-content.favorite-channel-item.generic-channel-item.click"
			>
				<div
					className={favoritesChannelListStyles.avatarContainer}
					data-flx="app.favorites-channel-list-content.favorite-channel-item.div--3"
				>
					{isGroupDM ? (
						<GroupDMAvatar
							channel={channel}
							size={24}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.group-dm-avatar"
						/>
					) : recipient ? (
						<StatusAwareAvatar
							user={recipient}
							size={24}
							isTyping={isTyping}
							showOffline={true}
							className={favoritesChannelListStyles.avatar}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.status-aware-avatar"
						/>
					) : guild ? (
						<GuildIcon
							id={guild.id}
							name={guild.name}
							icon={guild.icon}
							className={favoritesChannelListStyles.avatar}
							sizePx={24}
							containerProps={{'aria-hidden': true}}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.guild-icon"
						/>
					) : (
						<div
							className={favoritesChannelListStyles.avatarPlaceholder}
							aria-hidden
							data-flx="app.favorites-channel-list-content.favorite-channel-item.div--4"
						>
							DM
						</div>
					)}
					{!channel.isPrivate() && (
						<div
							className={clsx(
								favoritesChannelListStyles.channelBadge,
								shouldShowSelectedState && favoritesChannelListStyles.channelBadgeSelected,
							)}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.div--5"
						>
							{ChannelUtils.getIcon(channel, {
								className: clsx(
									favoritesChannelListStyles.channelBadgeIcon,
									shouldShowSelectedState && favoritesChannelListStyles.channelBadgeSelectedIcon,
								),
							})}
						</div>
					)}
				</div>
				<span
					className={favoritesChannelListStyles.displayName}
					data-flx="app.favorites-channel-list-content.favorite-channel-item.span--2"
				>
					{displayName}
				</span>
				<div
					className={favoritesChannelListStyles.actionsContainer}
					data-flx="app.favorites-channel-list-content.favorite-channel-item.div--6"
				>
					{canInvite && (
						<div
							className={favoritesChannelListStyles.hoverAffordance}
							data-flx="app.favorites-channel-list-content.favorite-channel-item.div--7"
						>
							<ChannelItemIcon
								icon={UserPlusIcon}
								label={i18n._(INVITE_PEOPLE_DESCRIPTOR)}
								onClick={handleInvite}
								selected={shouldShowSelectedState}
								data-flx="app.favorites-channel-list-content.favorite-channel-item.channel-item-icon.invite"
							/>
						</div>
					)}
					{unreadState.hasMentions && (
						<MentionBadge
							mentionCount={mentionCount}
							size="small"
							data-flx="app.favorites-channel-list-content.favorite-channel-item.mention-badge"
						/>
					)}
				</div>
			</GenericChannelItem>
		);
	},
);
const FavoriteCategoryItem = observer(
	({
		category,
		isCollapsed,
		onToggle,
		onAddChannel,
	}: {
		category: {id: string; name: string};
		isCollapsed: boolean;
		onToggle: () => void;
		onAddChannel: () => void;
	}) => {
		const {i18n} = useLingui();
		const elementRef = useRef<HTMLDivElement | null>(null);
		const getDropTargetRect = useDragTargetRect(elementRef);
		const [dropIndicator, setDropIndicator] = useState<FavoriteDropIndicator | null>(null);
		const setCategoryDropIndicator = useCallback((indicator: FavoriteDropIndicator | null) => {
			setDropIndicator((current) => {
				if (
					current?.position === indicator?.position &&
					current?.isValid === indicator?.isValid &&
					(current === null) === (indicator === null)
				) {
					return current;
				}
				return indicator;
			});
		}, []);
		const resetCategoryDropIndicator = useCallback(() => {
			setCategoryDropIndicator(null);
		}, [setCategoryDropIndicator]);
		const [isFocused, setIsFocused] = useState(false);
		const {keyboardModeEnabled} = KeyboardMode;
		const showKeyboardAffordances = keyboardModeEnabled && isFocused;
		const [{isDragging}, dragRef, preview] = useDrag<CategoryDragItem, unknown, {isDragging: boolean}>({
			type: DragItemType.FAVORITES_CATEGORY,
			item: {
				type: DragItemType.FAVORITES_CATEGORY,
				categoryId: category.id,
			},
			collect: (monitor) => ({
				isDragging: monitor.isDragging(),
			}),
			end: resetCategoryDropIndicator,
		});
		const [{isOver, draggedItemType}, dropRef] = useDrop<
			DragItem,
			unknown,
			{isOver: boolean; draggedItemType: string | symbol | null}
		>({
			accept: [DragItemType.FAVORITES_CHANNEL, DragItemType.FAVORITES_CATEGORY],
			hover: (item, monitor) => {
				if (!isCategoryDragItem(item)) {
					resetCategoryDropIndicator();
					return;
				}
				if (item.categoryId === category.id) {
					resetCategoryDropIndicator();
					return;
				}
				const clientOffset = monitor.getClientOffset();
				if (!clientOffset) return;
				const hoverBoundingRect = getDropTargetRect();
				if (!hoverBoundingRect) return;
				const dropPos = computeVerticalDropPosition(clientOffset, hoverBoundingRect);
				setCategoryDropIndicator({
					position: dropPos === 'before' ? 'top' : 'bottom',
					isValid: true,
				});
			},
			drop: (item, monitor) => {
				resetCategoryDropIndicator();
				if (isCategoryDragItem(item)) {
					if (item.categoryId === category.id) return;
					const node = elementRef.current;
					if (!node) return;
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const hoverBoundingRect = node.getBoundingClientRect();
					const dropPos = computeVerticalDropPosition(clientOffset, hoverBoundingRect);
					const position = dropPos === 'center' ? 'after' : dropPos;
					const cats = Favorites.sortedCategories;
					let targetIndex = cats.findIndex((c) => c.id === category.id);
					if (targetIndex === -1) return;
					if (position === 'after') targetIndex += 1;
					Favorites.moveCategory(item.categoryId, targetIndex);
					return;
				}
				if (!isChannelDragItem(item)) return;
				if (item.parentId === category.id) return;
				const channels = Favorites.getChannelsInCategory(category.id);
				Favorites.moveChannel(item.channelId, category.id, channels.length);
			},
			collect: (monitor) => ({
				isOver: monitor.isOver(),
				draggedItemType: monitor.getItemType(),
			}),
		});
		const isCategoryDragOver = isOver && draggedItemType === DragItemType.FAVORITES_CATEGORY;
		const isChannelDragOver = isOver && draggedItemType === DragItemType.FAVORITES_CHANNEL;
		useEffect(() => {
			if (!isCategoryDragOver) resetCategoryDropIndicator();
		}, [isCategoryDragOver, resetCategoryDropIndicator]);
		useEffect(() => {
			preview(getEmptyImage());
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
		const refs = useMergeRefs([dragConnectorRef, dropConnectorRef, elementRef]);
		const handleContextMenu = (event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<FavoritesCategoryContextMenu
					category={category}
					onClose={onClose}
					onAddChannel={onAddChannel}
					data-flx="app.favorites-channel-list-content.handle-context-menu.favorites-category-context-menu"
				/>
			));
		};
		return (
			<GenericChannelItem
				ref={refs}
				style={{opacity: isDragging ? 0.5 : 1}}
				className={clsx(
					favoritesChannelListStyles.categoryItem,
					isChannelDragOver && favoritesChannelListStyles.favoriteItemOver,
					showKeyboardAffordances && favoritesChannelListStyles.keyboardFocus,
				)}
				isOver={isCategoryDragOver}
				dropIndicator={dropIndicator}
				onClick={onToggle}
				onContextMenu={handleContextMenu}
				onKeyDown={(e) => {
					if (!isKeyboardActivationKey(e.key)) return;
					e.preventDefault();
					onToggle();
				}}
				onFocus={() => setIsFocused(true)}
				onBlur={() => setIsFocused(false)}
				data-flx="app.favorites-channel-list-content.favorite-category-item.generic-channel-item.toggle"
			>
				<div
					className={favoritesChannelListStyles.categoryContent}
					data-flx="app.favorites-channel-list-content.favorite-category-item.div"
				>
					<span
						className={favoritesChannelListStyles.categoryName}
						data-flx="app.favorites-channel-list-content.favorite-category-item.span"
					>
						{category.name}
					</span>
					<CaretDownIcon
						weight="bold"
						className={favoritesChannelListStyles.categoryIcon}
						style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`}}
						data-flx="app.favorites-channel-list-content.favorite-category-item.caret-down-icon"
					/>
				</div>
				<div
					className={favoritesChannelListStyles.categoryActions}
					data-flx="app.favorites-channel-list-content.favorite-category-item.div--2"
				>
					<div
						className={favoritesChannelListStyles.hoverAffordance}
						data-flx="app.favorites-channel-list-content.favorite-category-item.div--3"
					>
						<Tooltip
							text={i18n._(ADD_CHANNEL_DESCRIPTOR)}
							data-flx="app.favorites-channel-list-content.favorite-category-item.tooltip"
						>
							<FocusRing
								offset={-2}
								ringClassName={channelItemSurfaceStyles.channelItemFocusRing}
								data-flx="app.favorites-channel-list-content.favorite-category-item.focus-ring"
							>
								<button
									type="button"
									className={favoritesChannelListStyles.categoryAddButton}
									aria-label={i18n._(ADD_CHANNEL_DESCRIPTOR)}
									onClick={(e) => {
										e.stopPropagation();
										onAddChannel();
									}}
									data-flx="app.favorites-channel-list-content.favorite-category-item.button.stop-propagation"
								>
									<PlusIcon
										weight="bold"
										className={favoritesChannelListStyles.categoryAddButtonIcon}
										data-flx="app.favorites-channel-list-content.favorite-category-item.plus-icon"
									/>
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				</div>
			</GenericChannelItem>
		);
	},
);
const UncategorizedGroup = ({children}: {children: React.ReactNode}) => {
	const [{isOver}, dropRef] = useDrop<ChannelDragItem, unknown, {isOver: boolean}>({
		accept: DragItemType.FAVORITES_CHANNEL,
		drop: (item, monitor) => {
			if (monitor.didDrop()) return;
			if (item.parentId === null) return;
			const channels = Favorites.getChannelsInCategory(null);
			Favorites.moveChannel(item.channelId, null, channels.length);
		},
		collect: (monitor) => ({
			isOver: monitor.isOver({shallow: true}),
		}),
	});
	const dropConnectorRef = useCallback(
		(node: ConnectableElement | null) => {
			dropRef(node);
		},
		[dropRef],
	);
	return (
		<div
			ref={dropConnectorRef}
			className={clsx(
				favoritesChannelListStyles.uncategorizedGroup,
				isOver && favoritesChannelListStyles.favoriteItemOver,
			)}
			data-flx="app.favorites-channel-list-content.uncategorized-group.div"
		>
			{children}
		</div>
	);
};
export const FavoritesChannelListContent = observer(() => {
	const {i18n} = useLingui();
	const isDraggingFavorite = useDragLayer((monitor) => {
		const itemType = monitor.getItemType();
		return (
			monitor.isDragging() &&
			(itemType === DragItemType.FAVORITES_CHANNEL || itemType === DragItemType.FAVORITES_CATEGORY)
		);
	});
	const scrollerRef = useRef<ScrollerHandle>(null);
	const getScrollElement = useCallback(() => {
		const scroller = scrollerRef.current;
		if (scroller == null) return null;
		return scroller.getScrollerNode();
	}, []);
	useDragAutoScroll({active: isDraggingFavorite, getScrollElement});
	const favorites = Favorites.sortedChannels;
	const categories = Favorites.sortedCategories;
	const hideMutedChannels = Favorites.hideMutedChannels;
	const channelGroups = useMemo(() => {
		const groups: Array<FavoriteChannelGroup> = [];
		const categoryMap = new Map<string | null, FavoriteChannelGroup>();
		categoryMap.set(null, {category: null, channels: []});
		for (const cat of categories) {
			categoryMap.set(cat.id, {category: cat, channels: []});
		}
		for (const fav of favorites) {
			const channel = Channels.getChannel(fav.channelId);
			if (!channel) continue;
			const guild = fav.guildId === ME ? null : Guilds.getGuild(fav.guildId);
			if (hideMutedChannels && channel.guildId) {
				if (UserGuildSettings.isCategoryOrChannelMuted(channel.guildId, channel.id)) {
					continue;
				}
			}
			const group = categoryMap.get(fav.parentId);
			if (group) {
				group.channels.push({favoriteChannel: fav, channel, guild: guild ?? null});
			}
		}
		for (const [, group] of categoryMap) {
			if (group.category || group.channels.length > 0 || group === categoryMap.get(null)) {
				groups.push(group);
			}
		}
		return groups;
	}, [favorites, categories, hideMutedChannels]);
	const hasVisibleChannels = channelGroups.some((group) => group.channels.length > 0);
	const handleContextMenu = useCallback((event: React.MouseEvent) => {
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<FavoritesChannelListContextMenu
				onClose={onClose}
				data-flx="app.favorites-channel-list-content.handle-context-menu.favorites-channel-list-context-menu"
			/>
		));
	}, []);
	if (!hasVisibleChannels && categories.length === 0) {
		return (
			<Scroller
				ref={scrollerRef}
				className={styles.channelListScroller}
				key="favorites-channel-list-empty-scroller"
				data-flx="app.favorites-channel-list-content.channel-list-scroller"
			>
				<div
					onContextMenu={handleContextMenu}
					role="region"
					aria-label={i18n._(EMPTY_FAVORITES_DESCRIPTOR)}
					data-flx="app.favorites-channel-list-content.region.context-menu"
				>
					<GuildChannelListSkeleton
						channelList={null}
						detachedBannerAspectRatio={null}
						data-flx="app.favorites-channel-list-content.guild-channel-list-skeleton"
					/>
					<div className={styles.bottomSpacer} data-flx="app.favorites-channel-list-content.bottom-spacer.empty" />
				</div>
			</Scroller>
		);
	}
	return (
		<Scroller
			ref={scrollerRef}
			className={styles.channelListScroller}
			key="favorites-channel-list-scroller"
			data-flx="app.favorites-channel-list-content.channel-list-scroller--2"
		>
			<div
				className={favoritesChannelListStyles.navigationContainer}
				onContextMenu={handleContextMenu}
				role="navigation"
				data-flx="app.favorites-channel-list-content.navigation.context-menu"
			>
				<div
					className={favoritesChannelListStyles.channelGroupsContainer}
					data-flx="app.favorites-channel-list-content.div"
				>
					{channelGroups.map((group) => {
						const isCollapsed = group.category ? Favorites.isCategoryCollapsed(group.category.id) : false;
						const handleAddChannel = () => {
							ModalCommands.push(
								modal(() => (
									<AddFavoriteChannelModal
										categoryId={group.category?.id}
										data-flx="app.favorites-channel-list-content.handle-add-channel.add-favorite-channel-modal"
									/>
								)),
							);
						};
						const content = (
							<>
								{group.category && (
									<FavoriteCategoryItem
										category={group.category}
										isCollapsed={isCollapsed}
										onToggle={() => Favorites.toggleCategoryCollapsed(group.category!.id)}
										onAddChannel={handleAddChannel}
										data-flx="app.favorites-channel-list-content.favorite-category-item"
									/>
								)}
								{!isCollapsed &&
									group.channels.map(({favoriteChannel, channel, guild}) => (
										<FavoriteChannelResolvedItem
											key={favoriteChannel.channelId}
											favoriteChannel={favoriteChannel}
											channel={channel}
											guild={guild}
											data-flx="app.favorites-channel-list-content.favorite-channel-resolved-item"
										/>
									))}
							</>
						);
						return (
							<div
								key={group.category?.id || 'uncategorized'}
								className={styles.channelGroup}
								data-flx="app.favorites-channel-list-content.channel-group"
							>
								{group.category ? (
									content
								) : (
									<UncategorizedGroup data-flx="app.favorites-channel-list-content.uncategorized-group">
										{content}
									</UncategorizedGroup>
								)}
							</div>
						);
					})}
				</div>
				<div className={styles.bottomSpacer} data-flx="app.favorites-channel-list-content.bottom-spacer" />
			</div>
		</Scroller>
	);
});
