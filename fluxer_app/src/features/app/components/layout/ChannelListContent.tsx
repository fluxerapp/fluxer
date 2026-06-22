// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ChannelItem} from '@app/features/app/components/layout/ChannelItem';
import channelItemStyles from '@app/features/app/components/layout/ChannelItem.module.css';
import {ChannelItemContent} from '@app/features/app/components/layout/ChannelItemContent';
import styles from '@app/features/app/components/layout/ChannelListContent.module.css';
import {
	CollapsedCategoryVoiceParticipants,
	CollapsedChannelAvatarStack,
} from '@app/features/app/components/layout/CollapsedCategoryVoiceParticipants';
import {GenericChannelItem} from '@app/features/app/components/layout/GenericChannelItem';
import {GuildDetachedBanner} from '@app/features/app/components/layout/GuildDetachedBanner';
import {NullSpaceDropIndicator} from '@app/features/app/components/layout/NullSpaceDropIndicator';
import {ScrollIndicatorOverlay} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import {DND_TYPES, type DragItem, type DropResult} from '@app/features/app/components/layout/types/DndTypes';
import {
	shouldShowChannelInCollapsedCategory,
	shouldShowChannelWhenHidingMutedChannels,
} from '@app/features/app/components/layout/utils/ChannelListVisibility';
import {createChannelMoveOperation} from '@app/features/app/components/layout/utils/ChannelMoveOperation';
import {organizeChannels} from '@app/features/app/components/layout/utils/ChannelOrganization';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {VoiceParticipantsList} from '@app/features/app/components/layout/VoiceParticipantsList';
import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import Channels from '@app/features/channel/state/Channels';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import type {Guild} from '@app/features/guild/models/Guild';
import {MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Permission from '@app/features/permissions/state/Permission';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {ChannelListContextMenu} from '@app/features/ui/action_menu/ChannelListContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as DimensionCommands from '@app/features/ui/commands/DimensionCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Scroller} from '@app/features/ui/components/Scroller';
import Dimension from '@app/features/ui/state/Dimension';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_CHANNELS_PER_CATEGORY} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {MotionValue} from 'motion';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {useDragLayer} from 'react-dnd';
import {GenericErrorModal} from '../alerts/GenericErrorModal';

const CATEGORY_FULL_DESCRIPTOR = msg({
	message: 'Category full',
	comment: 'Short label in the app layout channel list content.',
});
const THIS_CATEGORY_ALREADY_CONTAINS_THE_MAXIMUM_OF_CHANNELS_DESCRIPTOR = msg({
	message: 'This category already contains the maximum of {maxChannelsPerCategory} channels.',
	comment:
		'Modal body shown when a category cannot accept another channel because the per-category limit is reached. Limit is interpolated.',
});
const CHANNELS_DESCRIPTOR = msg({
	message: '{guildName} channels',
	comment: 'Short label in the app layout channel list content. Preserve {guildName}; it is inserted by code.',
});
const MEMBERS_SELECTED_DESCRIPTOR = msg({
	message: 'Members, selected',
	comment: 'Short label in the app layout channel list content.',
});
const NEW_MESSAGES_DESCRIPTOR = msg({
	message: 'New messages',
	comment: 'Short label in the app layout channel list content.',
});
const MEMBERS_PAGE_PERMISSIONS =
	Permissions.MANAGE_GUILD |
	Permissions.MANAGE_ROLES |
	Permissions.MANAGE_NICKNAMES |
	Permissions.BAN_MEMBERS |
	Permissions.MODERATE_MEMBERS |
	Permissions.KICK_MEMBERS;
const EMPTY_ARRAY: ReadonlyArray<never> = Object.freeze([]);
export const ChannelListContent = observer(({guild, scrollY}: {guild: Guild; scrollY: MotionValue<number>}) => {
	const {i18n} = useLingui();
	const channels = Channels.getGuildChannels(guild.id);
	const location = useLocation();
	const userGuildSettings = UserGuildSettings.getSettings(guild.id);
	const isDraggingAnything = useDragLayer((monitor) => {
		if (!monitor.isDragging()) return false;
		const itemType = monitor.getItemType();
		return (
			itemType === DND_TYPES.CHANNEL || itemType === DND_TYPES.CATEGORY || itemType === DND_TYPES.VOICE_PARTICIPANT
		);
	});
	const [activeDragItem, setActiveDragItem] = useState<DragItem | null>(null);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const stickToBottomRef = useRef(false);
	const pendingScrollTopRef = useRef<number | null>(null);
	const scrollPersistRafRef = useRef<number | null>(null);
	const channelListNavigationRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: '[data-channel-list-focus-item="true"]',
		orientation: 'vertical',
		loop: true,
		enabled: KeyboardMode.keyboardModeEnabled,
		restoreFocusOnWindowFocus: false,
		manageTabIndex: true,
	});
	const connectedChannelId = MediaEngine.channelId;
	const hideMutedChannels = userGuildSettings?.hide_muted_channels ?? false;
	const showFadedUnreadOnMutedChannels = Accessibility.showFadedUnreadOnMutedChannels;
	const isMobile = MobileLayout.enabled;
	const canViewMembers =
		!isMobile && ((Permission.getGuildPermissions(guild.id) ?? 0n) & MEMBERS_PAGE_PERMISSIONS) !== 0n;
	const guildPrefix = `/channels/${guild.id}/`;
	let selectedChannelInGuildId: string | null = null;
	let isMembersSelected = false;
	if (location.pathname.startsWith(guildPrefix)) {
		const tail = location.pathname.slice(guildPrefix.length);
		const slash = tail.indexOf('/');
		const segment = slash === -1 ? tail : tail.slice(0, slash);
		if (segment === 'members') {
			isMembersSelected = true;
		} else if (segment.length > 0) {
			selectedChannelInGuildId = segment;
		}
	}
	const handleMembersClick = useCallback(() => {
		RouterUtils.transitionTo(Routes.guildMembers(guild.id));
	}, [guild.id]);
	const handleMembersKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			handleMembersClick();
		},
		[handleMembersClick],
	);
	const collapsedCategories = useMemo(() => {
		const overrides = userGuildSettings?.channel_overrides;
		if (!overrides) return null;
		let collapsed: Set<string> | null = null;
		for (const channelId in overrides) {
			if (overrides[channelId as keyof typeof overrides].collapsed) {
				if (!collapsed) collapsed = new Set<string>();
				collapsed.add(channelId);
			}
		}
		return collapsed;
	}, [userGuildSettings]);
	const toggleCategory = useCallback(
		(categoryId: string) => {
			UserGuildSettingsCommands.toggleChannelCollapsed(guild.id, categoryId);
		},
		[guild.id],
	);
	const channelGroups = useMemo(() => organizeChannels(channels), [channels]);
	const showTrailingDropZone = channelGroups.length > 0;
	const channelIndicatorDependencies = useMemo(
		() => [channels.length, ReadStates.version, userGuildSettings, hideMutedChannels, showFadedUnreadOnMutedChannels],
		[channels.length, ReadStates.version, userGuildSettings, hideMutedChannels, showFadedUnreadOnMutedChannels],
	);
	const getChannelScrollContainer = useCallback(() => scrollerRef.current?.getScrollerNode() ?? null, [scrollerRef]);
	const handleChannelDrop = useCallback(
		(item: DragItem, result: DropResult) => {
			if (!result) return;
			const guildChannels = Channels.getGuildChannels(guild.id);
			const operation = createChannelMoveOperation({
				channels: guildChannels,
				dragItem: item,
				dropResult: result,
			});
			if (!operation) return;
			void (async () => {
				try {
					await GuildCommands.moveChannel(guild.id, operation);
				} catch (error) {
					if (failureCode(error) === APIErrorCodes.MAX_CATEGORY_CHANNELS) {
						ModalCommands.push(
							ModalCommands.modal(() => (
								<GenericErrorModal
									title={i18n._(CATEGORY_FULL_DESCRIPTOR)}
									message={i18n._(THIS_CATEGORY_ALREADY_CONTAINS_THE_MAXIMUM_OF_CHANNELS_DESCRIPTOR, {
										maxChannelsPerCategory: MAX_CHANNELS_PER_CATEGORY,
									})}
									data-flx="app.channel-list-content.handle-channel-drop.confirm-modal"
								/>
							)),
						);
						return;
					}
					throw error;
				}
			})();
		},
		[guild.id],
	);
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			const scrollTop = event.currentTarget.scrollTop;
			const scrollHeight = event.currentTarget.scrollHeight;
			const offsetHeight = event.currentTarget.offsetHeight;
			stickToBottomRef.current = scrollHeight - (scrollTop + offsetHeight) <= 8;
			scrollY.set(scrollTop);
			pendingScrollTopRef.current = scrollTop;
			if (scrollPersistRafRef.current != null) return;
			scrollPersistRafRef.current = requestAnimationFrame(() => {
				scrollPersistRafRef.current = null;
				const pendingScrollTop = pendingScrollTopRef.current;
				if (pendingScrollTop == null) return;
				DimensionCommands.updateChannelListScroll(guild.id, pendingScrollTop);
			});
		},
		[scrollY, guild.id],
	);
	useEffect(() => {
		return () => {
			if (scrollPersistRafRef.current != null) {
				cancelAnimationFrame(scrollPersistRafRef.current);
				scrollPersistRafRef.current = null;
			}
		};
	}, [guild.id]);
	const handleResize = useCallback((_entry: ResizeObserverEntry, _type: 'container' | 'content') => {
		if (stickToBottomRef.current && scrollerRef.current) {
			scrollerRef.current.scrollToBottom({animate: false});
		}
	}, []);
	useEffect(() => {
		const guildDimensions = Dimension.getGuildDimensions(guild.id);
		if (guildDimensions.scrollTo) {
			const element = document.querySelector(`[data-channel-id="${guildDimensions.scrollTo}"]`);
			if (element && scrollerRef.current) {
				scrollerRef.current.scrollIntoViewNode({node: element as HTMLElement, shouldScrollToStart: false});
			}
			DimensionCommands.clearChannelListScrollTo(guild.id);
		} else if (guildDimensions.scrollTop && guildDimensions.scrollTop > 0 && scrollerRef.current) {
			scrollerRef.current.scrollTo({to: guildDimensions.scrollTop, animate: false});
		}
	}, [guild.id]);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<ChannelListContextMenu
					guild={guild}
					onClose={onClose}
					data-flx="app.channel-list-content.handle-context-menu.channel-list-context-menu"
				/>
			));
		},
		[guild],
	);
	const hasVisibleUnreadInChannel = (channelId: string): boolean => {
		const unreadCount = ReadStates.getUnreadCount(channelId);
		const mentionCount = ReadStates.getMentionCount(channelId);
		const isMuted =
			UserGuildSettings.isCategoryMuted(guild.id, channelId) || UserGuildSettings.isChannelMuted(guild.id, channelId);
		const channel = Channels.getChannel(channelId);
		const unreadBadgesLevel = channel
			? UserGuildSettings.resolvedUnreadBadgesLevel({
					id: channel.id,
					guildId: channel.guildId ?? undefined,
					parentId: channel.parentId ?? undefined,
					type: channel.type,
				})
			: null;
		const unreadState = getChannelUnreadState({
			unreadCount,
			mentionCount,
			isMuted,
			showFadedUnreadOnMutedChannels,
			unreadBadgesLevel,
		});
		return unreadState.hasVisibleUnread;
	};
	return (
		<div
			className={styles.channelListScrollerWrapper}
			data-flx="app.channel-list-content.channel-list-scroller-wrapper"
		>
			<Scroller
				ref={scrollerRef}
				className={styles.channelListScroller}
				onScroll={handleScroll}
				onResize={handleResize}
				key={guild.id}
				data-flx="app.channel-list-content.channel-list-scroller"
			>
				<div
					className={styles.navigationContainer}
					onContextMenu={handleContextMenu}
					role="navigation"
					aria-label={i18n._(CHANNELS_DESCRIPTOR, {guildName: guild.name})}
					ref={channelListNavigationRef}
					data-flx="app.channel-list-content.navigation-container.context-menu"
				>
					<GuildDetachedBanner guild={guild} data-flx="app.channel-list-content.guild-detached-banner" />
					<div className={styles.topDropZone} data-flx="app.channel-list-content.top-drop-zone">
						<NullSpaceDropIndicator
							isDraggingAnything={isDraggingAnything}
							onChannelDrop={handleChannelDrop}
							variant="top"
							data-flx="app.channel-list-content.null-space-drop-indicator"
						/>
					</div>
					{canViewMembers && (
						<>
							<div className={styles.membersSection} data-flx="app.channel-list-content.members-section">
								<GenericChannelItem
									containerClassName={channelItemStyles.container}
									className={clsx(
										channelItemStyles.channelItem,
										channelItemStyles.channelItemRegular,
										isMembersSelected && channelItemStyles.channelItemSelected,
										!isMembersSelected && channelItemStyles.channelItemHoverable,
									)}
									isSelected={isMembersSelected}
									aria-label={isMembersSelected ? i18n._(MEMBERS_SELECTED_DESCRIPTOR) : i18n._(MEMBERS_DESCRIPTOR)}
									aria-current={isMembersSelected ? 'page' : undefined}
									onClick={handleMembersClick}
									onKeyDown={handleMembersKeyDown}
									data-flx="app.channel-list-content.generic-channel-item.members-click"
								>
									<ChannelItemContent
										icon={
											<UsersIcon
												size={20}
												className={clsx(
													channelItemStyles.channelItemIcon,
													isMembersSelected
														? channelItemStyles.channelItemIconSelected
														: channelItemStyles.channelItemIconUnselected,
												)}
												data-flx="app.channel-list-content.users-icon"
											/>
										}
										name={i18n._(MEMBERS_DESCRIPTOR)}
										data-flx="app.channel-list-content.channel-item-content"
									/>
								</GenericChannelItem>
							</div>
							<div className={styles.membersSeparator} data-flx="app.channel-list-content.members-separator" />
						</>
					)}
					<div className={styles.channelGroupsContainer} data-flx="app.channel-list-content.channel-groups-container">
						{channelGroups.map((group) => {
							const isCollapsed = group.category ? (collapsedCategories?.has(group.category.id) ?? false) : false;
							const isNullSpace = !group.category;
							const isCategoryMuted = group.category
								? UserGuildSettings.isChannelMuted(guild.id, group.category.id)
								: false;
							type ChannelRow = (typeof group.textChannels)[number];
							let filteredTextChannels: Array<ChannelRow>;
							let filteredVoiceChannels: Array<ChannelRow>;
							if (hideMutedChannels) {
								filteredTextChannels = [];
								for (const ch of group.textChannels) {
									const isMuted = UserGuildSettings.isChannelMuted(guild.id, ch.id);
									if (
										shouldShowChannelWhenHidingMutedChannels({
											isMuted,
											isSelected: ch.id === selectedChannelInGuildId,
											isConnected: false,
											hasVisibleUnread: isMuted && hasVisibleUnreadInChannel(ch.id),
										})
									) {
										filteredTextChannels.push(ch);
									}
								}
								filteredVoiceChannels = [];
								for (const ch of group.voiceChannels) {
									const isMuted = UserGuildSettings.isChannelMuted(guild.id, ch.id);
									if (
										shouldShowChannelWhenHidingMutedChannels({
											isMuted,
											isSelected: ch.id === selectedChannelInGuildId,
											isConnected: ch.id === connectedChannelId,
											hasVisibleUnread: isMuted && hasVisibleUnreadInChannel(ch.id),
										})
									) {
										filteredVoiceChannels.push(ch);
									}
								}
							} else {
								filteredTextChannels = group.textChannels;
								filteredVoiceChannels = group.voiceChannels;
							}
							let visibleTextChannels: ReadonlyArray<ChannelRow> = filteredTextChannels;
							let visibleVoiceChannels: ReadonlyArray<ChannelRow> = filteredVoiceChannels;
							let connectedChannelInGroup = false;
							if (isCollapsed) {
								const showTextSelected = selectedChannelInGuildId;
								const showSet = new Set<string>();
								for (const ch of filteredTextChannels) {
									if (
										shouldShowChannelInCollapsedCategory({
											isCategoryMuted,
											isSelected: ch.id === showTextSelected,
											hasVisibleUnread: hasVisibleUnreadInChannel(ch.id),
										})
									) {
										showSet.add(ch.id);
									}
								}
								if (showSet.size === 0) {
									visibleTextChannels = EMPTY_ARRAY;
								} else if (showSet.size === filteredTextChannels.length) {
									visibleTextChannels = filteredTextChannels;
								} else {
									const next: Array<ChannelRow> = [];
									for (const ch of filteredTextChannels) if (showSet.has(ch.id)) next.push(ch);
									visibleTextChannels = next;
								}
								const voiceSet = new Set<string>();
								if (selectedChannelInGuildId) voiceSet.add(selectedChannelInGuildId);
								if (connectedChannelId) {
									for (const ch of filteredVoiceChannels) {
										if (ch.id === connectedChannelId) {
											connectedChannelInGroup = true;
											voiceSet.add(connectedChannelId);
											break;
										}
									}
								}
								for (const ch of filteredVoiceChannels) {
									if (
										shouldShowChannelInCollapsedCategory({
											isCategoryMuted,
											isSelected: ch.id === selectedChannelInGuildId,
											hasVisibleUnread: hasVisibleUnreadInChannel(ch.id),
										})
									) {
										voiceSet.add(ch.id);
									}
								}
								if (voiceSet.size === 0) {
									visibleVoiceChannels = EMPTY_ARRAY;
								} else {
									const next: Array<ChannelRow> = [];
									let connectedRow: ChannelRow | null = null;
									for (const ch of filteredVoiceChannels) {
										if (!voiceSet.has(ch.id)) continue;
										if (ch.id === connectedChannelId) {
											connectedRow = ch;
										} else {
											next.push(ch);
										}
									}
									visibleVoiceChannels = connectedRow ? [connectedRow, ...next] : next;
								}
							}
							if (isNullSpace && filteredTextChannels.length === 0 && filteredVoiceChannels.length === 0) {
								return null;
							}
							if (
								hideMutedChannels &&
								group.category &&
								filteredTextChannels.length === 0 &&
								filteredVoiceChannels.length === 0
							) {
								return null;
							}
							const showTextChannels = !isCollapsed || visibleTextChannels.length > 0;
							const showVoiceChannels = !isCollapsed || visibleVoiceChannels.length > 0;
							return (
								<div
									key={group.category?.id || 'null-space'}
									className={styles.channelGroup}
									data-flx="app.channel-list-content.channel-group"
								>
									{group.category && (
										<ChannelItem
											guild={guild}
											channel={group.category}
											isCollapsed={isCollapsed}
											onToggle={() => toggleCategory(group.category!.id)}
											isDraggingAnything={isDraggingAnything}
											activeDragItem={activeDragItem}
											onChannelDrop={handleChannelDrop}
											onDragStateChange={setActiveDragItem}
											isSelectedByPath={selectedChannelInGuildId === group.category.id}
											isOnMembersRoute={isMembersSelected}
											data-flx="app.channel-list-content.channel-item"
										/>
									)}
									{isCollapsed && group.category && !connectedChannelInGroup && (
										<CollapsedCategoryVoiceParticipants
											guild={guild}
											voiceChannels={filteredVoiceChannels}
											data-flx="app.channel-list-content.collapsed-category-voice-participants"
										/>
									)}
									{showTextChannels &&
										visibleTextChannels.map((ch) => (
											<ChannelItem
												key={ch.id}
												guild={guild}
												channel={ch}
												isDraggingAnything={isDraggingAnything}
												activeDragItem={activeDragItem}
												onChannelDrop={handleChannelDrop}
												onDragStateChange={setActiveDragItem}
												isSelectedByPath={selectedChannelInGuildId === ch.id}
												isOnMembersRoute={isMembersSelected}
												data-flx="app.channel-list-content.channel-item--2"
											/>
										))}
									{showVoiceChannels &&
										visibleVoiceChannels.map((ch) => {
											const channelRow = (
												<ChannelItem
													key={ch.id}
													guild={guild}
													channel={ch}
													isDraggingAnything={isDraggingAnything}
													activeDragItem={activeDragItem}
													onChannelDrop={handleChannelDrop}
													onDragStateChange={setActiveDragItem}
													isSelectedByPath={selectedChannelInGuildId === ch.id}
													isOnMembersRoute={isMembersSelected}
													data-flx="app.channel-list-content.channel-item--3"
												/>
											);
											if (isCollapsed && connectedChannelId && ch.id === connectedChannelId) {
												return (
													<React.Fragment key={ch.id}>
														{channelRow}
														<CollapsedChannelAvatarStack
															guild={guild}
															channel={ch}
															data-flx="app.channel-list-content.collapsed-channel-avatar-stack"
														/>
													</React.Fragment>
												);
											}
											return (
												<React.Fragment key={ch.id}>
													{channelRow}
													{!isCollapsed && (
														<VoiceParticipantsList
															guild={guild}
															channel={ch}
															data-flx="app.channel-list-content.voice-participants-list"
														/>
													)}
												</React.Fragment>
											);
										})}
								</div>
							);
						})}
					</div>
					{showTrailingDropZone && (
						<div className={styles.bottomDropZone} data-flx="app.channel-list-content.bottom-drop-zone">
							<NullSpaceDropIndicator
								isDraggingAnything={isDraggingAnything}
								onChannelDrop={handleChannelDrop}
								variant="bottom"
								data-flx="app.channel-list-content.null-space-drop-indicator--2"
							/>
						</div>
					)}
					<div className={styles.bottomSpacer} data-flx="app.channel-list-content.bottom-spacer" />
				</div>
			</Scroller>
			<ScrollIndicatorOverlay
				getScrollContainer={getChannelScrollContainer}
				dependencies={channelIndicatorDependencies}
				label={i18n._(NEW_MESSAGES_DESCRIPTOR)}
				data-flx="app.channel-list-content.scroll-indicator-overlay"
			/>
		</div>
	);
});
