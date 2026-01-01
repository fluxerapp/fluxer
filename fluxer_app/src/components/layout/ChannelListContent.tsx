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
import {observer} from 'mobx-react-lite';
import type {MotionValue} from 'motion';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as DimensionActionCreators from '~/actions/DimensionActionCreators';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {APIErrorCodes, MAX_CHANNELS_PER_CATEGORY} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ChannelListContextMenu} from '~/components/uikit/ContextMenu/ChannelListContextMenu';
import type {ScrollerHandle} from '~/components/uikit/Scroller';
import {Scroller} from '~/components/uikit/Scroller';
import {ChannelListScrollbarProvider} from '~/contexts/ChannelListScrollbarContext';
import {HttpError} from '~/lib/HttpError';
import {useLocation} from '~/lib/router';
import type {GuildRecord} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import DimensionStore from '~/stores/DimensionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {ChannelItem} from './ChannelItem';
import styles from './ChannelListContent.module.css';
import {CollapsedCategoryVoiceParticipants, CollapsedChannelAvatarStack} from './CollapsedCategoryVoiceParticipants';
import {GuildDetachedBanner} from './GuildDetachedBanner';
import {NullSpaceDropIndicator} from './NullSpaceDropIndicator';
import {ScrollIndicatorOverlay} from './ScrollIndicatorOverlay';
import type {DragItem, DropResult} from './types/dnd';
import {createChannelMoveOperation} from './utils/channelMoveOperation';
import {organizeChannels} from './utils/channelOrganization';
import {VoiceParticipantsList} from './VoiceParticipantsList';

const mergeUniqueById = <T extends {id: string}>(items: ReadonlyArray<T>): Array<T> => {
	const seen = new Set<string>();
	const unique: Array<T> = [];
	for (const item of items) {
		if (seen.has(item.id)) {
			continue;
		}
		seen.add(item.id);
		unique.push(item);
	}
	return unique;
};

export const ChannelListContent = observer(({guild, scrollY}: {guild: GuildRecord; scrollY: MotionValue<number>}) => {
	const {t} = useLingui();
	const channels = ChannelStore.getGuildChannels(guild.id);
	const location = useLocation();
	const userGuildSettings = UserGuildSettingsStore.getSettings(guild.id);
	const [isDraggingAnything, setIsDraggingAnything] = React.useState(false);
	const [activeDragItem, setActiveDragItem] = React.useState<DragItem | null>(null);
	const scrollerRef = React.useRef<ScrollerHandle>(null);
	const stickToBottomRef = React.useRef(false);
	const hasScrollbar = true;

	const connectedChannelId = MediaEngineStore.channelId;
	const hideMutedChannels = userGuildSettings?.hide_muted_channels ?? false;

	const collapsedCategories = React.useMemo(() => {
		const collapsed = new Set<string>();
		if (userGuildSettings?.channel_overrides) {
			for (const [channelId, override] of Object.entries(userGuildSettings.channel_overrides)) {
				if (override.collapsed) collapsed.add(channelId);
			}
		}
		return collapsed;
	}, [userGuildSettings]);

	const toggleCategory = React.useCallback(
		(categoryId: string) => {
			UserGuildSettingsActionCreators.toggleChannelCollapsed(guild.id, categoryId);
		},
		[guild.id],
	);

	const channelGroups = React.useMemo(() => organizeChannels(channels), [channels]);
	const showTrailingDropZone = channelGroups.length > 0;
	const channelIndicatorDependencies = React.useMemo(
		() => [channels.length, ReadStateStore.version],
		[channels.length, ReadStateStore.version],
	);
	const getChannelScrollContainer = React.useCallback(
		() => scrollerRef.current?.getScrollerNode() ?? null,
		[scrollerRef],
	);

	const handleChannelDrop = React.useCallback(
		(item: DragItem, result: DropResult) => {
			if (!result) return;
			const guildChannels = ChannelStore.getGuildChannels(guild.id);
			const operation = createChannelMoveOperation({
				channels: guildChannels,
				dragItem: item,
				dropResult: result,
			});
			if (!operation) return;

			void (async () => {
				try {
					await GuildActionCreators.moveChannel(guild.id, operation);
				} catch (error) {
					if (error instanceof HttpError) {
						const body = error.body as {code?: string} | undefined;
						if (body?.code === APIErrorCodes.MAX_CATEGORY_CHANNELS) {
							ModalActionCreators.push(
								ModalActionCreators.modal(() => (
									<ConfirmModal
										title={t`Category full`}
										description={t`This category already contains the maximum of ${MAX_CHANNELS_PER_CATEGORY} channels.`}
										primaryText={t`Understood`}
										onPrimary={() => {}}
									/>
								)),
							);
							return;
						}
					}
					throw error;
				}
			})();
		},
		[guild.id],
	);

	React.useEffect(() => {
		const handleDragStart = () => setIsDraggingAnything(true);
		const handleDragEnd = () => setIsDraggingAnything(false);
		document.addEventListener('dragstart', handleDragStart);
		document.addEventListener('dragend', handleDragEnd);
		return () => {
			document.removeEventListener('dragstart', handleDragStart);
			document.removeEventListener('dragend', handleDragEnd);
		};
	}, []);

	const handleScroll = React.useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			const scrollTop = event.currentTarget.scrollTop;
			const scrollHeight = event.currentTarget.scrollHeight;
			const offsetHeight = event.currentTarget.offsetHeight;

			stickToBottomRef.current = scrollHeight - (scrollTop + offsetHeight) <= 8;

			scrollY.set(scrollTop);
			DimensionActionCreators.updateChannelListScroll(guild.id, scrollTop);
		},
		[scrollY, guild.id],
	);

	const handleResize = React.useCallback((_entry: ResizeObserverEntry, type: 'container' | 'content') => {
		if (type !== 'content') return;

		if (stickToBottomRef.current && scrollerRef.current) {
			scrollerRef.current.scrollToBottom({animate: false});
		}
	}, []);

	React.useEffect(() => {
		const guildDimensions = DimensionStore.getGuildDimensions(guild.id);

		if (guildDimensions.scrollTo) {
			const element = document.querySelector(`[data-channel-id="${guildDimensions.scrollTo}"]`);
			if (element && scrollerRef.current) {
				scrollerRef.current.scrollIntoViewNode({node: element as HTMLElement, shouldScrollToStart: false});
			}
			DimensionActionCreators.clearChannelListScrollTo(guild.id);
		} else if (guildDimensions.scrollTop && guildDimensions.scrollTop > 0 && scrollerRef.current) {
			scrollerRef.current.scrollTo({to: guildDimensions.scrollTop, animate: false});
		}
	}, [guild.id]);

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent) => {
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<ChannelListContextMenu guild={guild} onClose={onClose} />
			));
		},
		[guild],
	);

	return (
		<ChannelListScrollbarProvider value={{hasScrollbar}}>
			<div className={styles.channelListScrollerWrapper}>
				<Scroller
					ref={scrollerRef}
					className={styles.channelListScroller}
					onScroll={handleScroll}
					onResize={handleResize}
					key={guild.id}
				>
					<div className={styles.navigationContainer} onContextMenu={handleContextMenu} role="navigation">
						<GuildDetachedBanner guild={guild} />
						<div className={styles.topDropZone}>
							<NullSpaceDropIndicator
								isDraggingAnything={isDraggingAnything}
								onChannelDrop={handleChannelDrop}
								variant="top"
							/>
						</div>
						<div className={styles.channelGroupsContainer}>
							{channelGroups.map((group) => {
								const isCollapsed = group.category ? collapsedCategories.has(group.category.id) : false;
								const isNullSpace = !group.category;

								const selectedTextChannels = group.textChannels.filter((ch) =>
									location.pathname.startsWith(`/channels/${guild.id}/${ch.id}`),
								);
								const selectedVoiceChannels = group.voiceChannels.filter((ch) =>
									location.pathname.startsWith(`/channels/${guild.id}/${ch.id}`),
								);
								const unreadTextChannels = group.textChannels.filter((ch) => {
									const unreadCount = ReadStateStore.getUnreadCount(ch.id);
									const mentionCount = ReadStateStore.getMentionCount(ch.id);
									return unreadCount > 0 || mentionCount > 0;
								});
								const unreadVoiceChannels = group.voiceChannels.filter((ch) => {
									const unreadCount = ReadStateStore.getUnreadCount(ch.id);
									const mentionCount = ReadStateStore.getMentionCount(ch.id);
									return unreadCount > 0 || mentionCount > 0;
								});

								const selectedTextIds = new Set(selectedTextChannels.map((ch) => ch.id));
								const selectedVoiceIds = new Set(selectedVoiceChannels.map((ch) => ch.id));

								const filteredTextChannels = hideMutedChannels
									? group.textChannels.filter(
											(ch) =>
												selectedTextIds.has(ch.id) || !UserGuildSettingsStore.isGuildOrChannelMuted(guild.id, ch.id),
										)
									: group.textChannels;

								const filteredVoiceChannels = hideMutedChannels
									? group.voiceChannels.filter(
											(ch) =>
												selectedVoiceIds.has(ch.id) ||
												ch.id === connectedChannelId ||
												!UserGuildSettingsStore.isGuildOrChannelMuted(guild.id, ch.id),
										)
									: group.voiceChannels;

								const visibleTextChannels = isCollapsed
									? hideMutedChannels
										? mergeUniqueById(filteredTextChannels.filter((ch) => selectedTextIds.has(ch.id)))
										: mergeUniqueById([...selectedTextChannels, ...unreadTextChannels])
									: filteredTextChannels;

								let visibleVoiceChannels: typeof filteredVoiceChannels = filteredVoiceChannels;
								if (isCollapsed) {
									if (hideMutedChannels) {
										const collapsedVoiceChannels: typeof filteredVoiceChannels = [];
										if (connectedChannelId) {
											const connected = filteredVoiceChannels.find((ch) => ch.id === connectedChannelId);
											if (connected) collapsedVoiceChannels.push(connected);
										}
										for (const ch of filteredVoiceChannels) {
											if (selectedVoiceIds.has(ch.id)) {
												collapsedVoiceChannels.push(ch);
											}
										}
										visibleVoiceChannels = mergeUniqueById(collapsedVoiceChannels);
									} else {
										visibleVoiceChannels = mergeUniqueById([...selectedVoiceChannels, ...unreadVoiceChannels]);
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

								const connectedChannelInGroup =
									isCollapsed && connectedChannelId
										? filteredVoiceChannels.some((vc) => vc.id === connectedChannelId)
										: false;

								if (isCollapsed && connectedChannelInGroup && connectedChannelId) {
									const hasIt = visibleVoiceChannels.some((c) => c.id === connectedChannelId);
									if (!hasIt) {
										const connected = filteredVoiceChannels.find((c) => c.id === connectedChannelId);
										if (connected) visibleVoiceChannels = [connected, ...visibleVoiceChannels];
									} else {
										visibleVoiceChannels = [
											...visibleVoiceChannels.filter((c) => c.id === connectedChannelId),
											...visibleVoiceChannels.filter((c) => c.id !== connectedChannelId),
										];
									}
								}

								const showTextChannels = !isCollapsed || visibleTextChannels.length > 0;
								const showVoiceChannels = !isCollapsed || visibleVoiceChannels.length > 0;

								return (
									<div key={group.category?.id || 'null-space'} className={styles.channelGroup}>
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
											/>
										)}

										{isCollapsed && group.category && !connectedChannelInGroup && (
											<CollapsedCategoryVoiceParticipants guild={guild} voiceChannels={filteredVoiceChannels} />
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
													/>
												);

												if (isCollapsed && connectedChannelId && ch.id === connectedChannelId) {
													return (
														<React.Fragment key={ch.id}>
															{channelRow}
															<CollapsedChannelAvatarStack guild={guild} channel={ch} />
														</React.Fragment>
													);
												}

												return (
													<React.Fragment key={ch.id}>
														{channelRow}
														{!isCollapsed && <VoiceParticipantsList guild={guild} channel={ch} />}
													</React.Fragment>
												);
											})}
									</div>
								);
							})}
						</div>
						{showTrailingDropZone && (
							<div className={styles.bottomDropZone}>
								<NullSpaceDropIndicator
									isDraggingAnything={isDraggingAnything}
									onChannelDrop={handleChannelDrop}
									variant="bottom"
								/>
							</div>
						)}
						<div className={styles.bottomSpacer} />
					</div>
				</Scroller>
				<ScrollIndicatorOverlay
					getScrollContainer={getChannelScrollContainer}
					dependencies={channelIndicatorDependencies}
					label={t`New Messages`}
				/>
			</div>
		</ChannelListScrollbarProvider>
	);
});
