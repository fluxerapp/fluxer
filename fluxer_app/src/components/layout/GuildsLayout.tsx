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

import type {DragEndEvent} from '@dnd-kit/core';
import {DndContext, PointerSensor, useSensor, useSensors} from '@dnd-kit/core';
import {restrictToVerticalAxis} from '@dnd-kit/modifiers';
import {arrayMove, SortableContext, verticalListSortingStrategy} from '@dnd-kit/sortable';
import {useLingui} from '@lingui/react/macro';

import {ExclamationMarkIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as DimensionActionCreators from '~/actions/DimensionActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {ChannelTypes} from '~/Constants';
import {ClaimAccountModal} from '~/components/modals/ClaimAccountModal';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {Platform} from '~/lib/Platform';
import {useLocation} from '~/lib/router';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import CallStateStore from '~/stores/CallStateStore';
import ChannelStore from '~/stores/ChannelStore';
import DimensionStore from '~/stores/DimensionStore';
import GuildAvailabilityStore from '~/stores/GuildAvailabilityStore';
import GuildListStore from '~/stores/GuildListStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import InitializationStore from '~/stores/InitializationStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import NagbarStore from '~/stores/NagbarStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {useActiveNagbars, useNagbarConditions} from './app-layout/hooks';
import {NagbarContainer} from './app-layout/NagbarContainer';
import {TopNagbarContext} from './app-layout/TopNagbarContext';
import styles from './GuildsLayout.module.css';
import {AddGuildButton} from './guild-list/AddGuildButton';
import {DownloadButton} from './guild-list/DownloadButton';
import {FavoritesButton} from './guild-list/FavoritesButton';
import {FluxerButton} from './guild-list/FluxerButton';
import {DMListItem} from './guild-list/GuildListDMItem';
import {GuildListItem} from './guild-list/GuildListItem';
import {HelpButton} from './guild-list/HelpButton';
import {MobileMentionToast} from './MobileMentionToast';
import {OutlineFrame} from './OutlineFrame';
import {ScrollIndicatorOverlay} from './ScrollIndicatorOverlay';
import {UserArea} from './UserArea';

const isSelectedPath = (pathname: string, path: string) => {
	return pathname.startsWith(path);
};

const DM_LIST_REMOVAL_DELAY_MS = 750;

const getUnreadDMChannels = () => {
	const dmChannels = ChannelStore.dmChannels;
	return dmChannels.filter((channel) => ReadStateStore.hasUnread(channel.id));
};

const GuildList = observer(() => {
	const {t} = useLingui();
	const [isDragging, setIsDragging] = React.useState(false);
	const guilds = GuildListStore.guilds;
	const mobileLayout = MobileLayoutStore;
	const sensors = useSensors(useSensor(PointerSensor, {activationConstraint: {distance: 8}}));
	const unavailableGuilds = GuildAvailabilityStore.unavailableGuilds;
	const unreadDMChannelsRaw = getUnreadDMChannels();
	const unreadDMChannelIds = unreadDMChannelsRaw.map((c) => c.id).join(',');
	const unreadDMChannels = React.useMemo(() => unreadDMChannelsRaw, [unreadDMChannelIds]);
	const scrollRef = React.useRef<HTMLDivElement>(null);
	const location = useLocation();
	const hasUnavailableGuilds = unavailableGuilds.size > 0;
	const unavailableCount = unavailableGuilds.size;
	const guildReadVersion = GuildReadStateStore.version;
	const readVersion = ReadStateStore.version;
	const guildIndicatorDependencies = React.useMemo(
		() => [guilds.length, guildReadVersion, readVersion, unreadDMChannelIds],
		[guilds.length, guildReadVersion, readVersion, unreadDMChannelIds],
	);
	const getGuildScrollContainer = React.useCallback(() => scrollRef.current, []);
	const [visibleDMChannels, setVisibleDMChannels] = React.useState(unreadDMChannels);
	const pinnedCallChannel =
		MediaEngineStore.connected && MediaEngineStore.channelId
			? (() => {
					const channel = ChannelStore.getChannel(MediaEngineStore.channelId);
					if (!channel) return null;
					if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) return null;
					const hasActiveCall = CallStateStore.hasActiveCall(channel.id);
					if (!hasActiveCall) return null;
					return channel;
				})()
			: null;
	const filteredDMChannels = pinnedCallChannel
		? visibleDMChannels.filter((channel) => channel.id !== pinnedCallChannel.id)
		: visibleDMChannels;
	const hasVisibleDMChannels = filteredDMChannels.length > 0 || Boolean(pinnedCallChannel);
	const shouldCollapseFavoritesSpacing = !hasVisibleDMChannels && !hasUnavailableGuilds;
	const shouldShowTopDivider = (guilds.length > 0 || hasUnavailableGuilds) && !hasVisibleDMChannels;
	const shouldShowEmptyStateDivider = !hasVisibleDMChannels && !hasUnavailableGuilds && guilds.length === 0;
	const removalTimers = React.useRef<Map<string, ReturnType<typeof setTimeout>>>(new Map());

	React.useEffect(() => {
		const unreadIds = new Set(unreadDMChannels.map((channel) => channel.id));

		setVisibleDMChannels((current) => {
			const leftover = current.filter((channel) => !unreadIds.has(channel.id));

			for (const channel of leftover) {
				if (!removalTimers.current.has(channel.id)) {
					const timer = setTimeout(() => {
						removalTimers.current.delete(channel.id);
						setVisibleDMChannels((latest) => latest.filter((latestChannel) => latestChannel.id !== channel.id));
					}, DM_LIST_REMOVAL_DELAY_MS);
					removalTimers.current.set(channel.id, timer);
				}
			}

			return [...unreadDMChannels, ...leftover];
		});

		for (const channel of unreadDMChannels) {
			const timer = removalTimers.current.get(channel.id);
			if (timer) {
				clearTimeout(timer);
				removalTimers.current.delete(channel.id);
			}
		}
	}, [unreadDMChannels]);

	React.useEffect(() => {
		return () => {
			removalTimers.current.forEach((timer) => clearTimeout(timer));
			removalTimers.current.clear();
		};
	}, []);

	const renderDMListItems = (channels: Array<ChannelRecord>) =>
		channels.map((channel, index) => {
			const isSelected = isSelectedPath(location.pathname, Routes.dmChannel(channel.id));
			const isLastItem = index === channels.length - 1;

			return (
				<div key={channel.id} className={styles.dmListItemWrapper}>
					<DMListItem
						channel={channel}
						isSelected={isSelected}
						className={isLastItem ? styles.guildListItemNoMargin : undefined}
					/>
				</div>
			);
		});

	const handleDragEnd = (event: DragEndEvent) => {
		const {active, over} = event;
		if (over && active.id !== over.id) {
			const oldIndex = guilds.findIndex((guild) => guild.id === active.id);
			const newIndex = guilds.findIndex((guild) => guild.id === over.id);
			const newArray = arrayMove(guilds.slice(), oldIndex, newIndex);
			UserSettingsActionCreators.update({guildPositions: newArray.map((guild) => guild.id)});
		}
		setIsDragging(false);
	};

	const handleScroll = React.useCallback((event: React.UIEvent<HTMLDivElement>) => {
		const scrollTop = event.currentTarget.scrollTop;
		DimensionActionCreators.updateGuildListScroll(scrollTop);
	}, []);

	React.useEffect(() => {
		const scrollTop = DimensionStore.getGuildListDimensions().scrollTop;
		if (scrollTop > 0 && scrollRef.current) {
			scrollRef.current.scrollTop = scrollTop;
		}
	}, []);

	return (
		<div ref={scrollRef} className={styles.guildListScrollContainer} onScroll={handleScroll}>
			<div className={styles.guildListContent}>
				<div className={styles.guildListTopSection}>
					<FluxerButton />
					<FavoritesButton className={shouldCollapseFavoritesSpacing ? styles.guildListItemNoMargin : undefined} />

					<div className={styles.dmListSection}>
						{pinnedCallChannel && (
							<div className={styles.dmListItemWrapper} key={`pinned-call-${pinnedCallChannel.id}`}>
								<DMListItem
									channel={pinnedCallChannel}
									isSelected={isSelectedPath(location.pathname, Routes.dmChannel(pinnedCallChannel.id))}
									voiceCallActive
								/>
							</div>
						)}
						{renderDMListItems(filteredDMChannels)}
					</div>

					{hasVisibleDMChannels && <div className={styles.guildDivider} />}
				</div>

				<div className={styles.guildListGuildsSection}>
					{hasUnavailableGuilds && (
						<Tooltip
							position="right"
							type={'error'}
							maxWidth="xl"
							size="large"
							text={() =>
								unavailableCount === 1
									? t`${unavailableCount} community is temporarily unavailable due to a flux capacitor malfunction.`
									: t`${unavailableCount} communities are temporarily unavailable due to a flux capacitor malfunction.`
							}
						>
							<div className={styles.unavailableContainer}>
								<div className={styles.unavailableBadge}>
									<ExclamationMarkIcon weight="regular" className={styles.unavailableIcon} />
								</div>
							</div>
						</Tooltip>
					)}

					{shouldShowTopDivider && <div className={styles.guildDivider} />}

					{guilds.length > 0 && (
						<DndContext
							modifiers={[restrictToVerticalAxis]}
							onDragEnd={handleDragEnd}
							onDragStart={() => setIsDragging(true)}
							sensors={sensors}
						>
							<SortableContext
								disabled={guilds.length === 1 || mobileLayout.enabled}
								items={guilds.map((guild) => guild.id)}
								strategy={verticalListSortingStrategy}
							>
								{(() => {
									const selectedGuildIndex = guilds.findIndex((g) =>
										isSelectedPath(location.pathname, Routes.guildChannel(g.id)),
									);
									return guilds.map((guild, index) => (
										<GuildListItem
											key={guild.id}
											isSortingList={isDragging}
											guild={guild}
											isSelected={isSelectedPath(location.pathname, Routes.guildChannel(guild.id))}
											guildIndex={index}
											selectedGuildIndex={selectedGuildIndex}
										/>
									));
								})()}
							</SortableContext>
						</DndContext>
					)}

					{shouldShowEmptyStateDivider && <div className={styles.guildDivider} />}

					<AddGuildButton />
					{!Platform.isElectron && !Platform.isPWA && <DownloadButton />}
					<HelpButton />
				</div>
			</div>
			<ScrollIndicatorOverlay
				getScrollContainer={getGuildScrollContainer}
				dependencies={guildIndicatorDependencies}
				label={t`New`}
			/>
		</div>
	);
});

export const GuildsLayout = observer(({children}: {children: React.ReactNode}) => {
	const mobileLayout = MobileLayoutStore;
	const user = UserStore.currentUser;
	const location = useLocation();
	const shouldReserveUserAreaSpace = !!user && !mobileLayout.enabled;
	const showGuildListOnMobile =
		mobileLayout.enabled &&
		(location.pathname === Routes.ME ||
			(Routes.isChannelRoute(location.pathname) && location.pathname.split('/').length === 3));

	const showBottomNav =
		mobileLayout.enabled &&
		(location.pathname === Routes.ME ||
			Routes.isFavoritesRoute(location.pathname) ||
			location.pathname === Routes.NOTIFICATIONS ||
			location.pathname === Routes.YOU ||
			(Routes.isGuildChannelRoute(location.pathname) && location.pathname.split('/').length === 3));

	const nagbarConditions = useNagbarConditions();
	const activeNagbars = useActiveNagbars(nagbarConditions);
	const prevNagbarCount = React.useRef(activeNagbars.length);
	const isReady = InitializationStore.isReady;

	React.useEffect(() => {
		if (prevNagbarCount.current !== activeNagbars.length) {
			prevNagbarCount.current = activeNagbars.length;
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
	}, [activeNagbars.length]);

	const THIRTY_MINUTES_MS = 30 * 60 * 1000;
	React.useEffect(() => {
		if (!isReady) return;
		if (!user) return;
		if (NagbarStore.claimAccountModalShownThisSession) return;
		if (user.isClaimed()) return;
		if (location.pathname === Routes.PENDING_VERIFICATION) return;

		const accountAgeMs = SnowflakeUtils.age(user.id);
		if (accountAgeMs < THIRTY_MINUTES_MS) return;

		NagbarStore.markClaimAccountModalShown();
		ModalActionCreators.push(modal(() => <ClaimAccountModal />));
	}, [isReady, user, location.pathname]);

	const shouldShowSidebarDivider = !mobileLayout.enabled;

	return (
		<div
			className={clsx(
				styles.guildsLayoutContainer,
				mobileLayout.enabled && !showGuildListOnMobile && styles.guildsLayoutContainerMobile,
				shouldReserveUserAreaSpace && styles.guildsLayoutReserveSpace,
				showBottomNav && styles.guildsLayoutReserveMobileBottomNav,
			)}
		>
			{(!mobileLayout.enabled || showGuildListOnMobile) && <GuildList />}
			<div
				className={clsx(
					styles.contentContainer,
					mobileLayout.enabled && !showGuildListOnMobile && styles.contentContainerMobile,
				)}
			>
				<TopNagbarContext.Provider value={activeNagbars.length > 0}>
					<OutlineFrame
						className={styles.outlineFrame}
						sidebarDivider={shouldShowSidebarDivider}
						topBanner={<MobileMentionToast />}
						nagbar={
							activeNagbars.length > 0 ? (
								<div className={styles.nagbarStack}>
									<NagbarContainer nagbars={activeNagbars} />
								</div>
							) : null
						}
					>
						<div className={styles.contentInner}>{children}</div>
					</OutlineFrame>
				</TopNagbarContext.Provider>
			</div>
			{!mobileLayout.enabled && user && (
				<div className={styles.userAreaWrapper}>
					<UserArea user={user} />
				</div>
			)}
		</div>
	);
});
