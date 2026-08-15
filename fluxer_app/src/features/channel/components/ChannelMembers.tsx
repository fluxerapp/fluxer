// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelMembers.module.css';
import {MemberListContainer} from '@app/features/channel/components/MemberListContainer';
import {MemberListItem} from '@app/features/channel/components/MemberListItem';
import {MemberListUnavailableFallback} from '@app/features/channel/components/shared/MemberListUnavailableFallback';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {OFFLINE_DESCRIPTOR, ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {resolveMemberListCustomStatus} from '@app/features/member/hooks/useMemberListCustomStatus';
import {resolveMemberListPresence} from '@app/features/member/hooks/useMemberListPresence';
import {useMemberListSubscription} from '@app/features/member/hooks/useMemberListSubscription';
import {resolveMemberListViewportModel} from '@app/features/member/state/MemberListViewportStateMachine';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {
	buildMemberListLayout,
	buildMemberListRowOffsets,
	getGroupLayoutForRow,
	getTotalRowsFromLayout,
} from '@app/features/member/utils/MemberListLayout';
import {
	areNormalizedMemberListRangesEqual,
	buildMemberListRangeWindow,
	buildMemberListRenderWindow,
	type NormalizedMemberListRanges,
	normalizeMemberListRanges,
} from '@app/features/member/utils/MemberListRangeUtils';
import type {GroupDMMemberGroup} from '@app/features/member/utils/MemberListUtils';
import * as MemberListUtils from '@app/features/member/utils/MemberListUtils';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import Presence from '@app/features/presence/state/Presence';
import {openRoleContextMenu, openRoleContextMenuForElement} from '@app/features/ui/action_menu/RoleContextMenu';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {useLingui as useLinguiRuntime} from '@lingui/react';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type {ReactNode, UIEvent} from 'react';
import {memo, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const MEMBER_ITEM_HEIGHT = 44;
const GROUP_HEADER_HEIGHT = 32;
const INITIAL_SUBSCRIPTION_RANGE: [number, number] = [0, MEMBER_LIST_RANGE_MAX_SPAN];
const INITIAL_RENDER_RANGE: [number, number] = [0, 64];
const INITIAL_SUBSCRIPTION_RANGES = normalizeMemberListRanges([INITIAL_SUBSCRIPTION_RANGE]);
const INITIAL_RENDER_RANGES = normalizeMemberListRanges([INITIAL_RENDER_RANGE]);
const EMPTY_MEMBER_LIST_RANGES = normalizeMemberListRanges([]);
const SUBSCRIPTION_BUFFER_ROWS = 12;
const SUBSCRIPTION_OVERSCAN_PAGES = 0;
const RENDER_BUFFER_ROWS = 6;
const AVATAR_DEFER_AFTER_SCROLL_IDLE_MS = 180;
const MEMBER_LIST_AVATAR_MEDIA_SIZE = 64;
const INITIAL_LOADING_SKELETON_COUNT = 24;
const MEMBER_LIST_SKELETON_INDEXES = Array.from({length: INITIAL_LOADING_SKELETON_COUNT}, (_, index) => index);

function getSeededRandom(seed: number): number {
	const x = Math.sin(seed) * 10000;
	return x - Math.floor(x);
}

function SkeletonMemberItem({index}: {index: number}) {
	const baseSeed = (index + 1) * 17;
	const nameWidth = 40 + getSeededRandom(baseSeed) * 40;
	const statusWidth = 30 + getSeededRandom(baseSeed + 1) * 50;
	return (
		<div className={styles.skeletonItem} data-flx="channel.channel-members.skeleton-member-item.skeleton-item">
			<div className={styles.skeletonContent} data-flx="channel.channel-members.skeleton-member-item.skeleton-content">
				<div
					className={styles.skeletonAvatar}
					data-flx="channel.channel-members.skeleton-member-item.skeleton-avatar"
				/>
				<div
					className={styles.skeletonUserInfoContainer}
					data-flx="channel.channel-members.skeleton-member-item.skeleton-user-info-container"
				>
					<div
						className={styles.skeletonName}
						style={{width: `${Math.min(nameWidth, 95)}%`}}
						data-flx="channel.channel-members.skeleton-member-item.skeleton-name"
					/>
					<div
						className={styles.skeletonStatus}
						style={{width: `${Math.min(statusWidth, 95)}%`}}
						data-flx="channel.channel-members.skeleton-member-item.skeleton-status"
					/>
				</div>
			</div>
		</div>
	);
}

const MemberListLoadingSkeleton = memo(function MemberListLoadingSkeleton() {
	return (
		<div className={styles.membersList}>
			{MEMBER_LIST_SKELETON_INDEXES.map((index) => (
				<SkeletonMemberItem key={`member-loading-skeleton-${index}`} index={index} />
			))}
		</div>
	);
});

interface MemberListGroupHeaderContentProps {
	name: string;
	count: number;
}

function MemberListGroupHeaderContent({name, count}: MemberListGroupHeaderContentProps) {
	return (
		<>
			<span className={styles.groupHeaderLabel}>{name}</span>
			<span className={styles.groupHeaderSeparator}>{'—'}</span>
			<span className={styles.groupHeaderCount}>{count}</span>
		</>
	);
}

interface GroupDMMemberListGroupProps {
	group: GroupDMMemberGroup;
	channelId: string;
	ownerId: string | null;
}

const GroupDMMemberListGroup = observer(({group, channelId, ownerId}: GroupDMMemberListGroupProps) => (
	<div className={styles.groupContainer} data-flx="channel.channel-members.group-dm-member-list-group.group-container">
		<div className={styles.groupHeader} data-flx="channel.channel-members.group-dm-member-list-group.group-header">
			<MemberListGroupHeaderContent name={group.displayName} count={group.count} />
		</div>
		<div className={styles.membersList} data-flx="channel.channel-members.group-dm-member-list-group.members-list">
			{group.users.map((user) => {
				const status = Presence.getStatus(user.id);
				return (
					<MemberListItem
						key={user.id}
						user={user}
						channelId={channelId}
						status={status}
						isOwner={user.id === ownerId}
						disableBackdrop={true}
						data-flx="channel.channel-members.group-dm-member-list-group.member-list-item"
					/>
				);
			})}
		</div>
		<div className={styles.groupSpacer} data-flx="channel.channel-members.group-dm-member-list-group.group-spacer" />
	</div>
));

interface LazyMemberListProps {
	guild: Guild;
	channel: Channel;
}

const LazyMemberList = observer(function LazyMemberList({guild, channel}: LazyMemberListProps) {
	const {i18n} = useLingui();
	const subscriptionRangesRef = useRef<NormalizedMemberListRanges>(INITIAL_SUBSCRIPTION_RANGES);
	const renderRangesRef = useRef<NormalizedMemberListRanges>(INITIAL_RENDER_RANGES);
	const scrollFrameRef = useRef<number | null>(null);
	const avatarDeferTimerRef = useRef<number | null>(null);
	const avatarDeferDeadlineRef = useRef(0);
	const pendingScrollMetricsRef = useRef<{scrollTop: number; clientHeight: number} | null>(null);
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const [renderWindowRanges, setRenderWindowRanges] = useState<NormalizedMemberListRanges>(INITIAL_RENDER_RANGES);
	const [deferAvatarLoad, setDeferAvatarLoad] = useState(false);
	const memberListIdentityKey = MemberSidebar.getListIdentityKey(guild.id, channel.id);
	const memberListUpdatesDisabled = (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0;
	const currentUserId = Authentication.currentUserId;
	const lacksMemberViewPermission =
		currentUserId != null && !PermissionUtils.can(Permissions.VIEW_CHANNEL_MEMBERS, currentUserId, channel.toJSON());
	const {subscribe} = useMemberListSubscription({
		guildId: guild.id,
		channelId: channel.id,
		enabled: !memberListUpdatesDisabled && !lacksMemberViewPermission,
	});
	const memberListState = MemberSidebar.getList(guild.id, channel.id);
	const memberCount = memberListState?.memberCount ?? 0;
	const groups = memberListState?.groups ?? [];
	const zoomLevel = Accessibility.zoomLevel;
	const layouts = useMemo(() => buildMemberListLayout(groups), [groups]);
	const groupById = useMemo(() => new Map(groups.map((group) => [group.id, group])), [groups]);
	const totalRows = useMemo(() => {
		if (layouts.length > 0) {
			return getTotalRowsFromLayout(layouts);
		}
		return memberCount;
	}, [layouts, memberCount]);
	const zoomFactor = getAppZoomFactor();
	const scaledMemberItemHeight = MEMBER_ITEM_HEIGHT * zoomFactor;
	const scaledGroupHeaderHeight = GROUP_HEADER_HEIGHT * zoomFactor;
	const rowOffsets = useMemo(
		() =>
			layouts.length > 0
				? buildMemberListRowOffsets(layouts, totalRows, {
						memberHeight: scaledMemberItemHeight,
						headerHeight: scaledGroupHeaderHeight,
					})
				: null,
		[layouts, totalRows, scaledMemberItemHeight, scaledGroupHeaderHeight, zoomLevel],
	);
	const contentHeight = useMemo(
		() => (rowOffsets != null ? rowOffsets[rowOffsets.length - 1]! : Math.max(0, totalRows * scaledMemberItemHeight)),
		[rowOffsets, totalRows, scaledMemberItemHeight],
	);
	const subscribedRanges = memberListState?.subscribedRanges ?? EMPTY_MEMBER_LIST_RANGES;
	const viewportModel = useMemo(
		() =>
			resolveMemberListViewportModel({
				hasReceivedInitialPayload: Boolean(memberListState?.hasReceivedInitialPayload),
				requestedRanges: renderWindowRanges,
				subscribedRanges,
				totalRows,
			}),
		[memberListState?.hasReceivedInitialPayload, renderWindowRanges, subscribedRanges, totalRows],
	);
	const {isInitialLoading, renderRanges} = viewportModel;
	const getGroupName = useCallback(
		(groupId: string) => {
			if (groupId === 'online') {
				return i18n._(ONLINE_DESCRIPTOR);
			}
			if (groupId === 'offline') {
				return i18n._(OFFLINE_DESCRIPTOR);
			}
			return guild.getRole(groupId)?.name ?? groupId;
		},
		[guild, i18n],
	);
	const commitRangeUpdate = useCallback(
		(scrollTop: number, clientHeight: number) => {
			const nextSubscriptionRanges = buildMemberListRangeWindow({
				scrollTop,
				clientHeight,
				rowHeight: scaledMemberItemHeight,
				rowOffsets,
				bufferRows: SUBSCRIPTION_BUFFER_ROWS,
				overscanPages: SUBSCRIPTION_OVERSCAN_PAGES,
				totalRows: totalRows > 0 ? totalRows : undefined,
			});
			const nextRenderRanges = buildMemberListRenderWindow({
				scrollTop,
				clientHeight,
				rowHeight: scaledMemberItemHeight,
				rowOffsets,
				bufferRows: RENDER_BUFFER_ROWS,
				totalRows: totalRows > 0 ? totalRows : undefined,
			});
			if (!areNormalizedMemberListRangesEqual(nextRenderRanges, renderRangesRef.current)) {
				renderRangesRef.current = nextRenderRanges;
				setRenderWindowRanges(nextRenderRanges);
			}
			if (!areNormalizedMemberListRangesEqual(nextSubscriptionRanges, subscriptionRangesRef.current)) {
				subscriptionRangesRef.current = nextSubscriptionRanges;
				subscribe(nextSubscriptionRanges);
			}
		},
		[subscribe, totalRows, rowOffsets, scaledMemberItemHeight, zoomLevel],
	);
	const finishAvatarLoadingDeferralAfterIdle = useCallback(() => {
		avatarDeferTimerRef.current = null;
		const remainingMs = avatarDeferDeadlineRef.current - performance.now();
		if (remainingMs > 0) {
			avatarDeferTimerRef.current = window.setTimeout(finishAvatarLoadingDeferralAfterIdle, remainingMs);
			return;
		}
		setDeferAvatarLoad(false);
	}, []);
	const markAvatarLoadingDeferred = useCallback(() => {
		if (!deferAvatarLoad) {
			setDeferAvatarLoad(true);
		}
		avatarDeferDeadlineRef.current = performance.now() + AVATAR_DEFER_AFTER_SCROLL_IDLE_MS;
		if (avatarDeferTimerRef.current == null) {
			avatarDeferTimerRef.current = window.setTimeout(
				finishAvatarLoadingDeferralAfterIdle,
				AVATAR_DEFER_AFTER_SCROLL_IDLE_MS,
			);
		}
	}, [deferAvatarLoad, finishAvatarLoadingDeferralAfterIdle]);
	const flushScrollRangeUpdate = useCallback(() => {
		scrollFrameRef.current = null;
		const metrics = pendingScrollMetricsRef.current;
		pendingScrollMetricsRef.current = null;
		if (!metrics) {
			return;
		}
		commitRangeUpdate(metrics.scrollTop, metrics.clientHeight);
	}, [commitRangeUpdate]);
	const scheduleRangeUpdate = useCallback(
		(scrollTop: number, clientHeight: number) => {
			pendingScrollMetricsRef.current = {scrollTop, clientHeight};
			if (scrollFrameRef.current != null) {
				return;
			}
			scrollFrameRef.current = window.requestAnimationFrame(flushScrollRangeUpdate);
		},
		[flushScrollRangeUpdate],
	);
	const scheduleRangeUpdateFromScroller = useCallback(() => {
		const scrollerState = scrollerRef.current?.getScrollerState();
		if (!scrollerState) {
			return;
		}
		scheduleRangeUpdate(scrollerState.scrollTop, scrollerState.offsetHeight);
	}, [scheduleRangeUpdate]);
	const handleScroll = useCallback(
		(event: UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			markAvatarLoadingDeferred();
			scheduleRangeUpdate(target.scrollTop, target.clientHeight);
		},
		[markAvatarLoadingDeferred, scheduleRangeUpdate],
	);
	const handleResize = useCallback(() => {
		scheduleRangeUpdateFromScroller();
	}, [scheduleRangeUpdateFromScroller]);
	useEffect(() => {
		setDeferAvatarLoad(false);
		const initialSubscriptionRanges = INITIAL_SUBSCRIPTION_RANGES;
		const initialRenderRanges = INITIAL_RENDER_RANGES;
		subscriptionRangesRef.current = initialSubscriptionRanges;
		renderRangesRef.current = initialRenderRanges;
		setRenderWindowRanges(initialRenderRanges);
	}, [memberListIdentityKey, guild.id]);
	useEffect(() => {
		scheduleRangeUpdateFromScroller();
	}, [scheduleRangeUpdateFromScroller, totalRows]);
	useEffect(() => {
		return () => {
			if (scrollFrameRef.current != null) {
				window.cancelAnimationFrame(scrollFrameRef.current);
				scrollFrameRef.current = null;
			}
			if (avatarDeferTimerRef.current != null) {
				window.clearTimeout(avatarDeferTimerRef.current);
				avatarDeferTimerRef.current = null;
			}
			avatarDeferDeadlineRef.current = 0;
			pendingScrollMetricsRef.current = null;
		};
	}, [memberListIdentityKey, guild.id]);
	if (lacksMemberViewPermission) {
		return (
			<MemberListContainer
				channelId={channel.id}
				identityKey={memberListIdentityKey}
				data-flx="channel.channel-members.lazy-member-list.member-list-container"
			>
				<MemberListUnavailableFallback
					variant="permission_denied"
					data-flx="channel.channel-members.lazy-member-list.member-list-unavailable-fallback"
				/>
			</MemberListContainer>
		);
	}
	if (memberListUpdatesDisabled) {
		return (
			<MemberListContainer
				channelId={channel.id}
				identityKey={memberListIdentityKey}
				data-flx="channel.channel-members.lazy-member-list.member-list-container--2"
			>
				<MemberListUnavailableFallback data-flx="channel.channel-members.lazy-member-list.member-list-unavailable-fallback--2" />
			</MemberListContainer>
		);
	}
	if (isInitialLoading || !memberListState) {
		return (
			<MemberListContainer
				channelId={channel.id}
				identityKey={memberListIdentityKey}
				data-flx="channel.channel-members.lazy-member-list.member-list-container--3"
			>
				<MemberListLoadingSkeleton />
			</MemberListContainer>
		);
	}
	const virtualRows: Array<ReactNode> = [];
	for (const [rangeStart, rangeEnd] of renderRanges) {
		const firstRow = Math.max(0, rangeStart);
		const lastRow = totalRows > 0 ? Math.min(rangeEnd, totalRows - 1) : -1;
		for (let rowIndex = firstRow; rowIndex <= lastRow; rowIndex += 1) {
			const rowTop = rowOffsets != null ? rowOffsets[rowIndex]! : rowIndex * scaledMemberItemHeight;
			const rowStyle = {transform: `translateY(${rowTop}px)`};
			if (layouts.length === 0) {
				const item = memberListState.items.get(rowIndex);
				if (!item) {
					virtualRows.push(
						<div
							key={`member-skeleton-${rowIndex}`}
							className={clsx(styles.virtualRow, styles.virtualMemberRow)}
							style={rowStyle}
							data-flx="channel.channel-members.lazy-member-list.virtual-row.skeleton"
						>
							<SkeletonMemberItem
								index={rowIndex}
								data-flx="channel.channel-members.lazy-member-list.virtual-row.skeleton-member-item"
							/>
						</div>,
					);
					continue;
				}
				const member = MemberSidebar.materializeItemMember(guild.id, item);
				if (!member) {
					continue;
				}
				const user = member.user;
				const displayName = member.nick ?? NicknameUtils.getNickname(user, guild.id);
				const status = resolveMemberListPresence({guildId: guild.id, channelId: channel.id, userId: user.id});
				const customStatus = resolveMemberListCustomStatus({
					guildId: guild.id,
					channelId: channel.id,
					userId: user.id,
				});
				const roleColor = member.getColorString?.() ?? undefined;
				virtualRows.push(
					<div
						key={`member-${rowIndex}-${user.id}`}
						className={clsx(styles.virtualRow, styles.virtualMemberRow)}
						style={rowStyle}
						data-flx="channel.channel-members.lazy-member-list.virtual-row.member"
					>
						<MemberListItem
							user={user}
							channelId={channel.id}
							guildId={guild.id}
							guildMember={member}
							status={status}
							customStatus={customStatus}
							isOwner={guild.isOwner(user.id)}
							roleColor={roleColor}
							displayName={displayName}
							disableBackdrop={true}
							deferAvatarLoad={deferAvatarLoad}
							deferCustomStatusMedia={deferAvatarLoad}
							avatarMediaSize={MEMBER_LIST_AVATAR_MEDIA_SIZE}
							data-flx="channel.channel-members.lazy-member-list.virtual-row.member-list-item"
						/>
					</div>,
				);
				continue;
			}
			const layout = getGroupLayoutForRow(layouts, rowIndex);
			if (!layout) {
				continue;
			}
			if (rowIndex === layout.headerRowIndex) {
				const group = groupById.get(layout.id) ?? {id: layout.id, count: layout.count};
				const role = group.id === 'online' || group.id === 'offline' ? null : (guild.getRole(group.id) ?? null);
				const groupName = getGroupName(group.id);
				const groupRowContent = <MemberListGroupHeaderContent name={groupName} count={group.count} />;
				if (role) {
					virtualRows.push(
						<div
							key={`group-${rowIndex}-${group.id}`}
							className={clsx(styles.virtualRow, styles.virtualGroupRow)}
							style={rowStyle}
							role="button"
							tabIndex={0}
							onContextMenu={(event) => openRoleContextMenu(event, role.id)}
							onKeyDown={(event) => {
								if (event.key !== 'ContextMenu' && !(event.shiftKey && event.key === 'F10')) return;
								event.preventDefault();
								event.stopPropagation();
								openRoleContextMenuForElement(event.currentTarget, role.id);
							}}
							data-member-list-focus-item="true"
							data-flx="channel.channel-members.lazy-member-list.virtual-row.group"
						>
							{groupRowContent}
						</div>,
					);
				} else {
					virtualRows.push(
						<div
							key={`group-${rowIndex}-${group.id}`}
							className={clsx(styles.virtualRow, styles.virtualGroupRow)}
							style={rowStyle}
							data-flx="channel.channel-members.lazy-member-list.virtual-row.group"
						>
							{groupRowContent}
						</div>,
					);
				}
				continue;
			}
			const item = memberListState.items.get(rowIndex);
			if (!item) {
				virtualRows.push(
					<div
						key={`member-skeleton-${rowIndex}`}
						className={clsx(styles.virtualRow, styles.virtualMemberRow)}
						style={rowStyle}
						data-flx="channel.channel-members.lazy-member-list.virtual-row.skeleton--2"
					>
						<SkeletonMemberItem
							index={rowIndex}
							data-flx="channel.channel-members.lazy-member-list.virtual-row.skeleton-member-item--2"
						/>
					</div>,
				);
				continue;
			}
			const member = MemberSidebar.materializeItemMember(guild.id, item);
			if (!member) {
				continue;
			}
			const user = member.user;
			const displayName = member.nick ?? NicknameUtils.getNickname(user, guild.id);
			const status = resolveMemberListPresence({guildId: guild.id, channelId: channel.id, userId: user.id});
			const customStatus = resolveMemberListCustomStatus({
				guildId: guild.id,
				channelId: channel.id,
				userId: user.id,
			});
			const roleColor = member.getColorString?.() ?? undefined;
			virtualRows.push(
				<div
					key={`member-${rowIndex}-${user.id}`}
					className={clsx(styles.virtualRow, styles.virtualMemberRow)}
					style={rowStyle}
					data-flx="channel.channel-members.lazy-member-list.virtual-row.member--2"
				>
					<MemberListItem
						user={user}
						channelId={channel.id}
						guildId={guild.id}
						guildMember={member}
						status={status}
						customStatus={customStatus}
						isOwner={guild.isOwner(user.id)}
						roleColor={roleColor}
						displayName={displayName}
						disableBackdrop={true}
						deferAvatarLoad={deferAvatarLoad}
						deferCustomStatusMedia={deferAvatarLoad}
						avatarMediaSize={MEMBER_LIST_AVATAR_MEDIA_SIZE}
						data-flx="channel.channel-members.lazy-member-list.virtual-row.member-list-item--2"
					/>
				</div>,
			);
		}
	}
	return (
		<MemberListContainer
			channelId={channel.id}
			identityKey={memberListIdentityKey}
			scrollerRef={scrollerRef}
			onScroll={handleScroll}
			onResize={handleResize}
			estimatedContentSize={contentHeight}
			data-flx="channel.channel-members.lazy-member-list.member-list-container--4"
		>
			<div
				className={styles.virtualListContent}
				style={{height: `${contentHeight}px`}}
				data-flx="channel.channel-members.lazy-member-list.virtual-list-content"
			>
				{virtualRows}
			</div>
		</MemberListContainer>
	);
});

interface ChannelMembersProps {
	guild?: Guild | null;
	channel: Channel;
}

export const ChannelMembers = observer(function ChannelMembers({guild = null, channel}: ChannelMembersProps) {
	useLinguiRuntime();
	if (channel.type === ChannelTypes.GROUP_DM) {
		const currentUserId = Authentication.currentUserId;
		const allUserIds = currentUserId ? [currentUserId, ...channel.recipientIds] : channel.recipientIds;
		const users = allUserIds.map((id) => Users.getUser(id)).filter((user): user is User => user != null);
		const memberGroups = MemberListUtils.getGroupDMMemberGroups(users);
		return (
			<OutlineFrame hideTopBorder data-flx="channel.channel-members.outline-frame">
				<MemberListContainer channelId={channel.id} data-flx="channel.channel-members.member-list-container">
					{memberGroups.map((group) => (
						<GroupDMMemberListGroup
							key={group.id}
							group={group}
							channelId={channel.id}
							ownerId={channel.ownerId}
							data-flx="channel.channel-members.group-dm-member-list-group"
						/>
					))}
				</MemberListContainer>
			</OutlineFrame>
		);
	}
	if (!guild) {
		return null;
	}
	const frameSides = guild ? {left: false} : undefined;
	return (
		<OutlineFrame hideTopBorder sides={frameSides} data-flx="channel.channel-members.outline-frame--2">
			<LazyMemberList guild={guild} channel={channel} data-flx="channel.channel-members.lazy-member-list" />
		</OutlineFrame>
	);
});
