// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import {
	getRememberedSkeletonMemberGroups,
	reportSkeletonMemberLayout,
	SKELETON_UNMEASURED_WIDTH_PX,
	SkeletonMemberSurfaceKind,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelMembers.module.css';
import {hasVisibleCompactMemberCustomStatus} from '@app/features/channel/components/CompactMemberCustomStatus';
import {MemberListContainer} from '@app/features/channel/components/MemberListContainer';
import {MemberListItem} from '@app/features/channel/components/MemberListItem';
import {
	MEMBER_LIST_GROUP_HEADER_HEIGHT_PX,
	MEMBER_LIST_ITEM_HEIGHT_PX,
	MEMBER_LIST_METRICS_STYLE,
	MEMBER_LIST_SCROLL_CHUNK_PX,
	MEMBER_LIST_SUBSCRIPTION_VIEWPORT_RATIO,
} from '@app/features/channel/components/MemberListMetrics';
import {
	MemberListSkeleton,
	MemberListSkeletonRow,
	MemberListSkeletonVariant,
} from '@app/features/channel/components/MemberListSkeleton';
import {MemberListUnavailableFallback} from '@app/features/channel/components/shared/MemberListUnavailableFallback';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {OFFLINE_DESCRIPTOR, ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useMemberListSubscription} from '@app/features/member/hooks/useMemberListSubscription';
import {resolveMemberListViewportModel} from '@app/features/member/state/MemberListViewportStateMachine';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {
	buildMemberListLayout,
	buildMemberListRowOffsets,
	getGroupLayoutForRow,
	getTotalRowsFromLayout,
	quantizeMemberListScrollSpan,
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
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {openRoleContextMenu, openRoleContextMenuForElement} from '@app/features/ui/action_menu/RoleContextMenu';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {MEDIA_PROXY_AVATAR_SIZE_DEFAULT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {useLingui as useLinguiRuntime} from '@lingui/react';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type {ReactNode, UIEvent} from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const INITIAL_SUBSCRIPTION_RANGE: [number, number] = [0, MEMBER_LIST_RANGE_MAX_SPAN];
const INITIAL_RENDER_RANGE: [number, number] = [0, 64];
const INITIAL_SUBSCRIPTION_RANGES = normalizeMemberListRanges([INITIAL_SUBSCRIPTION_RANGE]);
const INITIAL_RENDER_RANGES = normalizeMemberListRanges([INITIAL_RENDER_RANGE]);
const EMPTY_MEMBER_LIST_RANGES = normalizeMemberListRanges([]);
const SUBSCRIPTION_OVERSCAN_PAGES = 0;
const MIN_SUBSCRIPTION_BUFFER_ROWS = 1;
const RESIDENT_SUBSCRIPTION_RANGE: [number, number] = [0, MEMBER_LIST_RANGE_MAX_SPAN];
const EMPTY_MEMBER_GROUPS: NonNullable<ReturnType<typeof MemberSidebar.getList>>['groups'] = [];

function measureMemberGroupHeadingWidthPx(element: HTMLElement | null): number {
	if (element == null) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	const firstChild = element.firstElementChild;
	const lastChild = element.lastElementChild;
	if (firstChild == null || lastChild == null) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	const left = firstChild.getBoundingClientRect().left;
	const right = lastChild.getBoundingClientRect().right;
	if (!Number.isFinite(left) || !Number.isFinite(right) || right <= left) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	return Math.round((right - left) / getRemScaleForDocument(element.ownerDocument));
}

interface GroupHeadingWidthTracking {
	groupHeadingWidths: Map<string, number>;
	registerGroupHeading: (node: HTMLDivElement | null) => void;
	captureGroupHeadingWidths: () => void;
}

function useGroupHeadingWidthTracking(): GroupHeadingWidthTracking {
	const trackingRef = useRef<GroupHeadingWidthTracking | null>(null);
	if (trackingRef.current == null) {
		const groupHeadingWidths = new Map<string, number>();
		const groupHeadingNodes = new Map<string, HTMLDivElement>();
		trackingRef.current = {
			groupHeadingWidths,
			registerGroupHeading: (node) => {
				if (node == null) {
					return;
				}
				const groupId = node.dataset.memberGroupId;
				if (groupId == null) {
					return;
				}
				groupHeadingNodes.set(groupId, node);
			},
			captureGroupHeadingWidths: () => {
				for (const [groupId, node] of groupHeadingNodes) {
					if (!node.isConnected) {
						groupHeadingNodes.delete(groupId);
						continue;
					}
					const widthPx = measureMemberGroupHeadingWidthPx(node);
					if (widthPx !== SKELETON_UNMEASURED_WIDTH_PX) {
						groupHeadingWidths.set(groupId, widthPx);
					}
				}
			},
		};
	}
	return trackingRef.current;
}

interface MemberListGroupHeaderContentProps {
	name: string;
	count: number;
}

function MemberListGroupHeaderContent({name, count}: MemberListGroupHeaderContentProps) {
	return (
		<>
			<span
				className={styles.groupHeaderLabel}
				data-flx="channel.channel-members.member-list-group-header-content.group-header-label"
			>
				{name}
			</span>
			<span
				className={styles.groupHeaderSeparator}
				data-flx="channel.channel-members.member-list-group-header-content.group-header-separator"
			>
				{'—'}
			</span>
			<span
				className={styles.groupHeaderCount}
				data-flx="channel.channel-members.member-list-group-header-content.group-header-count"
			>
				{count}
			</span>
		</>
	);
}

interface GroupDMMemberListGroupProps {
	group: GroupDMMemberGroup;
	channelId: string;
	ownerId: string | null;
	onHeadingRef: (node: HTMLDivElement | null) => void;
}

const GroupDMMemberListGroup = observer(({group, channelId, ownerId, onHeadingRef}: GroupDMMemberListGroupProps) => (
	<div className={styles.groupContainer} data-flx="channel.channel-members.group-dm-member-list-group.group-container">
		<div
			ref={onHeadingRef}
			data-member-group-id={group.id}
			className={styles.groupHeader}
			data-flx="channel.channel-members.group-dm-member-list-group.group-header"
		>
			<MemberListGroupHeaderContent
				name={group.displayName}
				count={group.count}
				data-flx="channel.channel-members.group-dm-member-list-group.member-list-group-header-content"
			/>
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
	const pendingScrollMetricsRef = useRef<{scrollTop: number; clientHeight: number} | null>(null);
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const viewportHeightRef = useRef<number | null>(null);
	const [renderWindowRanges, setRenderWindowRanges] = useState<NormalizedMemberListRanges>(INITIAL_RENDER_RANGES);
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
	const groups = memberListState?.groups ?? EMPTY_MEMBER_GROUPS;
	const populatedGroups = useMemo(() => groups.filter((group) => group.count > 0), [groups]);
	const memberGroupCountsKey = populatedGroups.map((group) => `${group.id}:${group.count}`).join(',');
	const {groupHeadingWidths, registerGroupHeading, captureGroupHeadingWidths} = useGroupHeadingWidthTracking();
	const memberSurfaceKind =
		channel.type === ChannelTypes.GUILD_VOICE ? SkeletonMemberSurfaceKind.GUILD_VOICE : SkeletonMemberSurfaceKind.GUILD;
	const rememberedMemberGroups = getRememberedSkeletonMemberGroups(channel.id, memberSurfaceKind);
	const zoomLevel = Accessibility.zoomLevel;
	const layouts = useMemo(() => buildMemberListLayout(groups), [groups]);
	const groupById = useMemo(() => new Map(groups.map((group) => [group.id, group])), [groups]);
	const totalRows = useMemo(() => {
		if (layouts.length > 0) {
			return getTotalRowsFromLayout(layouts);
		}
		return memberCount;
	}, [layouts, memberCount]);
	const remScale = getAppRemScale();
	const scaledMemberItemHeight = MEMBER_LIST_ITEM_HEIGHT_PX * remScale;
	const scaledGroupHeaderHeight = MEMBER_LIST_GROUP_HEADER_HEIGHT_PX * remScale;
	const scaledScrollChunk = MEMBER_LIST_SCROLL_CHUNK_PX * remScale;
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
	const renderRangesKey = renderRanges.map(([start, end]) => `${start}-${end}`).join(',');
	useSkeletonLayoutReport(
		() => {
			if (memberListUpdatesDisabled || lacksMemberViewPermission || !memberListState?.hasReceivedInitialPayload) {
				return;
			}
			captureGroupHeadingWidths();
			reportSkeletonMemberLayout(
				channel.id,
				memberSurfaceKind,
				populatedGroups.map((group) => ({
					rowCount: group.count,
					headingWidthPx: groupHeadingWidths.get(group.id) ?? SKELETON_UNMEASURED_WIDTH_PX,
				})),
			);
		},
		`${channel.id}|${memberSurfaceKind}|${memberGroupCountsKey}|${renderRangesKey}|${memberListState?.hasReceivedInitialPayload ?? false}|${lacksMemberViewPermission}|${memberListUpdatesDisabled}`,
	);
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
			const subscriptionBufferRows = Math.max(
				MIN_SUBSCRIPTION_BUFFER_ROWS,
				Math.ceil((clientHeight * MEMBER_LIST_SUBSCRIPTION_VIEWPORT_RATIO) / scaledMemberItemHeight),
			);
			const nextSubscriptionRanges = normalizeMemberListRanges([
				RESIDENT_SUBSCRIPTION_RANGE,
				...buildMemberListRangeWindow({
					scrollTop,
					clientHeight,
					rowHeight: scaledMemberItemHeight,
					rowOffsets,
					bufferRows: subscriptionBufferRows,
					overscanPages: SUBSCRIPTION_OVERSCAN_PAGES,
					totalRows: totalRows > 0 ? totalRows : undefined,
				}),
			]);
			const renderSpan = quantizeMemberListScrollSpan(scrollTop, clientHeight, scaledScrollChunk);
			const nextRenderRanges = buildMemberListRenderWindow({
				scrollTop: renderSpan.top,
				clientHeight: renderSpan.height,
				rowHeight: scaledMemberItemHeight,
				rowOffsets,
				bufferRows: 0,
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
		[subscribe, totalRows, rowOffsets, scaledMemberItemHeight, scaledScrollChunk, zoomLevel],
	);
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
		const scrollerState = scrollerRef.current?.readViewportMetrics();
		if (!scrollerState) {
			return;
		}
		scheduleRangeUpdate(scrollerState.scrollTop, scrollerState.offsetHeight);
	}, [scheduleRangeUpdate]);
	const handleScroll = useCallback(
		(event: UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			let viewportHeight = viewportHeightRef.current;
			if (viewportHeight == null) {
				viewportHeight = target.clientHeight;
				viewportHeightRef.current = viewportHeight;
			}
			scheduleRangeUpdate(target.scrollTop, viewportHeight);
		},
		[scheduleRangeUpdate],
	);
	const handleResize = useCallback(() => {
		viewportHeightRef.current = null;
		scheduleRangeUpdateFromScroller();
	}, [scheduleRangeUpdateFromScroller]);
	useEffect(() => {
		const initialSubscriptionRanges = INITIAL_SUBSCRIPTION_RANGES;
		const initialRenderRanges = INITIAL_RENDER_RANGES;
		subscriptionRangesRef.current = initialSubscriptionRanges;
		renderRangesRef.current = initialRenderRanges;
		viewportHeightRef.current = null;
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
				<MemberListSkeleton
					variant={MemberListSkeletonVariant.GUILD}
					memberGroups={rememberedMemberGroups}
					data-flx="channel.channel-members.lazy-member-list.member-list-skeleton"
				/>
			</MemberListContainer>
		);
	}
	const virtualRows: Array<ReactNode> = [];
	const renderedMemberIds = new Set<string>();
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
							<MemberListSkeletonRow
								index={rowIndex}
								variant={MemberListSkeletonVariant.GUILD}
								data-flx="channel.channel-members.lazy-member-list.member-list-skeleton-row"
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
				if (renderedMemberIds.has(user.id)) {
					continue;
				}
				renderedMemberIds.add(user.id);
				const displayName = member.nick
					? NicknameUtils.formatNicknameForStreamerMode(member.nick)
					: NicknameUtils.getNickname(user, guild.id);
				const roleColor = member.getColorString?.() ?? undefined;
				virtualRows.push(
					<div
						key={`member-${user.id}`}
						className={clsx(styles.virtualRow, styles.virtualMemberRow)}
						style={rowStyle}
						data-flx="channel.channel-members.lazy-member-list.virtual-row.member"
					>
						<MemberListItem
							user={user}
							channelId={channel.id}
							guildId={guild.id}
							guildMember={member}
							isOwner={guild.isOwner(user.id)}
							roleColor={roleColor}
							displayName={displayName}
							disableBackdrop={true}
							avatarMediaSize={MEDIA_PROXY_AVATAR_SIZE_DEFAULT}
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
				const groupRowContent = (
					<MemberListGroupHeaderContent
						name={groupName}
						count={group.count}
						data-flx="channel.channel-members.lazy-member-list.member-list-group-header-content"
					/>
				);
				if (role) {
					virtualRows.push(
						<div
							key={`group-${group.id}`}
							ref={registerGroupHeading}
							data-member-group-id={group.id}
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
							key={`group-${group.id}`}
							ref={registerGroupHeading}
							data-member-group-id={group.id}
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
						<MemberListSkeletonRow
							index={rowIndex}
							variant={MemberListSkeletonVariant.GUILD}
							data-flx="channel.channel-members.lazy-member-list.member-list-skeleton-row--2"
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
			if (renderedMemberIds.has(user.id)) {
				continue;
			}
			renderedMemberIds.add(user.id);
			const displayName = member.nick
				? NicknameUtils.formatNicknameForStreamerMode(member.nick)
				: NicknameUtils.getNickname(user, guild.id);
			const roleColor = member.getColorString?.() ?? undefined;
			virtualRows.push(
				<div
					key={`member-${user.id}`}
					className={clsx(styles.virtualRow, styles.virtualMemberRow)}
					style={rowStyle}
					data-flx="channel.channel-members.lazy-member-list.virtual-row.member--2"
				>
					<MemberListItem
						user={user}
						channelId={channel.id}
						guildId={guild.id}
						guildMember={member}
						isOwner={guild.isOwner(user.id)}
						roleColor={roleColor}
						displayName={displayName}
						disableBackdrop={true}
						avatarMediaSize={MEDIA_PROXY_AVATAR_SIZE_DEFAULT}
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
				style={{...MEMBER_LIST_METRICS_STYLE, height: `${contentHeight}px`}}
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

const GroupDMChannelMembers = observer(function GroupDMChannelMembers({channel}: {channel: Channel}) {
	const currentUserId = Authentication.currentUserId;
	const allUserIds = currentUserId ? [currentUserId, ...channel.recipientIds] : channel.recipientIds;
	const users = allUserIds.map((id) => Users.getUser(id)).filter((user): user is User => user != null);
	const memberGroups = MemberListUtils.getGroupDMMemberGroups(users);
	const memberGroupContentKey = memberGroups
		.map((group) => `${group.id}:${group.displayName}:${group.count}`)
		.join(',');
	const {groupHeadingWidths, registerGroupHeading, captureGroupHeadingWidths} = useGroupHeadingWidthTracking();
	useSkeletonLayoutReport(() => {
		captureGroupHeadingWidths();
		reportSkeletonMemberLayout(
			channel.id,
			SkeletonMemberSurfaceKind.GROUP_DM,
			memberGroups.map((group) => ({
				rowCount: group.count,
				headingWidthPx: groupHeadingWidths.get(group.id) ?? SKELETON_UNMEASURED_WIDTH_PX,
				subtextFlags: group.users.map((user) => hasVisibleCompactMemberCustomStatus(Presence.getCustomStatus(user.id))),
			})),
		);
	}, `${channel.id}|${memberGroupContentKey}`);
	return (
		<OutlineFrame hideTopBorder data-flx="channel.channel-members.outline-frame">
			<MemberListContainer channelId={channel.id} data-flx="channel.channel-members.member-list-container">
				{memberGroups.map((group) => (
					<GroupDMMemberListGroup
						key={group.id}
						group={group}
						channelId={channel.id}
						ownerId={channel.ownerId}
						onHeadingRef={registerGroupHeading}
						data-flx="channel.channel-members.group-dm-member-list-group"
					/>
				))}
			</MemberListContainer>
		</OutlineFrame>
	);
});

export const ChannelMembers = observer(function ChannelMembers({guild = null, channel}: ChannelMembersProps) {
	useLinguiRuntime();
	if (channel.type === ChannelTypes.GROUP_DM) {
		return <GroupDMChannelMembers channel={channel} data-flx="channel.channel-members.group-dm-channel-members" />;
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
