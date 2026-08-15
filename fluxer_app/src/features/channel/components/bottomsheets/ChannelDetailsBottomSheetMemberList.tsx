// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {CompactMemberCustomStatus} from '@app/features/channel/components/CompactMemberCustomStatus';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import {MemberListUnavailableFallback} from '@app/features/channel/components/shared/MemberListUnavailableFallback';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {OFFLINE_DESCRIPTOR, ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {resolveMemberListCustomStatus} from '@app/features/member/hooks/useMemberListCustomStatus';
import {resolveMemberListPresence} from '@app/features/member/hooks/useMemberListPresence';
import {useMemberListSubscription} from '@app/features/member/hooks/useMemberListSubscription';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {
	buildMemberListLayout,
	getTotalMemberCount,
	getTotalRowsFromLayout,
} from '@app/features/member/utils/MemberListLayout';
import {buildMemberListRangeWindow} from '@app/features/member/utils/MemberListRangeUtils';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {OwnerCrownIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import {GuildFeatures, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const COMMUNITY_OWNER_DESCRIPTOR = msg({
	message: 'Community owner',
	comment: 'Short label in the channel details bottom sheet member list. Keep it concise.',
});
const MEMBER_ITEM_HEIGHT = 56;
const INITIAL_MEMBER_RANGE: [number, number] = [0, MEMBER_LIST_RANGE_MAX_SPAN];
const SCROLL_BUFFER_ROWS = 10;
const SUBSCRIPTION_OVERSCAN_PAGES = 0;
const AVATAR_DEFER_AFTER_SCROLL_IDLE_MS = 180;
const MEMBER_LIST_AVATAR_MEDIA_SIZE = 80;

function isScrollableOverflow(value: string): boolean {
	return value === 'auto' || value === 'scroll' || value === 'overlay';
}

function findScrollableParent(node: HTMLElement | null): HTMLElement | null {
	let currentNode = node?.parentElement ?? null;
	while (currentNode) {
		const computedStyle = window.getComputedStyle(currentNode);
		if (isScrollableOverflow(computedStyle.overflowY) || isScrollableOverflow(computedStyle.overflow)) {
			return currentNode;
		}
		currentNode = currentNode.parentElement;
	}
	return null;
}

function addVisibleMember(params: {
	groupedItems: Map<string, Array<GuildMember>>;
	seenMemberIds: Set<string>;
	groupId: string;
	member: GuildMember;
}): void {
	const {groupedItems, seenMemberIds, groupId, member} = params;
	const userId = member.user.id;
	if (seenMemberIds.has(userId)) {
		return;
	}
	seenMemberIds.add(userId);
	groupedItems.get(groupId)?.push(member);
}

export const SkeletonMemberItem = () => (
	<div
		className={styles.skeletonItem}
		data-flx="channel.channel-details-bottom-sheet-member-list.skeleton-member-item.skeleton-item"
	>
		<div
			className={clsx(styles.skeletonAvatar, styles.skeleton)}
			data-flx="channel.channel-details-bottom-sheet-member-list.skeleton-member-item.skeleton-avatar"
		/>
		<div
			className={styles.skeletonInfo}
			data-flx="channel.channel-details-bottom-sheet-member-list.skeleton-member-item.skeleton-info"
		>
			<div
				className={clsx(styles.skeletonName, styles.skeleton)}
				data-flx="channel.channel-details-bottom-sheet-member-list.skeleton-member-item.skeleton-name"
			/>
			<div
				className={clsx(styles.skeletonStatus, styles.skeleton)}
				data-flx="channel.channel-details-bottom-sheet-member-list.skeleton-member-item.skeleton-status"
			/>
		</div>
	</div>
);
export const MobileMemberListItem = observer(
	({
		guild,
		channelId,
		member,
		deferAvatarLoad = false,
		onLongPress,
	}: {
		guild: Guild;
		channelId: string;
		member: GuildMember;
		deferAvatarLoad?: boolean;
		onLongPress?: (member: GuildMember) => void;
	}) => {
		const {i18n} = useLingui();
		const isTyping = TypingIndicator.isMemberListTyping(channelId, member.user.id, Authentication.currentUserId);
		const status = resolveMemberListPresence({guildId: guild.id, channelId, userId: member.user.id});
		const memberListCustomStatus = resolveMemberListCustomStatus({
			guildId: guild.id,
			channelId,
			userId: member.user.id,
		});
		const handleLongPress = useCallback(() => {
			onLongPress?.(member);
		}, [member, onLongPress]);
		const displayName = member.nick ?? NicknameUtils.getNickname(member.user, guild.id);
		const avatarUrl = useMemo(
			() =>
				AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId: guild.id,
					user: member.user,
					memberAvatar: member.avatar,
					avatarUnset: member.isAvatarUnset(),
					animated: false,
					size: MEMBER_LIST_AVATAR_MEDIA_SIZE,
				}),
			[guild.id, member],
		);
		const hoverAvatarUrl = useMemo(
			() =>
				AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId: guild.id,
					user: member.user,
					memberAvatar: member.avatar,
					avatarUnset: member.isAvatarUnset(),
					animated: true,
					size: MEMBER_LIST_AVATAR_MEDIA_SIZE,
				}),
			[guild.id, member],
		);
		const content = (
			<PreloadableUserPopout
				user={member.user}
				isWebhook={false}
				guildId={guild.id}
				guildMember={member}
				position="left-start"
				data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.preloadable-user-popout"
			>
				<div
					className={`${styles.memberListItem} ${
						!member.isCurrentUser() && isOfflineStatus(status) ? styles.memberListItemOffline : ''
					}`}
					data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-list-item"
				>
					<StatusAwareAvatar
						user={member.user}
						size={40}
						isTyping={isTyping}
						showOffline={member.user.id === Authentication.currentUserId || isTyping}
						guildId={guild.id}
						status={status}
						avatarUrl={avatarUrl}
						hoverAvatarUrl={hoverAvatarUrl}
						mediaSize={MEMBER_LIST_AVATAR_MEDIA_SIZE}
						deferImageLoad={deferAvatarLoad}
						data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.status-aware-avatar"
					/>
					<div
						className={styles.memberContent}
						data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-content"
					>
						<div
							className={styles.memberNameRow}
							data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-name-row"
						>
							<span
								className={styles.memberName}
								style={{color: member.getColorString()}}
								data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-name"
							>
								{displayName}
							</span>
							{guild.isOwner(member.user.id) && !guild.features.has(GuildFeatures.HIDE_OWNER_CROWN) && (
								<div
									className={styles.crownContainer}
									data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.crown-container"
								>
									<Tooltip
										text={i18n._(COMMUNITY_OWNER_DESCRIPTOR)}
										data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.tooltip"
									>
										<OwnerCrownIcon
											className={styles.crownIcon}
											data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.crown-icon"
										/>
									</Tooltip>
								</div>
							)}
							{member.user.bot && (
								<UserTag
									className={styles.memberTag}
									system={member.user.system}
									data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-tag"
								/>
							)}
						</div>
						{!member.user.bot && (
							<CompactMemberCustomStatus
								customStatus={memberListCustomStatus}
								userId={member.user.id}
								className={styles.memberCustomStatus}
								showText={true}
								deferMediaLoad={deferAvatarLoad}
								data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.member-custom-status"
							/>
						)}
					</div>
				</div>
			</PreloadableUserPopout>
		);
		if (onLongPress) {
			return (
				<LongPressable
					onLongPress={handleLongPress}
					delay={500}
					data-flx="channel.channel-details-bottom-sheet-member-list.mobile-member-list-item.long-pressable"
				>
					{content}
				</LongPressable>
			);
		}
		return content;
	},
);

interface LazyMemberListGroupProps {
	guild: Guild;
	group: {id: string; count: number};
	channelId: string;
	members: Array<GuildMember>;
	deferAvatarLoad?: boolean;
	onMemberLongPress?: (member: GuildMember) => void;
}

const LazyMemberListGroup = observer(
	({guild, group, channelId, members, deferAvatarLoad = false, onMemberLongPress}: LazyMemberListGroupProps) => {
		const {i18n} = useLingui();
		const groupName = (() => {
			switch (group.id) {
				case 'online':
					return i18n._(ONLINE_DESCRIPTOR);
				case 'offline':
					return i18n._(OFFLINE_DESCRIPTOR);
				default: {
					const role = guild.getRole(group.id);
					return role?.name ?? group.id;
				}
			}
		})();
		return (
			<div
				className={styles.memberGroupContainer}
				data-flx="channel.channel-details-bottom-sheet-member-list.lazy-member-list-group.member-group-container"
			>
				<div
					className={styles.memberGroupHeader}
					data-flx="channel.channel-details-bottom-sheet-member-list.lazy-member-list-group.member-group-header"
				>
					{groupName}—{group.count}
				</div>
				<div
					className={styles.memberGroupList}
					data-flx="channel.channel-details-bottom-sheet-member-list.lazy-member-list-group.member-group-list"
				>
					{members.map((member, index) => (
						<React.Fragment key={member.user.id}>
							<MobileMemberListItem
								guild={guild}
								channelId={channelId}
								member={member}
								deferAvatarLoad={deferAvatarLoad}
								onLongPress={onMemberLongPress}
								data-flx="channel.channel-details-bottom-sheet-member-list.lazy-member-list-group.mobile-member-list-item"
							/>
							{index < members.length - 1 && (
								<div
									className={styles.memberDivider}
									data-flx="channel.channel-details-bottom-sheet-member-list.lazy-member-list-group.member-divider"
								/>
							)}
						</React.Fragment>
					))}
				</div>
			</div>
		);
	},
);
const LazyGuildMemberList = observer(
	({
		guild,
		channel,
		onMemberLongPress,
		enabled = true,
	}: {
		guild: Guild;
		channel: Channel;
		onMemberLongPress?: (member: GuildMember) => void;
		enabled?: boolean;
	}) => {
		const subscribedRangesRef = useRef<Array<[number, number]>>([INITIAL_MEMBER_RANGE]);
		const listContainerRef = useRef<HTMLDivElement | null>(null);
		const scrollAnimationFrameRef = useRef<number | null>(null);
		const avatarDeferTimerRef = useRef<number | null>(null);
		const [deferAvatarLoad, setDeferAvatarLoad] = useState(false);
		const memberListUpdatesDisabled = (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0;
		const currentUserId = Authentication.currentUserId;
		const lacksMemberViewPermission =
			currentUserId != null && !PermissionUtils.can(Permissions.VIEW_CHANNEL_MEMBERS, currentUserId, channel.toJSON());
		const {subscribe} = useMemberListSubscription({
			guildId: guild.id,
			channelId: channel.id,
			enabled: enabled && !memberListUpdatesDisabled && !lacksMemberViewPermission,
		});
		const memberListState = MemberSidebar.getList(guild.id, channel.id);
		const isLoading = !memberListState || memberListState.items.size === 0;
		const memberCount = memberListState?.memberCount ?? 0;
		const groups = memberListState?.groups ?? [];
		const layouts = useMemo(() => buildMemberListLayout(groups), [groups]);
		const totalMembers = useMemo(() => Math.max(memberCount, getTotalMemberCount(groups)), [groups, memberCount]);
		const totalRows = useMemo(() => {
			if (layouts.length > 0) {
				return getTotalRowsFromLayout(layouts);
			}
			return totalMembers;
		}, [layouts, totalMembers]);
		const updateSubscribedRange = useCallback(
			(scrollTop: number, clientHeight: number) => {
				const nextRanges = buildMemberListRangeWindow({
					scrollTop,
					clientHeight,
					rowHeight: MEMBER_ITEM_HEIGHT,
					bufferRows: SCROLL_BUFFER_ROWS,
					overscanPages: SUBSCRIPTION_OVERSCAN_PAGES,
					totalRows: totalRows > 0 ? totalRows : undefined,
				});
				subscribedRangesRef.current = nextRanges;
				subscribe(nextRanges);
			},
			[subscribe, totalRows],
		);
		const markAvatarLoadingDeferred = useCallback(() => {
			setDeferAvatarLoad(true);
			if (avatarDeferTimerRef.current != null) {
				window.clearTimeout(avatarDeferTimerRef.current);
			}
			avatarDeferTimerRef.current = window.setTimeout(() => {
				avatarDeferTimerRef.current = null;
				setDeferAvatarLoad(false);
			}, AVATAR_DEFER_AFTER_SCROLL_IDLE_MS);
		}, []);
		useEffect(() => {
			if (!enabled || memberListUpdatesDisabled || lacksMemberViewPermission) {
				return;
			}
			const listContainer = listContainerRef.current;
			if (!listContainer) {
				return;
			}
			const scrollParent = findScrollableParent(listContainer);
			if (!scrollParent) {
				return;
			}
			const processScroll = () => {
				scrollAnimationFrameRef.current = null;
				updateSubscribedRange(scrollParent.scrollTop, scrollParent.clientHeight);
			};
			const handleScroll = () => {
				markAvatarLoadingDeferred();
				if (scrollAnimationFrameRef.current !== null) {
					return;
				}
				scrollAnimationFrameRef.current = window.requestAnimationFrame(processScroll);
			};
			processScroll();
			scrollParent.addEventListener('scroll', handleScroll, {passive: true});
			window.addEventListener('resize', handleScroll);
			const resizeObserver =
				typeof ResizeObserver === 'undefined'
					? null
					: new ResizeObserver(() => {
							handleScroll();
						});
			resizeObserver?.observe(scrollParent);
			return () => {
				scrollParent.removeEventListener('scroll', handleScroll);
				window.removeEventListener('resize', handleScroll);
				resizeObserver?.disconnect();
				if (scrollAnimationFrameRef.current !== null) {
					window.cancelAnimationFrame(scrollAnimationFrameRef.current);
					scrollAnimationFrameRef.current = null;
				}
				if (avatarDeferTimerRef.current !== null) {
					window.clearTimeout(avatarDeferTimerRef.current);
					avatarDeferTimerRef.current = null;
				}
			};
		}, [
			enabled,
			memberListUpdatesDisabled,
			lacksMemberViewPermission,
			updateSubscribedRange,
			markAvatarLoadingDeferred,
		]);
		if (lacksMemberViewPermission) {
			return (
				<div
					className={styles.memberListFallbackContainer}
					data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-fallback-container"
				>
					<MemberListUnavailableFallback
						className={styles.memberListFallback}
						variant="permission_denied"
						data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-fallback"
					/>
				</div>
			);
		}
		if (memberListUpdatesDisabled) {
			return (
				<div
					className={styles.memberListFallbackContainer}
					data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-fallback-container"
				>
					<MemberListUnavailableFallback
						className={styles.memberListFallback}
						data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-fallback"
					/>
				</div>
			);
		}
		if (isLoading) {
			return (
				<div
					className={styles.memberListContent}
					data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-content"
				>
					<div
						className={styles.memberGroupContainer}
						data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-group-container"
					>
						<div
							className={clsx(styles.memberGroupHeader, styles.skeletonHeader, styles.skeleton)}
							data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-group-header"
						/>
						<div
							className={styles.memberGroupList}
							data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-group-list"
						>
							{Array.from({length: 10}).map((_, i) => (
								<React.Fragment key={i}>
									<SkeletonMemberItem data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.skeleton-member-item" />
									{i < 9 && (
										<div
											className={styles.memberDivider}
											data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-divider"
										/>
									)}
								</React.Fragment>
							))}
						</div>
					</div>
				</div>
			);
		}
		const subscribedRanges =
			memberListState.subscribedRanges.length > 0 ? memberListState.subscribedRanges : subscribedRangesRef.current;
		const groupedItems: Map<string, Array<GuildMember>> = new Map();
		const seenMemberIds = new Set<string>();
		const groupById = new Map(groups.map((group) => [group.id, group]));
		for (const layout of layouts) {
			const members: Array<GuildMember> = [];
			groupedItems.set(layout.id, members);
			if (layout.count === 0) {
				continue;
			}
			for (const [rangeStart, rangeEnd] of subscribedRanges) {
				const firstRow = Math.max(rangeStart, layout.headerRowIndex + 1);
				const lastRow = Math.min(rangeEnd, layout.rowEndIndex);
				if (firstRow > lastRow) {
					continue;
				}
				for (let rowIndex = firstRow; rowIndex <= lastRow; rowIndex += 1) {
					const item = memberListState.items.get(rowIndex);
					if (!item) {
						continue;
					}
					const member = MemberSidebar.materializeItemMember(guild.id, item);
					if (!member) {
						continue;
					}
					addVisibleMember({
						groupedItems,
						seenMemberIds,
						groupId: layout.id,
						member,
					});
				}
			}
		}
		return (
			<div
				className={styles.memberListContent}
				ref={listContainerRef}
				data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.member-list-content--2"
			>
				{layouts.map((layout) => {
					const group = groupById.get(layout.id);
					if (!group) {
						return null;
					}
					const members = groupedItems.get(group.id) ?? [];
					if (members.length === 0) {
						return null;
					}
					return (
						<LazyMemberListGroup
							key={group.id}
							guild={guild}
							group={group}
							channelId={channel.id}
							members={members}
							deferAvatarLoad={deferAvatarLoad}
							onMemberLongPress={onMemberLongPress}
							data-flx="channel.channel-details-bottom-sheet-member-list.lazy-guild-member-list.lazy-member-list-group"
						/>
					);
				})}
			</div>
		);
	},
);
export const GuildMemberList = observer(
	({
		guild,
		channel,
		onMemberLongPress,
		enabled = true,
	}: {
		guild: Guild;
		channel: Channel;
		onMemberLongPress?: (member: GuildMember) => void;
		enabled?: boolean;
	}) => {
		return (
			<LazyGuildMemberList
				guild={guild}
				channel={channel}
				onMemberLongPress={onMemberLongPress}
				enabled={enabled}
				data-flx="channel.channel-details-bottom-sheet-member-list.guild-member-list.lazy-guild-member-list"
			/>
		);
	},
);
