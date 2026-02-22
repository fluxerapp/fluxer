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

import styles from '@app/components/channel/ChannelMembers.module.css';
import {MemberListContainer} from '@app/components/channel/MemberListContainer';
import {MemberListItem} from '@app/components/channel/MemberListItem';
import {MemberListUnavailableFallback} from '@app/components/channel/shared/MemberListUnavailableFallback';
import {OutlineFrame} from '@app/components/layout/OutlineFrame';
import {resolveMemberListPresence} from '@app/hooks/useMemberListPresence';
import {useMemberListSubscription} from '@app/hooks/useMemberListSubscription';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {GuildMemberRecord} from '@app/records/GuildMemberRecord';
import type {GuildRecord} from '@app/records/GuildRecord';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import MemberSidebarStore from '@app/stores/MemberSidebarStore';
import UserStore from '@app/stores/UserStore';
import type {GroupDMMemberGroup} from '@app/utils/MemberListUtils';
import * as MemberListUtils from '@app/utils/MemberListUtils';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type {UIEvent} from 'react';
import {useCallback, useMemo, useState} from 'react';

const MEMBER_ITEM_HEIGHT = 44;
const INITIAL_MEMBER_RANGE: [number, number] = [0, 99];
const SCROLL_BUFFER = 50;

function getSeededRandom(seed: number): number {
	const x = Math.sin(seed) * 10000;
	return x - Math.floor(x);
}

function SkeletonMemberItem({index}: {index: number}) {
	const baseSeed = (index + 1) * 17;
	const nameWidth = 40 + getSeededRandom(baseSeed) * 40;
	const statusWidth = 30 + getSeededRandom(baseSeed + 1) * 50;

	return (
		<div className={styles.skeletonItem}>
			<div className={styles.skeletonContent}>
				<div className={styles.skeletonAvatar} />
				<div className={styles.skeletonUserInfoContainer}>
					<div className={styles.skeletonName} style={{width: `${Math.min(nameWidth, 95)}%`}} />
					<div className={styles.skeletonStatus} style={{width: `${Math.min(statusWidth, 95)}%`}} />
				</div>
			</div>
		</div>
	);
}

interface GroupDMMemberListGroupProps {
	group: GroupDMMemberGroup;
	channelId: string;
	ownerId: string | null;
}

const GroupDMMemberListGroup = observer(({group, channelId, ownerId}: GroupDMMemberListGroupProps) => (
	<div className={styles.groupContainer}>
		<div className={styles.groupHeader}>
			{group.displayName} {'\u2014'} {group.count}
		</div>
		<div className={styles.membersList}>
			{group.users.map((user) => (
				<MemberListItem
					key={user.id}
					user={user}
					channelId={channelId}
					isOwner={user.id === ownerId}
					disableBackdrop={true}
				/>
			))}
		</div>
		<div className={styles.groupSpacer} />
	</div>
));

interface LazyMemberListGroupProps {
	guild: GuildRecord;
	group: {id: string; count: number};
	groupCount: number;
	channelId: string;
	members: Array<GuildMemberRecord>;
}

const LazyMemberListGroup = observer(({guild, group, groupCount, channelId, members}: LazyMemberListGroupProps) => {
	const {t} = useLingui();
	const groupName = useMemo(() => {
		if (group.id === 'online') {
			return t`Online`;
		}
		if (group.id === 'offline') {
			return t`Offline`;
		}

		const role = guild.getRole(group.id);
		return role?.name ?? group.id;
	}, [group.id, guild, t]);

	return (
		<div className={styles.groupContainer}>
			<div className={styles.groupHeader}>
				{groupName} {'\u2014'} {groupCount}
			</div>
			<div className={styles.membersList}>
				{members.map((member) => {
					const user = member.user;
					const userId = user.id;

					return (
						<MemberListItem
							key={userId}
							user={user}
							channelId={channelId}
							guildId={guild.id}
							isOwner={guild.isOwner(userId)}
							roleColor={member.getColorString?.() ?? undefined}
							displayName={NicknameUtils.getNickname(user, guild.id)}
							disableBackdrop={true}
						/>
					);
				})}
			</div>
			<div className={styles.groupSpacer} />
		</div>
	);
});

interface LazyMemberListProps {
	guild: GuildRecord;
	channel: ChannelRecord;
}

const LazyMemberList = observer(function LazyMemberList({guild, channel}: LazyMemberListProps) {
	const [subscribedRange, setSubscribedRange] = useState<[number, number]>(INITIAL_MEMBER_RANGE);
	const memberListUpdatesDisabled = (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0;

	const {subscribe} = useMemberListSubscription({
		guildId: guild.id,
		channelId: channel.id,
		enabled: !memberListUpdatesDisabled,
		allowInitialUnfocusedLoad: true,
	});

	const memberListState = MemberSidebarStore.getList(guild.id, channel.id);
	const isLoading = !memberListState || memberListState.items.size === 0;

	const handleScroll = useCallback(
		(event: UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			const scrollTop = target.scrollTop;
			const clientHeight = target.clientHeight;

			const startIndex = Math.max(0, Math.floor(scrollTop / MEMBER_ITEM_HEIGHT) - SCROLL_BUFFER);
			const endIndex = Math.ceil((scrollTop + clientHeight) / MEMBER_ITEM_HEIGHT) + SCROLL_BUFFER;

			if (startIndex !== subscribedRange[0] || endIndex !== subscribedRange[1]) {
				const nextRange: [number, number] = [startIndex, endIndex];
				setSubscribedRange(nextRange);
				subscribe([nextRange]);
			}
		},
		[subscribedRange, subscribe],
	);

	if (memberListUpdatesDisabled) {
		return (
			<MemberListContainer channelId={channel.id}>
				<MemberListUnavailableFallback />
			</MemberListContainer>
		);
	}

	if (isLoading) {
		return (
			<MemberListContainer channelId={channel.id}>
				<div className={styles.groupContainer}>
					<div className={clsx(styles.groupHeader, styles.skeletonHeader, styles.skeleton)} />
					<div className={styles.membersList}>
						{Array.from({length: 10}).map((_, index) => (
							<SkeletonMemberItem key={index} index={index} />
						))}
					</div>
				</div>
			</MemberListContainer>
		);
	}

	const groupedItems: Map<string, Array<GuildMemberRecord>> = new Map();
	const groups = memberListState.groups;
	const seenMemberIds = new Set<string>();
	const groupCounts = new Map<string, number>();
	const orderedMembers: Array<GuildMemberRecord> = [];

	for (const group of groups) {
		groupedItems.set(group.id, []);
		groupCounts.set(group.id, group.count);
	}

	const sortedItems = Array.from(memberListState.items.entries()).sort(([leftIndex], [rightIndex]) => {
		return leftIndex - rightIndex;
	});
	let groupIndex = 0;
	let groupStartIndex = 0;
	let groupEndIndex = groups.length > 0 ? groups[0].count - 1 : -1;

	for (const [memberIndex, item] of sortedItems) {
		while (groupIndex < groups.length && memberIndex > groupEndIndex) {
			groupStartIndex = groupEndIndex + 1;
			groupIndex += 1;
			if (groupIndex < groups.length) {
				groupEndIndex = groupStartIndex + groups[groupIndex].count - 1;
			}
		}

		if (groupIndex >= groups.length) {
			continue;
		}

		const member = item.data;
		const userId = member.user.id;

		if (seenMemberIds.has(userId)) {
			continue;
		}

		seenMemberIds.add(userId);
		orderedMembers.push(member);
		const members = groupedItems.get(groups[groupIndex].id);
		if (members) {
			members.push(member);
		}
	}

	const hasPresenceOnlyGroups =
		groups.length > 0 &&
		groupedItems.has('online') &&
		groupedItems.has('offline') &&
		groups.every((group) => group.id === 'online' || group.id === 'offline');
	if (hasPresenceOnlyGroups) {
		const onlineMembers: Array<GuildMemberRecord> = [];
		const offlineMembers: Array<GuildMemberRecord> = [];

		for (const member of orderedMembers) {
			const status = resolveMemberListPresence({
				guildId: guild.id,
				channelId: channel.id,
				userId: member.user.id,
				enabled: true,
			});

			if (isOfflineStatus(status)) {
				offlineMembers.push(member);
			} else {
				onlineMembers.push(member);
			}
		}

		if (groupedItems.has('online')) {
			groupedItems.set('online', onlineMembers);
		}
		if (groupedItems.has('offline')) {
			groupedItems.set('offline', offlineMembers);
		}

		const isFullyLoaded = seenMemberIds.size >= memberListState.memberCount;
		if (isFullyLoaded) {
			if (groupCounts.has('online')) {
				groupCounts.set('online', onlineMembers.length);
			}
			if (groupCounts.has('offline')) {
				groupCounts.set('offline', offlineMembers.length);
			}
		}
	}

	return (
		<MemberListContainer channelId={channel.id} onScroll={handleScroll}>
			{groups.map((group) => {
				const members = groupedItems.get(group.id) ?? [];
				const groupCount = groupCounts.get(group.id) ?? group.count;
				if (members.length === 0) {
					return null;
				}

				return (
					<LazyMemberListGroup
						key={group.id}
						guild={guild}
						group={group}
						groupCount={groupCount}
						channelId={channel.id}
						members={members}
					/>
				);
			})}
		</MemberListContainer>
	);
});

interface ChannelMembersProps {
	guild?: GuildRecord | null;
	channel: ChannelRecord;
}

export const ChannelMembers = observer(function ChannelMembers({guild = null, channel}: ChannelMembersProps) {
	if (channel.type === ChannelTypes.GROUP_DM) {
		const currentUserId = AuthenticationStore.currentUserId;
		const allUserIds = currentUserId ? [currentUserId, ...channel.recipientIds] : channel.recipientIds;
		const users = allUserIds.map((id) => UserStore.getUser(id)).filter((user): user is UserRecord => user != null);
		const memberGroups = MemberListUtils.getGroupDMMemberGroups(users);

		return (
			<OutlineFrame hideTopBorder>
				<MemberListContainer channelId={channel.id}>
					{memberGroups.map((group) => (
						<GroupDMMemberListGroup key={group.id} group={group} channelId={channel.id} ownerId={channel.ownerId} />
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
		<OutlineFrame hideTopBorder sides={frameSides}>
			<LazyMemberList guild={guild} channel={channel} />
		</OutlineFrame>
	);
});
