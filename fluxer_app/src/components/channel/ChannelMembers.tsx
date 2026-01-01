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
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {ChannelTypes} from '~/Constants';
import {MemberListContainer} from '~/components/channel/MemberListContainer';
import {MemberListItem} from '~/components/channel/MemberListItem';
import {OutlineFrame} from '~/components/layout/OutlineFrame';
import {useMemberListSubscription} from '~/hooks/useMemberListSubscription';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import UserStore from '~/stores/UserStore';
import type {GroupDMMemberGroup, MemberGroup} from '~/utils/MemberListUtils';
import * as MemberListUtils from '~/utils/MemberListUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './ChannelMembers.module.css';

const MEMBER_ITEM_HEIGHT = 44;
const INITIAL_MEMBER_RANGE: [number, number] = [0, 99];
const SCROLL_BUFFER = 50;

const SkeletonMemberItem = ({index}: {index: number}) => {
	const seededRandom = (seed: number) => {
		const x = Math.sin(seed) * 10000;
		return x - Math.floor(x);
	};

	const baseSeed = (index + 1) * 17;
	const nameWidth = 40 + seededRandom(baseSeed) * 40;
	const statusWidth = 30 + seededRandom(baseSeed + 1) * 50;

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
};

const _MemberListGroup = observer(
	({guild, group, channelId}: {guild: GuildRecord; group: MemberGroup; channelId: string}) => (
		<div className={styles.groupContainer}>
			<div className={styles.groupHeader}>
				{group.displayName} — {group.count}
			</div>
			<div className={styles.membersList}>
				{group.members.map((member: GuildMemberRecord) => {
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
	),
);

interface GroupDMMemberListGroupProps {
	group: GroupDMMemberGroup;
	channelId: string;
	ownerId: string | null;
}

const GroupDMMemberListGroup = observer(({group, channelId, ownerId}: GroupDMMemberListGroupProps) => (
	<div className={styles.groupContainer}>
		<div className={styles.groupHeader}>
			{group.displayName} — {group.count}
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
	channelId: string;
	members: Array<GuildMemberRecord>;
}

const LazyMemberListGroup = observer(({guild, group, channelId, members}: LazyMemberListGroupProps) => {
	const {t} = useLingui();
	const getGroupName = () => {
		switch (group.id) {
			case 'online':
				return t`Online`;
			case 'offline':
				return t`Offline`;
			default: {
				const role = guild.getRole(group.id);
				return role?.name ?? group.id;
			}
		}
	};
	const groupName = getGroupName();

	return (
		<div className={styles.groupContainer}>
			<div className={styles.groupHeader}>
				{groupName} — {group.count}
			</div>
			<div className={styles.membersList}>
				{members.map((member: GuildMemberRecord) => {
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

const LazyMemberList = observer(({guild, channel}: LazyMemberListProps) => {
	const [subscribedRange, setSubscribedRange] = React.useState<[number, number]>(INITIAL_MEMBER_RANGE);

	const {subscribe} = useMemberListSubscription({
		guildId: guild.id,
		channelId: channel.id,
		enabled: true,
		allowInitialUnfocusedLoad: true,
	});

	const memberListState = MemberSidebarStore.getList(guild.id, channel.id);
	const isLoading = !memberListState || memberListState.items.size === 0;

	const handleScroll = React.useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			const scrollTop = target.scrollTop;
			const clientHeight = target.clientHeight;

			const startIndex = Math.max(0, Math.floor(scrollTop / MEMBER_ITEM_HEIGHT) - SCROLL_BUFFER);
			const endIndex = Math.ceil((scrollTop + clientHeight) / MEMBER_ITEM_HEIGHT) + SCROLL_BUFFER;

			if (startIndex !== subscribedRange[0] || endIndex !== subscribedRange[1]) {
				const newRange: [number, number] = [startIndex, endIndex];
				setSubscribedRange(newRange);
				subscribe([newRange]);
			}
		},
		[subscribedRange, subscribe],
	);

	if (isLoading) {
		return (
			<MemberListContainer channelId={channel.id}>
				<div className={styles.groupContainer}>
					<div className={clsx(styles.groupHeader, styles.skeletonHeader, styles.skeleton)} />
					<div className={styles.membersList}>
						{Array.from({length: 10}).map((_, i) => (
							<SkeletonMemberItem key={i} index={i} />
						))}
					</div>
				</div>
			</MemberListContainer>
		);
	}

	const groupedItems: Map<string, Array<GuildMemberRecord>> = new Map();
	const groups = memberListState.groups;
	const seenMemberIds = new Set<string>();

	for (const group of groups) {
		groupedItems.set(group.id, []);
	}

	let currentGroup: string | null = null;
	const sortedItems = Array.from(memberListState.items.entries()).sort(([a], [b]) => a - b);
	for (const [, item] of sortedItems) {
		if (item.type === 'group') {
			currentGroup = (item.data as {id: string}).id;
		} else if (item.type === 'member' && currentGroup) {
			const member = item.data as GuildMemberRecord;
			if (!seenMemberIds.has(member.user.id)) {
				seenMemberIds.add(member.user.id);
				const members = groupedItems.get(currentGroup);
				if (members) {
					members.push(member);
				}
			}
		}
	}

	return (
		<MemberListContainer channelId={channel.id} onScroll={handleScroll}>
			{groups.map((group) => {
				const members = groupedItems.get(group.id) ?? [];
				if (members.length === 0) {
					return null;
				}
				return (
					<LazyMemberListGroup key={group.id} guild={guild} group={group} channelId={channel.id} members={members} />
				);
			})}
		</MemberListContainer>
	);
});

interface ChannelMembersProps {
	guild?: GuildRecord | null;
	channel: ChannelRecord;
}

export const ChannelMembers = observer(({guild = null, channel}: ChannelMembersProps) => {
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;

	if (isGroupDM) {
		const currentUserId = AuthenticationStore.currentUserId;
		const allUserIds = currentUserId ? [currentUserId, ...channel.recipientIds] : channel.recipientIds;
		const users = allUserIds.map((id) => UserStore.getUser(id)).filter((u): u is UserRecord => u !== null);
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
