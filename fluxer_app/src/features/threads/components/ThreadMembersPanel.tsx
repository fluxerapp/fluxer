// SPDX-License-Identifier: AGPL-3.0-or-later

import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelMembers.module.css';
import {MemberListContainer} from '@app/features/channel/components/MemberListContainer';
import {MemberListGroupHeader} from '@app/features/channel/components/MemberListGroupHeader';
import {MemberListItem} from '@app/features/channel/components/MemberListItem';
import {MemberListSkeleton, MemberListSkeletonVariant} from '@app/features/channel/components/MemberListSkeleton';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import {OFFLINE_DESCRIPTOR, ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import ThreadRoster, {type ThreadRosterMember} from '@app/features/threads/state/ThreadRoster';
import type {User} from '@app/features/user/models/User';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {isOfflineStatus, type StatusType} from '@fluxer/constants/src/StatusConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

function useThreadRosterSubscription(thread: Channel | undefined): void {
	const guildId = thread?.guildId;
	const threadId = thread?.id;
	useEffect(() => {
		if (!guildId || !threadId) return;
		return ThreadRoster.subscribe(guildId, threadId);
	}, [guildId, threadId]);
}

interface RosterRow {
	user: User;
	member: GuildMember | undefined;
	status: StatusType;
	mobile?: boolean;
	customStatus: CustomStatus | null;
}

interface RosterGroupProps {
	id: string;
	guildRole: GuildRole | null;
	label: string;
	members: ReadonlyArray<RosterRow>;
	thread: Channel;
	ownerId: string | null;
}

const RosterGroup = observer(({id, guildRole, label, members, thread, ownerId}: RosterGroupProps) => {
	if (members.length === 0) return null;
	return (
		<div className={styles.groupContainer} data-flx="threads.thread-members-panel.roster-group.group-container">
			<MemberListGroupHeader
				groupId={id}
				guildRole={guildRole}
				name={label}
				count={members.length}
				className={styles.groupHeader}
				data-flx="threads.thread-members-panel.roster-group.group-header"
			/>
			<div className={styles.membersList} data-flx="threads.thread-members-panel.roster-group.members-list">
				{members.map(({user, member, status, mobile, customStatus}) => (
					<MemberListItem
						key={user.id}
						user={user}
						channelId={thread.id}
						guildId={thread.guildId}
						guildMember={member}
						roleColor={member?.getColorString()}
						status={status}
						isMobile={mobile}
						customStatus={customStatus}
						isOwner={user.id === ownerId}
						disableBackdrop={true}
						data-flx="threads.thread-members-panel.roster-group.member-list-item"
					/>
				))}
			</div>
			<div className={styles.groupSpacer} data-flx="threads.thread-members-panel.roster-group.group-spacer" />
		</div>
	);
});

function resolveMembers(guildId: string | undefined, roster: ReadonlyArray<ThreadRosterMember>): Array<RosterRow> {
	const members: Array<RosterRow> = [];
	for (const entry of roster) {
		const user = Users.getUser(entry.userId);
		if (!user) continue;
		const member = guildId ? (GuildMembers.getMember(guildId, entry.userId) ?? undefined) : undefined;
		if (user.id === Authentication.currentUserId) {
			const status = LocalPresence.getStatus();
			members.push({user, member, status, customStatus: isOfflineStatus(status) ? null : LocalPresence.customStatus});
		} else {
			members.push({user, member, status: entry.status, mobile: entry.mobile, customStatus: entry.customStatus});
		}
	}
	members.sort((a, b) =>
		NicknameUtils.getNickname(a.user, guildId).localeCompare(NicknameUtils.getNickname(b.user, guildId)),
	);
	return members;
}

function getHoistedRoles(guild: Guild | undefined): Array<GuildRole> {
	if (!guild) return [];
	return Object.values(guild.roles)
		.filter((role) => role.hoist && role.id !== guild.id)
		.sort((a, b) => b.effectiveHoistPosition - a.effectiveHoistPosition || (BigInt(a.id) < BigInt(b.id) ? -1 : 1));
}

export const ThreadMembersList = observer(({thread}: {thread: Channel}) => {
	const {i18n} = useLingui();
	useThreadRosterSubscription(thread);
	const roster = ThreadRoster.getMembers(thread.id);
	if (!roster) {
		return (
			<MemberListSkeleton
				variant={MemberListSkeletonVariant.GUILD}
				data-flx="threads.thread-members-panel.thread-members-list.member-list-skeleton"
			/>
		);
	}
	const guild = thread.guildId ? Guilds.getGuild(thread.guildId) : undefined;
	const ownerId = guild?.ownerId ?? null;
	const hoistedRoles = getHoistedRoles(guild);
	const roleGroups = new Map<string, Array<RosterRow>>(hoistedRoles.map((role) => [role.id, []]));
	const online: Array<RosterRow> = [];
	const offline: Array<RosterRow> = [];
	for (const row of resolveMembers(thread.guildId, roster)) {
		if (isOfflineStatus(row.status)) {
			offline.push(row);
			continue;
		}
		const role = hoistedRoles.find((candidate) => row.member?.roles.has(candidate.id));
		(role ? roleGroups.get(role.id)! : online).push(row);
	}
	return (
		<>
			{hoistedRoles.map((role) => (
				<RosterGroup
					key={role.id}
					id={role.id}
					guildRole={role}
					label={role.name}
					members={roleGroups.get(role.id)!}
					thread={thread}
					ownerId={ownerId}
					data-flx="threads.thread-members-panel.thread-members-list.roster-group.role"
				/>
			))}
			<RosterGroup
				id="online"
				guildRole={null}
				label={i18n._(ONLINE_DESCRIPTOR)}
				members={online}
				thread={thread}
				ownerId={ownerId}
				data-flx="threads.thread-members-panel.thread-members-list.roster-group.online"
			/>
			<RosterGroup
				id="offline"
				guildRole={null}
				label={i18n._(OFFLINE_DESCRIPTOR)}
				members={offline}
				thread={thread}
				ownerId={ownerId}
				data-flx="threads.thread-members-panel.thread-members-list.roster-group.offline"
			/>
		</>
	);
});

export const ThreadMembersPanel = observer(({thread}: {thread: Channel}) => (
	<OutlineFrame hideTopBorder sides={{left: false}} data-flx="threads.thread-members-panel.outline-frame">
		<MemberListContainer channelId={thread.id} data-flx="threads.thread-members-panel.member-list-container">
			<ThreadMembersList thread={thread} data-flx="threads.thread-members-panel.thread-members-list" />
		</MemberListContainer>
	</OutlineFrame>
));
