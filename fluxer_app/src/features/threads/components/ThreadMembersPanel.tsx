// SPDX-License-Identifier: AGPL-3.0-or-later

import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelMembers.module.css';
import {MemberListContainer} from '@app/features/channel/components/MemberListContainer';
import {MemberListItem} from '@app/features/channel/components/MemberListItem';
import {MemberListSkeleton, MemberListSkeletonVariant} from '@app/features/channel/components/MemberListSkeleton';
import type {Channel} from '@app/features/channel/models/Channel';
import Guilds from '@app/features/guild/state/Guilds';
import {OFFLINE_DESCRIPTOR, ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
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

export function useThreadRosterSubscription(thread: Channel | undefined): void {
	const guildId = thread?.guildId;
	const threadId = thread?.id;
	useEffect(() => {
		if (!guildId || !threadId) return;
		return ThreadRoster.subscribe(guildId, threadId);
	}, [guildId, threadId]);
}

interface RosterRow {
	user: User;
	status: StatusType;
	customStatus: CustomStatus | null;
}

interface RosterGroupProps {
	id: string;
	label: string;
	members: ReadonlyArray<RosterRow>;
	thread: Channel;
	ownerId: string | null;
}

const RosterGroup = observer(({id, label, members, thread, ownerId}: RosterGroupProps) => {
	const {i18n} = useLingui();
	if (members.length === 0) return null;
	return (
		<div className={styles.groupContainer} data-flx="threads.thread-members-panel.roster-group.group-container">
			<div
				className={styles.groupHeader}
				data-member-group-id={id}
				data-flx="threads.thread-members-panel.roster-group.group-header"
			>
				<span className={styles.groupHeaderLabel} data-flx="threads.thread-members-panel.roster-group.label">
					{label}
				</span>
				<span className={styles.groupHeaderSeparator} data-flx="threads.thread-members-panel.roster-group.separator">
					{'—'}
				</span>
				<span className={styles.groupHeaderCount} data-flx="threads.thread-members-panel.roster-group.count">
					{getCachedNumberFormat(i18n.locale).format(members.length)}
				</span>
			</div>
			<div className={styles.membersList} data-flx="threads.thread-members-panel.roster-group.members-list">
				{members.map(({user, status, customStatus}) => (
					<MemberListItem
						key={user.id}
						user={user}
						channelId={thread.id}
						guildId={thread.guildId}
						guildMember={thread.guildId ? (GuildMembers.getMember(thread.guildId, user.id) ?? undefined) : undefined}
						status={status}
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
		if (user.id === Authentication.currentUserId) {
			const status = LocalPresence.getStatus();
			members.push({user, status, customStatus: isOfflineStatus(status) ? null : LocalPresence.customStatus});
		} else {
			members.push({user, status: entry.status, customStatus: entry.customStatus});
		}
	}
	members.sort((a, b) =>
		NicknameUtils.getNickname(a.user, guildId).localeCompare(NicknameUtils.getNickname(b.user, guildId)),
	);
	return members;
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
	const members = resolveMembers(thread.guildId, roster);
	const ownerId = thread.guildId ? (Guilds.getGuild(thread.guildId)?.ownerId ?? null) : null;
	return (
		<>
			<RosterGroup
				id="online"
				label={i18n._(ONLINE_DESCRIPTOR)}
				members={members.filter((member) => !isOfflineStatus(member.status))}
				thread={thread}
				ownerId={ownerId}
				data-flx="threads.thread-members-panel.thread-members-list.roster-group.online"
			/>
			<RosterGroup
				id="offline"
				label={i18n._(OFFLINE_DESCRIPTOR)}
				members={members.filter((member) => isOfflineStatus(member.status))}
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
