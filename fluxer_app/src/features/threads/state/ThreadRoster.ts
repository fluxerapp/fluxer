// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildMembers from '@app/features/member/state/GuildMembers';
import {type StatusType, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {makeAutoObservable, observable} from 'mobx';

const MAX_MEMBER_LIST_SUBSCRIPTIONS = 10;

export interface ThreadMemberListPayload {
	guild_id: string;
	thread_id: string;
	members: ReadonlyArray<{
		user_id: string;
		join_timestamp: string | null;
		flags: number;
		member: GuildMemberData | null;
		presence?: {status?: string} | null;
	}>;
}

export interface ThreadRosterMember {
	readonly userId: string;
	readonly joinTimestamp: string | null;
	readonly online: boolean;
}

const OFFLINE_STATUSES = new Set<string>([StatusTypes.OFFLINE, StatusTypes.INVISIBLE]);

function isOnline(status: string | undefined): boolean {
	return status != null && !OFFLINE_STATUSES.has(status as StatusType);
}

class ThreadRoster {
	private readonly rosters = observable.map<string, ReadonlyArray<ThreadRosterMember>>();
	private readonly subscriptionCounts = observable.map<string, {guildId: string; count: number}>();

	constructor() {
		makeAutoObservable<this, 'rosters' | 'subscriptionCounts'>(
			this,
			{rosters: false, subscriptionCounts: false},
			{autoBind: true},
		);
	}

	getMembers(threadId: string): ReadonlyArray<ThreadRosterMember> | undefined {
		return this.rosters.get(threadId);
	}

	subscribedThreadIds(guildId: string): ReadonlyArray<string> {
		const ids: Array<string> = [];
		for (const [threadId, entry] of this.subscriptionCounts) {
			if (entry.guildId === guildId) ids.push(threadId);
		}
		ids.sort();
		return ids.slice(0, MAX_MEMBER_LIST_SUBSCRIPTIONS);
	}

	subscribe(guildId: string, threadId: string): () => void {
		const entry = this.subscriptionCounts.get(threadId);
		this.subscriptionCounts.set(threadId, {guildId, count: (entry?.count ?? 0) + 1});
		return () => this.release(threadId);
	}

	private release(threadId: string): void {
		const entry = this.subscriptionCounts.get(threadId);
		if (!entry) return;
		if (entry.count <= 1) {
			this.subscriptionCounts.delete(threadId);
			this.rosters.delete(threadId);
			return;
		}
		this.subscriptionCounts.set(threadId, {...entry, count: entry.count - 1});
	}

	handleListUpdate(payload: ThreadMemberListPayload): void {
		if (!this.subscriptionCounts.has(payload.thread_id)) return;
		const members: Array<ThreadRosterMember> = [];
		for (const entry of payload.members) {
			if (entry.member) {
				GuildMembers.hydrateIfMissing(payload.guild_id, entry.member);
			}
			members.push({
				userId: entry.user_id,
				joinTimestamp: entry.join_timestamp,
				online: isOnline(entry.presence?.status),
			});
		}
		this.rosters.set(payload.thread_id, members);
	}

	clearThread(threadId: string): void {
		this.rosters.delete(threadId);
	}

	reset(): void {
		this.rosters.clear();
	}
}

export default new ThreadRoster();
