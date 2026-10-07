// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {makeAutoObservable, observable} from 'mobx';

export interface ThreadMembership {
	readonly threadId: string;
	readonly joinTimestamp: string;
	readonly flags: number;
	readonly muted: boolean;
	readonly muteEndTime: string | null;
}

function toMembership(threadId: string, member: ThreadMemberResponse): ThreadMembership {
	return {
		threadId,
		joinTimestamp: member.join_timestamp,
		flags: member.flags,
		muted: member.muted ?? false,
		muteEndTime: member.mute_config?.end_time ?? null,
	};
}

class ThreadMemberships {
	private readonly memberships = observable.map<string, ThreadMembership>();

	constructor() {
		makeAutoObservable<this, 'memberships'>(this, {memberships: false}, {autoBind: true});
	}

	get(threadId: string): ThreadMembership | undefined {
		return this.memberships.get(threadId);
	}

	isMember(threadId: string): boolean {
		return this.memberships.has(threadId);
	}

	isMuted(threadId: string): boolean {
		const membership = this.memberships.get(threadId);
		if (!membership?.muted) return false;
		return membership.muteEndTime == null || Date.parse(membership.muteEndTime) > Date.now();
	}

	get joinedThreadIds(): ReadonlyArray<string> {
		return Array.from(this.memberships.keys());
	}

	set(threadId: string, member: ThreadMemberResponse): void {
		this.memberships.set(threadId, toMembership(threadId, member));
	}

	remove(threadId: string): void {
		this.memberships.delete(threadId);
	}

	removeMany(threadIds: Iterable<string>): void {
		for (const threadId of threadIds) {
			this.memberships.delete(threadId);
		}
	}

	clear(): void {
		this.memberships.clear();
	}
}

export default new ThreadMemberships();
