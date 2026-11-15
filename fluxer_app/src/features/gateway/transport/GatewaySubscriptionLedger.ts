// SPDX-License-Identifier: AGPL-3.0-or-later

interface GatewayGuildSubscription {
	active?: boolean;
	member_list_channels?: Record<string, Array<[number, number]>>;
	typing?: boolean;
	members?: Array<string>;
	sync?: boolean;
	threads?: boolean;
	thread_member_lists?: Array<string>;
	focused_channel_id?: string | null;
}

export type GatewayGuildSubscriptions = Record<string, GatewayGuildSubscription>;

interface GatewayGuildSubscriptionRelease {
	member_list_channels?: Record<string, []>;
	members?: [];
	threads?: false;
	thread_member_lists?: [];
	focused_channel_id?: null;
}

export type GatewaySubscriptionRelease = Record<string, GatewayGuildSubscriptionRelease>;

interface MemberListSubscription {
	readonly guildId: string;
	readonly channelId: string;
}

export class GatewaySubscriptionLedger {
	private memberList: MemberListSubscription | null = null;
	private readonly memberGuilds = new Set<string>();
	private readonly threadGuilds = new Set<string>();
	private readonly focusedGuilds = new Set<string>();

	record(subscriptions: GatewayGuildSubscriptions): void {
		for (const [guildId, subscription] of Object.entries(subscriptions)) {
			for (const [channelId, ranges] of Object.entries(subscription.member_list_channels ?? {})) {
				if (ranges.length > 0) {
					this.memberList = {guildId, channelId};
				} else if (this.memberList?.guildId === guildId && this.memberList.channelId === channelId) {
					this.memberList = null;
				}
			}
			if (subscription.members !== undefined) {
				toggle(this.memberGuilds, guildId, subscription.members.length > 0);
			}
			if (subscription.threads === false) {
				this.threadGuilds.delete(guildId);
			} else if (subscription.threads === true || (subscription.thread_member_lists?.length ?? 0) > 0) {
				this.threadGuilds.add(guildId);
			}
			if (subscription.focused_channel_id !== undefined) {
				toggle(this.focusedGuilds, guildId, subscription.focused_channel_id !== null);
			}
		}
	}

	release(): GatewaySubscriptionRelease | null {
		const release: GatewaySubscriptionRelease = {};
		const entry = (guildId: string): GatewayGuildSubscriptionRelease => {
			release[guildId] ??= {};
			return release[guildId];
		};
		if (this.memberList !== null) {
			entry(this.memberList.guildId).member_list_channels = {[this.memberList.channelId]: []};
		}
		for (const guildId of this.memberGuilds) {
			entry(guildId).members = [];
		}
		for (const guildId of this.threadGuilds) {
			Object.assign(entry(guildId), {threads: false, thread_member_lists: []});
		}
		for (const guildId of this.focusedGuilds) {
			entry(guildId).focused_channel_id = null;
		}
		return Object.keys(release).length === 0 ? null : release;
	}

	clear(): void {
		this.memberList = null;
		this.memberGuilds.clear();
		this.threadGuilds.clear();
		this.focusedGuilds.clear();
	}
}

function toggle(set: Set<string>, value: string, present: boolean): void {
	if (present) {
		set.add(value);
	} else {
		set.delete(value);
	}
}
