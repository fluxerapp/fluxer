// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable} from 'mobx';

class GuildBans {
	private readonly bannedUsersByGuild = new Map<string, Set<string>>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	noteBan(guildId: string, userId: string): void {
		let set = this.bannedUsersByGuild.get(guildId);
		if (!set) {
			set = new Set();
			this.bannedUsersByGuild.set(guildId, set);
		}
		set.add(userId);
	}

	noteUnban(guildId: string, userId: string): void {
		const set = this.bannedUsersByGuild.get(guildId);
		if (!set) return;
		set.delete(userId);
		if (set.size === 0) {
			this.bannedUsersByGuild.delete(guildId);
		}
	}

	isKnownBanned(guildId: string, userId: string): boolean {
		return this.bannedUsersByGuild.get(guildId)?.has(userId) ?? false;
	}

	reset(): void {
		this.bannedUsersByGuild.clear();
	}
}

const guildBans = new GuildBans();

AccountScopedWork.registerCancellation(() => guildBans.reset());

export default guildBans;
