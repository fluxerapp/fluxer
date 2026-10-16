// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {RefreshedCache} from '@app/api/utils/RefreshedCache';

interface HashSetBlocklistOptions {
	name: string;
	channel: string;
	load: () => Promise<Iterable<string>>;
	normalize: (value: string) => string;
	refreshedMessage: string;
}
export class HashSetBlocklistCache extends RefreshedCache {
	private banned = new Set<string>();
	constructor(private readonly hashOptions: HashSetBlocklistOptions) {
		super({name: hashOptions.name, channels: [hashOptions.channel]});
	}
	protected async load(): Promise<void> {
		const next = new Set<string>();
		for (const value of await this.hashOptions.load()) if (value) next.add(this.hashOptions.normalize(value));
		this.banned = next;
		Logger.debug({count: next.size}, this.hashOptions.refreshedMessage);
	}
	isBanned(value: string): boolean {
		return this.banned.has(this.hashOptions.normalize(value));
	}
	add(value: string): void {
		this.banned.add(this.hashOptions.normalize(value));
	}
	remove(value: string): void {
		this.banned.delete(this.hashOptions.normalize(value));
	}
	get size(): number {
		return this.banned.size;
	}
	resetForTesting(): void {
		this.resetRefreshState();
		this.banned = new Set();
	}
}
