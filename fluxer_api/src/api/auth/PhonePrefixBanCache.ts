// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {PHONE_PREFIX_BAN_REFRESH_CHANNEL} from '@app/api/constants/PhonePrefixBan';
import {Logger} from '@app/api/Logger';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const BUILT_IN_BANNED_PHONE_PREFIXES: ReadonlyArray<string> = [
	'+93',
	'+95',
	'+223',
	'+225',
	'+228',
	'+233',
	'+236',
	'+240',
	'+244',
	'+245',
	'+255',
	'+256',
	'+257',
	'+261',
	'+263',
	'+269',
	'+387',
	'+996',
];

export class PhonePrefixBanCache {
	private prefixes: Set<string> = new Set();
	private prefixLengths: ReadonlyArray<number> = [];
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'phone-prefix ban cache',
		channels: [PHONE_PREFIX_BAN_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh phone-prefix ban cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh phone-prefix ban cache after notification');
			}
		},
	});

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	initialize(): Promise<void> {
		return this.refreshSubscription.start(this.kvClient);
	}

	async refresh(): Promise<void> {
		const list = await this.adminRepository.loadAllBannedPhonePrefixes();
		const next = new Set<string>();
		for (const p of BUILT_IN_BANNED_PHONE_PREFIXES) {
			if (p.length > 0) next.add(p);
		}
		for (const p of list) {
			if (p.length > 0) next.add(p);
		}
		this.prefixes = next;
		this.prefixLengths = this.computeSortedLengths();
		this.consecutiveFailures = 0;
	}

	isBlocked(phone: string): boolean {
		const size = this.prefixes.size;
		if (size === 0) return false;
		if (size < PhonePrefixBanCache.SMALL_TABLE_THRESHOLD) {
			for (const prefix of this.prefixes) {
				if (phone.startsWith(prefix)) return true;
			}
			return false;
		}
		const lengths = this.prefixLengths;
		const phoneLen = phone.length;
		for (let i = 0; i < lengths.length; i++) {
			const len = lengths[i]!;
			if (len > phoneLen) return false;
			if (this.prefixes.has(phone.substring(0, len))) return true;
		}
		return false;
	}

	private static readonly SMALL_TABLE_THRESHOLD = 16;

	ban(prefix: string): void {
		if (prefix.length === 0) return;
		if (this.prefixes.has(prefix)) return;
		this.prefixes.add(prefix);
		this.prefixLengths = this.computeSortedLengths();
	}

	unban(prefix: string): void {
		if (!this.prefixes.delete(prefix)) return;
		this.prefixLengths = this.computeSortedLengths();
	}

	snapshot(): ReadonlySet<string> {
		return this.prefixes;
	}

	resetForTests(): void {
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down phone-prefix ban cache');
		});
		this.prefixes = new Set();
		this.prefixLengths = [];
	}

	private computeSortedLengths(): ReadonlyArray<number> {
		const seen = new Set<number>();
		for (const p of this.prefixes) seen.add(p.length);
		return [...seen].sort((a, b) => a - b);
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const phonePrefixBanCache = new PhonePrefixBanCache();
