// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const PROCESSED_TTL_SECONDS = 7 * 24 * 60 * 60;
type ClaimResult = 'claimed' | 'already_processed' | 'in_flight';

export class KvOnceClaims {
	private readonly keyPrefix: string;
	private readonly inFlightTtlSeconds: number;
	constructor(
		private readonly kv: IKVProvider,
		options: {keyPrefix?: string; inFlightTtlSeconds?: number} = {},
	) {
		this.keyPrefix = options.keyPrefix ?? '';
		this.inFlightTtlSeconds = options.inFlightTtlSeconds ?? 90;
	}
	async tryClaim(id: string): Promise<ClaimResult> {
		const key = this.keyPrefix + id;
		if (await this.kv.setnx(key, 'in_flight', this.inFlightTtlSeconds)) return 'claimed';
		return (await this.kv.get(key)) === 'processed' ? 'already_processed' : 'in_flight';
	}
	async markProcessed(id: string): Promise<void> {
		await this.kv.setex(this.keyPrefix + id, PROCESSED_TTL_SECONDS, 'processed');
	}
	async releaseClaim(id: string): Promise<void> {
		await this.kv.del(this.keyPrefix + id);
	}
}
