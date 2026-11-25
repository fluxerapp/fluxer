// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export interface IFederationInboxCursor {
	getLastSeq(origin: string): Promise<number | null>;
	setLastSeq(origin: string, seq: number): Promise<void>;
}

export class FederationInboxCursor implements IFederationInboxCursor {
	constructor(private readonly kv: Pick<IKVProvider, 'get' | 'set'>) {}

	private key(origin: string): string {
		return `federation:inbox:last_seq:${origin}`;
	}

	async getLastSeq(origin: string): Promise<number | null> {
		const raw = await this.kv.get(this.key(origin));
		if (raw === null) return null;
		const value = Number(raw);
		return Number.isSafeInteger(value) && value >= 0 ? value : null;
	}

	async setLastSeq(origin: string, seq: number): Promise<void> {
		await this.kv.set(this.key(origin), String(seq));
	}
}