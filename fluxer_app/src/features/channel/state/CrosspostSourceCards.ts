// SPDX-License-Identifier: AGPL-3.0-or-later

import {fetchCrosspostSource} from '@app/features/channel/commands/CrosspostSourceCommands';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {CrosspostSourceGuildResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';
import {makeAutoObservable} from 'mobx';

const CARD_TTL_MS = 60_000;

export type CrosspostSourceCardState =
	| {status: 'loading'}
	| {status: 'ready'; card: CrosspostSourceGuildResponse; fetchedAt: number}
	| {status: 'unavailable'; fetchedAt: number}
	| {status: 'failed'; fetchedAt: number};

class CrosspostSourceCards {
	entries: Map<string, CrosspostSourceCardState> = new Map();
	private inflight = new Set<string>();

	constructor() {
		makeAutoObservable<CrosspostSourceCards, 'inflight'>(this, {inflight: false}, {autoBind: true});
	}

	get(sourceGuildId: string): CrosspostSourceCardState | null {
		return this.entries.get(sourceGuildId) ?? null;
	}

	ensure(sourceGuildId: string, channelId: string, messageId: string): void {
		if (this.inflight.has(sourceGuildId)) {
			return;
		}
		const entry = this.entries.get(sourceGuildId);
		if (entry != null && entry.status !== 'loading' && Date.now() - entry.fetchedAt < CARD_TTL_MS) {
			return;
		}
		this.inflight.add(sourceGuildId);
		if (entry == null) {
			this.setEntry(sourceGuildId, {status: 'loading'});
		}
		void this.load(sourceGuildId, channelId, messageId);
	}

	private async load(sourceGuildId: string, channelId: string, messageId: string): Promise<void> {
		try {
			const response = await fetchCrosspostSource(channelId, messageId);
			this.setEntry(sourceGuildId, {status: 'ready', card: response.guild, fetchedAt: Date.now()});
		} catch (error) {
			if (error instanceof HttpError && (error.status === 404 || error.status === 403)) {
				this.setEntry(sourceGuildId, {status: 'unavailable', fetchedAt: Date.now()});
			} else if (this.entries.get(sourceGuildId)?.status !== 'ready') {
				this.setEntry(sourceGuildId, {status: 'failed', fetchedAt: 0});
			}
		} finally {
			this.inflight.delete(sourceGuildId);
		}
	}

	private setEntry(sourceGuildId: string, state: CrosspostSourceCardState): void {
		this.entries = new Map(this.entries).set(sourceGuildId, state);
	}
}

export default new CrosspostSourceCards();
