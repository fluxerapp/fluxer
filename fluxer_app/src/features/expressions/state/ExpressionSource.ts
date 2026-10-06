// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ExpressionKind} from '@app/features/expressions/commands/ExpressionMetadataCommands';
import {
	type ExpressionSourceGuild,
	fetchExpressionSource,
} from '@app/features/expressions/commands/ExpressionSourceCommands';
import {makeAutoObservable} from 'mobx';

export type ExpressionSourceState =
	| {status: 'idle'}
	| {status: 'loading'}
	| {status: 'available'; guild: ExpressionSourceGuild}
	| {status: 'unavailable'};

const IDLE_SOURCE: ExpressionSourceState = {status: 'idle'};

function sourceKey(kind: ExpressionKind, id: string): string {
	return `${kind}:${id}`;
}

class ExpressionSourceStore {
	sources: Map<string, ExpressionSourceState> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getSource(kind: ExpressionKind, id: string): ExpressionSourceState {
		return this.sources.get(sourceKey(kind, id)) ?? IDLE_SOURCE;
	}

	async fetchSource(kind: ExpressionKind, id: string): Promise<void> {
		const key = sourceKey(kind, id);
		if (this.getSource(kind, id).status !== 'idle') {
			return;
		}
		this.setSource(key, {status: 'loading'});
		try {
			const result = await fetchExpressionSource(kind, id);
			this.setSource(key, result.available ? {status: 'available', guild: result.guild} : {status: 'unavailable'});
		} catch {
			this.setSource(key, {status: 'unavailable'});
		}
	}

	private setSource(key: string, state: ExpressionSourceState): void {
		this.sources = new Map(this.sources).set(key, state);
	}
}

export default new ExpressionSourceStore();
