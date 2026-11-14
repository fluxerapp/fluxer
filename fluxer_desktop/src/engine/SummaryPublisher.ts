// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EngineClock, EngineTimer} from '@electron/engine/EngineClock';
import type {AccountEngineAccountSummary} from '@fluxer/desktop_ipc/src/AccountEngineContract';

const SUMMARY_PERSIST_DEBOUNCE_MS = 2_000;

interface SummaryPublisherOptions {
	readonly clock: EngineClock;
	readonly coalesceMs: number;
	readonly emit: (summaries: ReadonlyArray<AccountEngineAccountSummary>) => void;
	readonly persist: (summary: AccountEngineAccountSummary) => void;
}

export class SummaryPublisher {
	private readonly options: SummaryPublisherOptions;
	private readonly summaries = new Map<string, AccountEngineAccountSummary>();
	private readonly changed = new Set<string>();
	private readonly unpersisted = new Set<string>();
	private emitTimer: EngineTimer | null = null;
	private persistTimer: EngineTimer | null = null;

	constructor(options: SummaryPublisherOptions) {
		this.options = options;
	}

	seed(summary: AccountEngineAccountSummary): void {
		this.summaries.set(summary.accountKey, summary);
	}

	update(summary: AccountEngineAccountSummary): void {
		this.summaries.set(summary.accountKey, summary);
		this.changed.add(summary.accountKey);
		this.unpersisted.add(summary.accountKey);
		if (this.emitTimer === null) {
			this.emitTimer = this.options.clock.setTimeout(() => this.emitNow(), this.options.coalesceMs);
		}
		if (this.persistTimer === null) {
			this.persistTimer = this.options.clock.setTimeout(() => this.persistNow(), SUMMARY_PERSIST_DEBOUNCE_MS);
		}
	}

	remove(accountKey: string): void {
		this.summaries.delete(accountKey);
		this.changed.delete(accountKey);
		this.unpersisted.delete(accountKey);
	}

	private emitNow(): void {
		if (this.emitTimer !== null) {
			this.options.clock.clearTimeout(this.emitTimer);
			this.emitTimer = null;
		}
		if (this.changed.size === 0) {
			return;
		}
		const batch: Array<AccountEngineAccountSummary> = [];
		for (const accountKey of this.changed) {
			const summary = this.summaries.get(accountKey);
			if (summary !== undefined) {
				batch.push(summary);
			}
		}
		this.changed.clear();
		if (batch.length > 0) {
			this.options.emit(batch);
		}
	}

	persistNow(): void {
		if (this.persistTimer !== null) {
			this.options.clock.clearTimeout(this.persistTimer);
			this.persistTimer = null;
		}
		for (const accountKey of this.unpersisted) {
			const summary = this.summaries.get(accountKey);
			if (summary !== undefined) {
				this.options.persist(summary);
			}
		}
		this.unpersisted.clear();
	}
}
