// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DownloadEntryEvent} from '@app/types/electron';
import {makeAutoObservable} from 'mobx';

const MAX_ENTRIES = 6;

function isClearable(entry: DownloadEntryEvent): boolean {
	return entry.state === 'completed' || entry.state === 'canceled' || entry.state === 'failed';
}

class DownloadsStore {
	entries: Array<DownloadEntryEvent> = [];
	private unsubscribe: (() => void) | null = null;

	constructor() {
		makeAutoObservable<DownloadsStore, 'unsubscribe'>(this, {unsubscribe: false}, {autoBind: true});
	}

	get hasEntries(): boolean {
		return this.entries.length > 0;
	}

	subscribe(): void {
		if (this.unsubscribe) return;
		const electronApi = getElectronAPI();
		const unsubscribe = electronApi?.onDownloadChanged?.((entry) => {
			this.upsert(entry);
		});
		this.unsubscribe = unsubscribe ?? null;
	}

	unsubscribeFromNative(): void {
		this.unsubscribe?.();
		this.unsubscribe = null;
	}

	upsert(entry: DownloadEntryEvent): void {
		const index = this.entries.findIndex((existing) => existing.id === entry.id);
		if (index === -1) {
			this.entries = [entry, ...this.entries];
			this.trimToLimit();
			return;
		}
		const next = this.entries.slice();
		next[index] = entry;
		this.entries = next;
	}

	private trimToLimit(): void {
		if (this.entries.length <= MAX_ENTRIES) return;
		const overflow = this.entries.length - MAX_ENTRIES;
		const dropped = new Set<string>();
		for (let index = this.entries.length - 1; index >= 0 && dropped.size < overflow; index -= 1) {
			const candidate = this.entries[index];
			if (isClearable(candidate)) dropped.add(candidate.id);
		}
		for (let index = this.entries.length - 1; index >= 0 && dropped.size < overflow; index -= 1) {
			const candidate = this.entries[index];
			if (dropped.has(candidate.id)) continue;
			dropped.add(candidate.id);
			if (!isClearable(candidate)) this.cancel(candidate);
		}
		this.entries = this.entries.filter((entry) => !dropped.has(entry.id));
	}

	open(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadOpen?.(entry.filePath);
	}

	reveal(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadReveal?.(entry.filePath);
	}

	cancel(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadCancel?.(entry.id);
	}

	dismiss(entry: DownloadEntryEvent): void {
		this.entries = this.entries.filter((existing) => existing.id !== entry.id);
	}

	clearAll(): void {
		this.entries = this.entries.filter((entry) => entry.state === 'started' || entry.state === 'progressing');
	}
}

export default new DownloadsStore();
