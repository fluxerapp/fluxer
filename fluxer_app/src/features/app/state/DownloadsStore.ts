// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DownloadEntryEvent} from '@app/types/electron';
import {makeAutoObservable} from 'mobx';

export function isDownloadInProgress(entry: DownloadEntryEvent): boolean {
	return entry.state === 'started' || entry.state === 'progressing';
}

class DownloadsStore {
	entries: ReadonlyArray<DownloadEntryEvent> = [];
	private stopListening: (() => void) | null = null;
	private revision = 0;

	constructor() {
		makeAutoObservable<DownloadsStore, 'stopListening' | 'revision'>(
			this,
			{stopListening: false, revision: false},
			{autoBind: true},
		);
	}

	get hasEntries(): boolean {
		return this.entries.length > 0;
	}

	get activeCount(): number {
		return this.entries.filter(isDownloadInProgress).length;
	}

	subscribe(): void {
		const api = getElectronAPI();
		if (this.stopListening != null || api?.onDownloadsChanged == null) return;
		this.stopListening = api.onDownloadsChanged(this.setEntries);
		this.load();
	}

	unsubscribe(): void {
		this.stopListening?.();
		this.stopListening = null;
		this.revision++;
		this.entries = [];
	}

	load(): void {
		const request = getElectronAPI()?.downloadsList?.();
		if (request == null) return;
		const revision = this.revision;
		void request.then((entries) => {
			if (revision === this.revision) this.setEntries(entries);
		});
	}

	private setEntries(entries: ReadonlyArray<DownloadEntryEvent>): void {
		this.revision++;
		this.entries = entries;
	}

	open(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadOpen?.(entry.id);
	}

	reveal(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadReveal?.(entry.id);
	}

	cancel(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadCancel?.(entry.id);
	}

	dismiss(entry: DownloadEntryEvent): void {
		getElectronAPI()?.downloadDismiss?.(entry.id);
	}

	clearList(): void {
		getElectronAPI()?.downloadsClear?.();
	}
}

export default new DownloadsStore();
