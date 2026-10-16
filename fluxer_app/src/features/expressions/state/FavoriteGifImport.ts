// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FavoriteGifEntry} from '@app/features/channel/components/pickers/gif/FavoriteGifTypes';
import * as FavoriteGifCommands from '@app/features/expressions/commands/FavoriteGifCommands';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import {MAX_FAVORITE_GIFS} from '@fluxer/constants/src/LimitConstants';
import {makeAutoObservable, runInAction} from 'mobx';

const RESOLVE_BATCH_SIZE = 100;
const BATCH_DELAY_MS = 700;

function isValidUrl(value: string): boolean {
	try {
		const url = new URL(value);
		return url.protocol === 'http:' || url.protocol === 'https:';
	} catch {
		return false;
	}
}

class FavoriteGifImport {
	isRunning = false;
	isDone = false;
	totalToImport = 0;
	processedCount = 0;
	importedCount = 0;
	skippedCount = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get progress(): number {
		if (this.totalToImport === 0) return 0;
		return this.processedCount / this.totalToImport;
	}

	reset(): void {
		this.isRunning = false;
		this.isDone = false;
		this.totalToImport = 0;
		this.processedCount = 0;
		this.importedCount = 0;
		this.skippedCount = 0;
	}

	cancelImport(): void {
		this.isRunning = false;
	}

	async startImport(rawText: string): Promise<void> {
		if (this.isRunning) return;
		const lines = rawText
			.split('\n')
			.map((line) => line.trim())
			.filter((line) => line.length > 0 && isValidUrl(line));
		const uniqueUrls = [...new Set(lines)];
		if (uniqueUrls.length === 0) return;
		runInAction(() => {
			this.isRunning = true;
			this.isDone = false;
			this.totalToImport = uniqueUrls.length;
			this.processedCount = 0;
			this.importedCount = 0;
			this.skippedCount = 0;
		});
		let urlIndex = 0;
		while (urlIndex < uniqueUrls.length && this.isRunning) {
			const batch = uniqueUrls.slice(urlIndex, urlIndex + RESOLVE_BATCH_SIZE);
			urlIndex += RESOLVE_BATCH_SIZE;
			const current = FavoriteGif.favoriteGifs;
			const existingUrls = new Set(current.map((e) => e.url));
			const remaining = MAX_FAVORITE_GIFS - current.length;
			const urlsToResolve: Array<string> = [];
			let skipped = 0;
			for (const url of batch) {
				if (existingUrls.has(url)) {
					skipped++;
				} else if (urlsToResolve.length < remaining) {
					urlsToResolve.push(url);
				} else {
					skipped++;
				}
			}
			let resolved: Array<FavoriteGifEntry> = [];
			let resolveFailures = 0;
			if (urlsToResolve.length > 0) {
				try {
					resolved = await FavoriteGifCommands.resolveGifUrls(urlsToResolve);
				} catch {
					resolveFailures = urlsToResolve.length;
				}
			}
			const currentUrls = new Set(FavoriteGif.favoriteGifs.map((entry) => entry.url));
			const uniqueResolved: Array<FavoriteGifEntry> = [];
			let resolvedDuplicates = 0;
			for (const entry of resolved) {
				if (currentUrls.has(entry.url)) {
					resolvedDuplicates++;
					continue;
				}
				currentUrls.add(entry.url);
				uniqueResolved.push(entry);
			}
			if (uniqueResolved.length > 0) {
				FavoriteGif.replaceAll([...FavoriteGif.favoriteGifs, ...uniqueResolved]);
			}
			runInAction(() => {
				this.processedCount += batch.length;
				this.importedCount += uniqueResolved.length;
				this.skippedCount += skipped + resolveFailures + resolvedDuplicates;
			});
			if (urlIndex < uniqueUrls.length && this.isRunning) {
				await new Promise<void>((resolve) => setTimeout(resolve, BATCH_DELAY_MS));
			}
		}
		runInAction(() => {
			this.isRunning = false;
			this.isDone = true;
		});
	}
}

export default new FavoriteGifImport();
