// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DiscoveryGuild} from '@app/features/discovery/commands/DiscoveryCommands';
import * as DiscoveryCommands from '@app/features/discovery/commands/DiscoveryCommands';
import {makeAutoObservable, runInAction} from 'mobx';

const DEFAULT_DISCOVERY_PAGE_SIZE = 36;

class Discovery {
	guilds: Array<DiscoveryGuild> = [];
	total = 0;
	loading = false;
	error = false;
	query = '';
	category: number | null = null;
	language: string | null = null;
	tag: string | null = null;
	sortBy = 'member_count';
	categoryCounts: ReadonlyMap<number, number> | null = null;
	categories: Array<{
		id: number;
		name: string;
		listed_in_all?: boolean;
	}> = [];
	categoriesLoaded = false;
	categoriesError = false;
	private activeSearchToken = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	async search(params: {
		query?: string;
		category?: number | null;
		language?: string | null;
		tag?: string | null;
		sortBy?: string;
		offset?: number;
		limit?: number;
	}): Promise<void> {
		const query = params.query ?? this.query;
		const category = params.category !== undefined ? params.category : this.category;
		const language = params.language !== undefined ? params.language : this.language;
		const tag = params.tag !== undefined ? params.tag : this.tag;
		const sortBy = params.sortBy ?? this.sortBy;
		const offset = params.offset ?? 0;
		const limit = params.limit ?? DEFAULT_DISCOVERY_PAGE_SIZE;
		const searchToken = ++this.activeSearchToken;
		const searchModeChanged = query.length > 0 !== this.query.length > 0;
		runInAction(() => {
			this.loading = true;
			this.error = false;
			if (offset === 0 && searchModeChanged) {
				this.guilds = [];
				this.total = 0;
			}
			this.query = query;
			this.category = category;
			this.language = language;
			this.tag = tag;
			this.sortBy = sortBy;
		});
		try {
			const result = await DiscoveryCommands.searchGuilds({
				query: query || undefined,
				category: category ?? undefined,
				language: language ?? undefined,
				tag: tag ?? undefined,
				sort_by: sortBy,
				limit,
				offset,
			});
			runInAction(() => {
				if (searchToken !== this.activeSearchToken) {
					return;
				}
				if (offset === 0) {
					this.guilds = result.guilds;
				} else {
					this.guilds = [...this.guilds, ...result.guilds];
				}
				this.total = result.total;
				this.loading = false;
			});
		} catch {
			runInAction(() => {
				if (searchToken !== this.activeSearchToken) {
					return;
				}
				this.loading = false;
				this.error = true;
			});
		}
	}

	async loadCategories(): Promise<void> {
		if (this.categoriesLoaded) return;
		try {
			const categories = await DiscoveryCommands.getCategories();
			runInAction(() => {
				this.categories = categories;
				this.categoriesLoaded = true;
				this.categoriesError = false;
			});
		} catch {
			runInAction(() => {
				this.categoriesError = true;
			});
		}
	}

	getCategoryMatchCount(categoryId: number): number | null {
		const counts = this.categoryCounts;
		if (counts == null) {
			return null;
		}
		return counts.get(categoryId) ?? 0;
	}

	get listedInAllMatchCount(): number | null {
		const counts = this.categoryCounts;
		if (counts == null) {
			return null;
		}
		let total = 0;
		for (const category of this.categories) {
			if (category.listed_in_all !== false) {
				total += counts.get(category.id) ?? 0;
			}
		}
		return total;
	}

	reset(): void {
		this.activeSearchToken += 1;
		this.guilds = [];
		this.total = 0;
		this.categoryCounts = null;
		this.loading = false;
		this.error = false;
		this.query = '';
		this.category = null;
		this.language = null;
		this.tag = null;
		this.sortBy = 'member_count';
	}
}

export default new Discovery();
