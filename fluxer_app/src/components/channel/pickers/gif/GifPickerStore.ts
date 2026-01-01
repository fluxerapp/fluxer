/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import lodash, {type DebouncedFunc} from 'lodash';
import {makeAutoObservable, runInAction} from 'mobx';
import type {TenorFeatured, TenorGif} from '~/actions/TenorActionCreators';
import * as TenorActionCreators from '~/actions/TenorActionCreators';
import type {View} from './types';

export class GifPickerStore {
	featured: TenorFeatured = {categories: [], gifs: []};
	gifs: Array<TenorGif> = [];

	loading = false;
	initialFeaturedLoading = false;

	searchTerm = '';
	previousSearchTerm = '';
	suggestions: Array<string> = [];

	view: View = 'default';
	pendingView: View | null = null;
	hasSearchResults = false;
	shouldRenderSearchResults = false;

	private disposed = false;

	private searchDebounced: DebouncedFunc<(term: string) => void>;
	private suggestDebounced: DebouncedFunc<(term: string) => void>;

	constructor() {
		makeAutoObservable<GifPickerStore, 'searchDebounced' | 'suggestDebounced' | 'disposed'>(
			this,
			{
				searchDebounced: false,
				suggestDebounced: false,
				disposed: false,
			},
			{autoBind: true},
		);

		this.searchDebounced = lodash.debounce(this.performSearch, 400);
		this.suggestDebounced = lodash.debounce(this.performSuggest, 300);
	}

	dispose() {
		this.disposed = true;
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
	}

	get isLandingPage(): boolean {
		return this.view === 'default' && !this.searchTerm.trim();
	}

	get isShowingFeatured(): boolean {
		return this.view === 'default' && (!this.searchTerm.trim() || !this.shouldRenderSearchResults);
	}

	get gifsToRender(): Array<TenorGif> {
		return this.gifs;
	}

	get shouldShowNoResults(): boolean {
		if (this.loading) return false;
		if (!this.shouldRenderSearchResults) return false;

		if (this.view === 'trending') return this.gifs.length === 0;

		if (this.view === 'default') {
			return Boolean(this.searchTerm.trim()) && this.hasSearchResults === false;
		}

		return false;
	}

	goToDefaultView() {
		this.pendingView = null;

		this.view = 'default';

		if (!this.searchTerm.trim()) {
			this.shouldRenderSearchResults = false;
			this.loading = false;
		}
	}

	goToTrending() {
		if (this.pendingView === 'trending') return;
		if (this.view === 'trending' && this.gifs.length > 0) return;

		this.pendingView = 'trending';
		this.loading = true;

		this.searchTerm = '';
		this.previousSearchTerm = '';
		this.suggestions = [];
		this.hasSearchResults = false;
		this.shouldRenderSearchResults = true;

		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();

		void this.loadTrending();
	}

	async ensureFeaturedLoaded() {
		if (this.view !== 'default') return;
		if (this.featured.categories.length > 0 || this.featured.gifs.length > 0) return;

		this.loading = true;
		this.initialFeaturedLoading = true;

		try {
			const data = await TenorActionCreators.getFeatured();
			runInAction(() => {
				if (this.disposed) return;
				if (this.view !== 'default') return;
				this.featured = data;
				this.loading = false;
				this.initialFeaturedLoading = false;
			});
		} catch {
			runInAction(() => {
				if (this.disposed) return;
				if (this.view !== 'default') return;
				this.loading = false;
				this.initialFeaturedLoading = false;
			});
		}
	}

	private async loadTrending() {
		try {
			const results = await TenorActionCreators.getTrending();
			runInAction(() => {
				if (this.disposed) return;
				if (this.pendingView !== 'trending') return;

				this.view = 'trending';
				this.pendingView = null;
				this.gifs = results;
				this.loading = false;
				this.hasSearchResults = results.length > 0;
			});
		} catch {
			runInAction(() => {
				if (this.disposed) return;
				if (this.pendingView !== 'trending') return;
				this.pendingView = null;
				this.loading = false;
			});
		}
	}

	setSearchTerm(term: string) {
		const limited = term.slice(0, 100);

		if (!limited.trim()) {
			this.searchTerm = limited;
			this.resetSearch();
			return;
		}

		this.previousSearchTerm = this.searchTerm;
		this.searchTerm = limited;
		this.loading = true;
		this.shouldRenderSearchResults = true;

		const trimmed = limited.trim();
		this.searchDebounced(trimmed);

		this.triggerSuggestions();
	}

	resetSearch() {
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();

		this.searchTerm = '';
		this.previousSearchTerm = '';
		this.gifs = [];
		this.suggestions = [];
		this.hasSearchResults = false;
		this.loading = false;
		this.shouldRenderSearchResults = false;
	}

	triggerSuggestions() {
		const trimmed = this.searchTerm.trim();
		if (!trimmed) {
			runInAction(() => {
				this.suggestions = [];
			});
			return;
		}
		this.suggestDebounced(trimmed);
	}

	flushSearch() {
		this.searchDebounced.cancel();
		const term = this.searchTerm.trim();
		if (term) {
			this.loading = true;
			this.shouldRenderSearchResults = true;
			void this.performSearch(term);
		}
	}

	private async performSearch(term: string) {
		if (!term) {
			runInAction(() => this.resetSearch());
			return;
		}

		try {
			const results = await TenorActionCreators.search(term);
			runInAction(() => {
				if (this.disposed) return;
				if (this.searchTerm.trim() !== term) return;

				this.gifs = results;
				this.loading = false;
				this.hasSearchResults = results.length > 0;
				this.shouldRenderSearchResults = true;
			});
		} catch {
			runInAction(() => {
				if (this.disposed) return;
				if (this.searchTerm.trim() !== term) return;
				this.loading = false;
			});
		}
	}

	private async performSuggest(term: string) {
		if (!term) {
			runInAction(() => {
				this.suggestions = [];
			});
			return;
		}

		try {
			const suggestions = await TenorActionCreators.suggest(term);
			runInAction(() => {
				if (this.disposed) return;
				if (this.searchTerm.trim() !== term) return;
				this.suggestions = suggestions;
			});
		} catch {}
	}
}
