// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGifPickerRequestId,
	createGifPickerSnapshot,
	GIF_PICKER_LOADING_SKELETON_DELAY_MS,
	GIF_PICKER_SEARCH_DEBOUNCE_MS,
	GIF_PICKER_SEARCH_MAX_WAIT_MS,
	GIF_PICKER_SUGGEST_DEBOUNCE_MS,
	GIF_PICKER_SUGGEST_MAX_WAIT_MS,
	type GifPickerMachineEvent,
	type GifPickerMachineSnapshot,
	type GifPickerModel,
	type GifPickerRequestId,
	sanitizeGifPickerSearchTerm,
	selectGifPickerModel,
	transitionGifPickerSnapshot,
} from '@app/features/channel/components/pickers/gif/GifPickerStateMachine';
import type {View} from '@app/features/channel/components/pickers/gif/GifPickerTypes';
import type {Gif, GifFeatured} from '@app/features/expressions/commands/GifCommands';
import * as GifCommands from '@app/features/expressions/commands/GifCommands';
import type {DebouncedFunction} from '@app/features/platform/utils/scroll_manager/shared';
import debounce from 'lodash/debounce';
import {makeAutoObservable, observableRef, runInAction} from 'mobx';

const EMPTY_FEATURED: GifFeatured = Object.freeze({categories: [], gifs: []});
const TRENDING_CACHE_KEY = 'trending';

type GifPickerCategoryCacheKey = typeof TRENDING_CACHE_KEY | `category:${string}`;
type GifPickerLoadingSkeletonMode = 'none' | 'delayed' | 'immediate';
type GifPickerSearchCachePolicy =
	| {kind: 'none'}
	| {
			kind: 'category';
			cacheKey: GifPickerCategoryCacheKey;
	  };

const UNCACHED_SEARCH_CACHE_POLICY: GifPickerSearchCachePolicy = Object.freeze({kind: 'none'});

function hasFeaturedContent(featured: GifFeatured): boolean {
	return featured.categories.length > 0 || featured.gifs.length > 0;
}

function toSearchTerm(term: string): string {
	return sanitizeGifPickerSearchTerm(term).trim();
}

function getCategoryCacheKey(term: string): GifPickerCategoryCacheKey {
	return `category:${term}`;
}

export class GifPickerState {
	featured: GifFeatured = EMPTY_FEATURED;
	gifs: Array<Gif> = [];
	suggestions: Array<string> = [];
	featuredFavoritePreviewSeed = Math.random();
	loadingSkeletonEpoch = 0;
	lastFetchError: unknown = null;
	private disposed = false;
	private requestSequence = 0;
	private snapshot: GifPickerMachineSnapshot = createGifPickerSnapshot();
	private categoryResultCache = new Map<GifPickerCategoryCacheKey, Array<Gif>>();
	private loadingSkeletonTimerId: NodeJS.Timeout | null = null;
	private pendingSearchSkeletonMode: GifPickerLoadingSkeletonMode = 'none';
	private searchDebounced: DebouncedFunction<(term: string) => void>;
	private suggestDebounced: DebouncedFunction<(term: string) => void>;

	constructor() {
		makeAutoObservable<
			GifPickerState,
			| 'searchDebounced'
			| 'suggestDebounced'
			| 'disposed'
			| 'requestSequence'
			| 'snapshot'
			| 'categoryResultCache'
			| 'loadingSkeletonTimerId'
			| 'pendingSearchSkeletonMode'
		>(
			this,
			{
				searchDebounced: false,
				suggestDebounced: false,
				disposed: false,
				requestSequence: false,
				snapshot: observableRef,
				categoryResultCache: false,
				loadingSkeletonTimerId: false,
				pendingSearchSkeletonMode: false,
			},
			{autoBind: true},
		);
		this.searchDebounced = debounce(
			(term: string) => {
				void this.performSearch(term);
			},
			GIF_PICKER_SEARCH_DEBOUNCE_MS,
			{maxWait: GIF_PICKER_SEARCH_MAX_WAIT_MS},
		);
		this.suggestDebounced = debounce(
			(term: string) => {
				void this.performSuggest(term);
			},
			GIF_PICKER_SUGGEST_DEBOUNCE_MS,
			{maxWait: GIF_PICKER_SUGGEST_MAX_WAIT_MS},
		);
	}

	dispose(): void {
		this.disposed = true;
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
	}

	private get model(): GifPickerModel {
		return selectGifPickerModel(this.snapshot);
	}

	private transition(event: GifPickerMachineEvent): GifPickerModel {
		const hadLoadingSkeleton = this.model.shouldShowLoadingSkeleton;
		this.snapshot = transitionGifPickerSnapshot(this.snapshot, event);
		const model = this.model;
		if (!hadLoadingSkeleton && model.shouldShowLoadingSkeleton) {
			this.loadingSkeletonEpoch += 1;
		}
		return model;
	}

	private nextRequestId(): GifPickerRequestId {
		this.requestSequence += 1;
		return createGifPickerRequestId(this.requestSequence);
	}

	private isActiveFeaturedRequest(requestId: GifPickerRequestId): boolean {
		return this.snapshot.context.activeFeaturedRequestId === requestId;
	}

	private isActiveTrendingRequest(requestId: GifPickerRequestId): boolean {
		const activeRequest = this.snapshot.context.activeResultRequest;
		return activeRequest?.id === requestId && activeRequest.kind === 'trending';
	}

	private isActiveSearchRequest(requestId: GifPickerRequestId, term: string): boolean {
		const activeRequest = this.snapshot.context.activeResultRequest;
		return (
			activeRequest?.id === requestId &&
			activeRequest.kind === 'search' &&
			activeRequest.term === term &&
			toSearchTerm(this.searchTerm) === term
		);
	}

	private isActiveSuggestRequest(requestId: GifPickerRequestId, term: string): boolean {
		const activeRequest = this.snapshot.context.activeSuggestRequest;
		return activeRequest?.id === requestId && activeRequest.term === term && toSearchTerm(this.searchTerm) === term;
	}

	private clearLoadingSkeletonTimer(): void {
		if (this.loadingSkeletonTimerId == null) {
			return;
		}
		clearTimeout(this.loadingSkeletonTimerId);
		this.loadingSkeletonTimerId = null;
	}

	private cacheCategoryResults(key: GifPickerCategoryCacheKey, results: Array<Gif>): void {
		this.categoryResultCache.set(key, results);
	}

	private openCachedTrending(): boolean {
		const cachedResults = this.categoryResultCache.get(TRENDING_CACHE_KEY);
		if (!cachedResults) {
			return false;
		}
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.gifs = cachedResults;
		this.lastFetchError = null;
		this.transition({type: 'gifPicker.trendingCacheOpened', resultCount: cachedResults.length});
		return true;
	}

	private openCachedCategory(term: string): boolean {
		const searchTerm = toSearchTerm(term);
		if (!searchTerm) {
			return false;
		}
		const cachedResults = this.categoryResultCache.get(getCategoryCacheKey(searchTerm));
		if (!cachedResults) {
			return false;
		}
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.gifs = cachedResults;
		this.lastFetchError = null;
		this.transition({type: 'gifPicker.categoryCacheOpened', term: searchTerm, resultCount: cachedResults.length});
		return true;
	}

	private scheduleLoadingSkeleton(
		requestId: GifPickerRequestId,
		kind: 'search' | 'trending',
		term: string | null,
		delayMs: number,
	): void {
		this.clearLoadingSkeletonTimer();
		this.loadingSkeletonTimerId = setTimeout(() => {
			runInAction(() => {
				if (this.disposed) {
					return;
				}
				this.loadingSkeletonTimerId = null;
				this.transition({
					type: 'gifPicker.loadingSkeletonElapsed',
					requestId,
					kind,
					term,
				});
			});
		}, delayMs);
	}

	private getTypedSearchSkeletonMode(): GifPickerLoadingSkeletonMode {
		if (this.model.shouldShowLoadingSkeleton) {
			return 'immediate';
		}
		if (this.model.isShowingResults && this.model.resultKind === 'search') {
			return 'delayed';
		}
		return 'immediate';
	}

	get view(): View {
		return this.model.view;
	}

	get searchTerm(): string {
		return this.model.searchTerm;
	}

	get previousSearchTerm(): string {
		return this.model.committedSearchTerm;
	}

	get loading(): boolean {
		return this.model.isLoading;
	}

	get shouldShowLoadingSkeleton(): boolean {
		return this.model.shouldShowLoadingSkeleton;
	}

	get loadingSkeletonKey(): string {
		return `gif-picker-loading-skeleton-${this.loadingSkeletonEpoch}`;
	}

	get initialFeaturedLoading(): boolean {
		return this.model.initialFeaturedLoading;
	}

	get hasSearchResults(): boolean {
		return this.model.isShowingResults && this.gifs.length > 0;
	}

	get shouldRenderSearchResults(): boolean {
		return this.model.isShowingResults || this.model.isResultsLoading;
	}

	get pendingView(): View | null {
		return this.model.isResultsLoading && this.model.resultKind === 'trending' ? 'trending' : null;
	}

	get isLandingPage(): boolean {
		return this.model.isLandingPage;
	}

	get isShowingFeatured(): boolean {
		return this.model.isShowingFeatured;
	}

	get isShowingFavorites(): boolean {
		return this.model.isShowingFavorites;
	}

	get gifsToRender(): Array<Gif> {
		return this.model.isShowingResults ? this.gifs : [];
	}

	get shouldShowNoResults(): boolean {
		return this.model.shouldShowNoResults;
	}

	get shouldShowError(): boolean {
		return this.model.shouldShowError;
	}

	goToDefaultView(): void {
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.suggestions = [];
		this.transition({type: 'gifPicker.defaultOpened'});
		this.randomizeFeaturedFavoritePreview();
	}

	randomizeFeaturedFavoritePreview(): void {
		this.featuredFavoritePreviewSeed = Math.random();
	}

	goToFavorites(): void {
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.suggestions = [];
		this.transition({type: 'gifPicker.favoritesOpened'});
	}

	goToTrending(): void {
		if (this.view === 'trending' && this.gifs.length > 0 && !this.loading) {
			return;
		}
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.suggestions = [];
		if (this.openCachedTrending()) {
			return;
		}
		const requestId = this.nextRequestId();
		this.transition({type: 'gifPicker.trendingRequested', requestId, showLoadingSkeleton: true});
		void this.loadTrending(requestId);
	}

	goToCategory(term: string): void {
		const searchTerm = sanitizeGifPickerSearchTerm(term);
		const trimmed = toSearchTerm(searchTerm);
		if (!trimmed) {
			this.resetSearch();
			return;
		}
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
		this.suggestions = [];
		if (this.openCachedCategory(trimmed)) {
			return;
		}
		this.pendingSearchSkeletonMode = 'immediate';
		this.transition({type: 'gifPicker.searchTermChanged', term: searchTerm, showLoadingSkeleton: true});
		void this.performSearch(trimmed, 'immediate', {kind: 'category', cacheKey: getCategoryCacheKey(trimmed)});
	}

	async ensureFeaturedLoaded(): Promise<void> {
		if (this.view !== 'default') {
			return;
		}
		if (hasFeaturedContent(this.featured)) {
			return;
		}
		const requestId = this.nextRequestId();
		this.transition({
			type: 'gifPicker.featuredRequested',
			requestId,
			hasContent: hasFeaturedContent(this.featured),
		});
		try {
			const data = await GifCommands.getFeatured();
			runInAction(() => {
				if (this.disposed) return;
				if (!this.isActiveFeaturedRequest(requestId)) return;
				this.transition({
					type: 'gifPicker.featuredSucceeded',
					requestId,
					hasContent: hasFeaturedContent(data),
				});
				this.featured = data;
				this.lastFetchError = null;
			});
		} catch (error) {
			runInAction(() => {
				if (this.disposed) return;
				this.lastFetchError = error;
				this.transition({type: 'gifPicker.featuredFailed', requestId});
			});
		}
	}

	private async loadTrending(requestId: GifPickerRequestId): Promise<void> {
		try {
			const results = await GifCommands.getTrending();
			runInAction(() => {
				if (this.disposed) return;
				if (!this.isActiveTrendingRequest(requestId)) return;
				this.clearLoadingSkeletonTimer();
				this.transition({
					type: 'gifPicker.trendingSucceeded',
					requestId,
					resultCount: results.length,
				});
				this.cacheCategoryResults(TRENDING_CACHE_KEY, results);
				this.gifs = results;
				this.lastFetchError = null;
			});
		} catch (error) {
			runInAction(() => {
				if (this.disposed) return;
				this.clearLoadingSkeletonTimer();
				this.lastFetchError = error;
				this.transition({type: 'gifPicker.trendingFailed', requestId});
			});
		}
	}

	setSearchTerm(term: string): void {
		const searchTerm = sanitizeGifPickerSearchTerm(term);
		const trimmed = toSearchTerm(searchTerm);
		if (!trimmed) {
			this.resetSearch();
			return;
		}
		this.clearLoadingSkeletonTimer();
		const skeletonMode = this.getTypedSearchSkeletonMode();
		this.pendingSearchSkeletonMode = skeletonMode;
		this.transition({
			type: 'gifPicker.searchTermChanged',
			term: searchTerm,
			showLoadingSkeleton: skeletonMode === 'immediate',
		});
		this.searchDebounced(trimmed);
		this.triggerSuggestions();
	}

	resetSearch(rawTerm = ''): void {
		this.searchDebounced.cancel();
		this.suggestDebounced.cancel();
		this.clearLoadingSkeletonTimer();
		this.pendingSearchSkeletonMode = 'none';
		this.suggestions = [];
		if (rawTerm) {
			this.transition({type: 'gifPicker.searchTermChanged', term: rawTerm});
			return;
		}
		this.transition({type: 'gifPicker.searchCleared'});
	}

	triggerSuggestions(): void {
		const trimmed = toSearchTerm(this.searchTerm);
		if (!trimmed) {
			this.suggestions = [];
			return;
		}
		this.suggestDebounced(trimmed);
	}

	flushSearch(): void {
		this.searchDebounced.cancel();
		const term = toSearchTerm(this.searchTerm);
		if (term) {
			void this.performSearch(term, this.pendingSearchSkeletonMode);
		}
	}

	private async performSearch(
		term: string,
		skeletonMode = this.pendingSearchSkeletonMode,
		cachePolicy: GifPickerSearchCachePolicy = UNCACHED_SEARCH_CACHE_POLICY,
	): Promise<void> {
		const searchTerm = toSearchTerm(term);
		if (!searchTerm) {
			runInAction(() => this.resetSearch());
			return;
		}
		if (toSearchTerm(this.searchTerm) !== searchTerm) {
			return;
		}
		const requestId = this.nextRequestId();
		this.transition({
			type: 'gifPicker.searchRequested',
			requestId,
			term: searchTerm,
			showLoadingSkeleton: skeletonMode === 'immediate',
		});
		if (skeletonMode === 'delayed') {
			this.scheduleLoadingSkeleton(requestId, 'search', searchTerm, GIF_PICKER_LOADING_SKELETON_DELAY_MS);
		} else {
			this.clearLoadingSkeletonTimer();
		}
		this.pendingSearchSkeletonMode = 'none';
		try {
			const results = await GifCommands.search(searchTerm);
			runInAction(() => {
				if (this.disposed) return;
				if (!this.isActiveSearchRequest(requestId, searchTerm)) return;
				this.clearLoadingSkeletonTimer();
				this.transition({
					type: 'gifPicker.searchSucceeded',
					requestId,
					term: searchTerm,
					resultCount: results.length,
				});
				if (cachePolicy.kind === 'category') {
					this.cacheCategoryResults(cachePolicy.cacheKey, results);
				}
				this.gifs = results;
				this.lastFetchError = null;
			});
		} catch (error) {
			runInAction(() => {
				if (this.disposed) return;
				this.clearLoadingSkeletonTimer();
				this.lastFetchError = error;
				this.transition({type: 'gifPicker.searchFailed', requestId, term: searchTerm});
			});
		}
	}

	private async performSuggest(term: string): Promise<void> {
		const searchTerm = toSearchTerm(term);
		if (!searchTerm) {
			runInAction(() => {
				this.suggestions = [];
			});
			return;
		}
		const requestId = this.nextRequestId();
		this.transition({type: 'gifPicker.suggestRequested', requestId, term: searchTerm});
		try {
			const suggestions = await GifCommands.suggest(searchTerm);
			runInAction(() => {
				if (this.disposed) return;
				if (!this.isActiveSuggestRequest(requestId, searchTerm)) return;
				this.transition({type: 'gifPicker.suggestSucceeded', requestId, term: searchTerm});
				this.suggestions = suggestions;
			});
		} catch (error) {
			runInAction(() => {
				if (this.disposed) return;
				this.lastFetchError = error;
				this.transition({type: 'gifPicker.suggestFailed', requestId, term: searchTerm});
			});
		}
	}
}
