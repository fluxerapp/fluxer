// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGifPickerRequestId,
	createGifPickerSnapshot,
	selectGifPickerModel,
	transitionGifPickerSnapshot,
} from '@app/features/channel/components/pickers/gif/GifPickerStateMachine';
import {describe, expect, it} from 'vitest';

describe('GifPickerStateMachine', () => {
	it('starts on the featured surface without showing a loading state', () => {
		const model = selectGifPickerModel(createGifPickerSnapshot());
		expect(model.view).toBe('default');
		expect(model.isShowingFeatured).toBe(true);
		expect(model.initialFeaturedLoading).toBe(false);
		expect(model.shouldShowNoResults).toBe(false);
	});

	it('shows the initial spinner only while the first featured request has no cached content', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.featuredRequested',
			requestId: createGifPickerRequestId(1),
			hasContent: false,
		});
		expect(selectGifPickerModel(snapshot).initialFeaturedLoading).toBe(true);
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.featuredSucceeded',
			requestId: createGifPickerRequestId(1),
			hasContent: true,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.initialFeaturedLoading).toBe(false);
		expect(model.isShowingFeatured).toBe(true);
	});

	it('keeps featured content visible while a new search is pending', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.featuredSucceeded',
			requestId: createGifPickerRequestId(1),
			hasContent: true,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'cats',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(2),
			term: 'cats',
			showLoadingSkeleton: false,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.searchTerm).toBe('cats');
		expect(model.pendingSearchTerm).toBe('cats');
		expect(model.isResultsLoading).toBe(true);
		expect(model.isShowingFeatured).toBe(true);
		expect(model.isShowingResults).toBe(false);
	});

	it('keeps old search results visible until the newer query succeeds', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'cats',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchSucceeded',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			resultCount: 20,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'dogs',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(2),
			term: 'dogs',
			showLoadingSkeleton: false,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.searchTerm).toBe('dogs');
		expect(model.committedSearchTerm).toBe('cats');
		expect(model.isResultsLoading).toBe(true);
		expect(model.isShowingResults).toBe(true);
		expect(model.shouldShowNoResults).toBe(false);
	});

	it('ignores stale search completions after the term changes', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'cats',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'dogs',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchSucceeded',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			resultCount: 20,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.searchTerm).toBe('dogs');
		expect(model.committedSearchTerm).toBe('');
		expect(model.isResultsLoading).toBe(true);
		expect(model.isShowingFeatured).toBe(true);
	});

	it('shows no-results only after an authoritative empty result succeeds', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'zzzzzz',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(1),
			term: 'zzzzzz',
			showLoadingSkeleton: false,
		});
		expect(selectGifPickerModel(snapshot).shouldShowNoResults).toBe(false);
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchSucceeded',
			requestId: createGifPickerRequestId(1),
			term: 'zzzzzz',
			resultCount: 0,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.isShowingResults).toBe(true);
		expect(model.shouldShowNoResults).toBe(true);
		expect(model.shouldShowError).toBe(false);
	});

	it('keeps old results visible when a newer search fails', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'cats',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchSucceeded',
			requestId: createGifPickerRequestId(1),
			term: 'cats',
			resultCount: 20,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'dogs',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(2),
			term: 'dogs',
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchFailed',
			requestId: createGifPickerRequestId(2),
			term: 'dogs',
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.isShowingResults).toBe(true);
		expect(model.committedSearchTerm).toBe('cats');
		expect(model.shouldShowNoResults).toBe(false);
		expect(model.shouldShowError).toBe(false);
	});

	it('shows an error state when trending fails without any retained results', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.trendingRequested',
			requestId: createGifPickerRequestId(1),
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.trendingFailed',
			requestId: createGifPickerRequestId(1),
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.isLoading).toBe(false);
		expect(model.shouldShowError).toBe(true);
		expect(model.errorKind).toBe('trending');
	});

	it('can replace the current surface with a loading skeleton immediately for uncached trending results', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.trendingRequested',
			requestId: createGifPickerRequestId(1),
			showLoadingSkeleton: true,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.isResultsLoading).toBe(true);
		expect(model.shouldShowLoadingSkeleton).toBe(true);
		expect(model.shouldShowNoResults).toBe(false);
		expect(model.shouldShowError).toBe(false);
	});

	it('keeps featured content visible until a delayed search skeleton event matches the active request', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.featuredSucceeded',
			requestId: createGifPickerRequestId(1),
			hasContent: true,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'cats',
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: createGifPickerRequestId(2),
			term: 'cats',
			showLoadingSkeleton: false,
		});
		expect(selectGifPickerModel(snapshot).shouldShowLoadingSkeleton).toBe(false);
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.loadingSkeletonElapsed',
			requestId: createGifPickerRequestId(99),
			kind: 'search',
			term: 'cats',
		});
		expect(selectGifPickerModel(snapshot).shouldShowLoadingSkeleton).toBe(false);
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.loadingSkeletonElapsed',
			requestId: createGifPickerRequestId(2),
			kind: 'search',
			term: 'cats',
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.isShowingFeatured).toBe(true);
		expect(model.shouldShowLoadingSkeleton).toBe(true);
	});

	it('shows the skeleton immediately when typing from a cleared non-search results surface', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.trendingRequested',
			requestId: createGifPickerRequestId(1),
			showLoadingSkeleton: false,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.trendingSucceeded',
			requestId: createGifPickerRequestId(1),
			resultCount: 20,
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'dogs',
			showLoadingSkeleton: true,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.view).toBe('default');
		expect(model.resultKind).toBe('trending');
		expect(model.isResultsLoading).toBe(true);
		expect(model.shouldShowLoadingSkeleton).toBe(true);
	});

	it('opens cached category results without showing a loading skeleton', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.categoryCacheOpened',
			term: 'cats',
			resultCount: 12,
		});
		const model = selectGifPickerModel(snapshot);
		expect(model.searchTerm).toBe('cats');
		expect(model.committedSearchTerm).toBe('cats');
		expect(model.isShowingResults).toBe(true);
		expect(model.isResultsLoading).toBe(false);
		expect(model.shouldShowLoadingSkeleton).toBe(false);
	});

	it('moves to favorites as an explicit non-fetching surface', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {type: 'gifPicker.favoritesOpened'});
		const model = selectGifPickerModel(snapshot);
		expect(model.view).toBe('favorites');
		expect(model.isShowingFavorites).toBe(true);
		expect(model.isLoading).toBe(false);
	});
});
