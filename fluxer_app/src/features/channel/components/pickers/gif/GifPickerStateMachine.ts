// SPDX-License-Identifier: AGPL-3.0-or-later

import type {View} from '@app/features/channel/components/pickers/gif/GifPickerTypes';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

declare const gifPickerRequestIdBrand: unique symbol;

export type GifPickerRequestId = number & {
	readonly [gifPickerRequestIdBrand]: 'GifPickerRequestId';
};

export type GifPickerSurface = 'featured' | 'results' | 'favorites';
export type GifPickerResultKind = 'search' | 'trending' | null;
export type GifPickerFetchErrorKind = 'featured' | 'search' | 'trending' | null;

export const GIF_PICKER_MAX_SEARCH_TERM_LENGTH = 100;
export const GIF_PICKER_SEARCH_DEBOUNCE_MS = 350;
export const GIF_PICKER_SEARCH_MAX_WAIT_MS = 1000;
export const GIF_PICKER_LOADING_SKELETON_DELAY_MS = 800;
export const GIF_PICKER_SUGGEST_DEBOUNCE_MS = 250;
export const GIF_PICKER_SUGGEST_MAX_WAIT_MS = 900;

export interface GifPickerResultRequest {
	id: GifPickerRequestId;
	kind: Exclude<GifPickerResultKind, null>;
	term: string | null;
}

export interface GifPickerSuggestRequest {
	id: GifPickerRequestId;
	term: string;
}

export interface GifPickerMachineContext {
	searchTerm: string;
	committedSearchTerm: string;
	pendingSearchTerm: string | null;
	surface: GifPickerSurface;
	hasFeaturedContent: boolean;
	isFeaturedLoading: boolean;
	isResultsLoading: boolean;
	loadingSkeletonVisible: boolean;
	activeFeaturedRequestId: GifPickerRequestId | null;
	activeResultRequest: GifPickerResultRequest | null;
	activeSuggestRequest: GifPickerSuggestRequest | null;
	resultKind: GifPickerResultKind;
	resultCount: number;
	resultStatus: 'idle' | 'success' | 'empty' | 'error';
	errorKind: GifPickerFetchErrorKind;
}

export interface GifPickerModel {
	view: View;
	searchTerm: string;
	committedSearchTerm: string;
	surface: GifPickerSurface;
	isLoading: boolean;
	isFeaturedLoading: boolean;
	isResultsLoading: boolean;
	shouldShowLoadingSkeleton: boolean;
	initialFeaturedLoading: boolean;
	isLandingPage: boolean;
	isShowingFeatured: boolean;
	isShowingFavorites: boolean;
	isShowingResults: boolean;
	shouldShowNoResults: boolean;
	shouldShowError: boolean;
	errorKind: GifPickerFetchErrorKind;
	resultKind: GifPickerResultKind;
	pendingSearchTerm: string | null;
}

export type GifPickerMachineEvent =
	| {
			type: 'gifPicker.defaultOpened';
	  }
	| {
			type: 'gifPicker.favoritesOpened';
	  }
	| {
			type: 'gifPicker.featuredRequested';
			requestId: GifPickerRequestId;
			hasContent: boolean;
	  }
	| {
			type: 'gifPicker.featuredSucceeded';
			requestId: GifPickerRequestId;
			hasContent: boolean;
	  }
	| {
			type: 'gifPicker.featuredFailed';
			requestId: GifPickerRequestId;
	  }
	| {
			type: 'gifPicker.trendingRequested';
			requestId: GifPickerRequestId;
			showLoadingSkeleton: boolean;
	  }
	| {
			type: 'gifPicker.trendingCacheOpened';
			resultCount: number;
	  }
	| {
			type: 'gifPicker.trendingSucceeded';
			requestId: GifPickerRequestId;
			resultCount: number;
	  }
	| {
			type: 'gifPicker.trendingFailed';
			requestId: GifPickerRequestId;
	  }
	| {
			type: 'gifPicker.searchTermChanged';
			term: string;
			showLoadingSkeleton?: boolean;
	  }
	| {
			type: 'gifPicker.searchCleared';
	  }
	| {
			type: 'gifPicker.searchRequested';
			requestId: GifPickerRequestId;
			term: string;
			showLoadingSkeleton: boolean;
	  }
	| {
			type: 'gifPicker.categoryCacheOpened';
			term: string;
			resultCount: number;
	  }
	| {
			type: 'gifPicker.searchSucceeded';
			requestId: GifPickerRequestId;
			term: string;
			resultCount: number;
	  }
	| {
			type: 'gifPicker.searchFailed';
			requestId: GifPickerRequestId;
			term: string;
	  }
	| {
			type: 'gifPicker.suggestRequested';
			requestId: GifPickerRequestId;
			term: string;
	  }
	| {
			type: 'gifPicker.suggestSucceeded';
			requestId: GifPickerRequestId;
			term: string;
	  }
	| {
			type: 'gifPicker.suggestFailed';
			requestId: GifPickerRequestId;
			term: string;
	  }
	| {
			type: 'gifPicker.loadingSkeletonElapsed';
			requestId: GifPickerRequestId;
			kind: Exclude<GifPickerResultKind, null>;
			term: string | null;
	  };

export type GifPickerMachineSnapshot = SnapshotFrom<typeof gifPickerStateMachine>;

export function createGifPickerRequestId(value: number): GifPickerRequestId {
	if (!Number.isSafeInteger(value) || value < 0) {
		throw new Error('GifPickerRequestId must be a non-negative safe integer');
	}
	return value as GifPickerRequestId;
}

export function sanitizeGifPickerSearchTerm(term: string): string {
	return term.slice(0, GIF_PICKER_MAX_SEARCH_TERM_LENGTH);
}

function trimSearchTerm(term: string): string {
	return sanitizeGifPickerSearchTerm(term).trim();
}

function isActiveFeaturedRequest(context: GifPickerMachineContext, requestId: GifPickerRequestId): boolean {
	return context.activeFeaturedRequestId === requestId;
}

function isActiveResultRequest(
	context: GifPickerMachineContext,
	requestId: GifPickerRequestId,
	kind: Exclude<GifPickerResultKind, null>,
	term: string | null,
): boolean {
	const activeRequest = context.activeResultRequest;
	return activeRequest?.id === requestId && activeRequest.kind === kind && activeRequest.term === term;
}

function isActiveSuggestRequest(
	context: GifPickerMachineContext,
	requestId: GifPickerRequestId,
	term: string,
): boolean {
	const activeRequest = context.activeSuggestRequest;
	return activeRequest?.id === requestId && activeRequest.term === term;
}

export const gifPickerStateMachine = setup({
	types: {} as {
		context: GifPickerMachineContext;
		events: GifPickerMachineEvent;
	},
	actions: {
		openDefault: assign({
			searchTerm: '',
			pendingSearchTerm: null,
			surface: 'featured',
			isResultsLoading: false,
			loadingSkeletonVisible: false,
			activeResultRequest: null,
			activeSuggestRequest: null,
			errorKind: null,
		}),
		openFavorites: assign({
			searchTerm: '',
			committedSearchTerm: '',
			pendingSearchTerm: null,
			surface: 'favorites',
			isResultsLoading: false,
			loadingSkeletonVisible: false,
			activeResultRequest: null,
			activeSuggestRequest: null,
			errorKind: null,
		}),
		requestFeatured: assign(({event}) => {
			if (event.type !== 'gifPicker.featuredRequested') {
				return {};
			}
			return {
				activeFeaturedRequestId: event.requestId,
				hasFeaturedContent: event.hasContent,
				isFeaturedLoading: true,
				errorKind: null,
			};
		}),
		applyFeaturedSuccess: assign(({context, event}) => {
			if (event.type !== 'gifPicker.featuredSucceeded') {
				return {};
			}
			if (!isActiveFeaturedRequest(context, event.requestId)) {
				return {};
			}
			return {
				activeFeaturedRequestId: null,
				hasFeaturedContent: event.hasContent,
				isFeaturedLoading: false,
				errorKind: null,
			};
		}),
		applyFeaturedFailure: assign(({context, event}) => {
			if (event.type !== 'gifPicker.featuredFailed') {
				return {};
			}
			if (!isActiveFeaturedRequest(context, event.requestId)) {
				return {};
			}
			return {
				activeFeaturedRequestId: null,
				isFeaturedLoading: false,
				errorKind: 'featured',
			};
		}),
		requestTrending: assign(({event}) => {
			if (event.type !== 'gifPicker.trendingRequested') {
				return {};
			}
			return {
				searchTerm: '',
				pendingSearchTerm: null,
				isResultsLoading: true,
				loadingSkeletonVisible: event.showLoadingSkeleton,
				activeResultRequest: {
					id: event.requestId,
					kind: 'trending',
					term: null,
				},
				activeSuggestRequest: null,
				errorKind: null,
			};
		}),
		openTrendingCache: assign(({event}) => {
			if (event.type !== 'gifPicker.trendingCacheOpened') {
				return {};
			}
			return {
				searchTerm: '',
				pendingSearchTerm: null,
				surface: 'results',
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				activeSuggestRequest: null,
				resultKind: 'trending',
				resultCount: event.resultCount,
				resultStatus: event.resultCount > 0 ? 'success' : 'empty',
				errorKind: null,
			};
		}),
		applyTrendingSuccess: assign(({context, event}) => {
			if (event.type !== 'gifPicker.trendingSucceeded') {
				return {};
			}
			if (!isActiveResultRequest(context, event.requestId, 'trending', null)) {
				return {};
			}
			return {
				surface: 'results',
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				resultKind: 'trending',
				resultCount: event.resultCount,
				resultStatus: event.resultCount > 0 ? 'success' : 'empty',
				errorKind: null,
			};
		}),
		applyTrendingFailure: assign(({context, event}) => {
			if (event.type !== 'gifPicker.trendingFailed') {
				return {};
			}
			if (!isActiveResultRequest(context, event.requestId, 'trending', null)) {
				return {};
			}
			return {
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				resultStatus: context.resultCount > 0 ? context.resultStatus : 'error',
				errorKind: 'trending',
			};
		}),
		applySearchTerm: assign(({event}) => {
			if (event.type !== 'gifPicker.searchTermChanged') {
				return {};
			}
			const searchTerm = sanitizeGifPickerSearchTerm(event.term);
			const trimmedTerm = searchTerm.trim();
			if (!trimmedTerm) {
				return {
					searchTerm,
					pendingSearchTerm: null,
					surface: 'featured',
					isResultsLoading: false,
					loadingSkeletonVisible: false,
					activeResultRequest: null,
					activeSuggestRequest: null,
					resultStatus: 'idle',
					errorKind: null,
				};
			}
			return {
				searchTerm,
				pendingSearchTerm: trimmedTerm,
				isResultsLoading: true,
				loadingSkeletonVisible: event.showLoadingSkeleton === true,
				errorKind: null,
			};
		}),
		clearSearch: assign({
			searchTerm: '',
			pendingSearchTerm: null,
			surface: 'featured',
			isResultsLoading: false,
			loadingSkeletonVisible: false,
			activeResultRequest: null,
			activeSuggestRequest: null,
			resultStatus: 'idle',
			errorKind: null,
		}),
		requestSearch: assign(({event}) => {
			if (event.type !== 'gifPicker.searchRequested') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term) {
				return {};
			}
			return {
				pendingSearchTerm: term,
				isResultsLoading: true,
				loadingSkeletonVisible: event.showLoadingSkeleton,
				activeResultRequest: {
					id: event.requestId,
					kind: 'search',
					term,
				},
				errorKind: null,
			};
		}),
		openCategoryCache: assign(({event}) => {
			if (event.type !== 'gifPicker.categoryCacheOpened') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term) {
				return {};
			}
			return {
				searchTerm: term,
				committedSearchTerm: term,
				pendingSearchTerm: null,
				surface: 'results',
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				activeSuggestRequest: null,
				resultKind: 'search',
				resultCount: event.resultCount,
				resultStatus: event.resultCount > 0 ? 'success' : 'empty',
				errorKind: null,
			};
		}),
		applySearchSuccess: assign(({context, event}) => {
			if (event.type !== 'gifPicker.searchSucceeded') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term || trimSearchTerm(context.searchTerm) !== term) {
				return {};
			}
			if (!isActiveResultRequest(context, event.requestId, 'search', term)) {
				return {};
			}
			return {
				surface: 'results',
				committedSearchTerm: term,
				pendingSearchTerm: null,
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				resultKind: 'search',
				resultCount: event.resultCount,
				resultStatus: event.resultCount > 0 ? 'success' : 'empty',
				errorKind: null,
			};
		}),
		applySearchFailure: assign(({context, event}) => {
			if (event.type !== 'gifPicker.searchFailed') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term || trimSearchTerm(context.searchTerm) !== term) {
				return {};
			}
			if (!isActiveResultRequest(context, event.requestId, 'search', term)) {
				return {};
			}
			return {
				pendingSearchTerm: null,
				isResultsLoading: false,
				loadingSkeletonVisible: false,
				activeResultRequest: null,
				resultStatus: context.resultCount > 0 ? context.resultStatus : 'error',
				errorKind: 'search',
			};
		}),
		requestSuggest: assign(({event}) => {
			if (event.type !== 'gifPicker.suggestRequested') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term) {
				return {};
			}
			return {
				activeSuggestRequest: {
					id: event.requestId,
					term,
				},
			};
		}),
		clearSuggest: assign(({context, event}) => {
			if (event.type !== 'gifPicker.suggestSucceeded' && event.type !== 'gifPicker.suggestFailed') {
				return {};
			}
			const term = trimSearchTerm(event.term);
			if (!term || !isActiveSuggestRequest(context, event.requestId, term)) {
				return {};
			}
			return {
				activeSuggestRequest: null,
			};
		}),
		showLoadingSkeleton: assign(({context, event}) => {
			if (event.type !== 'gifPicker.loadingSkeletonElapsed') {
				return {};
			}
			if (!isActiveResultRequest(context, event.requestId, event.kind, event.term)) {
				return {};
			}
			return {
				loadingSkeletonVisible: true,
			};
		}),
	},
}).createMachine({
	id: 'gifPicker',
	context: {
		searchTerm: '',
		committedSearchTerm: '',
		pendingSearchTerm: null,
		surface: 'featured',
		hasFeaturedContent: false,
		isFeaturedLoading: false,
		isResultsLoading: false,
		loadingSkeletonVisible: false,
		activeFeaturedRequestId: null,
		activeResultRequest: null,
		activeSuggestRequest: null,
		resultKind: null,
		resultCount: 0,
		resultStatus: 'idle',
		errorKind: null,
	},
	initial: 'default',
	states: {
		default: {
			on: {
				'gifPicker.defaultOpened': {actions: 'openDefault'},
				'gifPicker.favoritesOpened': {target: 'favorites', actions: 'openFavorites'},
				'gifPicker.featuredRequested': {actions: 'requestFeatured'},
				'gifPicker.featuredSucceeded': {actions: 'applyFeaturedSuccess'},
				'gifPicker.featuredFailed': {actions: 'applyFeaturedFailure'},
				'gifPicker.trendingRequested': {actions: 'requestTrending'},
				'gifPicker.trendingCacheOpened': {target: 'trending', actions: 'openTrendingCache'},
				'gifPicker.trendingSucceeded': {target: 'trending', actions: 'applyTrendingSuccess'},
				'gifPicker.trendingFailed': {actions: 'applyTrendingFailure'},
				'gifPicker.searchTermChanged': {actions: 'applySearchTerm'},
				'gifPicker.searchCleared': {actions: 'clearSearch'},
				'gifPicker.searchRequested': {actions: 'requestSearch'},
				'gifPicker.categoryCacheOpened': {actions: 'openCategoryCache'},
				'gifPicker.searchSucceeded': {actions: 'applySearchSuccess'},
				'gifPicker.searchFailed': {actions: 'applySearchFailure'},
				'gifPicker.suggestRequested': {actions: 'requestSuggest'},
				'gifPicker.suggestSucceeded': {actions: 'clearSuggest'},
				'gifPicker.suggestFailed': {actions: 'clearSuggest'},
				'gifPicker.loadingSkeletonElapsed': {actions: 'showLoadingSkeleton'},
			},
		},
		trending: {
			on: {
				'gifPicker.defaultOpened': {target: 'default', actions: 'openDefault'},
				'gifPicker.favoritesOpened': {target: 'favorites', actions: 'openFavorites'},
				'gifPicker.trendingRequested': {actions: 'requestTrending'},
				'gifPicker.trendingCacheOpened': {actions: 'openTrendingCache'},
				'gifPicker.trendingSucceeded': {actions: 'applyTrendingSuccess'},
				'gifPicker.trendingFailed': {actions: 'applyTrendingFailure'},
				'gifPicker.searchTermChanged': {target: 'default', actions: 'applySearchTerm'},
				'gifPicker.searchCleared': {target: 'default', actions: 'clearSearch'},
				'gifPicker.searchRequested': {target: 'default', actions: 'requestSearch'},
				'gifPicker.categoryCacheOpened': {target: 'default', actions: 'openCategoryCache'},
				'gifPicker.searchSucceeded': {target: 'default', actions: 'applySearchSuccess'},
				'gifPicker.searchFailed': {target: 'default', actions: 'applySearchFailure'},
				'gifPicker.loadingSkeletonElapsed': {actions: 'showLoadingSkeleton'},
			},
		},
		favorites: {
			on: {
				'gifPicker.defaultOpened': {target: 'default', actions: 'openDefault'},
				'gifPicker.searchTermChanged': {target: 'default', actions: 'applySearchTerm'},
				'gifPicker.searchCleared': {target: 'default', actions: 'clearSearch'},
				'gifPicker.searchRequested': {target: 'default', actions: 'requestSearch'},
				'gifPicker.categoryCacheOpened': {target: 'default', actions: 'openCategoryCache'},
				'gifPicker.searchSucceeded': {target: 'default', actions: 'applySearchSuccess'},
				'gifPicker.searchFailed': {target: 'default', actions: 'applySearchFailure'},
				'gifPicker.loadingSkeletonElapsed': {actions: 'showLoadingSkeleton'},
			},
		},
	},
});

export function createGifPickerSnapshot(): GifPickerMachineSnapshot {
	return initialTransition(gifPickerStateMachine)[0];
}

export function transitionGifPickerSnapshot(
	snapshot: GifPickerMachineSnapshot,
	event: GifPickerMachineEvent,
): GifPickerMachineSnapshot {
	return transition(gifPickerStateMachine, snapshot, event)[0] as GifPickerMachineSnapshot;
}

export function selectGifPickerModel(snapshot: GifPickerMachineSnapshot): GifPickerModel {
	const view = snapshot.value as View;
	const {context} = snapshot;
	const hasSearchTerm = trimSearchTerm(context.searchTerm).length > 0;
	const isShowingFeatured = view === 'default' && context.surface === 'featured';
	const isShowingFavorites = view === 'favorites' && context.surface === 'favorites';
	const isShowingResults = context.surface === 'results' && (view === 'default' || view === 'trending');
	const isLoading = context.isFeaturedLoading || context.isResultsLoading;
	const shouldShowLoadingSkeleton = context.isResultsLoading && context.loadingSkeletonVisible;
	const shouldShowNoResults =
		!context.isResultsLoading &&
		isShowingResults &&
		context.resultStatus === 'empty' &&
		(view === 'trending' || hasSearchTerm);
	const shouldShowError =
		!isLoading &&
		((context.errorKind === 'featured' && isShowingFeatured && !context.hasFeaturedContent) ||
			(context.errorKind !== null &&
				context.errorKind !== 'featured' &&
				(!isShowingFeatured || !context.hasFeaturedContent) &&
				context.resultStatus === 'error' &&
				context.resultCount === 0));
	return {
		view,
		searchTerm: context.searchTerm,
		committedSearchTerm: context.committedSearchTerm,
		surface: context.surface,
		isLoading,
		isFeaturedLoading: context.isFeaturedLoading,
		isResultsLoading: context.isResultsLoading,
		shouldShowLoadingSkeleton,
		initialFeaturedLoading: context.isFeaturedLoading && isShowingFeatured && !context.hasFeaturedContent,
		isLandingPage: view === 'default' && !hasSearchTerm,
		isShowingFeatured,
		isShowingFavorites,
		isShowingResults,
		shouldShowNoResults,
		shouldShowError,
		errorKind: context.errorKind,
		resultKind: context.resultKind,
		pendingSearchTerm: context.pendingSearchTerm,
	};
}
