// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGifPickerRequestId,
	createGifPickerSnapshot,
	selectGifPickerModel,
	transitionGifPickerSnapshot,
} from '@app/features/channel/components/pickers/gif/GifPickerStateMachine';
import {bench, describe} from 'vitest';

const SEARCH_TERMS = Array.from({length: 1_000}, (_, index) => `query-${index}`);

describe('GifPickerStateMachine benchmarks', () => {
	bench('coalesces 1k search term transitions while preserving the visible surface', () => {
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.featuredSucceeded',
			requestId: createGifPickerRequestId(1),
			hasContent: true,
		});
		for (const term of SEARCH_TERMS) {
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchTermChanged',
				term,
			});
		}
		selectGifPickerModel(snapshot);
	});

	bench('rejects 1k stale search completions', () => {
		let snapshot = createGifPickerSnapshot();
		for (let index = 0; index < SEARCH_TERMS.length; index += 1) {
			const term = SEARCH_TERMS[index];
			const requestId = createGifPickerRequestId(index + 1);
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchTermChanged',
				term,
			});
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchRequested',
				requestId,
				term,
				showLoadingSkeleton: false,
			});
		}
		for (let index = 0; index < SEARCH_TERMS.length - 1; index += 1) {
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchSucceeded',
				requestId: createGifPickerRequestId(index + 1),
				term: SEARCH_TERMS[index],
				resultCount: 24,
			});
		}
		selectGifPickerModel(snapshot);
	});

	bench('commits latest search result after a fast query burst', () => {
		let snapshot = createGifPickerSnapshot();
		for (let index = 0; index < SEARCH_TERMS.length; index += 1) {
			const term = SEARCH_TERMS[index];
			const requestId = createGifPickerRequestId(index + 1);
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchTermChanged',
				term,
			});
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.searchRequested',
				requestId,
				term,
				showLoadingSkeleton: false,
			});
		}
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchSucceeded',
			requestId: createGifPickerRequestId(SEARCH_TERMS.length),
			term: SEARCH_TERMS[SEARCH_TERMS.length - 1],
			resultCount: 24,
		});
		selectGifPickerModel(snapshot);
	});

	bench('rejects 1k stale skeleton timer events before showing the active one', () => {
		const activeRequestId = createGifPickerRequestId(SEARCH_TERMS.length + 1);
		let snapshot = createGifPickerSnapshot();
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchTermChanged',
			term: 'active',
		});
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.searchRequested',
			requestId: activeRequestId,
			term: 'active',
			showLoadingSkeleton: false,
		});
		for (let index = 0; index < SEARCH_TERMS.length; index += 1) {
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.loadingSkeletonElapsed',
				requestId: createGifPickerRequestId(index + 1),
				kind: 'search',
				term: SEARCH_TERMS[index],
			});
		}
		snapshot = transitionGifPickerSnapshot(snapshot, {
			type: 'gifPicker.loadingSkeletonElapsed',
			requestId: activeRequestId,
			kind: 'search',
			term: 'active',
		});
		selectGifPickerModel(snapshot);
	});

	bench('opens 1k cached category snapshots without entering loading', () => {
		let snapshot = createGifPickerSnapshot();
		for (const term of SEARCH_TERMS) {
			snapshot = transitionGifPickerSnapshot(snapshot, {
				type: 'gifPicker.categoryCacheOpened',
				term,
				resultCount: 24,
			});
		}
		selectGifPickerModel(snapshot);
	});
});
