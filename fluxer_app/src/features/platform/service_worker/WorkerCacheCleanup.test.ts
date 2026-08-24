// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldDeleteWorkerCache} from '@app/features/platform/service_worker/WorkerCacheCleanup';
import {describe, expect, it} from 'vitest';

describe('WorkerCacheCleanup', () => {
	const expectedCaches = new Set(['fluxer-precache-current', 'fluxer-navigation-current']);

	it('reclaims every cache the current worker no longer writes to', () => {
		expect(shouldDeleteWorkerCache('fluxer-assets-current', expectedCaches)).toBe(true);
		expect(shouldDeleteWorkerCache('fluxer-assets-previous', expectedCaches)).toBe(true);
		expect(shouldDeleteWorkerCache('fluxer-expression-assets', expectedCaches)).toBe(true);
		expect(shouldDeleteWorkerCache('fluxer-expression-assets-2026.604', expectedCaches)).toBe(true);
		expect(shouldDeleteWorkerCache('fluxer-precache-previous', expectedCaches)).toBe(true);
		expect(shouldDeleteWorkerCache('fluxer-navigation-previous', expectedCaches)).toBe(true);
	});

	it('keeps the current caches and anything the worker does not own', () => {
		expect(shouldDeleteWorkerCache('fluxer-precache-current', expectedCaches)).toBe(false);
		expect(shouldDeleteWorkerCache('fluxer-navigation-current', expectedCaches)).toBe(false);
		expect(shouldDeleteWorkerCache('third-party-cache', expectedCaches)).toBe(false);
		expect(shouldDeleteWorkerCache('fluxer', expectedCaches)).toBe(false);
	});
});
