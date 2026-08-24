// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	STREAM_PREVIEW_CONTENT_TYPE_JPEG,
	STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS,
	STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS,
	STREAM_PREVIEW_REFRESH_INTERVAL_MS,
} from '@fluxer/constants/src/StreamConstants';
import {useEffect, useState} from 'react';

const logger = new Logger('useStreamPreview');
const PREVIEW_CACHE_MAX = 24;

type StreamPreviewTimerHandle = number | NodeJS.Timeout;

export type StreamPreviewStatus = 'inactive' | 'idle' | 'loading' | 'ready' | 'missing' | 'error';

export interface StreamPreviewState {
	previewUrl: string | null;
	isPreviewLoading: boolean;
	previewStatus: StreamPreviewStatus;
	isPreviewUnavailable: boolean;
}

interface PreviewRecord {
	streamKey: string;
	status: Exclude<StreamPreviewStatus, 'inactive'>;
	url: string | null;
	fetchedAt: number;
	missingAttemptCount: number;
	nextFetchAt: number;
	lastUsedAt: number;
	promise: Promise<void> | null;
	timerId: StreamPreviewTimerHandle | null;
	timerTargetAt: number;
	subscribers: Set<() => void>;
}

const isCacheFresh = (fetchedAt: number) => Date.now() - fetchedAt < STREAM_PREVIEW_REFRESH_INTERVAL_MS;

const INACTIVE_PREVIEW_STATE: StreamPreviewState = {
	previewUrl: null,
	isPreviewLoading: false,
	previewStatus: 'inactive',
	isPreviewUnavailable: true,
};

const previewRecords = new Map<string, PreviewRecord>();

function createPreviewRecord(streamKey: string): PreviewRecord {
	return {
		streamKey,
		status: 'idle',
		url: null,
		fetchedAt: 0,
		missingAttemptCount: 0,
		nextFetchAt: 0,
		lastUsedAt: Date.now(),
		promise: null,
		timerId: null,
		timerTargetAt: 0,
		subscribers: new Set(),
	};
}

function getPreviewRecord(streamKey: string): PreviewRecord {
	let record = previewRecords.get(streamKey);
	if (!record) {
		record = createPreviewRecord(streamKey);
		previewRecords.set(streamKey, record);
	}
	record.lastUsedAt = Date.now();
	return record;
}

function clearPreviewTimer(record: PreviewRecord): void {
	if (record.timerId !== null) {
		clearTimeout(record.timerId);
		record.timerId = null;
		record.timerTargetAt = 0;
	}
}

function clearPreviewUrl(record: PreviewRecord): void {
	if (!record.url) return;
	URL.revokeObjectURL(record.url);
	record.url = null;
}

function prunePreviewRecords(): void {
	for (const [streamKey, record] of previewRecords) {
		if (record.subscribers.size === 0 && record.promise === null && record.url === null) {
			clearPreviewTimer(record);
			previewRecords.delete(streamKey);
		}
	}
	const recordsWithUrls = Array.from(previewRecords.values()).filter((record) => record.url !== null);
	if (recordsWithUrls.length <= PREVIEW_CACHE_MAX) return;
	recordsWithUrls.sort((a, b) => a.lastUsedAt - b.lastUsedAt);
	let urlRecordCount = recordsWithUrls.length;
	for (const record of recordsWithUrls) {
		if (urlRecordCount <= PREVIEW_CACHE_MAX) break;
		if (record.subscribers.size > 0) continue;
		clearPreviewUrl(record);
		record.status = 'idle';
		record.fetchedAt = 0;
		urlRecordCount -= 1;
		if (record.promise === null) {
			previewRecords.delete(record.streamKey);
		}
	}
}

function notifyPreviewRecord(record: PreviewRecord): void {
	for (const subscriber of record.subscribers) {
		subscriber();
	}
}

function getPreviewRecordState(record: PreviewRecord): StreamPreviewState {
	const isMissingDuringInitialRetry =
		record.status === 'missing' &&
		record.url === null &&
		record.missingAttemptCount < STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS;
	return {
		previewUrl: record.url,
		isPreviewLoading: record.status === 'loading' || isMissingDuringInitialRetry,
		previewStatus: record.status,
		isPreviewUnavailable: record.url === null && (record.status === 'missing' || record.status === 'error'),
	};
}

function isSamePreviewState(a: StreamPreviewState, b: StreamPreviewState): boolean {
	return (
		a.previewUrl === b.previewUrl &&
		a.isPreviewLoading === b.isPreviewLoading &&
		a.previewStatus === b.previewStatus &&
		a.isPreviewUnavailable === b.isPreviewUnavailable
	);
}

function applyPreviewRecordState(record: PreviewRecord, setState: (state: StreamPreviewState) => void): void {
	const nextState = getPreviewRecordState(record);
	setState(nextState);
}

function schedulePreviewFetch(record: PreviewRecord, delayMs: number): void {
	if (record.subscribers.size === 0) return;
	const delay = Math.max(0, delayMs);
	const targetAt = Date.now() + delay;
	if (record.timerId !== null && record.timerTargetAt <= targetAt) return;
	clearPreviewTimer(record);
	record.timerTargetAt = targetAt;
	record.timerId = setTimeout(() => {
		record.timerId = null;
		record.timerTargetAt = 0;
		void fetchPreviewRecord(record);
	}, delay);
}

function scheduleNextPreviewFetch(record: PreviewRecord): void {
	if (record.subscribers.size === 0) return;
	const now = Date.now();
	if (record.status === 'ready' && record.fetchedAt > 0) {
		schedulePreviewFetch(record, Math.max(0, record.fetchedAt + STREAM_PREVIEW_REFRESH_INTERVAL_MS - now));
		return;
	}
	if (record.nextFetchAt > 0) {
		schedulePreviewFetch(record, Math.max(0, record.nextFetchAt - now));
		return;
	}
	schedulePreviewFetch(record, 0);
}

function markPreviewReady(record: PreviewRecord, url: string, contentType: string): void {
	clearPreviewUrl(record);
	record.url = url;
	record.status = 'ready';
	record.fetchedAt = Date.now();
	record.nextFetchAt = record.fetchedAt + STREAM_PREVIEW_REFRESH_INTERVAL_MS;
	record.missingAttemptCount = 0;
	record.lastUsedAt = record.fetchedAt;
	logger.debug('useStreamPreview: preview ready', {
		contentType,
		streamKey: record.streamKey,
	});
	notifyPreviewRecord(record);
	prunePreviewRecords();
}

function markPreviewMissing(record: PreviewRecord, status: number | null): void {
	clearPreviewUrl(record);
	const now = Date.now();
	record.status = 'missing';
	record.fetchedAt = now;
	record.lastUsedAt = now;
	record.missingAttemptCount += 1;
	const shouldFastRetry = record.missingAttemptCount < STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS;
	record.nextFetchAt =
		now + (shouldFastRetry ? STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS : STREAM_PREVIEW_REFRESH_INTERVAL_MS);
	logger.debug('useStreamPreview: preview missing', {
		attempt: record.missingAttemptCount,
		nextFetchInMs: record.nextFetchAt - now,
		status,
		streamKey: record.streamKey,
	});
	notifyPreviewRecord(record);
	prunePreviewRecords();
}

function markPreviewError(record: PreviewRecord, error: unknown): void {
	const now = Date.now();
	record.status = 'error';
	record.fetchedAt = now;
	record.lastUsedAt = now;
	record.nextFetchAt = now + STREAM_PREVIEW_REFRESH_INTERVAL_MS;
	logger.error('preview fetch failed', error);
	notifyPreviewRecord(record);
}

function isMissingPreviewError(error: unknown): error is HttpError {
	return error instanceof HttpError && error.status === 404;
}

async function fetchPreviewRecord(record: PreviewRecord): Promise<void> {
	if (record.promise) return record.promise;
	if (record.url && isCacheFresh(record.fetchedAt)) {
		record.status = 'ready';
		notifyPreviewRecord(record);
		scheduleNextPreviewFetch(record);
		return;
	}
	clearPreviewTimer(record);
	record.status = 'loading';
	notifyPreviewRecord(record);
	const promise = (async () => {
		try {
			const response = await http.get<ArrayBuffer>(Endpoints.STREAM_PREVIEW(record.streamKey), {
				parse: 'binary',
			});
			logger.debug('useStreamPreview: HTTP response', {
				ok: response.ok,
				status: response.status,
				hasBody: !!response.body,
				streamKey: record.streamKey,
			});
			if (!response.ok || !response.body) {
				markPreviewMissing(record, response.status);
				return;
			}
			const contentType = response.headers['content-type'] || STREAM_PREVIEW_CONTENT_TYPE_JPEG;
			const blob = new Blob([response.body], {type: contentType});
			const url = URL.createObjectURL(blob);
			markPreviewReady(record, url, contentType);
		} catch (err) {
			if (isMissingPreviewError(err)) {
				markPreviewMissing(record, err.status);
				return;
			}
			markPreviewError(record, err);
		}
	})();
	record.promise = promise;
	await promise.finally(() => {
		if (record.promise === promise) {
			record.promise = null;
		}
		scheduleNextPreviewFetch(record);
		prunePreviewRecords();
	});
}

function startPreviewRecord(record: PreviewRecord): void {
	record.lastUsedAt = Date.now();
	if (record.url && isCacheFresh(record.fetchedAt)) {
		record.status = 'ready';
		notifyPreviewRecord(record);
		scheduleNextPreviewFetch(record);
		return;
	}
	if (record.promise) {
		record.status = 'loading';
		notifyPreviewRecord(record);
		return;
	}
	scheduleNextPreviewFetch(record);
}

export function useStreamPreview(enabled: boolean, streamKey: string): StreamPreviewState {
	const [state, setState] = useState<StreamPreviewState>(() =>
		enabled && streamKey ? getPreviewRecordState(getPreviewRecord(streamKey)) : INACTIVE_PREVIEW_STATE,
	);
	useEffect(() => {
		if (!enabled || !streamKey) {
			setState(INACTIVE_PREVIEW_STATE);
			return;
		}
		const record = getPreviewRecord(streamKey);
		const applyState = () => {
			const nextState = getPreviewRecordState(record);
			setState((previousState) => (isSamePreviewState(previousState, nextState) ? previousState : nextState));
		};
		record.subscribers.add(applyState);
		applyPreviewRecordState(record, setState);
		startPreviewRecord(record);
		return () => {
			record.subscribers.delete(applyState);
			if (record.subscribers.size === 0) {
				clearPreviewTimer(record);
			}
			prunePreviewRecords();
		};
	}, [enabled, streamKey]);
	return state;
}
