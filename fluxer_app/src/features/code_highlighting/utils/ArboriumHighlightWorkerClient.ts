// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ArboriumHighlightWorkerRequest,
	ArboriumHighlightWorkerResponse,
} from '@app/features/code_highlighting/workers/ArboriumHighlightWorker';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MAX_CODE_HIGHLIGHT_OUTPUT_LENGTH, MAX_CODE_HIGHLIGHT_SOURCE_LENGTH} from '@fluxer/constants/src/LimitConstants';

export interface HighlightCodeInWorkerOptions {
	signal?: AbortSignal;
}

interface HighlightJob {
	id: number;
	language: string;
	source: string;
	enqueuedAt: number;
	resolve: (highlightedHtml: string | null) => void;
	signal?: AbortSignal;
	abortListener: (() => void) | null;
	timeout: NodeJS.Timeout | null;
	queueTimeout: NodeJS.Timeout | null;
	settled: boolean;
}

const logger = new Logger('ArboriumHighlightWorkerClient');
const MAX_RETAINED_JOBS = 8;
const MAX_RETAINED_SOURCE_LENGTH = 128 * 1024;
const MAX_LANGUAGE_LENGTH = 128;
const MAX_QUEUE_AGE_MS = 12_000;
const WORKER_INITIALIZATION_TIMEOUT_MS = 10_000;
const WORKER_HIGHLIGHT_TIMEOUT_MS = 2_000;
const WORKER_IDLE_TIMEOUT_MS = 30_000;

let worker: Worker | null = null;
let activeJob: HighlightJob | null = null;
let nextJobId = 1;
let retainedSourceLength = 0;
let idleTimeout: NodeJS.Timeout | null = null;
const pendingJobs: Array<HighlightJob> = [];

function clearIdleTimeout(): void {
	if (!idleTimeout) {
		return;
	}
	clearTimeout(idleTimeout);
	idleTimeout = null;
}

function isSignalAborted(signal: AbortSignal | undefined): boolean {
	if (signal === undefined) {
		return false;
	}
	return signal.aborted;
}

function terminateWorker(): void {
	clearIdleTimeout();
	if (worker !== null) {
		worker.terminate();
	}
	worker = null;
}

function scheduleWorkerTermination(): void {
	clearIdleTimeout();
	if (!worker || activeJob || pendingJobs.length > 0) {
		return;
	}
	idleTimeout = setTimeout(() => {
		if (!activeJob && pendingJobs.length === 0) {
			terminateWorker();
		}
	}, WORKER_IDLE_TIMEOUT_MS);
}

function isWorkerResponse(value: unknown): value is ArboriumHighlightWorkerResponse {
	if (typeof value !== 'object' || value === null) {
		return false;
	}
	const response = value as Record<string, unknown>;
	if (typeof response.id !== 'number' || !Number.isSafeInteger(response.id) || response.id <= 0) {
		return false;
	}
	if (response.status === 'progress') {
		return response.phase === 'initializing' || response.phase === 'loading' || response.phase === 'highlighting';
	}
	if (response.status === 'success') {
		return typeof response.highlightedHtml === 'string';
	}
	if (response.status === 'skipped') {
		return response.reason === 'language' || response.reason === 'output_limit' || response.reason === 'source_limit';
	}
	return response.status === 'error' && typeof response.message === 'string' && response.message.length <= 4_096;
}

function clearJobTimers(job: HighlightJob): void {
	if (job.timeout) {
		clearTimeout(job.timeout);
		job.timeout = null;
	}
	if (job.queueTimeout) {
		clearTimeout(job.queueTimeout);
		job.queueTimeout = null;
	}
	if (job.abortListener && job.signal) {
		job.signal.removeEventListener('abort', job.abortListener);
		job.abortListener = null;
	}
}

function settleJob(job: HighlightJob, highlightedHtml: string | null, startNext: boolean): void {
	if (job.settled) {
		return;
	}
	job.settled = true;
	clearJobTimers(job);
	retainedSourceLength -= job.source.length;
	if (retainedSourceLength < 0) {
		throw new Error('Arborium highlight worker source accounting underflow');
	}
	job.resolve(highlightedHtml);
	if (startNext) {
		startNextJob();
	}
}

function removePendingJob(job: HighlightJob): void {
	const index = pendingJobs.indexOf(job);
	if (index >= 0) {
		pendingJobs.splice(index, 1);
	}
}

function failPendingJobs(): void {
	while (pendingJobs.length > 0) {
		const job = pendingJobs.shift();
		if (job) {
			settleJob(job, null, false);
		}
	}
}

function failWorkerInstance(instance: Worker, message: string, error?: unknown): void {
	if (instance !== worker) {
		return;
	}
	if (error === undefined) {
		logger.warn(message);
	} else {
		logger.error(message, error);
	}
	terminateWorker();
	const job = activeJob;
	activeJob = null;
	if (job) {
		settleJob(job, null, false);
	}
	failPendingJobs();
}

function armJobTimeout(instance: Worker, job: HighlightJob, timeoutMs: number): void {
	if (job.timeout) {
		clearTimeout(job.timeout);
	}
	job.timeout = setTimeout(() => {
		if (activeJob === null || activeJob.id !== job.id || worker !== instance) {
			return;
		}
		failWorkerInstance(instance, `Arborium highlight worker timed out after ${timeoutMs}ms`);
	}, timeoutMs);
}

function finishActiveJob(highlightedHtml: string | null): void {
	const job = activeJob;
	if (!job) {
		return;
	}
	activeJob = null;
	settleJob(job, highlightedHtml, true);
}

function handleWorkerMessage(instance: Worker, event: MessageEvent<unknown>): void {
	if (instance !== worker) {
		return;
	}
	if (!isWorkerResponse(event.data)) {
		failWorkerInstance(instance, 'Arborium highlight worker returned an invalid response');
		return;
	}
	const response = event.data;
	if (!activeJob || response.id !== activeJob.id) {
		failWorkerInstance(instance, 'Arborium highlight worker returned a response for the wrong job');
		return;
	}
	if (response.status === 'progress') {
		armJobTimeout(
			instance,
			activeJob,
			response.phase === 'highlighting' ? WORKER_HIGHLIGHT_TIMEOUT_MS : WORKER_INITIALIZATION_TIMEOUT_MS,
		);
		return;
	}
	if (response.status === 'error') {
		failWorkerInstance(instance, `Arborium highlight worker failed: ${response.message}`);
		return;
	}
	if (response.status === 'skipped') {
		if (response.reason !== 'language') {
			logger.warn(`Arborium highlight worker skipped a job because of its ${response.reason}`);
		}
		finishActiveJob(null);
		return;
	}
	if (response.highlightedHtml.length > MAX_CODE_HIGHLIGHT_OUTPUT_LENGTH) {
		failWorkerInstance(instance, 'Arborium highlight worker exceeded its output limit');
		return;
	}
	finishActiveJob(response.highlightedHtml);
}

function createWorker(): Worker {
	const instance = new Worker(
		new URL(
			/* webpackChunkName: "arborium-highlight.worker" */ '../workers/ArboriumHighlightWorker.ts',
			import.meta.url,
		),
		{name: 'arborium-highlight-worker'},
	);
	instance.addEventListener('message', (event) => handleWorkerMessage(instance, event));
	instance.addEventListener('error', (event) => {
		event.preventDefault();
		failWorkerInstance(
			instance,
			'Arborium highlight worker crashed',
			event.error === undefined || event.error === null ? event.message : event.error,
		);
	});
	instance.addEventListener('messageerror', () => {
		failWorkerInstance(instance, 'Arborium highlight worker message could not be decoded');
	});
	return instance;
}

function getWorker(): Worker {
	if (!worker) {
		worker = createWorker();
	}
	return worker;
}

function expireQueuedJob(job: HighlightJob): void {
	if (job.settled || activeJob === job) {
		return;
	}
	removePendingJob(job);
	settleJob(job, null, false);
}

function startNextJob(): void {
	if (activeJob) {
		return;
	}
	clearIdleTimeout();
	let job = pendingJobs.shift();
	while (job && (job.settled || Date.now() - job.enqueuedAt > MAX_QUEUE_AGE_MS)) {
		if (job && !job.settled) {
			settleJob(job, null, false);
		}
		job = pendingJobs.shift();
	}
	if (!job) {
		scheduleWorkerTermination();
		return;
	}
	activeJob = job;
	if (job.queueTimeout) {
		clearTimeout(job.queueTimeout);
		job.queueTimeout = null;
	}
	try {
		const instance = getWorker();
		armJobTimeout(instance, job, WORKER_INITIALIZATION_TIMEOUT_MS);
		instance.postMessage({
			id: job.id,
			language: job.language,
			source: job.source,
		} satisfies ArboriumHighlightWorkerRequest);
	} catch (error) {
		logger.error('Failed to start Arborium highlight worker', error);
		terminateWorker();
		activeJob = null;
		settleJob(job, null, true);
	}
}

function cancelJob(job: HighlightJob): void {
	if (job.settled) {
		return;
	}
	if (activeJob === job) {
		terminateWorker();
		activeJob = null;
		settleJob(job, null, false);
		startNextJob();
		return;
	}
	removePendingJob(job);
	settleJob(job, null, false);
}

export function highlightCodeInWorker(
	language: string,
	source: string,
	options: HighlightCodeInWorkerOptions = {},
): Promise<string | null> {
	if (
		source.length >= MAX_CODE_HIGHLIGHT_SOURCE_LENGTH ||
		language.length > MAX_LANGUAGE_LENGTH ||
		isSignalAborted(options.signal)
	) {
		return Promise.resolve(null);
	}
	const retainedJobCount = pendingJobs.length + (activeJob ? 1 : 0);
	if (retainedJobCount >= MAX_RETAINED_JOBS || retainedSourceLength + source.length > MAX_RETAINED_SOURCE_LENGTH) {
		logger.warn('Skipped Arborium highlighting because the worker queue is full');
		return Promise.resolve(null);
	}
	clearIdleTimeout();
	return new Promise((resolve) => {
		const job: HighlightJob = {
			id: nextJobId++,
			language,
			source,
			enqueuedAt: Date.now(),
			resolve,
			signal: options.signal,
			abortListener: null,
			timeout: null,
			queueTimeout: null,
			settled: false,
		};
		job.queueTimeout = setTimeout(() => expireQueuedJob(job), MAX_QUEUE_AGE_MS);
		if (job.signal) {
			job.abortListener = () => cancelJob(job);
			job.signal.addEventListener('abort', job.abortListener, {once: true});
		}
		retainedSourceLength += source.length;
		pendingJobs.push(job);
		startNextJob();
	});
}
