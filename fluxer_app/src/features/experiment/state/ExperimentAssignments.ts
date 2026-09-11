// SPDX-License-Identifier: AGPL-3.0-or-later

import {http} from '@app/features/platform/transport/RestTransport';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import {
	EXPERIMENT_MAX_POLL_INTERVAL_SECONDS,
	EXPERIMENT_MAX_POLL_JITTER_PERCENT,
	EXPERIMENT_MIN_POLL_INTERVAL_SECONDS,
	ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {makeAutoObservable, observable, runInAction} from 'mobx';

const logger = new Logger('ExperimentAssignments');

export const EXPERIMENT_ASSIGNMENTS_PATH = '/experiments';

export const EXPERIMENT_ASSIGNMENTS_VISIBILITY_REFRESH_MIN_INTERVAL_MS = 30 * MS_PER_SECOND;
export const EXPERIMENT_ASSIGNMENTS_HIDDEN_POLL_INTERVAL_MULTIPLIER = 4;
export const EXPERIMENT_ASSIGNMENTS_MAX_NOT_FOUND_STREAK = 3;

const MAX_BACKOFF_DOUBLINGS = 2;
const EXPERIMENT_ASSIGNMENTS_MIN_POLL_DELAY_MS = (EXPERIMENT_MIN_POLL_INTERVAL_SECONDS * MS_PER_SECOND) / 2;

type UnobservedField =
	| 'etag'
	| 'failureStreak'
	| 'generation'
	| 'inFlight'
	| 'lastFetchStartedAt'
	| 'listeners'
	| 'listening'
	| 'notFoundStreak'
	| 'pollTimerId'
	| 'pollingDisabled'
	| 'started';

function clampPollIntervalSeconds(seconds: number): number {
	if (!Number.isFinite(seconds)) {
		return EXPERIMENT_MIN_POLL_INTERVAL_SECONDS;
	}
	return Math.min(
		Math.max(Math.round(seconds), EXPERIMENT_MIN_POLL_INTERVAL_SECONDS),
		EXPERIMENT_MAX_POLL_INTERVAL_SECONDS,
	);
}

function clampPollJitterRatio(percent: number): number {
	if (!Number.isFinite(percent)) {
		return 0;
	}
	return Math.min(Math.max(Math.round(percent), 0), EXPERIMENT_MAX_POLL_JITTER_PERCENT) / 100;
}

function isDocumentHidden(): boolean {
	try {
		return typeof document !== 'undefined' && document.visibilityState === 'hidden';
	} catch {
		return false;
	}
}

class ExperimentAssignmentsStore {
	response: ExperimentAssignmentsResponse = INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE;
	private started = false;
	private etag: string | null = null;
	private listening = false;
	private pollTimerId: NodeJS.Timeout | null = null;
	private inFlight: Promise<void> | null = null;
	private failureStreak = 0;
	private notFoundStreak = 0;
	private pollingDisabled = false;
	private lastFetchStartedAt = 0;
	private generation = 0;
	private listeners = new Set<() => void>();

	constructor() {
		makeAutoObservable<this, UnobservedField>(
			this,
			{
				response: observable.ref,
				etag: false,
				failureStreak: false,
				generation: false,
				inFlight: false,
				lastFetchStartedAt: false,
				listeners: false,
				listening: false,
				notFoundStreak: false,
				pollTimerId: false,
				pollingDisabled: false,
				started: false,
			},
			{autoBind: true},
		);
	}

	start(): void {
		if (this.started || this.pollingDisabled) {
			return;
		}
		this.started = true;
		try {
			this.addVisibilityListener();
			if (isDocumentHidden()) {
				this.schedulePoll();
				return;
			}
			void this.refreshAndReschedule();
		} catch (err) {
			logger.warn('Failed to start experiment assignment polling:', err);
		}
	}

	stop(): void {
		this.started = false;
		this.generation += 1;
		try {
			this.removeVisibilityListener();
		} catch (err) {
			logger.warn('Failed to detach experiment assignment listeners:', err);
		}
		this.clearPollTimer();
	}

	reset(): void {
		this.stop();
		this.etag = null;
		this.inFlight = null;
		this.failureStreak = 0;
		this.notFoundStreak = 0;
		this.pollingDisabled = false;
		this.lastFetchStartedAt = 0;
		this.setResponse(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private setResponse(next: ExperimentAssignmentsResponse): void {
		if (this.response === next) {
			return;
		}
		runInAction(() => {
			this.response = next;
		});
		this.notifyListeners();
	}

	private notifyListeners(): void {
		for (const listener of [...this.listeners]) {
			try {
				listener();
			} catch (err) {
				logger.warn('Experiment assignment listener failed:', err);
			}
		}
	}

	private schedulePoll(): void {
		if (!this.started) {
			return;
		}
		this.clearPollTimer();
		this.pollTimerId = setTimeout(() => {
			this.pollTimerId = null;
			void this.refreshAndReschedule();
		}, this.nextDelayMs());
	}

	private nextDelayMs(): number {
		const seconds = clampPollIntervalSeconds(this.response.poll_interval_seconds);
		const multiplier = isDocumentHidden() ? EXPERIMENT_ASSIGNMENTS_HIDDEN_POLL_INTERVAL_MULTIPLIER : 1;
		const target = seconds * MS_PER_SECOND * multiplier * 2 ** this.failureStreak;
		const jitter = target * clampPollJitterRatio(this.response.poll_jitter_percent) * (Math.random() * 2 - 1);
		return Math.max(EXPERIMENT_ASSIGNMENTS_MIN_POLL_DELAY_MS, Math.round(target + jitter));
	}

	private async refreshAndReschedule(): Promise<void> {
		const generation = this.generation;
		try {
			await this.refresh();
		} finally {
			if (generation === this.generation) {
				this.schedulePoll();
			}
		}
	}

	private refresh(): Promise<void> {
		const existing = this.inFlight;
		if (existing !== null) {
			return existing;
		}
		const pending = this.fetchAssignments(this.generation).finally(() => {
			if (this.inFlight === pending) {
				this.inFlight = null;
			}
		});
		this.inFlight = pending;
		return pending;
	}

	private async fetchAssignments(generation: number): Promise<void> {
		this.lastFetchStartedAt = Date.now();
		const headers: Record<string, string> = {};
		if (this.etag !== null) {
			headers['If-None-Match'] = this.etag;
		}
		try {
			const response = await http.get<unknown>(EXPERIMENT_ASSIGNMENTS_PATH, {
				headers,
				mode: 'silent',
				parse: 'json',
			});
			if (generation !== this.generation) {
				return;
			}
			this.applyResponse(response);
		} catch (err) {
			if (generation !== this.generation) {
				return;
			}
			logger.warn('Experiment assignment request failed:', err);
			this.noteFailure();
		}
	}

	private applyResponse(response: RestResponse<unknown>): void {
		if (response.status === 404) {
			this.noteNotFound();
			return;
		}
		this.notFoundStreak = 0;
		if (response.status === 304) {
			this.failureStreak = 0;
			return;
		}
		if (response.status === 401 || response.status === 403) {
			logger.warn('Experiment assignment request was rejected with status:', response.status);
			this.noteFailure();
			return;
		}
		if (!response.ok) {
			logger.warn('Experiment assignment request returned status:', response.status);
			this.noteFailure();
			return;
		}
		const parsed = ExperimentAssignmentsResponse.safeParse(response.body);
		if (!parsed.success) {
			logger.warn('Discarding malformed experiment assignments:', parsed.error.message);
			this.failureStreak = 0;
			return;
		}
		const etag = response.headers.etag ?? null;
		this.etag = etag;
		this.failureStreak = 0;
		this.setResponse(parsed.data);
	}

	private noteFailure(): void {
		this.failureStreak = Math.min(this.failureStreak + 1, MAX_BACKOFF_DOUBLINGS);
	}

	private noteNotFound(): void {
		this.notFoundStreak += 1;
		this.noteFailure();
		if (this.notFoundStreak < EXPERIMENT_ASSIGNMENTS_MAX_NOT_FOUND_STREAK) {
			return;
		}
		logger.warn('Experiment assignment endpoint is unavailable; stopping polling for this session.');
		this.pollingDisabled = true;
		this.stop();
	}

	private handleVisibilityChange(): void {
		if (!this.started || isDocumentHidden()) {
			return;
		}
		if (Date.now() - this.lastFetchStartedAt < EXPERIMENT_ASSIGNMENTS_VISIBILITY_REFRESH_MIN_INTERVAL_MS) {
			return;
		}
		this.clearPollTimer();
		void this.refreshAndReschedule();
	}

	private addVisibilityListener(): void {
		if (this.listening) {
			return;
		}
		document.addEventListener('visibilitychange', this.handleVisibilityChange);
		this.listening = true;
	}

	private removeVisibilityListener(): void {
		if (!this.listening) {
			return;
		}
		document.removeEventListener('visibilitychange', this.handleVisibilityChange);
		this.listening = false;
	}

	private clearPollTimer(): void {
		if (this.pollTimerId === null) {
			return;
		}
		clearTimeout(this.pollTimerId);
		this.pollTimerId = null;
	}
}

export const ExperimentAssignments = new ExperimentAssignmentsStore();

export default ExperimentAssignments;
