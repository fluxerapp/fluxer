// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';

export interface QueueEntry<TMessage, TResult> {
	message: TMessage;
	success: (result?: TResult, error?: unknown) => void;
}

interface RetryInfo {
	retryAfter?: number;
}

export interface QueueConfig {
	logger?: Logger;
	defaultRetryAfter?: number;
}

export abstract class Queue<TMessage, TResult = void> {
	protected readonly logger: Logger;
	protected readonly defaultRetryAfter: number;
	protected readonly queue: Array<QueueEntry<TMessage, TResult>>;
	private retryTimerId: number | null;
	private isDraining: boolean;

	constructor(config: QueueConfig = {}) {
		this.logger = config.logger ?? new Logger('Queue');
		this.defaultRetryAfter = config.defaultRetryAfter ?? 100;
		this.queue = [];
		this.retryTimerId = null;
		this.isDraining = false;
	}

	protected abstract drain(
		message: TMessage,
		complete: (retry: RetryInfo | null, result?: TResult, error?: unknown) => void,
	): Promise<unknown> | undefined;

	enqueue(message: TMessage, success: (result?: TResult, error?: unknown) => void): void {
		this.queue.push({message, success});
		this.maybeProcessNext();
	}

	get queueLength(): number {
		return this.queue.length;
	}

	clear(): void {
		if (this.retryTimerId !== null) {
			clearTimeout(this.retryTimerId);
			this.retryTimerId = null;
		}
		this.queue.length = 0;
		this.isDraining = false;
	}

	peek(): TMessage | undefined {
		return this.queue[0]?.message;
	}

	private maybeProcessNext(): void {
		if (this.retryTimerId !== null || this.queue.length === 0 || this.isDraining) {
			return;
		}
		const entry = this.queue.shift();
		if (!entry) {
			this.isDraining = false;
			return;
		}
		this.isDraining = true;
		const {message, success} = entry;
		let hasCompleted = false;
		let drainResult: Promise<unknown> | undefined;
		const complete = (retry: RetryInfo | null, result?: TResult, error?: unknown): void => {
			if (hasCompleted) {
				this.logger.warn('Queue completion callback invoked more than once; ignoring extra call');
				return;
			}
			hasCompleted = true;
			this.isDraining = false;
			this.logger.debug(
				`Finished processing queued item; ${this.queue.length} ${this.queue.length === 1 ? 'item' : 'items'} remaining in queue`,
			);
			if (retry === null) {
				setTimeout(() => this.maybeProcessNext(), 0);
				try {
					success(result, error);
				} catch (callbackError) {
					this.logger.error('Error in queue success callback', callbackError);
				}
				return;
			}
			const delay = retry.retryAfter ?? this.defaultRetryAfter;
			this.logger.debug(
				`Pausing queue processing for ${delay}ms due to retry request; ${this.queue.length} ${this.queue.length === 1 ? 'item' : 'items'} waiting`,
			);
			this.retryTimerId = window.setTimeout(() => {
				this.queue.unshift(entry);
				this.retryTimerId = null;
				this.maybeProcessNext();
			}, delay);
		};
		this.logger.debug(
			`Processing queued item; ${this.queue.length} ${this.queue.length === 1 ? 'item' : 'items'} left after dequeue`,
		);
		try {
			drainResult = this.drain(message, complete);
		} catch (error) {
			this.logger.error('Unhandled error while draining queue item', error);
			if (!hasCompleted) {
				complete(null, undefined, error);
			}
		}
		if (drainResult && typeof drainResult.catch === 'function') {
			void drainResult.catch((error) => {
				this.logger.error('Unhandled error while draining queue item', error);
				if (!hasCompleted) {
					complete(null, undefined, error);
				}
			});
		}
	}
}
