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

import {Logger} from '~/lib/Logger';

export interface QueueEntry<TMessage, TResult = void> {
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
	): void;

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

		const complete = (retry: RetryInfo | null, result?: TResult, error?: unknown): void => {
			if (hasCompleted) {
				this.logger.warn('Queue completion callback invoked more than once; ignoring extra call');
				return;
			}

			hasCompleted = true;
			this.isDraining = false;

			this.logger.info(`Finished processing queued item; ${this.queue.length} item(s) remaining in queue`);

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

			this.logger.info(
				`Pausing queue processing for ${delay}ms due to retry request; ${this.queue.length} item(s) waiting`,
			);

			this.retryTimerId = window.setTimeout(() => {
				this.queue.unshift(entry);
				this.retryTimerId = null;
				this.maybeProcessNext();
			}, delay);
		};

		this.logger.info(`Processing queued item; ${this.queue.length} item(s) left after dequeue`);

		try {
			this.drain(message, complete);
		} catch (error) {
			this.logger.error('Unhandled error while draining queue item', error);
			if (!hasCompleted) {
				complete(null, undefined, error);
			}
		}
	}
}
