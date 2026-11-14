// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import {Future} from '../room/utils.ts';
import type {Throws} from './throws.ts';

export class DeferrableMapAbortError extends DOMException {
	reason: unknown;

	constructor(message: string, reason?: unknown) {
		super(message, 'AbortError');
		this.reason = reason;
	}
}

export class DeferrableMap<K, V> extends Map<K, V> {
	private pending: Map<K, Array<Future<V, DeferrableMapAbortError>>> = new Map();

	override set(key: K, value: V): this {
		super.set(key, value);

		const futures = this.pending?.get(key);
		if (futures) {
			for (const future of futures) {
				if (!future.isResolved) {
					future.resolve?.(value);
				}
			}
			this.pending.delete(key);
		}

		return this;
	}

	override get [Symbol.toStringTag](): string {
		return 'DeferrableMap';
	}

	getDeferred(key: K): Promise<V>;
	getDeferred(key: K, signal: AbortSignal): Promise<Throws<V, DeferrableMapAbortError>>;
	async getDeferred(key: K, signal?: AbortSignal) {
		const existing = this.get(key);
		if (typeof existing !== 'undefined') {
			return existing;
		}

		if (signal?.aborted) {
			throw new DeferrableMapAbortError('The operation was aborted.', signal.reason);
		}

		const future = new Future<V, DeferrableMapAbortError>(undefined, () => {
			const futures = this.pending.get(key);
			if (!futures) {
				return;
			}

			const idx = futures.indexOf(future);
			if (idx !== -1) {
				futures.splice(idx, 1);
			}
			if (futures.length === 0) {
				this.pending.delete(key);
			}
		});

		const existingFutures = this.pending.get(key);
		if (existingFutures) {
			existingFutures.push(future);
		} else {
			this.pending.set(key, [future]);
		}

		if (signal) {
			const onAbort = () => {
				if (!future.isResolved) {
					future.reject?.(new DeferrableMapAbortError('The operation was aborted.', signal.reason));
				}
			};
			signal.addEventListener('abort', onAbort, {once: true});

			future.promise.finally(() => {
				signal.removeEventListener('abort', onAbort);
			});
		}

		return future.promise;
	}
}
