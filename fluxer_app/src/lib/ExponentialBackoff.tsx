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

interface ExponentialBackoffOptions {
	minDelay: number;
	maxDelay: number;
	maxNumOfAttempts?: number;
	factor?: number;
	jitter?: boolean;
	jitterFactor?: number;
}

export class ExponentialBackoff {
	private attempts = 0;
	private readonly factor: number;
	private readonly maxAttempts: number;
	private readonly useJitter: boolean;
	private readonly jitterFactor: number;

	constructor(private readonly options: ExponentialBackoffOptions) {
		if (options.minDelay <= 0) {
			throw new Error('minDelay must be greater than 0');
		}
		if (options.maxDelay < options.minDelay) {
			throw new Error('maxDelay must be greater than or equal to minDelay');
		}
		this.factor = options.factor ?? 2;
		this.maxAttempts = options.maxNumOfAttempts ?? Number.POSITIVE_INFINITY;
		this.useJitter = options.jitter ?? true;
		this.jitterFactor = options.jitterFactor ?? 0.25;
		if (this.factor <= 1) {
			throw new Error('factor must be greater than 1');
		}
		if (this.maxAttempts <= 0) {
			throw new Error('maxNumOfAttempts must be greater than 0');
		}
		if (this.jitterFactor < 0 || this.jitterFactor > 1) {
			throw new Error('jitterFactor must be between 0 and 1');
		}
	}

	next(): number {
		this.attempts++;
		const baseDelay = Math.min(this.options.minDelay * this.factor ** (this.attempts - 1), this.options.maxDelay);
		if (this.useJitter) {
			const maxJitter = baseDelay * this.jitterFactor;
			const jitter = (Math.random() * 2 - 1) * maxJitter;
			return Math.max(this.options.minDelay, Math.min(baseDelay + jitter, this.options.maxDelay));
		}
		return baseDelay;
	}

	getCurrentAttempts(): number {
		return this.attempts;
	}

	getMaxAttempts(): number {
		return this.maxAttempts;
	}

	isExhausted(): boolean {
		return this.attempts >= this.maxAttempts;
	}

	getMinDelay(): number {
		return this.options.minDelay;
	}

	getMaxDelay(): number {
		return this.options.maxDelay;
	}

	reset(): void {
		this.attempts = 0;
	}

	static create(minDelay: number, maxDelay: number, maxAttempts?: number): ExponentialBackoff {
		return new ExponentialBackoff({
			minDelay,
			maxDelay,
			maxNumOfAttempts: maxAttempts,
		});
	}
}
