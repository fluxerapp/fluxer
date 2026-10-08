// SPDX-License-Identifier: AGPL-3.0-or-later

interface ExponentialBackoffOptions {
	minDelay: number;
	maxDelay: number;
	factor?: number;
	maxNumOfAttempts?: number;
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
