// SPDX-License-Identifier: AGPL-3.0-or-later

interface ExponentialBackoffOptions {
	minDelay: number;
	maxDelay: number;
	jitterFactor?: number;
}

export class ExponentialBackoff {
	private attempts = 0;
	private readonly jitterFactor: number;

	constructor(private readonly options: ExponentialBackoffOptions) {
		if (options.minDelay <= 0) {
			throw new Error('minDelay must be greater than 0');
		}
		if (options.maxDelay < options.minDelay) {
			throw new Error('maxDelay must be greater than or equal to minDelay');
		}
		this.jitterFactor = options.jitterFactor ?? 0.25;
		if (this.jitterFactor < 0 || this.jitterFactor > 1) {
			throw new Error('jitterFactor must be between 0 and 1');
		}
	}

	next(): number {
		this.attempts++;
		const baseDelay = Math.min(this.options.minDelay * 2 ** (this.attempts - 1), this.options.maxDelay);
		const maxJitter = baseDelay * this.jitterFactor;
		const jitter = (Math.random() * 2 - 1) * maxJitter;
		return Math.max(this.options.minDelay, Math.min(baseDelay + jitter, this.options.maxDelay));
	}

	getCurrentAttempts(): number {
		return this.attempts;
	}

	reset(): void {
		this.attempts = 0;
	}
}
