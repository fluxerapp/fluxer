// SPDX-License-Identifier: AGPL-3.0-or-later

export type HttpErrorType = 'aborted' | 'blocked' | 'network_error' | 'unresolvable_host' | 'unknown';

export class HttpError extends Error {
	constructor(
		message: string,
		public readonly status?: number,
		public readonly response?: Response,
		public readonly isExpected = false,
		public readonly errorType?: HttpErrorType,
	) {
		super(message);
		this.name = 'HttpError';
	}
}
