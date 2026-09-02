// SPDX-License-Identifier: AGPL-3.0-or-later

export class ScreenShareRollbackIncompleteError extends Error {
	readonly errors: ReadonlyArray<unknown>;

	constructor(errors: ReadonlyArray<unknown>) {
		super('Screen share failed and its capture cleanup did not finish');
		this.name = 'ScreenShareRollbackIncompleteError';
		this.errors = errors;
	}
}

export function isScreenShareRollbackIncompleteError(error: unknown): error is ScreenShareRollbackIncompleteError {
	return error instanceof ScreenShareRollbackIncompleteError;
}
