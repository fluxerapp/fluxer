// SPDX-License-Identifier: AGPL-3.0-or-later

export type BlueskyOAuthStoreKind = 'state' | 'session';

export class BlueskyOAuthStateInvalidError extends Error {
	constructor() {
		super('Bluesky OAuth state is invalid');
	}
}

export class BlueskyOAuthStoreError extends Error {
	constructor(
		readonly kind: BlueskyOAuthStoreKind,
		message: string,
	) {
		super(message);
	}
}
