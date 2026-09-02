// SPDX-License-Identifier: AGPL-3.0-or-later

export class VoiceMediaIdentityCapabilityError extends Error {
	constructor() {
		super('Voice media identity generation requires crypto.randomUUID');
		this.name = 'VoiceMediaIdentityCapabilityError';
	}
}

const RANDOM_UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u;

export class VoiceMediaIdentityInvariantError extends Error {
	constructor() {
		super('crypto.randomUUID must return a canonical version 4 UUID');
		this.name = 'VoiceMediaIdentityInvariantError';
	}
}

export function createVoiceMediaIdentity(): string {
	const cryptoPort = globalThis.crypto;
	if (cryptoPort == null || typeof cryptoPort.randomUUID !== 'function') {
		throw new VoiceMediaIdentityCapabilityError();
	}
	const identity = cryptoPort.randomUUID();
	if (!RANDOM_UUID_PATTERN.test(identity)) {
		throw new VoiceMediaIdentityInvariantError();
	}
	return identity;
}
