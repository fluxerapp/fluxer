// SPDX-License-Identifier: AGPL-3.0-or-later

import {verifyPassword} from '@app/api/utils/PasswordUtils';
import {constantTimeEquals, secretMatchesHash, sha256Hex} from '@app/api/utils/SecretHash';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const LEGACY_HASH_PREFIX = '$argon2';
const LEGACY_PROOF_TTL_SECONDS = 90 * 24 * 60 * 60;
const LEGACY_CONCURRENT_VERIFICATIONS = 2;
const LEGACY_QUEUED_VERIFICATIONS = 32;

export function hashStoredSecret(secret: string): string {
	return sha256Hex(secret);
}

export class StoredSecretVerifier {
	private activeLegacyVerifications = 0;
	private readonly legacyWaiters: Array<() => void> = [];

	constructor(private readonly cache: Pick<ICacheService, 'get' | 'set'>) {}

	async matches(scope: string, secret: string, storedHash: string): Promise<boolean> {
		if (!storedHash.startsWith(LEGACY_HASH_PREFIX)) {
			return secretMatchesHash(secret, storedHash);
		}
		const proofKey = `stored_secret_proof:${scope}:${sha256Hex(storedHash)}`;
		const proof = sha256Hex(`${secret}\0${storedHash}`);
		const knownProof = await this.cache.get<string>(proofKey).catch(() => null);
		if (knownProof) {
			return constantTimeEquals(knownProof, proof);
		}
		const release = await this.acquireLegacySlot();
		if (!release) {
			return false;
		}
		let valid: boolean;
		try {
			valid = await verifyPassword({password: secret, passwordHash: storedHash}).catch(() => false);
		} finally {
			release();
		}
		if (valid) {
			await this.cache.set(proofKey, proof, LEGACY_PROOF_TTL_SECONDS).catch(() => undefined);
		}
		return valid;
	}

	private async acquireLegacySlot(): Promise<(() => void) | null> {
		if (this.activeLegacyVerifications >= LEGACY_CONCURRENT_VERIFICATIONS) {
			if (this.legacyWaiters.length >= LEGACY_QUEUED_VERIFICATIONS) {
				return null;
			}
			await new Promise<void>((resolve) => this.legacyWaiters.push(resolve));
		} else {
			this.activeLegacyVerifications += 1;
		}
		return () => {
			const next = this.legacyWaiters.shift();
			if (next) {
				next();
				return;
			}
			this.activeLegacyVerifications -= 1;
		};
	}
}
