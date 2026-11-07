// SPDX-License-Identifier: AGPL-3.0-or-later

import {hashPassword} from '@app/api/utils/PasswordUtils';
import {hashStoredSecret, StoredSecretVerifier} from '@app/api/utils/StoredSecret';
import argon2 from 'argon2';
import {describe, expect, it, vi} from 'vitest';

function memoryCache() {
	const values = new Map<string, unknown>();
	return {
		get: async <T>(key: string) => (values.get(key) as T | undefined) ?? null,
		set: async <T>(key: string, value: T) => {
			values.set(key, value);
		},
	};
}

describe('StoredSecretVerifier', () => {
	it('checks new hashes without argon2', async () => {
		const verify = vi.spyOn(argon2, 'verify');
		const verifier = new StoredSecretVerifier(memoryCache());
		const hash = hashStoredSecret('right');
		expect(await verifier.matches('bot_token:1', 'right', hash)).toBe(true);
		expect(await verifier.matches('bot_token:1', 'wrong', hash)).toBe(false);
		expect(verify).not.toHaveBeenCalled();
		verify.mockRestore();
	});

	it('rejects wrong secrets for a proven legacy hash without argon2', async () => {
		const verifier = new StoredSecretVerifier(memoryCache());
		const hash = await hashPassword('right');
		expect(await verifier.matches('bot_token:1', 'right', hash)).toBe(true);
		const verify = vi.spyOn(argon2, 'verify');
		expect(await verifier.matches('bot_token:1', 'wrong', hash)).toBe(false);
		expect(await verifier.matches('bot_token:1', 'right', hash)).toBe(true);
		expect(verify).not.toHaveBeenCalled();
		verify.mockRestore();
	});

	it('does not reuse a proof for a different legacy hash', async () => {
		const verifier = new StoredSecretVerifier(memoryCache());
		expect(await verifier.matches('bot_token:1', 'old', await hashPassword('old'))).toBe(true);
		expect(await verifier.matches('bot_token:1', 'new', await hashPassword('new'))).toBe(true);
	});
});
