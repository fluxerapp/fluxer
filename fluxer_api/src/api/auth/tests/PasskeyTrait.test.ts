// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {createAuthHarness, createTestAccount, createUniqueEmail} from '@app/api/auth/tests/AuthTestUtils';
import {
	createSudoWebAuthnBody,
	createWebAuthnDevice,
	registerWebAuthnCredential,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {hasWayBackIn} from '@app/api/user/AccountTier';
import {UserTraits} from '@app/api/user/UserTraits';
import {backfillPasskeyTraits} from '@app/api/worker/tasks/BackfillPasskeyTrait';
import {ClaimAccountNextSteps} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AccountTiers} from '@fluxer/constants/src/AccountTierConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Me {
	account_tier: string;
	verification_need: string | null;
	traits: Array<string>;
}

interface Registered {
	token: string;
	user_id: string;
}

interface Credential {
	id: string;
}

describe('Passkey trait', () => {
	let harness: ApiTestHarness;
	let originalSelfHosted: boolean;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		originalSelfHosted = getConfig().instance.selfHosted;
	});
	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function me(token: string): Promise<Me> {
		return createBuilder<Me>(harness, token).get('/users/@me').execute();
	}

	async function loadUser(userId: string) {
		return await getUserRepository().findUniqueAssert(createUserID(BigInt(userId)));
	}

	async function storeCredentialWithoutTrait(userId: UserID): Promise<string> {
		const credentialId = randomBytes(16).toString('base64url');
		await getUserRepository().createWebAuthnCredential(
			userId,
			credentialId,
			randomBytes(65),
			0n,
			null,
			'Older passkey',
			null,
		);
		return credentialId;
	}

	async function createUsernameModeUnclaimed(): Promise<Registered> {
		getConfig().instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		await getInstanceConfigRepository().updateAuthConfig({instant_accounts: {enabled: true}});
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({global_name: 'Guest', date_of_birth: '1995-05-05'})
			.execute();
	}

	it('makes an unclaimed username account verified once it registers a passkey, whatever its two-factor setting', async () => {
		const account = await createUsernameModeUnclaimed();
		expect((await me(account.token)).account_tier).toBe(AccountTiers.UNCLAIMED);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({}));
		const registered = await me(account.token);
		expect(registered).toMatchObject({account_tier: AccountTiers.VERIFIED, verification_need: null});
		expect(registered.traits).toContain(UserTraits.PASSKEY);
		const enable = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: true, ...enable})
			.execute();
		expect((await me(account.token)).account_tier).toBe(AccountTiers.VERIFIED);
		const disable = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, ...disable})
			.execute();
		expect((await me(account.token)).account_tier).toBe(AccountTiers.VERIFIED);
		const user = await loadUser(account.user_id);
		expect(await hasWayBackIn({users: getUserRepository()}, user)).toBe(true);
	});

	it('removes the trait when the last passkey is deleted', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		expect((await me(account.token)).traits).toContain(UserTraits.PASSKEY);
		const [credential] = await createBuilder<Array<Credential>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credential!.id}`)
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect((await me(account.token)).traits).not.toContain(UserTraits.PASSKEY);
	});

	it('backfills the trait for stored passkeys and clears it where none is left', async () => {
		const holder = await createTestAccount(harness);
		const holderId = createUserID(BigInt(holder.userId));
		await storeCredentialWithoutTrait(holderId);
		const stale = await createTestAccount(harness);
		const staleId = createUserID(BigInt(stale.userId));
		const staleCredential = await storeCredentialWithoutTrait(staleId);
		await getUserRepository().setWebAuthnCredentialSupersededBy(staleId, staleCredential, 'replacement');
		const staleUser = await loadUser(stale.userId);
		await getUserRepository().patchUpsert(staleId, {traits: new Set([UserTraits.PASSKEY])}, staleUser.toRow());
		const summary = await backfillPasskeyTraits(createApiContext(), 1);
		expect(summary).toMatchObject({users: 2, set: 1, cleared: 1, failed: 0});
		expect((await loadUser(holder.userId)).traits.has(UserTraits.PASSKEY)).toBe(true);
		expect((await loadUser(stale.userId)).traits.has(UserTraits.PASSKEY)).toBe(false);
		expect(await backfillPasskeyTraits(createApiContext())).toMatchObject({set: 0, cleared: 0, failed: 0});
	});

	it('lets an email instance without delivery claim with an existing passkey and no password', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {enabled: false}});
		await getInstanceConfigRepository().isEmailEnabled();
		const account = await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'keyholder', consent: true, date_of_birth: '1995-05-05'})
			.execute();
		await storeCredentialWithoutTrait(createUserID(BigInt(account.user_id)));
		await backfillPasskeyTraits(createApiContext());
		expect((await me(account.token)).account_tier).toBe(AccountTiers.UNCLAIMED);
		const claimed = await createBuilder<{next_step: string; user: Me}>(harness, account.token)
			.post('/users/@me/claim')
			.body({email: createUniqueEmail('keyholder')})
			.execute();
		expect(claimed.next_step).toBe(ClaimAccountNextSteps.NONE);
		expect(claimed.user.account_tier).toBe(AccountTiers.VERIFIED);
	});
});
