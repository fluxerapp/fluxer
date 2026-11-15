// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	unclaimAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {EmailDeliveryModes} from '@fluxer/constants/src/AccountConfigConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface SudoModeRequiredBody {
	methods: {sso_reauth: boolean};
}

describe('sudo mode for SSO accounts without credentials', () => {
	let harness: ApiTestHarness;
	let adminToken: string;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
		]);
		adminToken = admin.token;
		await enableSso(harness, adminToken, {enforced: false});
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: EmailDeliveryModes.OFF}});
	});
	afterEach(async () => {
		vi.useRealTimers();
		await disableSso(harness, adminToken);
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function signInWithSso(): Promise<string> {
		const claims = {sub: `itest-sso-sudo-${Date.now()}-${Math.random()}`, email: createUniqueEmail('sso-sudo')};
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		const {token} = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.expect(200)
			.execute();
		return token;
	}

	function travelMinutes(minutes: number): void {
		vi.useFakeTimers({toFake: ['Date']});
		vi.setSystemTime(Date.now() + minutes * 60 * 1000);
	}

	it('lets a fresh SSO sign-in through without a proof', async () => {
		const token = await signInWithSso();
		await createBuilder(harness, token).post('/users/@me/disable').body({}).expect(204).execute();
	});

	it('asks an older SSO session to sign in again', async () => {
		const token = await signInWithSso();
		travelMinutes(11);
		const body = await createBuilder<SudoModeRequiredBody>(harness, token)
			.post('/users/@me/disable')
			.body({})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(body.methods.sso_reauth).toBe(true);
	});

	it('does not offer signing in again with SSO once SSO is turned off', async () => {
		const token = await signInWithSso();
		await disableSso(harness, adminToken);
		travelMinutes(11);
		const body = await createBuilder<SudoModeRequiredBody>(harness, token)
			.post('/users/@me/disable')
			.body({})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(body.methods.sso_reauth).toBe(false);
	});

	it('still lets an unclaimed account without SSO through without a proof', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		travelMinutes(11);
		await createBuilder(harness, account.token).post('/users/@me/disable').body({}).expect(204).execute();
	});
});
