// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {clearTestEmails, createTestAccount, findLastTestEmail, listTestEmails} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {deleteOneOrMany, upsertOne} from '../../database/CassandraQueryExecution';
import {getInstanceConfigRepository, getUserRepository} from '../../middleware/ServiceSingletons';
import {AuthorizedIps} from '../../Tables';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

interface LoginResponse {
	user_id: string;
	token: string;
}

async function seedAuthorizedIp(params: {
	harness: ApiTestHarness;
	email: string;
	password: string;
	ip: string;
}): Promise<void> {
	const {harness, email, password, ip} = params;
	await createBuilderWithoutAuth(harness)
		.post('/auth/login')
		.body({email, password})
		.header('x-forwarded-for', ip)
		.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.IP_AUTHORIZATION_REQUIRED)
		.execute();
	const emails = await listTestEmails(harness, {recipient: email});
	const ipEmail = findLastTestEmail(emails, 'ip_authorization');
	if (!ipEmail?.metadata?.token) {
		throw new Error('Missing IP authorization email token');
	}
	await createBuilderWithoutAuth(harness)
		.post('/auth/authorize-ip')
		.body({token: ipEmail.metadata.token})
		.expect(HTTP_STATUS.NO_CONTENT)
		.execute();
}

describe('User authorised IPs', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('requires sudo to forget authorised IPs', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.delete('/users/@me/authorized-ips')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
	});
	test('forgetting authorised IPs forces email verification on next login', async () => {
		const account = await createTestAccount(harness);
		const ip = '203.0.113.42';
		await seedAuthorizedIp({
			harness,
			email: account.email,
			password: account.password,
			ip,
		});
		const login = await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', ip)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, login.token)
			.delete('/users/@me/authorized-ips')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', ip)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.IP_AUTHORIZATION_REQUIRED)
			.execute();
	});
	test('accepts rotated IPv6 privacy addresses within the same /64', async () => {
		const account = await createTestAccount(harness);
		const firstIp = '2a01:e0a:d10:95b0:8f54:410e:f290:1c66';
		const rotatedIp = '2a01:e0a:d10:95b0:01e4:53a8:d0dd:7733';
		await seedAuthorizedIp({
			harness,
			email: account.email,
			password: account.password,
			ip: firstIp,
		});
		await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', rotatedIp)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('upgrades legacy exact IPv6 authorizations to prefix-aware trust', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const legacyIp = '2a01:e0a:d10:95b0:8f54:410e:f290:1c66';
		const rotatedIp = '2a01:e0a:d10:95b0:01e4:53a8:d0dd:7733';
		const laterRotatedIp = '2a01:e0a:d10:95b0:b53f:16d3:aff2:9b0f';
		await upsertOne(AuthorizedIps.insert({user_id: userId, ip: legacyIp}));
		await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', rotatedIp)
			.expect(HTTP_STATUS.OK)
			.execute();
		await deleteOneOrMany(
			AuthorizedIps.deleteByPk({
				user_id: userId,
				ip: legacyIp,
			}),
		);
		await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', laterRotatedIp)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('forgetting authorised IPs clears IPv6 prefix trust as well', async () => {
		const account = await createTestAccount(harness);
		const firstIp = '2a01:e0a:d10:95b0:8f54:410e:f290:1c66';
		const rotatedIp = '2a01:e0a:d10:95b0:01e4:53a8:d0dd:7733';
		const laterRotatedIp = '2a01:e0a:d10:95b0:b53f:16d3:aff2:9b0f';
		await seedAuthorizedIp({
			harness,
			email: account.email,
			password: account.password,
			ip: firstIp,
		});
		const login = await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', rotatedIp)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, login.token)
			.delete('/users/@me/authorized-ips')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', laterRotatedIp)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.IP_AUTHORIZATION_REQUIRED)
			.execute();
	});
	test('keeps IPv4 matching exact instead of widening to /24', async () => {
		const account = await createTestAccount(harness);
		const authorizedIp = '203.0.113.42';
		const nearbyIp = '203.0.113.43';
		await seedAuthorizedIp({
			harness,
			email: account.email,
			password: account.password,
			ip: authorizedIp,
		});
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', nearbyIp)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.IP_AUTHORIZATION_REQUIRED)
			.execute();
	});
	test('disabling IP authorization via config allows login immediately from a new IP', async () => {
		const account = await createTestAccount(harness);
		const ip = '203.0.113.99';

		const instanceConfigRepository = getInstanceConfigRepository();
		await instanceConfigRepository.setInstanceIntegrationsConfig({
			email: {
				disable_new_ip_authorization: true,
			},
		});

		try {
			const login = await createBuilderWithoutAuth<LoginResponse>(harness)
				.post('/auth/login')
				.body({email: account.email, password: account.password})
				.header('x-forwarded-for', ip)
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(login.token).toBeDefined();

			const isAuthorized = await getUserRepository().checkIpAuthorized(createUserID(BigInt(account.userId)), ip);
			expect(isAuthorized).toBe(true);
		} finally {
			await instanceConfigRepository.setInstanceIntegrationsConfig({
				email: {
					disable_new_ip_authorization: false,
				},
			});
		}
	});
	test('disabling email provider allows login immediately from a new IP', async () => {
		const account = await createTestAccount(harness);
		const ip = '203.0.113.100';

		await getInstanceConfigRepository().setInstanceIntegrationsConfig({
			email: {
				enabled: false,
				provider: 'none',
				disable_new_ip_authorization: false,
			},
		});

		const login = await createBuilderWithoutAuth<LoginResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.header('x-forwarded-for', ip)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(login.token).toBeDefined();

		const isAuthorized = await getUserRepository().checkIpAuthorized(createUserID(BigInt(account.userId)), ip);
		expect(isAuthorized).toBe(true);
		const emails = await listTestEmails(harness, {recipient: account.email});
		expect(findLastTestEmail(emails, 'ip_authorization')).toBeNull();
	});
});
