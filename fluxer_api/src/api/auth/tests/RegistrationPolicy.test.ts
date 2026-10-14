// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createUniqueEmail, createUniqueUsername} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface PendingRegistrationResponse {
	registration_pending_approval: true;
	user_id: string;
}

interface RegistrationTokenResponse {
	user_id: string;
	token: string;
}

function registrationBody(prefix: string): Record<string, unknown> {
	return {
		email: createUniqueEmail(prefix),
		username: createUniqueUsername(prefix),
		global_name: 'Registration Policy',
		password: 'a-strong-password',
		date_of_birth: '2000-01-01',
		consent: true,
	};
}

describe('Auth registration policy', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('blocks public registration when registration is closed', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'closed'});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body(registrationBody('closed'))
			.expect(403, APIErrorCodes.REGISTRATION_CLOSED)
			.execute();
	});

	it('allows a valid admin-issued registration URL while public registration is closed', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code} = await repository.createRegistrationUrl({
			label: 'Test link',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 1,
			approvalRequired: false,
		});
		const registration = await createBuilderWithoutAuth<RegistrationTokenResponse>(harness)
			.post('/auth/register')
			.body({...registrationBody('closedlink'), registration_url_code: code})
			.execute();
		expect(registration.token.length).toBeGreaterThan(0);
		expect(registration.user_id.length).toBeGreaterThan(0);
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({...registrationBody('closedlinkreuse'), registration_url_code: code})
			.expect(400, APIErrorCodes.REGISTRATION_URL_INVALID)
			.execute();
	});

	it('returns pending approval and blocks login when registration requires approval', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const body = registrationBody('approval');
		const registration = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(body)
			.execute();
		expect(registration.registration_pending_approval).toBe(true);
		expect(registration.user_id.length).toBeGreaterThan(0);
		expect(Object.keys(registration).sort()).toEqual(['registration_pending_approval', 'user_id']);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: body.email, password: body.password})
			.expect(403, APIErrorCodes.REGISTRATION_PENDING_APPROVAL)
			.execute();
	});

	it('refuses an approval registration without sign-in credentials', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({global_name: 'Nobody Signs In', date_of_birth: '2000-01-01', consent: true})
			.expect(400, APIErrorCodes.REGISTRATION_CREDENTIALS_REQUIRED)
			.execute();
		expect(await getInstanceConfigRepository().getPendingRegistrations()).toEqual([]);
	});

	it('refuses an approval registration with an email but no password', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const {password: _password, ...body} = registrationBody('nopassword');
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body(body)
			.expect(400, APIErrorCodes.REGISTRATION_CREDENTIALS_REQUIRED)
			.execute();
		expect(await getInstanceConfigRepository().getPendingRegistrations()).toEqual([]);
	});

	it('refuses an approval registration with a password but no email', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const {email: _email, ...body} = registrationBody('noemail');
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body(body)
			.expect(400, APIErrorCodes.REGISTRATION_CREDENTIALS_REQUIRED)
			.execute();
		expect(await getInstanceConfigRepository().getPendingRegistrations()).toEqual([]);
	});

	it('keeps the registration URL seat when an approval registration is refused', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'open', admin_registration_urls_enabled: true});
		const {code} = await repository.createRegistrationUrl({
			label: 'Reviewed link',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 1,
			approvalRequired: true,
		});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({global_name: 'Seat Keeper', date_of_birth: '2000-01-01', consent: true, registration_url_code: code})
			.expect(400, APIErrorCodes.REGISTRATION_CREDENTIALS_REQUIRED)
			.execute();
		const registration = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({...registrationBody('seatkeeper'), registration_url_code: code})
			.execute();
		expect(registration.registration_pending_approval).toBe(true);
	});

	it('still accepts an unclaimed registration in open mode', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'open'});
		const registration = await createBuilderWithoutAuth<RegistrationTokenResponse>(harness)
			.post('/auth/register')
			.body({global_name: 'Open Unclaimed', date_of_birth: '2000-01-01', consent: true})
			.execute();
		expect(registration.token.length).toBeGreaterThan(0);
	});
});
