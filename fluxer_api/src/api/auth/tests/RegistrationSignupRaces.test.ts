// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {
	InstanceConfigRepository,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const REGISTRATION_URLS_KEY = 'registration_urls';
const REGISTRATION_PENDING_APPROVALS_KEY = 'registration_pending_approvals';

interface RegistrationResponse {
	user_id?: string;
	token?: string;
	registration_pending_approval?: true;
	code?: string;
}

function registrationBody(prefix: string, registrationUrlCode?: string): Record<string, unknown> {
	return {
		email: createUniqueEmail(prefix),
		username: createUniqueUsername(prefix),
		global_name: 'Signup Race',
		password: 'a-strong-password',
		date_of_birth: '2000-01-01',
		consent: true,
		...(registrationUrlCode === undefined ? {} : {registration_url_code: registrationUrlCode}),
	};
}

describe('signups racing on registration URLs and pending approvals', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	const register = (prefix: string, registrationUrlCode?: string) =>
		createBuilderWithoutAuth<RegistrationResponse>(harness)
			.post('/auth/register')
			.body(registrationBody(prefix, registrationUrlCode))
			.executeRaw();

	const readAdminConfig = (): Promise<InstanceConfigResponse> =>
		createBuilder<InstanceConfigResponse>(harness, admin.token).get('/admin/instance/config').execute();

	const completeSso = async (prefix: string) => {
		const start = await createBuilderWithoutAuth<{state: string}>(harness)
			.post('/auth/sso/start')
			.body({redirect_to: '/me'})
			.execute();
		return createBuilderWithoutAuth(harness)
			.post('/auth/sso/complete')
			.body({code: createUniqueEmail(prefix), state: start.state})
			.executeRaw();
	};

	const failCreateAfterTheUserRowIsWritten = () => {
		const create = UserRepository.prototype.create;
		vi.spyOn(UserRepository.prototype, 'create').mockImplementationOnce(async function (
			this: UserRepository,
			row: UserRow,
		) {
			await create.call(this, row);
			throw new Error('the user indexes could not be written after the user row');
		});
	};

	const failAfterThePendingApprovalIsStored = () => {
		const addPendingRegistration = InstanceConfigRepository.prototype.addPendingRegistration;
		vi.spyOn(InstanceConfigRepository.prototype, 'addPendingRegistration').mockImplementationOnce(async function (
			this: InstanceConfigRepository,
			entry: Parameters<InstanceConfigRepository['addPendingRegistration']>[0],
		) {
			await addPendingRegistration.call(this, entry);
			throw new Error('the pending approval could not be published');
		});
	};

	const expectOnePendingAccount = async () => {
		const pending = (await readAdminConfig()).registration.pending_registrations;
		expect(pending).toHaveLength(1);
		const account = await new UserRepository().findUnique(createUserID(BigInt(pending[0]!.user_id)));
		expect(account?.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)).toBe(true);
	};

	it('never lets concurrent signups through a capped registration URL exceed max_uses', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Capped',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 2,
			approvalRequired: false,
		});

		const attempts = await Promise.all(Array.from({length: 6}, (_, index) => register(`capped${index}`, code)));

		const admitted = attempts.filter((attempt) => attempt.response.status === HTTP_STATUS.OK);
		const refused = attempts.filter((attempt) => attempt.response.status !== HTTP_STATUS.OK);
		expect(admitted).toHaveLength(2);
		for (const attempt of refused) {
			expect(attempt.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(attempt.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);
		}
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(2);
		expect(admitted.map((attempt) => attempt.json.user_id)).toContain(stored?.last_used_by_user_id);
	});

	it('admits exactly max_uses when 120 signups race through a registration URL capped at 40', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Capped at 40',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 40,
			approvalRequired: false,
		});
		const registerUntilDecided = async (prefix: string) => {
			for (let attempt = 0; attempt < 20; attempt += 1) {
				const result = await register(`${prefix}r${attempt}`, code);
				if (result.response.status !== HTTP_STATUS.SERVICE_UNAVAILABLE) return result;
			}
			throw new Error('a signup never reached a decision');
		};

		const attempts = await Promise.all(Array.from({length: 120}, (_, index) => registerUntilDecided(`surge${index}`)));

		const admitted = attempts.filter((attempt) => attempt.response.status === HTTP_STATUS.OK);
		expect(admitted).toHaveLength(40);
		for (const attempt of attempts.filter((entry) => entry.response.status !== HTTP_STATUS.OK)) {
			expect(attempt.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(attempt.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);
		}
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(40);
	});

	it('counts every concurrent signup through an uncapped registration URL', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Uncapped',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: null,
			approvalRequired: false,
		});

		const attempts = await Promise.all(Array.from({length: 5}, (_, index) => register(`uncapped${index}`, code)));

		expect(attempts.map((attempt) => attempt.response.status)).toEqual(Array(5).fill(HTTP_STATUS.OK));
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(5);
	});

	it('gives the seat and the pending entry back when the signup failed before the account was created', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Single use',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 1,
			approvalRequired: true,
		});
		failAfterThePendingApprovalIsStored();

		const failed = await register('seatreleased', code);
		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		const withdrawn = await readAdminConfig();
		expect(withdrawn.registration.pending_registrations).toEqual([]);
		expect(withdrawn.registration.urls.find((url) => url.id === registrationUrl.id)?.use_count).toBe(0);

		const retried = await register('seatreleasedretry', code);

		expect(retried.response.status).toBe(HTTP_STATUS.OK);
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored).toMatchObject({use_count: 1, last_used_by_user_id: retried.json.user_id});
	});

	it('keeps the seat when the account create itself failed, because the row may still have landed', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Single use',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 1,
			approvalRequired: false,
		});
		vi.spyOn(UserRepository.prototype, 'create').mockRejectedValueOnce(new Error('the user row write failed'));

		const failed = await register('seatkeptoncreate', code);
		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		const second = await register('seatkeptcreate2', code);

		expect(second.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(second.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(1);
	});

	it('keeps the seat of an account whose row was written before its creation failed', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registrationUrl} = await repository.createRegistrationUrl({
			label: 'Single use',
			createdByUserId: '1',
			expiresAt: null,
			maxUses: 1,
			approvalRequired: false,
		});
		failCreateAfterTheUserRowIsWritten();

		const failed = await register('seatkept', code);
		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		const second = await register('seatkeptsecond', code);

		expect(second.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(second.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(1);
	});

	it('honours the use count and cap already stored on a registration URL', async () => {
		const repository = getInstanceConfigRepository();
		await repository.setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const id = 'b3c4f0b2-8a6e-4c41-9f55-3f0c2a7d1e90';
		await repository.setConfig(
			REGISTRATION_URLS_KEY,
			JSON.stringify([
				{
					id,
					label: 'Issued earlier',
					code_hash: createHash('sha256').update(id).digest('hex'),
					created_by_user_id: '1400000000000000001',
					created_at: '2026-09-01T00:00:00.000Z',
					expires_at: null,
					max_uses: 2,
					use_count: 1,
					revoked_at: null,
					approval_required: false,
					last_used_at: '2026-09-02T00:00:00.000Z',
					last_used_by_user_id: '1400000000000000002',
				},
			]),
		);

		const before = (await readAdminConfig()).registration.urls.find((url) => url.id === id);
		expect(before).toMatchObject({use_count: 1, max_uses: 2, last_used_by_user_id: '1400000000000000002'});

		const first = await register('storedinvite', id);
		expect(first.response.status).toBe(HTTP_STATUS.OK);
		const second = await register('storedinviteagain', id);
		expect(second.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(second.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);

		const after = (await readAdminConfig()).registration.urls.find((url) => url.id === id);
		expect(after).toMatchObject({use_count: 2, max_uses: 2, last_used_by_user_id: first.json.user_id});
	});

	it('keeps every pending approval when approval-mode signups race', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const attempts = await Promise.all(Array.from({length: 5}, (_, index) => register(`pending${index}`)));

		expect(attempts.map((attempt) => attempt.json.registration_pending_approval)).toEqual(Array(5).fill(true));
		const pending = (await readAdminConfig()).registration.pending_registrations.map((entry) => entry.user_id);
		expect(pending.toSorted()).toEqual(attempts.map((attempt) => attempt.json.user_id).toSorted());
	});

	it('lists an approval-mode account whose signup failed after the account was created', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		vi.spyOn(UserRepository.prototype, 'createAuthorizedIp').mockRejectedValueOnce(
			new Error('the authorized IP write failed'),
		);

		const failed = await register('pendingstranded');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
	});

	it('lists an approval-mode account whose row was written before its creation failed', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failCreateAfterTheUserRowIsWritten();

		const failed = await register('pendingrowwritten');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
	});

	it('keeps the pending approval of an approval-mode signup whose account create failed', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		vi.spyOn(UserRepository.prototype, 'create').mockRejectedValueOnce(new Error('the user row write failed'));

		const failed = await register('pendingkept');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		expect((await readAdminConfig()).registration.pending_registrations).toHaveLength(1);
	});

	it('lists no pending approval for an approval-mode signup that failed before the account was created', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failAfterThePendingApprovalIsStored();

		const failed = await register('pendingnever');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		expect((await readAdminConfig()).registration.pending_registrations).toEqual([]);
	});

	it('lists an SSO account provisioned in approval mode whose provisioning failed after the account was created', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		vi.spyOn(UserRepository.prototype, 'upsertSettings').mockRejectedValueOnce(new Error('the settings write failed'));

		const failed = await completeSso('ssopendingstranded');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
	});

	it('lists an SSO account provisioned in approval mode whose row was written before its creation failed', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failCreateAfterTheUserRowIsWritten();

		const failed = await completeSso('ssopendingrowwritten');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
	});

	it('keeps the pending approval of an SSO signup in approval mode whose account create failed', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		vi.spyOn(UserRepository.prototype, 'create').mockRejectedValueOnce(new Error('the user row write failed'));

		const failed = await completeSso('ssopendingkept');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		expect((await readAdminConfig()).registration.pending_registrations).toHaveLength(1);
	});

	it('lists no pending approval for an SSO signup in approval mode that failed before the account was created', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failAfterThePendingApprovalIsStored();

		const failed = await completeSso('ssopendingnever');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		expect((await readAdminConfig()).registration.pending_registrations).toEqual([]);
	});

	it('keeps a stored pending approval listed until an admin decides it', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setConfig(
			REGISTRATION_PENDING_APPROVALS_KEY,
			JSON.stringify([
				{
					user_id: account.userId,
					username: 'stored_pending',
					discriminator: 1,
					global_name: null,
					email: account.email,
					requested_at: '2026-09-01T00:00:00.000Z',
					registration_url_id: null,
					client_ip: '127.0.0.1',
				},
			]),
		);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const fresh = await register('pendingafter');

		const listed = (await readAdminConfig()).registration.pending_registrations.map((entry) => entry.user_id);
		expect(listed.toSorted()).toEqual([account.userId, fresh.json.user_id].toSorted());

		const decided = await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${account.userId}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(decided.registration.pending_registrations.map((entry) => entry.user_id)).toEqual([fresh.json.user_id]);
		expect(
			JSON.parse(
				(await getInstanceConfigRepository().getConfig(REGISTRATION_PENDING_APPROVALS_KEY)) ?? 'null',
			) as Array<{user_id: string}>,
		).toEqual([expect.objectContaining({user_id: fresh.json.user_id})]);
	});
});
