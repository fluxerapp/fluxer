// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getKvMeta} from '@app/api/database/CassandraMetaRegistry';
import {
	type CassandraQueryExecutorForTesting,
	fetchOne,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {REGISTRATION_PENDING_APPROVAL_TRAIT} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {InstanceConfiguration, Users} from '@app/api/Tables';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	InstanceConfigResponse,
	PendingRegistrationListResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, afterEach, assert, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const REGISTRATION_PENDING_APPROVALS_KEY = 'registration_pending_approvals';

interface WriteFailure {
	table: 'users' | 'instance_configuration' | 'authorized_ips_v2' | 'user_settings';
	timing: 'before' | 'after';
}

interface RegistrationResponse {
	user_id?: string;
	token?: string;
	registration_pending_approval?: true;
	code?: string;
}

function registrationBody(prefix: string, registrationUrlCode?: string) {
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
	let storage: InMemoryCassandraQueryExecutor;
	let executor: CassandraQueryExecutorForTesting;
	let writeFailure: WriteFailure | null = null;
	let expectedFailure: WriteFailure | null = null;
	let observedFailure: WriteFailure | null = null;
	let failureEmail: string | null = null;
	let failureUserId: UserID | null = null;
	const requests = new Set<Promise<unknown>>();

	beforeAll(async () => {
		harness = await createAuthHarness();
		storage = new InMemoryCassandraQueryExecutor();
		executor = {
			async executeQuery<T, P extends CassandraParams>(query: PreparedQuery<P>): Promise<Array<T>> {
				const metadata = query.kvMeta ?? getKvMeta(query.cql);
				if (
					metadata?.table.name === 'users' &&
					metadata.action === 'patch' &&
					query.params.email === failureEmail &&
					typeof query.params.user_id === 'bigint'
				) {
					failureUserId = createUserID(query.params.user_id);
				}
				const failure = writeFailure;
				const matchesAccount = query.params.user_id === failureUserId;
				const matchesPending = query.params.key === REGISTRATION_PENDING_APPROVALS_KEY;
				if (
					failure &&
					metadata?.table.name === failure.table &&
					(metadata.action === 'patch' || metadata.action === 'upsert') &&
					(matchesAccount || matchesPending)
				) {
					writeFailure = null;
					if (failure.timing === 'after') {
						await storage.executeQuery<T>(query);
					}
					observedFailure = failure;
					throw new Error('The database write acknowledgment failed');
				}
				return storage.executeQuery<T>(query);
			},
			executeBatch: (queries, atomic) => storage.executeBatch(queries, atomic),
			reset: () => storage.reset(),
		};
	});

	beforeEach(async () => {
		setCassandraQueryExecutorForTesting(executor);
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
	});

	afterEach(async () => {
		writeFailure = null;
		try {
			await Promise.allSettled(requests);
			expect(observedFailure).toEqual(expectedFailure);
		} finally {
			setCassandraQueryExecutorForTesting(storage);
			expectedFailure = null;
			observedFailure = null;
			failureEmail = null;
			failureUserId = null;
			requests.clear();
		}
	});

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(storage);
		await harness.shutdown();
	});

	function failWrite(failure: WriteFailure): void {
		writeFailure = failure;
		expectedFailure = failure;
	}

	function register(prefix: string, registrationUrlCode?: string) {
		const body = registrationBody(prefix, registrationUrlCode);
		if (writeFailure) {
			failureEmail = body.email;
		}
		const operation = createBuilderWithoutAuth<RegistrationResponse>(harness)
			.post('/auth/register')
			.body(body)
			.executeRaw();
		requests.add(operation);
		return operation;
	}

	const readAdminConfig = (): Promise<InstanceConfigResponse> =>
		createBuilder<InstanceConfigResponse>(harness, admin.token).get('/admin/instance/config').execute();

	const readPendingRegistrations = async (): Promise<PendingRegistrationListResponse['pending_registrations']> =>
		(
			await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
				.get('/admin/instance/pending-registrations')
				.execute()
		).pending_registrations;

	const completeSso = (prefix: string) => {
		const email = createUniqueEmail(prefix);
		if (writeFailure) {
			failureEmail = email;
		}
		const operation = (async () => {
			const start = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/me'})
				.execute();
			return createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({code: email, state: start.state, client_binding: start.client_binding})
				.executeRaw();
		})();
		requests.add(operation);
		return operation;
	};

	async function expectStoredPendingEmpty(): Promise<void> {
		expect(await readPendingRegistrations()).toEqual([]);
		const stored = await fetchOne<{value: string}>(
			InstanceConfiguration.selectCql({where: InstanceConfiguration.where.eq('key')}),
			{key: REGISTRATION_PENDING_APPROVALS_KEY},
		);
		expect(JSON.parse(stored?.value ?? 'null')).toEqual([]);
	}

	async function expectFailedWriteRowAbsent(): Promise<void> {
		assert(failureUserId);
		expect(await new UserRepository().findUnique(failureUserId)).toBeNull();
	}

	async function expectFailedWriteRowPresent(): Promise<void> {
		assert(failureUserId);
		const row = await fetchOne<UserRow>(Users.selectCql({where: Users.where.eq('user_id')}), {user_id: failureUserId});
		expect(row?.email).toBe(failureEmail);
		expect(row?.traits?.has(REGISTRATION_PENDING_APPROVAL_TRAIT)).toBe(true);
	}

	const expectOnePendingAccount = async () => {
		const pending = await readPendingRegistrations();
		expect(pending).toHaveLength(1);
		const [entry] = pending;
		assert(entry);
		const account = await new UserRepository().findUnique(createUserID(BigInt(entry.user_id)));
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
		failWrite({table: 'instance_configuration', timing: 'after'});

		const failed = await register('seatreleased', code);
		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectStoredPendingEmpty();
		const withdrawn = await readAdminConfig();
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
		failWrite({table: 'users', timing: 'before'});

		const failed = await register('seatkeptoncreate', code);
		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectFailedWriteRowAbsent();
		const second = await register('seatkeptcreate2', code);

		expect(second.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(second.json.code).toBe(APIErrorCodes.REGISTRATION_URL_INVALID);
		const stored = (await readAdminConfig()).registration.urls.find((url) => url.id === registrationUrl.id);
		expect(stored?.use_count).toBe(1);
	});

	it('keeps every pending approval when approval-mode signups race', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const attempts = await Promise.all(Array.from({length: 5}, (_, index) => register(`pending${index}`)));

		expect(attempts.map((attempt) => attempt.json.registration_pending_approval)).toEqual(Array(5).fill(true));
		const pending = (await readPendingRegistrations()).map((entry) => entry.user_id);
		expect(pending.toSorted()).toEqual(attempts.map((attempt) => attempt.json.user_id).toSorted());
	});

	it('lists an approval-mode account whose signup failed after the account was created', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failWrite({table: 'authorized_ips_v2', timing: 'before'});

		const failed = await register('pendingstranded');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
		await expectFailedWriteRowPresent();
	});

	it('lists an SSO account provisioned in approval mode whose provisioning failed after the account was created', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failWrite({table: 'user_settings', timing: 'before'});

		const failed = await completeSso('ssopendingstranded');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
		await expectFailedWriteRowPresent();
	});

	it('lists an SSO account provisioned in approval mode whose row was written before its creation failed', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failWrite({table: 'users', timing: 'after'});

		const failed = await completeSso('ssopendingrowwritten');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectOnePendingAccount();
		await expectFailedWriteRowPresent();
	});

	it('keeps the pending approval of an SSO signup in approval mode whose account create failed', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failWrite({table: 'users', timing: 'before'});

		const failed = await completeSso('ssopendingkept');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectFailedWriteRowAbsent();
		expect(await readPendingRegistrations()).toHaveLength(1);
	});

	it('lists no pending approval for an SSO signup in approval mode that failed before the account was created', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		failWrite({table: 'instance_configuration', timing: 'after'});

		const failed = await completeSso('ssopendingnever');

		expect(failed.response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		await expectStoredPendingEmpty();
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

		const pendingRegistrations = await readPendingRegistrations();
		expect(pendingRegistrations.map((entry) => entry.user_id).toSorted()).toEqual(
			[account.userId, fresh.json.user_id].toSorted(),
		);
		expect(pendingRegistrations.find((entry) => entry.user_id === account.userId)?.invite_code).toBeNull();

		const decided = await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${account.userId}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(decided.pending_registrations.map((entry) => entry.user_id)).toEqual([fresh.json.user_id]);
		expect(
			JSON.parse((await getInstanceConfigRepository().getConfig(REGISTRATION_PENDING_APPROVALS_KEY)) ?? 'null'),
		).toEqual([expect.objectContaining({user_id: fresh.json.user_id})]);
	});
});
