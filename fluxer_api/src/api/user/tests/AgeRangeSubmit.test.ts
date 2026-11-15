// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {
	type AppAttestAgeAccepted,
	type AppAttestOutcome,
	createUsersServiceClient,
	type IUsersServiceClient,
	setInjectedUsersServiceClient,
} from '@app/api/infrastructure/UsersServiceClient';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {AppAttestKeyLookup, AppAttestKeys} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {setAgeRangeGeoLookupForTesting} from '@app/api/user/AgeRange';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {type AttestedAgeFloor, AttestedAgeFloors} from '@fluxer/constants/src/AgeRangeConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const geo = {countryCode: 'GB' as string | null};

const SUBMISSION = {key_id: `${'A'.repeat(43)}=`, assertion: 'AAAA', client_data: 'AAAA'};

function yearsAgo(years: number): string {
	const date = new Date();
	date.setUTCFullYear(date.getUTCFullYear() - years);
	date.setUTCDate(date.getUTCDate() - 1);
	return date.toISOString().slice(0, 10);
}

function acceptingClient(base: IUsersServiceClient, floor: AttestedAgeFloor, attestedAt: Date): IUsersServiceClient {
	return {
		getUserPartialResponses: (userIds) => base.getUserPartialResponses(userIds),
		invalidateUserCache: (userId) => base.invalidateUserCache(userId),
		appAttestCreateChallenge: (userId, purpose) => base.appAttestCreateChallenge(userId, purpose),
		appAttestRegisterKey: (params) => base.appAttestRegisterKey(params),
		appAttestSubmitAge: async (): Promise<AppAttestOutcome<AppAttestAgeAccepted>> => ({
			ok: true,
			value: {age_floor: floor, attested_at: attestedAt.toISOString()},
		}),
		playIntegrityCreateChallenge: (userId) => base.playIntegrityCreateChallenge(userId),
		playIntegritySubmitAge: (params) => base.playIntegritySubmitAge(params),
	};
}

describe('Age range submit', () => {
	let harness: ApiTestHarness;
	let base: IUsersServiceClient;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		base = createUsersServiceClient();
	});
	beforeEach(async () => {
		await harness.reset();
		setAgeRangeGeoLookupForTesting(async () => ({countryCode: geo.countryCode, regionCode: null}));
	});
	afterEach(() => {
		geo.countryCode = 'GB';
		setAgeRangeGeoLookupForTesting(null);
		Config.instance.selfHosted = false;
		setInjectedUsersServiceClient(base);
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function deferredAccount(): Promise<string> {
		const {token} = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/register')
			.body({username: 'attested', consent: true})
			.execute();
		return token;
	}

	async function setAdultDateOfBirth(token: string): Promise<void> {
		await createBuilder(harness, token)
			.put('/users/@me/date-of-birth')
			.body({date_of_birth: yearsAgo(30)})
			.execute();
	}

	async function adultAccount(): Promise<string> {
		const token = await deferredAccount();
		await setAdultDateOfBirth(token);
		return token;
	}

	async function submit(token: string, floor: AttestedAgeFloor, attestedAt: Date): Promise<void> {
		setInjectedUsersServiceClient(acceptingClient(base, floor, attestedAt));
		const result = await createBuilder<{age_floor: string}>(harness, token)
			.post('/users/@me/age-range')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.age_floor).toBe(floor);
	}

	type Me = {
		mature_content_unlocked: boolean;
		has_age_range: boolean;
		nsfw_allowed: boolean;
		date_of_birth_required: boolean;
	};

	async function me(token: string): Promise<Me> {
		return createBuilder<Me>(harness, token).get('/users/@me').execute();
	}

	async function storedFloor(userId: string): Promise<string | null> {
		return (await new UserRepository().findUniqueAssert(createUserID(BigInt(userId)))).attestedAgeFloor;
	}

	it('unlocks mature content for an adult in the UK without touching the date of birth', async () => {
		const token = await adultAccount();
		expect(await me(token)).toMatchObject({mature_content_unlocked: false, nsfw_allowed: true});
		await submit(token, AttestedAgeFloors.ADULT, new Date());
		expect(await me(token)).toMatchObject({mature_content_unlocked: true, has_age_range: true, nsfw_allowed: true});
	});

	it('asks for the date of birth before it takes an age range', async () => {
		const token = await deferredAccount();
		setInjectedUsersServiceClient(acceptingClient(base, AttestedAgeFloors.ADULT, new Date()));
		await createBuilder(harness, token)
			.post('/users/@me/age-range')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.DATE_OF_BIRTH_REQUIRED)
			.execute();
		expect(await me(token)).toMatchObject({mature_content_unlocked: false, date_of_birth_required: true});
	});

	it('refuses a range under 18 and stores nothing', async () => {
		const account = await createTestAccount(harness);
		for (const floor of [AttestedAgeFloors.MINOR, AttestedAgeFloors.UNDER_MINIMUM]) {
			setInjectedUsersServiceClient(acceptingClient(base, floor, new Date()));
			await createBuilder(harness, account.token)
				.post('/users/@me/age-range')
				.body(SUBMISSION)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.AGE_RANGE_NOT_ADULT)
				.execute();
		}
		expect(await storedFloor(account.userId)).toBeNull();
	});

	it('refuses a range from outside the UK and Brazil before asking the users service', async () => {
		const account = await createTestAccount(harness);
		geo.countryCode = 'SE';
		const client = acceptingClient(base, AttestedAgeFloors.ADULT, new Date());
		const submitAge = vi.spyOn(client, 'appAttestSubmitAge');
		setInjectedUsersServiceClient(client);
		await createBuilder(harness, account.token)
			.post('/users/@me/age-range')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.AGE_RANGE_NOT_NEEDED)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/play-integrity/challenge')
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.AGE_RANGE_NOT_NEEDED)
			.execute();
		expect(submitAge).not.toHaveBeenCalled();
		expect(await storedFloor(account.userId)).toBeNull();
	});

	it('refuses every age range on a self-hosted instance and ignores one already stored', async () => {
		const token = await adultAccount();
		await submit(token, AttestedAgeFloors.ADULT, new Date());
		Config.instance.selfHosted = true;
		await createBuilder(harness, token)
			.post('/users/@me/age-range')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.FEATURE_NOT_AVAILABLE_SELF_HOSTED)
			.execute();
		await createBuilder(harness, token)
			.post('/users/@me/app-attest/challenge')
			.body({purpose: 'attest'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.FEATURE_NOT_AVAILABLE_SELF_HOSTED)
			.execute();
		expect(await me(token)).toMatchObject({mature_content_unlocked: false});
	});

	it('deletes the stored age range with the account', async () => {
		const account = await createTestAccount(harness);
		await submit(account.token, AttestedAgeFloors.ADULT, new Date());
		const userId = createUserID(BigInt(account.userId));
		const stored = await new UserRepository().findUniqueAssert(userId);
		expect(stored.attestedAgeFloor).toBe(AttestedAgeFloors.ADULT);
		expect(stored.ageAttestedAt).not.toBeNull();

		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);

		const deleted = await new UserRepository().findUniqueAssert(userId);
		expect(deleted.attestedAgeFloor).toBeNull();
		expect(deleted.ageAttestedAt).toBeNull();
	});

	async function removeAgeRange(token: string): Promise<Me> {
		return createBuilder<Me>(harness, token).delete('/users/@me/age-range').expect(HTTP_STATUS.OK).execute();
	}

	async function seedAppAttestKey(userId: string, keyId: string): Promise<void> {
		const id = createUserID(BigInt(userId));
		await upsertOne(
			AppAttestKeys.upsertAll({
				user_id: id,
				key_id: keyId,
				public_key: Buffer.from('key'),
				environment: 'production',
				app_id: 'TEAM.app.fluxer',
				counter: 1n,
				created_at: new Date(),
				last_used_at: null,
			}),
		);
		await upsertOne(AppAttestKeyLookup.upsertAll({key_id: keyId, user_id: id}));
	}

	it('removes a stored age range so mature content is locked again', async () => {
		const token = await adultAccount();
		await submit(token, AttestedAgeFloors.ADULT, new Date());
		const presence = vi.spyOn(getGatewayService(), 'dispatchPresence');
		expect(await removeAgeRange(token)).toMatchObject({
			has_age_range: false,
			mature_content_unlocked: false,
			nsfw_allowed: true,
		});
		expect(presence.mock.calls.map(([call]) => call.event)).toEqual(['USER_UPDATE']);
	});

	it('deletes the App Attest keys and the stored row fields', async () => {
		const account = await createTestAccount(harness);
		await submit(account.token, AttestedAgeFloors.ADULT, new Date());
		await seedAppAttestKey(account.userId, `${'B'.repeat(43)}=`);
		await removeAgeRange(account.token);

		const userId = createUserID(BigInt(account.userId));
		const stored = await new UserRepository().findUniqueAssert(userId);
		expect(stored.attestedAgeFloor).toBeNull();
		expect(stored.ageAttestedAt).toBeNull();
		const keys = await fetchMany(AppAttestKeys.selectCql({where: AppAttestKeys.where.eq('user_id')}), {
			user_id: userId,
		});
		expect(keys).toEqual([]);
		const lookup = await fetchMany(AppAttestKeyLookup.selectCql({where: AppAttestKeyLookup.where.eq('key_id')}), {
			key_id: `${'B'.repeat(43)}=`,
		});
		expect(lookup).toEqual([]);
	});

	it('deletes every App Attest key after an iOS submit, whatever the outcome', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const outcomes: Array<{floor: AttestedAgeFloor; country: string; status: number}> = [
			{floor: AttestedAgeFloors.ADULT, country: 'GB', status: HTTP_STATUS.OK},
			{floor: AttestedAgeFloors.MINOR, country: 'BR', status: HTTP_STATUS.FORBIDDEN},
			{floor: AttestedAgeFloors.ADULT, country: 'SE', status: HTTP_STATUS.FORBIDDEN},
		];
		for (const outcome of outcomes) {
			await seedAppAttestKey(account.userId, `${'C'.repeat(43)}=`);
			geo.countryCode = outcome.country;
			setInjectedUsersServiceClient(acceptingClient(base, outcome.floor, new Date()));
			await createBuilder(harness, account.token)
				.post('/users/@me/age-range')
				.body(SUBMISSION)
				.expect(outcome.status)
				.execute();
			const keys = await fetchMany(AppAttestKeys.selectCql({where: AppAttestKeys.where.eq('user_id')}), {
				user_id: userId,
			});
			expect(keys).toEqual([]);
		}
	});

	it('succeeds and sends no events when no age range is stored', async () => {
		const token = await adultAccount();
		const presence = vi.spyOn(getGatewayService(), 'dispatchPresence');
		expect(await removeAgeRange(token)).toMatchObject({has_age_range: false, nsfw_allowed: true});
		expect(presence).not.toHaveBeenCalled();
	});
});
