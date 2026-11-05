// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AppAttestAgeAccepted,
	type AppAttestOutcome,
	createUsersServiceClient,
	type IUsersServiceClient,
	setInjectedUsersServiceClient,
} from '@app/api/infrastructure/UsersServiceClient';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {type AttestedAgeFloor, AttestedAgeFloors} from '@fluxer/constants/src/AgeAssuranceConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
	};
}

describe('Age assurance submit', () => {
	let harness: ApiTestHarness;
	let base: IUsersServiceClient;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		base = createUsersServiceClient();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		setInjectedUsersServiceClient(base);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function adultAccount(): Promise<string> {
		const {token} = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/register')
			.body({username: 'attested', consent: true})
			.execute();
		await createBuilder(harness, token)
			.put('/users/@me/date-of-birth')
			.body({date_of_birth: yearsAgo(30)})
			.execute();
		return token;
	}

	async function submit(token: string, floor: AttestedAgeFloor, attestedAt: Date): Promise<void> {
		setInjectedUsersServiceClient(acceptingClient(base, floor, attestedAt));
		const result = await createBuilder<{age_floor: string}>(harness, token)
			.post('/users/@me/age-assurance')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.age_floor).toBe(floor);
	}

	async function nsfwAllowed(token: string): Promise<boolean> {
		return (await createBuilder<{nsfw_allowed: boolean}>(harness, token).get('/users/@me').execute()).nsfw_allowed;
	}

	it('responds not available on an instance without App Attest', async () => {
		const token = await adultAccount();
		await createBuilder(harness, token)
			.post('/users/@me/app-attest/challenge')
			.body({purpose: 'attest'})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.APP_ATTEST_NOT_AVAILABLE)
			.execute();
		await createBuilder(harness, token)
			.post('/users/@me/age-assurance')
			.body(SUBMISSION)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.APP_ATTEST_NOT_AVAILABLE)
			.execute();
		expect(await nsfwAllowed(token)).toBe(true);
	});

	it('lets an attested minor floor win over an adult date of birth and never the other way', async () => {
		const token = await adultAccount();
		expect(await nsfwAllowed(token)).toBe(true);

		await submit(token, AttestedAgeFloors.MINOR, new Date());
		expect(await nsfwAllowed(token)).toBe(false);
		const settings = await createBuilder<{sensitive_content_friend_dm_filter: number}>(harness, token)
			.get('/users/@me/settings')
			.execute();
		expect(settings.sensitive_content_friend_dm_filter).not.toBe(SensitiveMediaFilterLevel.SHOW);

		await submit(token, AttestedAgeFloors.ADULT, new Date(Date.now() - 60_000));
		expect(await nsfwAllowed(token)).toBe(false);

		await submit(token, AttestedAgeFloors.ADULT, new Date(Date.now() + 1000));
		expect(await nsfwAllowed(token)).toBe(true);
	});
});
