// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HarvestDownloadUrlResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {expect} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import type {UserHarvest} from '../UserHarvestModel';
import {UserHarvestRepository} from '../UserHarvestRepository';

interface HarvestRequestResponse {
	harvest_id: string;
}

export async function requestHarvest(harness: ApiTestHarness, token: string): Promise<HarvestRequestResponse> {
	return createBuilder<HarvestRequestResponse>(harness, token).post('/users/@me/harvest').execute();
}

export async function fetchHarvestDownload(
	harness: ApiTestHarness,
	token: string,
	harvestId: string,
): Promise<HarvestDownloadUrlResponse> {
	return createBuilder<HarvestDownloadUrlResponse>(harness, token)
		.get(`/users/@me/harvest/${harvestId}/download`)
		.execute();
}

export async function expectHarvestDownloadFailsWithError(
	harness: ApiTestHarness,
	token: string,
	harvestId: string,
	expectedCode: string,
): Promise<void> {
	const {json} = await createBuilder<Record<string, unknown>>(harness, token)
		.get(`/users/@me/harvest/${harvestId}/download`)
		.expect(400)
		.executeWithResponse();
	const errorResponse = json as {
		code: string;
		message: string;
	};
	expect(errorResponse.code).toBe(expectedCode);
}

export async function markHarvestCompleted(userId: string, harvestId: string, expiresAt: Date): Promise<void> {
	const harvestRepository = new UserHarvestRepository();
	const userIdTyped = createUserID(BigInt(userId));
	const harvestIdTyped = BigInt(harvestId);
	await harvestRepository.markAsCompleted(userIdTyped, harvestIdTyped, `test/${harvestId}.zip`, 1024n, expiresAt);
}

export async function markHarvestFailed(userId: string, harvestId: string, errorMessage: string): Promise<void> {
	const harvestRepository = new UserHarvestRepository();
	await harvestRepository.markAsFailed(createUserID(BigInt(userId)), BigInt(harvestId), errorMessage);
}

export async function markHarvestStarted(userId: string, harvestId: string): Promise<void> {
	const harvestRepository = new UserHarvestRepository();
	await harvestRepository.markAsStarted(createUserID(BigInt(userId)), BigInt(harvestId));
}

export async function findHarvest(userId: string, harvestId: string): Promise<UserHarvest | null> {
	const harvestRepository = new UserHarvestRepository();
	return harvestRepository.findByUserAndHarvestId(createUserID(BigInt(userId)), BigInt(harvestId));
}
