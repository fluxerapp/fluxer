// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {UserHarvest} from '@app/api/user/UserHarvestModel';
import {UserHarvestRepository} from '@app/api/user/UserHarvestRepository';
import type {HarvestDownloadUrlResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {expect} from 'vitest';

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
	const harvest = await claimHarvest(harvestRepository, userId, harvestId);
	await harvestRepository.markAsCompleted(harvest, `test/${harvestId}.zip`, 1024n, expiresAt);
}

export async function markHarvestFailed(userId: string, harvestId: string, errorMessage: string): Promise<void> {
	const harvestRepository = new UserHarvestRepository();
	const harvest = await claimHarvest(harvestRepository, userId, harvestId);
	await harvestRepository.markAsFailed(harvest, errorMessage);
}

export async function markHarvestStarted(userId: string, harvestId: string): Promise<void> {
	const harvestRepository = new UserHarvestRepository();
	await claimHarvest(harvestRepository, userId, harvestId);
}

async function claimHarvest(
	repository: UserHarvestRepository,
	userId: string,
	harvestId: string,
): Promise<UserHarvest> {
	const harvest = await repository.findByUserAndHarvestId(createUserID(BigInt(userId)), BigInt(harvestId));
	if (!harvest) throw new Error(`Harvest ${harvestId} for user ${userId} not found`);
	return repository.markAsStarted(harvest);
}

export async function findHarvest(userId: string, harvestId: string): Promise<UserHarvest | null> {
	const harvestRepository = new UserHarvestRepository();
	return harvestRepository.findByUserAndHarvestId(createUserID(BigInt(userId)), BigInt(harvestId));
}
