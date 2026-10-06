// SPDX-License-Identifier: AGPL-3.0-or-later

import {createFriendship as channelCreateFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {RelationshipResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {expect} from 'vitest';

export async function sendFriendRequest(
	harness: ApiTestHarness,
	token: string,
	targetId: string,
	options: {
		staffForceAccept?: boolean;
	} = {},
): Promise<{
	response: Response;
	json: RelationshipResponse;
}> {
	const {response, json} = await createBuilder<RelationshipResponse>(harness, token)
		.post(`/users/@me/relationships/${targetId}`)
		.body(options.staffForceAccept ? {staff_force_accept: true} : {})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function sendFriendRequestByTag(
	harness: ApiTestHarness,
	token: string,
	username: string,
	discriminator: string,
): Promise<{
	response: Response;
	json: RelationshipResponse;
}> {
	const {response, json} = await createBuilder<RelationshipResponse>(harness, token)
		.post('/users/@me/relationships')
		.body({username, discriminator})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function acceptFriendRequest(
	harness: ApiTestHarness,
	token: string,
	targetId: string,
): Promise<{
	response: Response;
	json: RelationshipResponse;
}> {
	const {response, json} = await createBuilder<RelationshipResponse>(harness, token)
		.put(`/users/@me/relationships/${targetId}`)
		.body({})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function blockUser(
	harness: ApiTestHarness,
	token: string,
	targetId: string,
): Promise<{
	response: Response;
	json: RelationshipResponse;
}> {
	const {response, json} = await createBuilder<RelationshipResponse>(harness, token)
		.put(`/users/@me/relationships/${targetId}`)
		.body({type: RelationshipTypes.BLOCKED})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function removeRelationship(harness: ApiTestHarness, token: string, targetId: string): Promise<void> {
	await createBuilder<void>(harness, token).delete(`/users/@me/relationships/${targetId}`).expect(204).execute();
}

export async function listRelationships(
	harness: ApiTestHarness,
	token: string,
): Promise<{
	response: Response;
	json: Array<RelationshipResponse>;
}> {
	const {response, json} = await createBuilder<Array<RelationshipResponse>>(harness, token)
		.get('/users/@me/relationships')
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function updateFriendNickname(
	harness: ApiTestHarness,
	token: string,
	targetId: string,
	nickname: string | null,
): Promise<{
	response: Response;
	json: RelationshipResponse;
}> {
	const {response, json} = await createBuilder<RelationshipResponse>(harness, token)
		.patch(`/users/@me/relationships/${targetId}`)
		.body({nickname})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export function assertRelationshipType(relationship: RelationshipResponse, expectedType: number): void {
	expect(relationship.type).toBe(expectedType);
}

export function assertRelationshipId(relationship: RelationshipResponse, expectedId: string): void {
	expect(relationship.id).toBe(expectedId);
}

export function findRelationship(relations: Array<RelationshipResponse>, userId: string): RelationshipResponse | null {
	return relations.find((r) => r.id === userId) ?? null;
}

export const createFriendship = channelCreateFriendship;
