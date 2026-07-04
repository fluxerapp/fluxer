// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test, vi} from 'vitest';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {createUserID} from '../../BrandedTypes';
import type {User} from '../../models/User';
import {mapUserToAdminResponse} from './UserTypes';

function createStubUser(): User {
	return {
		id: createUserID(123n),
		username: 'tester',
		discriminator: 42,
		globalName: null,
		isBot: false,
		isSystem: false,
		flags: 0n,
		premiumFlags: 0,
		avatarHash: null,
		bannerHash: null,
		bio: null,
		pronouns: null,
		accentColor: null,
		email: null,
		emailVerified: false,
		emailBounced: false,
		hasVerifiedPhone: false,
		dateOfBirth: null,
		locale: 'en-US',
		premiumType: null,
		premiumSince: null,
		premiumUntil: null,
		premiumGraceEndsAt: null,
		premiumLifetimeSequence: null,
		suspiciousActivityFlags: 0,
		tempBannedUntil: null,
		pendingDeletionAt: null,
		pendingBulkMessageDeletionAt: null,
		deletionReasonCode: null,
		deletionPublicReason: null,
		acls: new Set<string>(),
		traits: new Set<string>(),
		totpSecret: null,
		authenticatorTypes: new Set<number>(),
		lastActiveAt: null,
		lastActiveIp: null,
	} as unknown as User;
}

describe('mapUserToAdminResponse', () => {
	test('includes the current activity payloads from the gateway service', async () => {
		const response = await mapUserToAdminResponse(
			createStubUser(),
			undefined,
			undefined,
				{
					getCurrentActivities: async () => [
						{name: 'Test Activity', type: 0, details: 'Queueing'},
						{name: 'Tauon', type: 2, details: 'Track'},
					],
				} as never,
			);

		expect(response.activities).toEqual([
			{name: 'Test Activity', type: 0, details: 'Queueing'},
			{name: 'Tauon', type: 2, details: 'Track'},
		]);
	});

	test('falls back to an empty list when the gateway has no visible activity', async () => {
		const response = await mapUserToAdminResponse(
			createStubUser(),
			undefined,
			undefined,
			{
				getCurrentActivities: async () => [],
			} as never,
		);

		expect(response.activities).toEqual([]);
	});

	test.each([new GatewayTimeoutError(), new BadGatewayError(), new ServiceUnavailableError()])(
		'falls back to an empty list when gateway activity lookup fails with %s',
		async (error) => {
			const response = await mapUserToAdminResponse(
				createStubUser(),
				undefined,
				undefined,
				{
					getCurrentActivities: async () => {
						throw error;
					},
				} as never,
			);

			expect(response.activities).toEqual([]);
		},
	);

	test('rethrows unexpected gateway errors', async () => {
		await expect(
			mapUserToAdminResponse(
				createStubUser(),
				undefined,
				undefined,
				{
					getCurrentActivities: async () => {
						throw new Error('boom');
					},
				} as never,
			),
		).rejects.toThrow('boom');
	});

	test('skips gateway activity lookups when activities are disabled', async () => {
		const getCurrentActivities = vi.fn(async () => [{name: 'Test Activity', type: 0, details: 'Queueing'}]);

		const response = await mapUserToAdminResponse(
			createStubUser(),
			undefined,
			undefined,
			{
				getCurrentActivities,
			} as never,
			{includeActivities: false},
		);

		expect(response.activities).toEqual([]);
		expect(getCurrentActivities).not.toHaveBeenCalled();
	});
});
