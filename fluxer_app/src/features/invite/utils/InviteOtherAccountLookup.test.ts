// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import {findInviteOnOtherAccount, inviteLookupCandidates} from '@app/features/invite/utils/InviteOtherAccountLookup';
import type {Account} from '@app/features/platform/state/AuthSession';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

const CURRENT_INSTANCE = 'https://fluxer.app/api';
const LOCAL_INSTANCE = 'https://local.example/api';
const OTHER_INSTANCE = 'https://other.example/api';

function instanceSnapshot(apiEndpoint: string): RuntimeConfigSnapshot {
	return {...FIXTURE_CURRENT_INSTANCE_SNAPSHOT, apiEndpoint, apiPublicEndpoint: apiEndpoint};
}

function account(instanceKey: string, userId: string, overrides: Partial<Account> = {}): Account {
	return {
		storageKey: `${instanceKey}::${userId}`,
		userId,
		token: `token.${userId}`,
		lastActive: FIXTURE_NOW,
		instance: instanceSnapshot(instanceKey),
		isValid: true,
		...overrides,
	};
}

const origin = {accountKey: `${CURRENT_INSTANCE}::1`, instanceKey: CURRENT_INSTANCE};

describe('inviteLookupCandidates', () => {
	it('skips the foreground account, its instance, invalid sessions and accounts without an instance', () => {
		const candidates = inviteLookupCandidates(
			[
				account(CURRENT_INSTANCE, '1'),
				account(CURRENT_INSTANCE, '2'),
				account(LOCAL_INSTANCE, '3', {isValid: false}),
				account(OTHER_INSTANCE, '4', {instance: undefined}),
				account(LOCAL_INSTANCE, '5'),
			],
			origin,
		);
		expect(candidates.map(({account: candidate}) => candidate.userId)).toEqual(['5']);
		expect(candidates[0].target.instanceKey).toBe(LOCAL_INSTANCE);
	});

	it('probes each instance once, through its most recently active account', () => {
		const candidates = inviteLookupCandidates(
			[
				account(LOCAL_INSTANCE, '3', {lastActive: FIXTURE_NOW - 10}),
				account(LOCAL_INSTANCE, '4', {lastActive: FIXTURE_NOW}),
				account(OTHER_INSTANCE, '5', {lastActive: FIXTURE_NOW - 5}),
			],
			origin,
		);
		expect(candidates.map(({account: candidate}) => candidate.userId)).toEqual(['4', '5']);
	});
});

describe('findInviteOnOtherAccount', () => {
	it('offers the first instance that knows the invite and ignores failures', async () => {
		const invite = {code: 'S1wHcsX8'} as Invite;
		const probed: Array<string> = [];
		const match = await findInviteOnOtherAccount(
			'S1wHcsX8',
			origin,
			[
				account(OTHER_INSTANCE, '5', {lastActive: FIXTURE_NOW}),
				account(LOCAL_INSTANCE, '3', {lastActive: FIXTURE_NOW - 10}),
			],
			async (code: string, target: InstanceHTTPTarget) => {
				probed.push(`${target.instanceKey}/${code}`);
				if (target.instanceKey === LOCAL_INSTANCE) return invite;
				throw new Error('Unknown invite');
			},
		);
		expect(probed).toEqual([`${OTHER_INSTANCE}/S1wHcsX8`, `${LOCAL_INSTANCE}/S1wHcsX8`]);
		expect(match?.account.userId).toBe('3');
		expect(match?.instanceKey).toBe(LOCAL_INSTANCE);
		expect(match?.invite).toBe(invite);
	});

	it('returns null when no other instance knows the invite', async () => {
		const match = await findInviteOnOtherAccount('missing', origin, [account(LOCAL_INSTANCE, '3')], async () => {
			throw new Error('Unknown invite');
		});
		expect(match).toBeNull();
	});
});
