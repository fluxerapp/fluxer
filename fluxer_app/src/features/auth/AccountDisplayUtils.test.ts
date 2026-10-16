// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {FIXTURE_CURRENT_INSTANCE_SNAPSHOT} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import {installHarnessBootstrap} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import type {Account} from '@app/features/platform/state/AuthSession';
import {describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => null}}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {getMember: () => null}}));
vi.mock('@app/features/navigation/state/SelectedGuild', () => ({default: {selectedGuildId: null}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {getRelationship: () => null}}));

installHarnessBootstrap();

const {runtimeInstanceKey} = await import('@app/features/app/state/InstanceSnapshotStore');

const {
	getAccountDisplayLabels,
	resolveAccountInstanceDomain,
	resolveAccountInstanceLabel,
	resolveSnapshotInstanceDomain,
	resolveSnapshotInstanceKey,
} = await import('@app/features/auth/AccountDisplayUtils');

const SNAPSHOT = FIXTURE_CURRENT_INSTANCE_SNAPSHOT;

function accountWith(instance: Account['instance']): Account {
	return {
		storageKey: 'https://fluxer.app/api::100000000000000001',
		userId: '100000000000000001',
		token: 'token',
		lastActive: 0,
		isValid: true,
		instance,
	};
}

const UNUSABLE_SNAPSHOTS: ReadonlyArray<readonly [string, Account['instance']]> = [
	['instance undefined', undefined],
	['blank endpoints', {...SNAPSHOT, apiEndpoint: '', webAppEndpoint: ''}],
	['blank webAppEndpoint', {...SNAPSHOT, webAppEndpoint: ''}],
	['unparsable endpoints', {...SNAPSHOT, apiEndpoint: 'not a url', webAppEndpoint: 'not a url'}],
	['userinfo and query in apiEndpoint', {...SNAPSHOT, apiEndpoint: 'https://o:p@fluxer.app/api?tenant=1'}],
	['endpoints missing entirely', {} as unknown as NonNullable<Account['instance']>],
];

describe('display helpers are total', () => {
	for (const [label, instance] of UNUSABLE_SNAPSHOTS) {
		test(`${label} never throws and never white-screens a tile`, () => {
			const account = accountWith(instance);
			expect(() => resolveSnapshotInstanceKey(instance)).not.toThrow();
			expect(() => resolveSnapshotInstanceDomain(instance)).not.toThrow();
			expect(() => resolveAccountInstanceDomain(account)).not.toThrow();
			expect(() => resolveAccountInstanceLabel(account)).not.toThrow();
			expect(() => getAccountDisplayLabels(account)).not.toThrow();
		});
	}

	test('instance undefined resolves to null everywhere', () => {
		expect(resolveSnapshotInstanceKey(undefined)).toBeNull();
		expect(resolveSnapshotInstanceDomain(undefined)).toBeNull();
		expect(resolveAccountInstanceDomain(accountWith(undefined))).toBeNull();
	});

	test('a blank webAppEndpoint resolves to a null domain', () => {
		expect(resolveSnapshotInstanceDomain({...SNAPSHOT, webAppEndpoint: ''})).toBeNull();
		expect(resolveAccountInstanceDomain(accountWith({...SNAPSHOT, webAppEndpoint: ''}))).toBeNull();
	});

	test('an unparsable endpoint resolves to null', () => {
		expect(resolveSnapshotInstanceKey({...SNAPSHOT, apiEndpoint: 'not a url'})).toBeNull();
		expect(resolveSnapshotInstanceDomain({...SNAPSHOT, webAppEndpoint: 'not a url'})).toBeNull();
	});
});

describe('instance key derivation', () => {
	test('uses the one exported runtimeInstanceKey', () => {
		expect(resolveSnapshotInstanceKey(SNAPSHOT)).toBe(runtimeInstanceKey(SNAPSHOT));
	});

	test('a trailing slash and mixed case never orphan an account', () => {
		const pathed = {...SNAPSHOT, apiEndpoint: 'HTTPS://Self.Hosted.Example/fluxer/api/'};
		expect(resolveSnapshotInstanceKey(pathed)).toBe('https://self.hosted.example/fluxer/api');
		expect(resolveSnapshotInstanceKey(pathed)).toBe(runtimeInstanceKey(pathed));
	});
});
