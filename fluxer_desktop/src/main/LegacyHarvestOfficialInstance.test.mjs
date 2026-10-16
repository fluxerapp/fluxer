// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {rekeyHarvestToOfficialInstance} = await import('./LegacyHarvestOfficialInstance.ts');

const USER_ID = '100000000000000002';
const OTHER_USER_ID = '100000000000000003';

function harvestFrom(origin, {localStorage = {}, stores = []} = {}) {
	return {
		version: 1,
		origin,
		capturedAt: 1_700_000_000_000,
		localStorage,
		stores,
		mediaDevices: [],
		serviceWorkersUnregistered: 0,
		cachesDeleted: 0,
		truncated: [],
	};
}

function legacyAccount(origin, userId) {
	return {
		userId,
		token: `token-${userId}`,
		localStorageData: {},
		lastActive: 1,
		instance: {
			apiEndpoint: `${origin}/api`,
			webAppEndpoint: origin,
			gatewayEndpoint: 'wss://gateway.fluxer.app',
			mediaEndpoint: 'https://fluxerusercontent.com',
		},
	};
}

function accountStore(records) {
	return {database: 'FluxerAccounts', version: 2, store: 'accounts', records};
}

function appStorageRow(scope, key, value) {
	return {key: null, value: {id: JSON.stringify([scope, key]), scope, key, value, updatedAt: {wall: 1, seq: 1}}};
}

describe('a harvest taken from the migrated official domain', () => {
	for (const [migrated, official] of [
		['https://fluxer.com', 'https://web.fluxer.app'],
		['https://canary.fluxer.com', 'https://web.canary.fluxer.app'],
	]) {
		test(`an old client account stored on ${migrated} is staged on the official instance`, () => {
			const harvest = harvestFrom(migrated, {
				localStorage: {token: 'live', userId: USER_ID},
				stores: [
					accountStore([
						{key: null, value: legacyAccount(migrated, USER_ID)},
						{key: null, value: legacyAccount(migrated, OTHER_USER_ID)},
					]),
				],
			});

			const staged = rekeyHarvestToOfficialInstance(harvest);

			assert.equal(staged.origin, migrated);
			assert.deepEqual(staged.localStorage, {token: 'live', userId: USER_ID});
			for (const record of staged.stores[0].records) {
				assert.equal(record.value.instance.apiEndpoint, `${official}/api`);
				assert.equal(record.value.instance.webAppEndpoint, official);
				assert.equal(record.value.instance.gatewayEndpoint, 'wss://gateway.fluxer.app');
				assert.equal(record.value.instance.mediaEndpoint, 'https://fluxerusercontent.com');
			}
			assert.equal(staged.stores[0].records[0].value.token, `token-${USER_ID}`);
			assert.equal(harvest.stores[0].records[0].value.instance.apiEndpoint, `${migrated}/api`);
		});

		test(`account-scoped state written on ${migrated} follows the account to the official instance`, () => {
			const migratedKey = `${migrated}/api::${USER_ID}`;
			const officialKey = `${official}/api::${USER_ID}`;
			const harvest = harvestFrom(migrated, {
				localStorage: {
					'fluxer:auth:active-account-key': migratedKey,
					[`fluxer:app-storage-mirror:${migratedKey}::Drafts`]: '{"drafts":{"1":"hello"}}',
					theme: 'dark',
				},
				stores: [
					accountStore([{key: null, value: {...legacyAccount(migrated, USER_ID), storageKey: migratedKey}}]),
					{
						database: 'fluxer-app-storage',
						version: 1,
						store: 'entries',
						records: [
							appStorageRow(migratedKey, 'Drafts', '{"drafts":{"1":"hello"}}'),
							appStorageRow('global', 'fluxer:auth:active-account-key', migratedKey),
							appStorageRow('global', 'BackgroundAccountPresence', JSON.stringify({[migratedKey]: {status: 'idle'}})),
							appStorageRow('global', 'Theme', '{"theme":"dark"}'),
						],
					},
				],
			});

			const staged = rekeyHarvestToOfficialInstance(harvest);

			assert.deepEqual(staged.localStorage, {
				'fluxer:auth:active-account-key': officialKey,
				[`fluxer:app-storage-mirror:${officialKey}::Drafts`]: '{"drafts":{"1":"hello"}}',
				theme: 'dark',
			});
			const [accounts, appStorage] = staged.stores;
			assert.equal(accounts.records[0].value.storageKey, officialKey);
			assert.equal(accounts.records[0].value.instance.apiEndpoint, `${official}/api`);
			assert.deepEqual(
				appStorage.records.map((record) => record.value),
				[
					appStorageRow(officialKey, 'Drafts', '{"drafts":{"1":"hello"}}').value,
					appStorageRow('global', 'fluxer:auth:active-account-key', officialKey).value,
					appStorageRow('global', 'BackgroundAccountPresence', JSON.stringify({[officialKey]: {status: 'idle'}})).value,
					appStorageRow('global', 'Theme', '{"theme":"dark"}').value,
				],
			);
			assert.equal(JSON.stringify(staged).includes(`${migrated}/api::`), false);
		});
	}

	test('values that are not plain data survive the rekey untouched', () => {
		const savedAt = new Date(5);
		const bytes = new Uint8Array([1, 2, 3]);
		const harvest = harvestFrom('https://canary.fluxer.com', {
			stores: [
				{
					database: 'FluxerCustomSounds',
					version: 2,
					store: 'customSounds',
					records: [{key: 'message', value: {soundType: 'message', savedAt, bytes, blob: {__blobRef: {blobId: 'a'}}}}],
				},
			],
		});

		const [record] = rekeyHarvestToOfficialInstance(harvest).stores[0].records;

		assert.equal(record.key, 'message');
		assert.equal(record.value.savedAt, savedAt);
		assert.equal(record.value.bytes, bytes);
		assert.deepEqual(record.value.blob, {__blobRef: {blobId: 'a'}});
	});

	test('an account on another instance keeps its own identity', () => {
		const selfHosted = legacyAccount('https://chat.example.test', OTHER_USER_ID);
		const harvest = harvestFrom('https://canary.fluxer.com', {
			localStorage: {[`fluxer:app-storage-mirror:https://chat.example.test/api::${OTHER_USER_ID}::Drafts`]: '{}'},
			stores: [accountStore([{key: null, value: selfHosted}])],
		});

		const staged = rekeyHarvestToOfficialInstance(harvest);

		assert.deepEqual(staged.localStorage, harvest.localStorage);
		assert.deepEqual(staged.stores[0].records[0].value, selfHosted);
	});
});

describe('a harvest taken from the legacy official web origin', () => {
	for (const origin of ['https://web.fluxer.app', 'https://web.canary.fluxer.app']) {
		test(`${origin} already is the official instance and is staged as harvested`, () => {
			const harvest = harvestFrom(origin, {
				localStorage: {'fluxer:auth:active-account-key': `${origin}/api::${USER_ID}`},
				stores: [accountStore([{key: null, value: legacyAccount(origin, USER_ID)}])],
			});

			assert.equal(rekeyHarvestToOfficialInstance(harvest), harvest);
		});
	}
});
