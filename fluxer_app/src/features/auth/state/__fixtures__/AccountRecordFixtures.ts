// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {StoredAccount} from '@app/features/auth/state/AccountStorage';
import {BOOTSTRAP_APP_PUBLIC} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';

const FIXTURE_CURRENT_API_ENDPOINT = 'https://fluxer.app/api';
export const FIXTURE_CURRENT_INSTANCE_KEY = 'https://fluxer.app/api';
const FIXTURE_PATHED_API_ENDPOINT = 'HTTPS://Self.Hosted.Example/fluxer/api/';
const FIXTURE_PATHED_INSTANCE_KEY = 'https://self.hosted.example/fluxer/api';
const FIXTURE_UNKEYABLE_API_ENDPOINT = 'https://operator:hunter2@fluxer.app/api?tenant=1';
export const FIXTURE_NOW = 1_755_000_000_000;

function runtimeConfigSnapshotFixture(apiEndpoint: string): RuntimeConfigSnapshot {
	return {
		apiEndpoint,
		apiPublicEndpoint: apiEndpoint,
		gatewayEndpoint: 'wss://gateway.fluxer.app',
		mediaEndpoint: 'https://media.fluxer.app',
		staticCdnEndpoint: 'https://cdn.fluxer.app',
		marketingEndpoint: 'https://fluxer.app',
		adminEndpoint: 'https://admin.fluxer.app',
		inviteEndpoint: 'https://flux.gg',
		giftEndpoint: 'https://flux.gift',
		webAppEndpoint: 'https://fluxer.app',
		uploadRelayEndpoint: 'https://upload.fluxer.app',
		gifProvider: 'tenor',
		gifProviderDisplayName: 'Tenor',
		gifAttributionRequired: true,
		apiCodeVersion: 9,
		features: {
			voice_enabled: true,
			stripe_enabled: false,
			self_hosted: false,
			presigned_attachment_uploads: true,
			emails_enabled: true,
			premium_enabled: false,
			stripe_serviceable: false,
			phone_verification_enabled: false,
		},
		sso: null,
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
			guild_create_access: true,
		},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		publicPushVapidKey: null,
		limits: {version: 1, traitDefinitions: [], rules: []},
		appPublic: BOOTSTRAP_APP_PUBLIC,
		agePolicy: null,
		domainMigration: null,
	};
}

export const FIXTURE_CURRENT_INSTANCE_SNAPSHOT: RuntimeConfigSnapshot =
	runtimeConfigSnapshotFixture(FIXTURE_CURRENT_API_ENDPOINT);

function storedAccountFixture(overrides: Partial<StoredAccount> & Pick<StoredAccount, 'userId'>): StoredAccount {
	return {
		token: `token.${overrides.userId}`,
		userData: {
			username: `user${overrides.userId}`,
			discriminator: '0001',
			globalName: null,
			email: null,
			avatar: null,
		},
		presenceIntent: null,
		localStorageData: {},
		managedStorageData: {},
		lastActive: FIXTURE_NOW - 1_000,
		instance: runtimeConfigSnapshotFixture(FIXTURE_CURRENT_API_ENDPOINT),
		...overrides,
	};
}

export const BULK_RECORD_CLASS_SIZE = 1025;

interface AccountRecordClass {
	readonly id: string;
	readonly description: string;
	readonly records: ReadonlyArray<StoredAccount>;
	readonly expectedStorageKeys: ReadonlyArray<string>;
}

const noInstanceRecord = storedAccountFixture({userId: '100000000000000001', instance: undefined});

const sameInstanceRecords = [
	storedAccountFixture({userId: '100000000000000002', lastActive: FIXTURE_NOW - 5_000}),
	storedAccountFixture({userId: '100000000000000003', lastActive: FIXTURE_NOW - 2_000}),
];

const pathedInstanceRecord = storedAccountFixture({
	userId: '100000000000000004',
	instance: runtimeConfigSnapshotFixture(FIXTURE_PATHED_API_ENDPOINT),
});

const corruptInstanceRecord = storedAccountFixture({
	userId: '100000000000000005',
	instance: runtimeConfigSnapshotFixture(FIXTURE_UNKEYABLE_API_ENDPOINT),
});

const nullTokenRecord = storedAccountFixture({userId: '100000000000000006', token: null, isValid: false});

const bulkRecords = Array.from({length: BULK_RECORD_CLASS_SIZE}, (_unused, index) =>
	storedAccountFixture({
		userId: `2${String(index).padStart(17, '0')}`,
		lastActive: FIXTURE_NOW - index,
	}),
);

export const FLUXER_ACCOUNTS_V2_RECORD_CLASSES: ReadonlyArray<AccountRecordClass> = [
	{
		id: 'no-instance',
		description: 'Unqualified active record recovered from exact credentials',
		records: [noInstanceRecord],
		expectedStorageKeys: [`${FIXTURE_CURRENT_INSTANCE_KEY}::${noInstanceRecord.userId}`],
	},
	{
		id: 'same-instance-pair',
		description: 'Class A, the overwhelming majority: two records on the current instance',
		records: sameInstanceRecords,
		expectedStorageKeys: sameInstanceRecords.map((record) => `${FIXTURE_CURRENT_INSTANCE_KEY}::${record.userId}`),
	},
	{
		id: 'api-endpoint-with-path',
		description: 'Class C: self-hosted instance whose apiEndpoint has a path, with case and trailing slash normalised',
		records: [pathedInstanceRecord],
		expectedStorageKeys: [`${FIXTURE_PATHED_INSTANCE_KEY}::${pathedInstanceRecord.userId}`],
	},
	{
		id: 'corrupt-instance',
		description: 'Unkeyable active record recovered from exact credentials',
		records: [corruptInstanceRecord],
		expectedStorageKeys: [`${FIXTURE_CURRENT_INSTANCE_KEY}::${corruptInstanceRecord.userId}`],
	},
	{
		id: 'null-token',
		description: 'Class A with a null token is listed, flagged invalid and never silently deleted',
		records: [nullTokenRecord],
		expectedStorageKeys: [`${FIXTURE_CURRENT_INSTANCE_KEY}::${nullTokenRecord.userId}`],
	},
	{
		id: 'bulk-1025',
		description: 'Class A at scale: 1025 records, above every 1024-sized batch boundary in the train',
		records: bulkRecords,
		expectedStorageKeys: bulkRecords.map((record) => `${FIXTURE_CURRENT_INSTANCE_KEY}::${record.userId}`),
	},
];

const FLUXER_ACCOUNTS_DB_NAME = 'FluxerAccounts';
export const FLUXER_ACCOUNTS_DB_VERSION = 2;
export const FLUXER_ACCOUNTS_STORE_NAME = 'accounts';
export const FLUXER_ACCOUNTS_KEY_PATH = 'userId';
export const FLUXER_ACCOUNTS_INDEX_NAME = 'lastActive';

export function openGoldenAccountsDatabase(factory: IDBFactory): Promise<IDBDatabase> {
	return new Promise<IDBDatabase>((resolve, reject) => {
		const request = factory.open(FLUXER_ACCOUNTS_DB_NAME, FLUXER_ACCOUNTS_DB_VERSION);
		request.onupgradeneeded = () => {
			request.result
				.createObjectStore(FLUXER_ACCOUNTS_STORE_NAME, {keyPath: FLUXER_ACCOUNTS_KEY_PATH})
				.createIndex(FLUXER_ACCOUNTS_INDEX_NAME, FLUXER_ACCOUNTS_INDEX_NAME);
		};
		request.onsuccess = () => resolve(request.result);
		request.onerror = () => reject(request.error ?? new Error('IndexedDB open failed'));
	});
}
