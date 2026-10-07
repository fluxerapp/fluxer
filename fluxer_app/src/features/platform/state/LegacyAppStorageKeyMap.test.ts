// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CONTENT_STORAGE_KEYS,
	DEPLOYED_MANAGED_KEY_PREFIXES,
	GOLDEN_DEPLOYED_STORAGE_ENTRIES,
	GOLDEN_LOCAL_STORAGE_CORPUS,
	GOLDEN_UNKNOWN_STORAGE_ENTRIES,
	OWNED_CONTENT_STORAGE_KEYS,
	SHARED_CONTENT_STORAGE_KEYS,
} from '@app/features/platform/state/__fixtures__/LegacyStorageFixtures';
import {
	readPersistedStoreNames,
	STORES_WITHOUT_DEPLOYED_DATA,
} from '@app/features/platform/state/__fixtures__/PersistedStoreNames';
import {AppStorageKey, isGlobalAppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {
	isLegacyAccountSwappedKey,
	isNotMigratedLegacyKey,
	LEGACY_APP_STORAGE_KEY_MAP,
	type LegacyAppStorageKeyRow,
	NOT_MIGRATED_LEGACY_KEYS,
	resolveLegacyAppStorageKey,
} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {describe, expect, test} from 'vitest';

const GOLDEN_USER_ID = '100000000000000003';
const DEPLOYED_PER_ACCOUNT_KEY_PATTERN = new RegExp(`^(?:${DEPLOYED_MANAGED_KEY_PREFIXES.join('|')})`);
const SHARED_DESPITE_DEPLOYED_PER_ACCOUNT: ReadonlySet<string> = new Set([
	'fluxer.lastPushEndpoint',
	'fluxer:media_caps:v2',
	'fluxer:media:volume',
	'fluxer:media:muted',
	'fluxer:media:playbackRate',
	'fluxer:media_player:playback-rate',
]);

function rowLabel(row: LegacyAppStorageKeyRow): string {
	return row.kind === 'exact' ? row.key : row.prefix;
}

describe('legacy key map totality', () => {
	test('every deployed golden key is mapped or explicitly not migrated', () => {
		for (const entry of GOLDEN_DEPLOYED_STORAGE_ENTRIES) {
			const covered = resolveLegacyAppStorageKey(entry.key) !== null || isNotMigratedLegacyKey(entry.key);
			expect(covered, entry.key).toBe(true);
		}
	});

	test('a mapped key is never also on the not-migrated list', () => {
		for (const key of NOT_MIGRATED_LEGACY_KEYS) {
			expect(resolveLegacyAppStorageKey(key), key).toBeNull();
		}
	});

	test('unknown keys resolve to nothing so the migration can bucket them as unclassified', () => {
		for (const entry of GOLDEN_UNKNOWN_STORAGE_ENTRIES) {
			expect(resolveLegacyAppStorageKey(entry.key), entry.key).toBeNull();
			expect(isNotMigratedLegacyKey(entry.key), entry.key).toBe(false);
		}
	});

	test('no two rows claim the same key', () => {
		const seen = new Set<string>();
		for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
			const label = rowLabel(row);
			expect(seen.has(label), label).toBe(false);
			seen.add(label);
		}
	});

	test('no key is classified by both the map and the registry of keys this build introduced', () => {
		for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
			const probe = row.kind === 'exact' ? row.key : `${row.prefix}:probe`;
			expect(isGlobalAppStorageKey(probe), probe).toBe(false);
		}
	});

	test('every golden entry resolves to the scope and fan-out its destination names', () => {
		const expectations = {
			global: {scope: 'global', fanout: 'preference'},
			'every-account': {scope: 'account', fanout: 'preference'},
			'content-account': {scope: 'account', fanout: 'content'},
			'shared-content': {scope: 'account', fanout: 'content'},
			'named-account': {scope: 'account', fanout: 'preference'},
		} as const;
		for (const entry of GOLDEN_DEPLOYED_STORAGE_ENTRIES) {
			if (entry.destination === 'raw') {
				expect(isNotMigratedLegacyKey(entry.key), entry.key).toBe(true);
				continue;
			}
			const match = resolveLegacyAppStorageKey(entry.key);
			expect({scope: match?.row.scope, fanout: match?.row.fanout}, entry.key).toEqual(expectations[entry.destination]);
			expect(match?.userId != null, entry.key).toBe(entry.destination === 'named-account');
		}
	});
});

describe('persisted stores', () => {
	test('every makePersistent store is carried under the exact name the store reads and writes', () => {
		const names = readPersistedStoreNames();
		expect(names.length).toBeGreaterThan(25);
		for (const name of names) {
			if (STORES_WITHOUT_DEPLOYED_DATA.has(name)) {
				expect(resolveLegacyAppStorageKey(name), name).toBeNull();
				continue;
			}
			const match = resolveLegacyAppStorageKey(name);
			expect(match?.row.kind, name).toBe('exact');
			expect(
				GOLDEN_DEPLOYED_STORAGE_ENTRIES.some((entry) => entry.key === name),
				`${name} has no golden corpus entry`,
			).toBe(true);
		}
	});

	test('no store name is left out of the audit as a stale exemption', () => {
		const names = new Set(readPersistedStoreNames());
		for (const name of STORES_WITHOUT_DEPLOYED_DATA) {
			expect(names.has(name), name).toBe(true);
		}
	});
});

describe('fan-out split', () => {
	test('exactly the golden content keys carry the content fan-out', () => {
		const contentRows = LEGACY_APP_STORAGE_KEY_MAP.filter((row) => row.fanout === 'content');
		expect(CONTENT_STORAGE_KEYS).toHaveLength(22);
		for (const key of CONTENT_STORAGE_KEYS) {
			expect(resolveLegacyAppStorageKey(key)?.row.fanout, key).toBe('content');
		}
		for (const row of contentRows) {
			const label = rowLabel(row);
			expect(
				CONTENT_STORAGE_KEYS.some((key) => (row.kind === 'exact' ? key === label : key.startsWith(label))),
				label,
			).toBe(true);
		}
	});

	test('every content row is account scoped', () => {
		for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
			if (row.fanout === 'content') {
				expect(row.scope, rowLabel(row)).toBe('account');
			}
		}
	});

	test('a key the deployed build swapped per account never fans out to another account', () => {
		for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
			const label = rowLabel(row);
			if (!DEPLOYED_PER_ACCOUNT_KEY_PATTERN.test(label)) {
				continue;
			}
			if (SHARED_DESPITE_DEPLOYED_PER_ACCOUNT.has(label)) {
				expect(row.scope, label).toBe('global');
				continue;
			}
			expect({scope: row.scope, fanout: row.fanout}, label).toEqual({scope: 'account', fanout: 'content'});
		}
	});

	test('the per-account swap rule matches the deployed build for every corpus key', () => {
		for (const entry of GOLDEN_LOCAL_STORAGE_CORPUS) {
			expect(isLegacyAccountSwappedKey(entry.key), entry.key).toBe(DEPLOYED_PER_ACCOUNT_KEY_PATTERN.test(entry.key));
		}
	});

	test('content the deployed build kept in one map for every account is the shared content', () => {
		expect([...SHARED_CONTENT_STORAGE_KEYS].sort()).toEqual(
			[
				'ChannelFrecencyLocal',
				'ChannelFrecencyThreadGuilds',
				'Drafts',
				'GuildMatureContentAgreeLocal',
				'SelectedGuild',
				'SelectedChannel',
				'Inbox',
				'Location',
				'SearchHistory',
				'MessageEdit',
				'TrustedDomain',
				'member_list_default_hidden_channel_overrides',
				'ThreadPanelWidth',
			].sort(),
		);
		for (const key of SHARED_CONTENT_STORAGE_KEYS) {
			expect(isLegacyAccountSwappedKey(key), key).toBe(false);
		}
		for (const key of OWNED_CONTENT_STORAGE_KEYS) {
			expect(isLegacyAccountSwappedKey(key), key).toBe(true);
		}
	});

	test('the synced-preference wire, dirty and ack keys are not migrated at all', () => {
		for (const key of [
			'UserSettings:syncedPreferencesWire',
			'UserSettings:syncedPreferencesDirtyFields',
			'UserSettings:syncedPreferencesRecentAck',
		]) {
			expect(resolveLegacyAppStorageKey(key), key).toBeNull();
			expect(isNotMigratedLegacyKey(key), key).toBe(true);
		}
		expect(resolveLegacyAppStorageKey('UserSettings:syncedPreferencesLocal')?.row.scope).toBe('account');
	});
});

describe('never scoped keys', () => {
	test('token, userId, runtimeConfig and AccountManager never reach scoped storage', () => {
		for (const key of ['token', 'userId', 'runtimeConfig', 'AccountManager']) {
			expect(resolveLegacyAppStorageKey(key), key).toBeNull();
			expect(isNotMigratedLegacyKey(key), key).toBe(true);
		}
	});
});

describe('voice keys', () => {
	test('every voice store stays shared by all accounts, as the stores read it', () => {
		for (const key of [
			'VoiceSettings',
			'LocalVoiceState',
			'VoiceSessionRestore',
			'ParticipantVolume',
			'StreamAudioPrefs',
			'EntranceSoundListenerPrefs',
			'AudioVolume',
			'VideoVolume',
		]) {
			expect(resolveLegacyAppStorageKey(key)?.row.scope, key).toBe('global');
		}
	});
});

describe('scheduled maintenance dismissals', () => {
	test('both deployed forms belong to the account that dismissed them', () => {
		for (const key of [
			'fluxer_scheduled_maintenance_dismissed:9876543210',
			'fluxer_scheduled_maintenance_dismissed:9876543210:in_progress',
		]) {
			const match = resolveLegacyAppStorageKey(key);
			expect(match?.row.scope, key).toBe('account');
			expect(match?.row.fanout, key).toBe('content');
			expect(match?.userId, key).toBeNull();
		}
	});

	test('the bare prefix with no maintenance id resolves to nothing', () => {
		expect(resolveLegacyAppStorageKey('fluxer_scheduled_maintenance_dismissed:')).toBeNull();
	});
});

describe('accessibility keys', () => {
	test('the suffixed neko keys turn their user id into the scope', () => {
		for (const prefix of ['Accessibility:showNeko', 'Accessibility:keepNekoStill', 'Accessibility:pinNekoToTextarea']) {
			const match = resolveLegacyAppStorageKey(`${prefix}:${GOLDEN_USER_ID}`);
			expect(match?.row.kind, prefix).toBe('suffixed');
			expect(match?.userId, prefix).toBe(GOLDEN_USER_ID);
		}
	});

	test('the un-suffixed neko keys carry no user id and fan out', () => {
		const showNeko = resolveLegacyAppStorageKey('Accessibility:showNeko');
		expect(showNeko?.userId).toBeNull();
		expect(showNeko?.row.fanout).toBe('preference');
	});

	test('the legacy Accessibility blob is carried into every account scope', () => {
		const match = resolveLegacyAppStorageKey(AppStorageKey.ACCESSIBILITY_LEGACY_STORE);
		expect(match?.row.scope).toBe('account');
		expect(match?.row.fanout).toBe('preference');
	});
});
