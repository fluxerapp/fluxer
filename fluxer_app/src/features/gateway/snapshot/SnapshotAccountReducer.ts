// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import type {
	SnapshotAuthSessionChangePayload,
	SnapshotFavoriteMemeDeletePayload,
	SnapshotUserConnectionsUpdatePayload,
	SnapshotUserNoteUpdatePayload,
	SnapshotUserSettingsUpdatePayload,
} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {
	accountMetadataFromReady,
	parseSnapshotSingleton,
	SNAPSHOT_SINGLETON_KEY,
	type SnapshotAccountMetadataRow,
	type SnapshotEmit,
	type SnapshotEntityRowMap,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import {AllLocales} from '@fluxer/constants/src/Locales';
import {TimeFormatTypes} from '@fluxer/constants/src/UserConstants';
import type {UserSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

type UserSettingsLocale = UserSettingsResponse['locale'];
type UserSettingsTimeFormat = UserSettingsResponse['time_format'];

const SUPPORTED_LOCALES = new Set<string>(AllLocales satisfies ReadonlyArray<UserSettingsLocale>);
const SUPPORTED_TIME_FORMATS = new Set<number>(
	Object.values(TimeFormatTypes) satisfies ReadonlyArray<UserSettingsTimeFormat>,
);

const isSupportedLocale = (value: string): value is UserSettingsLocale => SUPPORTED_LOCALES.has(value);
const isSupportedTimeFormat = (value: number): value is UserSettingsTimeFormat => SUPPORTED_TIME_FORMATS.has(value);

export interface SnapshotAccountState {
	readonly userSettings: SnapshotEntityRowMap['user_settings'] | null;
	readonly accountMetadata: SnapshotAccountMetadataRow | null;
}

export class SnapshotAccountReducer {
	private userSettings: SnapshotEntityRowMap['user_settings'] | null = null;
	private accountMetadata: SnapshotAccountMetadataRow | null = null;

	exportState(): SnapshotAccountState {
		return {
			userSettings: this.userSettings,
			accountMetadata: this.accountMetadata,
		};
	}

	importState(state: SnapshotAccountState): void {
		this.userSettings = state.userSettings;
		this.accountMetadata = state.accountMetadata;
	}

	load(entries: StateSnapshotEntries): void {
		this.userSettings = parseSnapshotSingleton(entries, 'user_settings');
		this.accountMetadata = parseSnapshotSingleton(entries, 'account_metadata');
	}

	initializeReady(data: ReadyPayload): void {
		this.userSettings = data.user_settings ? {...data.user_settings} : null;
		this.accountMetadata = accountMetadataFromReady(data);
	}

	emitReady(emit: SnapshotEmit): void {
		emit({
			kind: 'replaceEntity',
			entity: 'user_settings',
			entries: this.userSettings ? [{key: SNAPSHOT_SINGLETON_KEY, value: this.userSettings}] : [],
		});
		const accountMetadata = this.accountMetadata;
		if (accountMetadata == null) {
			throw new Error('Snapshot account metadata is missing during READY initialization');
		}
		emit({
			kind: 'replaceEntity',
			entity: 'account_metadata',
			entries: [{key: SNAPSHOT_SINGLETON_KEY, value: accountMetadata}],
		});
		emit({kind: 'replaceEntity', entity: 'user_connections', entries: []});
	}

	applyAuthSessionChange(emit: SnapshotEmit, data: SnapshotAuthSessionChangePayload): void {
		const authSessionIdHash = data.new_auth_session_id_hash;
		if (authSessionIdHash == null) {
			return;
		}
		if (typeof authSessionIdHash !== 'string') {
			throw new Error('AUTH_SESSION_CHANGE new_auth_session_id_hash must be a string or null');
		}
		if (authSessionIdHash.length === 0) {
			return;
		}
		const metadata = this.ensureAccountMetadata();
		metadata.authSessionIdHash = authSessionIdHash;
		this.emitAccountMetadata(emit);
	}

	applyUserSettingsUpdate(emit: SnapshotEmit, data: SnapshotUserSettingsUpdatePayload): void {
		const current = this.userSettings;
		if (current == null) {
			throw new Error('Snapshot user settings are unavailable before READY initialization');
		}
		const locale = isSupportedLocale(data.locale) ? data.locale : current.locale;
		const timeFormat = isSupportedTimeFormat(data.time_format) ? data.time_format : current.time_format;
		const next: SnapshotEntityRowMap['user_settings'] = {
			...current,
			status: data.status,
			theme: data.theme,
			time_format: timeFormat,
			locale,
			synced_preferences: data.synced_preferences ?? current.synced_preferences,
		};
		this.userSettings = next;
		emit({kind: 'upsert', entity: 'user_settings', key: SNAPSHOT_SINGLETON_KEY, value: next});
	}

	applyUserNoteUpdate(emit: SnapshotEmit, data: SnapshotUserNoteUpdatePayload): void {
		const metadata = this.ensureAccountMetadata();
		const note = data.note ?? '';
		if (note.length > 0) {
			metadata.notes = {...metadata.notes, [data.id]: note};
		} else {
			const remaining = {...metadata.notes};
			delete remaining[data.id];
			metadata.notes = remaining;
		}
		this.emitAccountMetadata(emit);
	}

	applyUserPinnedDmsUpdate(emit: SnapshotEmit, data: ReadonlyArray<string>): void {
		const metadata = this.ensureAccountMetadata();
		metadata.pinnedDmIds = [...data];
		this.emitAccountMetadata(emit);
	}

	applyUserConnectionsUpdate(emit: SnapshotEmit, data: SnapshotUserConnectionsUpdatePayload): void {
		emit({kind: 'upsert', entity: 'user_connections', key: SNAPSHOT_SINGLETON_KEY, value: [...data.connections]});
	}

	applyWebAuthnCredentialsUpdate(emit: SnapshotEmit, data: ReadonlyArray<WebAuthnCredential>): void {
		const metadata = this.ensureAccountMetadata();
		metadata.webAuthnCredentials = [...data];
		this.emitAccountMetadata(emit);
	}

	applyFavoriteMemeCreate(emit: SnapshotEmit, data: FavoriteMemeWire): void {
		const metadata = this.ensureAccountMetadata();
		if (metadata.favoriteMemes.some((meme) => meme.id === data.id)) {
			return;
		}
		metadata.favoriteMemes = [data, ...metadata.favoriteMemes];
		this.emitAccountMetadata(emit);
	}

	applyFavoriteMemeUpdate(emit: SnapshotEmit, data: FavoriteMemeWire): void {
		const metadata = this.ensureAccountMetadata();
		const index = metadata.favoriteMemes.findIndex((meme) => meme.id === data.id);
		if (index === -1) {
			return;
		}
		metadata.favoriteMemes = [
			...metadata.favoriteMemes.slice(0, index),
			data,
			...metadata.favoriteMemes.slice(index + 1),
		];
		this.emitAccountMetadata(emit);
	}

	applyFavoriteMemeDelete(emit: SnapshotEmit, data: SnapshotFavoriteMemeDeletePayload): void {
		const metadata = this.ensureAccountMetadata();
		const favoriteMemes = metadata.favoriteMemes.filter((meme) => meme.id !== data.id);
		if (favoriteMemes.length === metadata.favoriteMemes.length) {
			return;
		}
		metadata.favoriteMemes = favoriteMemes;
		this.emitAccountMetadata(emit);
	}

	private ensureAccountMetadata(): SnapshotAccountMetadataRow {
		if (this.accountMetadata == null) {
			throw new Error('Snapshot account metadata is unavailable before READY initialization');
		}
		return this.accountMetadata;
	}

	private emitAccountMetadata(emit: SnapshotEmit): void {
		emit({
			kind: 'upsert',
			entity: 'account_metadata',
			key: SNAPSHOT_SINGLETON_KEY,
			value: this.ensureAccountMetadata(),
		});
	}
}
