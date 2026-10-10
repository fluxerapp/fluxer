// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {
	type EntranceSoundScope,
	getEntranceSoundFallbackScopes,
	getEntranceSoundScopeId,
} from '@app/features/notification/utils/EntranceSoundScopes';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('EntranceSoundLibrary');

export interface EntranceSoundEntry {
	id: string;
	name: string;
	hash: string;
	extension: string;
	contentType: string;
	durationMs: number;
	sizeBytes: number;
	url: string;
	createdAt: string;
}

interface ApiSoundRow {
	id: string;
	name: string;
	hash: string;
	extension: string;
	content_type: string;
	duration_ms: number;
	size_bytes: number;
	url: string;
	created_at: string;
}

interface ApiSelectionRow {
	scope_id: string;
	sound_id: string;
}

interface ApiLibraryResponse {
	sounds: Array<ApiSoundRow>;
	selections: Array<ApiSelectionRow>;
}

function rowToEntry(row: ApiSoundRow): EntranceSoundEntry {
	return {
		id: row.id,
		name: row.name,
		hash: row.hash,
		extension: row.extension,
		contentType: row.content_type,
		durationMs: row.duration_ms,
		sizeBytes: row.size_bytes,
		url: row.url,
		createdAt: row.created_at,
	};
}

class EntranceSoundLibrary {
	sounds: Record<string, EntranceSoundEntry> = {};
	selections: Record<string, string> = {};
	loaded = false;
	loading = false;
	lastError: string | null = null;
	private loadPromise: Promise<void> | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get list(): Array<EntranceSoundEntry> {
		return Object.values(this.sounds).sort((a, b) => (b.createdAt > a.createdAt ? 1 : -1));
	}

	getById(soundId: string | null | undefined): EntranceSoundEntry | null {
		if (!soundId) return null;
		return this.sounds[soundId] ?? null;
	}

	getSelection(scope: EntranceSoundScope): string | null {
		return this.selections[getEntranceSoundScopeId(scope)] ?? null;
	}

	resolveForScope(scope: EntranceSoundScope): {sound: EntranceSoundEntry; scope: EntranceSoundScope} | null {
		for (const fallback of getEntranceSoundFallbackScopes(scope)) {
			const soundId = this.selections[getEntranceSoundScopeId(fallback)];
			if (!soundId) continue;
			const sound = this.sounds[soundId];
			if (sound) {
				return {sound, scope: fallback};
			}
		}
		return null;
	}

	async load(force = false): Promise<void> {
		if (this.loading && this.loadPromise) return this.loadPromise;
		if (this.loaded && !force) return;
		const request = (async () => {
			runInAction(() => {
				this.loading = true;
				this.lastError = null;
			});
			try {
				const response = await http.get<ApiLibraryResponse>(Endpoints.USER_ENTRANCE_SOUNDS);
				const data = response.body;
				runInAction(() => {
					this.sounds = {};
					for (const row of data.sounds) {
						this.sounds[row.id] = rowToEntry(row);
					}
					this.selections = {};
					for (const selection of data.selections) {
						this.selections[selection.scope_id] = selection.sound_id;
					}
					this.loaded = true;
				});
			} catch (error) {
				logger.warn('Failed to load entrance sound library', {error});
				runInAction(() => {
					this.lastError = error instanceof Error ? error.message : 'unknown';
				});
			} finally {
				runInAction(() => {
					this.loading = false;
					this.loadPromise = null;
				});
			}
		})();
		this.loadPromise = request;
		return request;
	}

	async uploadSound(params: {name: string; base64Audio: string}): Promise<EntranceSoundEntry> {
		const response = await http.post<ApiSoundRow>(Endpoints.USER_ENTRANCE_SOUNDS, {
			body: {name: params.name, audio: params.base64Audio},
		});
		const entry = rowToEntry(response.body);
		runInAction(() => {
			this.sounds = {...this.sounds, [entry.id]: entry};
		});
		return entry;
	}

	async renameSound(soundId: string, name: string): Promise<EntranceSoundEntry> {
		const response = await http.patch<ApiSoundRow>(Endpoints.USER_ENTRANCE_SOUND(soundId), {body: {name}});
		const entry = rowToEntry(response.body);
		runInAction(() => {
			this.sounds = {...this.sounds, [entry.id]: entry};
		});
		return entry;
	}

	async deleteSound(soundId: string): Promise<void> {
		await http.delete(Endpoints.USER_ENTRANCE_SOUND(soundId));
		runInAction(() => {
			const nextSounds = {...this.sounds};
			delete nextSounds[soundId];
			this.sounds = nextSounds;
			const nextSelections: Record<string, string> = {};
			for (const [scopeId, id] of Object.entries(this.selections)) {
				if (id !== soundId) nextSelections[scopeId] = id;
			}
			this.selections = nextSelections;
		});
	}

	async setSelection(scope: EntranceSoundScope, soundId: string | null): Promise<void> {
		const scopeId = getEntranceSoundScopeId(scope);
		await http.put(Endpoints.USER_ENTRANCE_SOUND_SELECTIONS, {body: {scope_id: scopeId, sound_id: soundId}});
		runInAction(() => {
			const next = {...this.selections};
			if (soundId === null) {
				delete next[scopeId];
			} else {
				next[scopeId] = soundId;
			}
			this.selections = next;
		});
	}
}

export default new EntranceSoundLibrary();
