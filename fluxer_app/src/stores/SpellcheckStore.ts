/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, runInAction} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import {getElectronAPI, isElectron} from '~/utils/NativeUtils';

const STORAGE_KEY = 'SpellcheckStore';

class SpellcheckStore {
	enabled = true;
	languages: Array<string> = [];
	availableLanguages: Array<string> = [];
	electronDisposer: (() => void) | null = null;
	private normalizeLanguages = (langs: Array<string> = []): Array<string> => Array.from(new Set(langs.filter(Boolean)));

	constructor() {
		makeAutoObservable(this, {electronDisposer: false as const}, {autoBind: true});
		void this.initialize();
	}

	private async initialize(): Promise<void> {
		await makePersistent(this, STORAGE_KEY, ['enabled', 'languages']);

		if (!isElectron()) return;

		this.attachElectronListener();
		await this.refreshAvailableLanguages();
		await this.pushToElectron();
	}

	private attachElectronListener(): void {
		const api = getElectronAPI();
		if (!api || this.electronDisposer) return;

		this.electronDisposer = api.onSpellcheckStateChanged((state) => {
			runInAction(() => {
				this.enabled = state.enabled;
				this.languages = this.normalizeLanguages(state.languages ?? []);
			});
		});
	}

	async refreshAvailableLanguages(): Promise<void> {
		const api = getElectronAPI();
		if (!api) return;
		const langs = await api.spellcheckGetAvailableLanguages();
		runInAction(() => {
			this.availableLanguages = langs ?? [];
		});
	}

	async pushToElectron(): Promise<void> {
		const api = getElectronAPI();
		if (!api) return;
		const languages = this.normalizeLanguages(this.languages);
		const state = await api.spellcheckSetState({enabled: this.enabled, languages: [...languages]});
		runInAction(() => {
			this.enabled = state.enabled;
			this.languages = this.normalizeLanguages(state.languages ?? languages);
		});
	}

	setEnabled(enabled: boolean): void {
		this.enabled = enabled;
		void this.pushToElectron();
	}

	setLanguages(languages: Array<string>): void {
		this.languages = this.normalizeLanguages(languages);
		void this.pushToElectron();
	}
}

export default new SpellcheckStore();
