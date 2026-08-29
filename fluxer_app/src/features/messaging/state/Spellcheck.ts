// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	SpellcheckBundledDictionary,
	SpellcheckEngine,
	SpellcheckResolvedEngineInfo,
} from '@app/features/platform/types/Electron';
import {getElectronAPI, isElectron} from '@app/features/ui/utils/NativeUtils';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {SpellcheckSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable, runInAction} from 'mobx';

function stringArrayEquals(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	if (a === b) return true;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		if (a[i] !== b[i]) return false;
	}
	return true;
}

class Spellcheck {
	enabled = true;
	engine: SpellcheckEngine = 'auto';
	autoDetect = true;
	languages: Array<string> = [];
	personalDictionary: Array<string> = [];
	availableLanguages: Array<string> = [];
	bundledDictionaries: Array<SpellcheckBundledDictionary> = [];
	resolvedEngine: SpellcheckResolvedEngineInfo | null = null;
	private providerActivationEngine: SpellcheckEngine | null = null;
	electronDisposers: Array<() => void> = [];

	constructor() {
		makeAutoObservable(this, {electronDisposers: false as const}, {autoBind: true});
		void this.initialize();
	}

	private normalizeLanguages(langs: Array<string> = []): Array<string> {
		return Array.from(new Set(langs.filter((l) => typeof l === 'string' && l.length > 0)));
	}

	private normalizePersonal(words: Array<string> = []): Array<string> {
		const seen = new Set<string>();
		const out: Array<string> = [];
		for (const raw of words) {
			if (typeof raw !== 'string') continue;
			const w = raw.trim();
			if (w.length === 0 || seen.has(w)) continue;
			seen.add(w);
			out.push(w);
		}
		return out;
	}

	get reloadRequired(): boolean {
		if (!this.providerActivationEngine) return false;
		const isLinuxDesktop = getElectronAPI()?.platform === 'linux';
		const wasHunspell = this.providerActivationEngine === 'hunspell';
		const isHunspell = isLinuxDesktop ? true : this.engine === 'hunspell';
		return wasHunspell !== isHunspell;
	}

	private async initialize(): Promise<void> {
		await makeSyncedField(this, {
			field: 'spellcheck',
			schema: SpellcheckSettingsSchema,
			persist: ['enabled', 'engine', 'autoDetect', 'languages', 'personalDictionary'],
			toMessage: (s) => ({
				enabled: s.enabled,
				engine: s.engine,
				autoDetect: s.autoDetect,
				languages: [...s.languages],
				personalDictionary: [...s.personalDictionary],
			}),
			applyMessage: (s, m) => {
				if (m.enabled !== undefined) s.enabled = m.enabled;
				if (m.engine === 'hunspell' || m.engine === 'system' || m.engine === 'auto') s.engine = m.engine;
				if (m.autoDetect !== undefined) s.autoDetect = m.autoDetect;
				if (m.languages) s.languages = s.normalizeLanguages([...m.languages]);
				if (m.personalDictionary) s.personalDictionary = s.normalizePersonal([...m.personalDictionary]);
				void s.pushToElectron();
			},
		});
		if (!isElectron()) return;
		this.attachElectronListeners();
		await Promise.all([this.refreshAvailableLanguages(), this.refreshBundledDictionaries()]);
		await this.pushToElectron();
	}

	private attachElectronListeners(): void {
		const api = getElectronAPI();
		if (!api) return;
		this.electronDisposers.push(
			api.onSpellcheckStateChanged((state) => {
				runInAction(() => {
					if (state.enabled !== undefined) this.enabled = state.enabled;
					if (state.engine === 'hunspell' || state.engine === 'system' || state.engine === 'auto') {
						this.engine = state.engine;
					}
					if (state.autoDetect !== undefined) this.autoDetect = state.autoDetect;
					if (state.languages) this.languages = this.normalizeLanguages(state.languages);
					if (state.personalDictionary) {
						this.personalDictionary = this.normalizePersonal(state.personalDictionary);
					}
				});
			}),
		);
		if (api.onSpellcheckEngineResolved) {
			this.electronDisposers.push(
				api.onSpellcheckEngineResolved((info) => {
					runInAction(() => {
						this.resolvedEngine = info;
						if (info.mode !== 'off' && this.providerActivationEngine === null) {
							this.providerActivationEngine = info.mode === 'hunspell' ? 'hunspell' : 'system';
						}
					});
				}),
			);
		}
	}

	async refreshAvailableLanguages(): Promise<void> {
		const api = getElectronAPI();
		if (!api) return;
		const langs = await api.spellcheckGetAvailableLanguages();
		runInAction(() => {
			this.availableLanguages = langs ?? [];
		});
	}

	async refreshBundledDictionaries(): Promise<void> {
		const api = getElectronAPI();
		if (!api?.spellcheckGetBundledDictionaries) return;
		const list = await api.spellcheckGetBundledDictionaries();
		runInAction(() => {
			this.bundledDictionaries = list ?? [];
		});
	}

	async pushToElectron(): Promise<void> {
		const api = getElectronAPI();
		if (!api) return;
		const state = await api.spellcheckSetState({
			enabled: this.enabled,
			engine: this.engine,
			autoDetect: this.autoDetect,
			languages: [...this.languages],
			personalDictionary: [...this.personalDictionary],
		});
		runInAction(() => {
			if (state.enabled !== undefined && state.enabled !== this.enabled) {
				this.enabled = state.enabled;
			}
			if (
				(state.engine === 'hunspell' || state.engine === 'system' || state.engine === 'auto') &&
				state.engine !== this.engine
			) {
				this.engine = state.engine;
			}
			if (state.autoDetect !== undefined && state.autoDetect !== this.autoDetect) {
				this.autoDetect = state.autoDetect;
			}
			if (state.languages) {
				const next = this.normalizeLanguages(state.languages);
				if (!stringArrayEquals(next, this.languages)) {
					this.languages = next;
				}
			}
			if (state.personalDictionary) {
				const next = this.normalizePersonal(state.personalDictionary);
				if (!stringArrayEquals(next, this.personalDictionary)) {
					this.personalDictionary = next;
				}
			}
		});
	}

	setEnabled(enabled: boolean): void {
		this.enabled = enabled;
		void this.pushToElectron();
	}

	setEngine(engine: SpellcheckEngine): void {
		this.engine = engine;
		void this.pushToElectron();
	}

	setAutoDetect(value: boolean): void {
		this.autoDetect = value;
		void this.pushToElectron();
	}

	setLanguages(languages: Array<string>): void {
		this.languages = this.normalizeLanguages(languages);
		void this.pushToElectron();
	}

	toggleLanguage(tag: string): void {
		const lower = tag.toLowerCase();
		const existing = this.languages.findIndex((l) => l.toLowerCase() === lower);
		if (existing >= 0) {
			this.languages = this.languages.filter((_, i) => i !== existing);
		} else {
			this.languages = [...this.languages, tag];
		}
		void this.pushToElectron();
	}

	addPersonalWord(word: string): void {
		const trimmed = word.trim();
		if (!trimmed) return;
		if (this.personalDictionary.includes(trimmed)) return;
		this.personalDictionary = [...this.personalDictionary, trimmed];
		void this.pushToElectron();
	}

	removePersonalWord(word: string): void {
		const trimmed = word.trim();
		if (!trimmed) return;
		this.personalDictionary = this.personalDictionary.filter((w) => w !== trimmed);
		void this.pushToElectron();
	}
}

export default new Spellcheck();
