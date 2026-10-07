// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import UserSettings from '@app/features/user/state/UserSettings';
import {create} from '@bufbuild/protobuf';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

export interface TranslationProvider {
	id: string;
	name: string;
	urlTemplate: string;
	enabled: boolean;
	isBuiltIn: boolean;
}

const logger = new Logger('Translation');
const BUILT_IN_TRANSLATION_PROVIDERS: ReadonlyArray<Omit<TranslationProvider, 'enabled'>> = [
	{
		id: 'google_translate',
		name: 'Google Translate',
		urlTemplate: 'https://translate.google.com/?sl=auto&tl=auto&text={query}&op=translate',
		isBuiltIn: true,
	},
	{
		id: 'deepl',
		name: 'DeepL',
		urlTemplate: 'https://www.deepl.com/translator#auto/auto/{query}',
		isBuiltIn: true,
	},
	{
		id: 'bing_translator',
		name: 'Bing Translator',
		urlTemplate: 'https://www.bing.com/translator/?text={query}',
		isBuiltIn: true,
	},
	{
		id: 'yandex_translate',
		name: 'Yandex Translate',
		urlTemplate: 'https://translate.yandex.com/?text={query}',
		isBuiltIn: true,
	},
	{
		id: 'reverso',
		name: 'Reverso',
		urlTemplate: 'https://www.reverso.net/text-translation#sl=auto&tl=eng&text={query}',
		isBuiltIn: true,
	},
	{
		id: 'linguee',
		name: 'Linguee',
		urlTemplate: 'https://www.linguee.com/english-german/search?source=auto&query={query}',
		isBuiltIn: true,
	},
	{
		id: 'papago',
		name: 'Papago',
		urlTemplate: 'https://papago.naver.com/?st={query}',
		isBuiltIn: true,
	},
];
export const SUGGESTED_DEFAULT_TRANSLATION_PROVIDER_ID = 'google_translate';

export function createDefaultTranslationProviders(): Array<TranslationProvider> {
	return BUILT_IN_TRANSLATION_PROVIDERS.map((provider) => ({
		...provider,
		enabled: provider.id === SUGGESTED_DEFAULT_TRANSLATION_PROVIDER_ID,
	}));
}

class Translation {
	engines: Array<TranslationProvider> = createDefaultTranslationProviders();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => makePersistent(this, 'Translation', ['engines'], {version: 1}));
	}

	get enabledEngines(): ReadonlyArray<TranslationProvider> {
		return this.engines.filter((engine) => engine.enabled);
	}

	get defaultEngineId(): string | null {
		const value = UserSettings.getSubPreference('searchEngines')?.translationProviderId;
		return typeof value === 'string' && value.length > 0 ? value : null;
	}

	get defaultEngine(): TranslationProvider | null {
		const id = this.defaultEngineId;
		if (id == null) return null;
		const engine = this.engines.find((entry) => entry.id === id && entry.enabled);
		return engine ?? null;
	}

	get effectiveDefaultEngine(): TranslationProvider | null {
		if (this.defaultEngine) return this.defaultEngine;
		const enabled = this.enabledEngines;
		return enabled.length === 1 ? enabled[0] : null;
	}

	get hasUserPreference(): boolean {
		return this.defaultEngine != null;
	}

	get nonDefaultEnabledEngines(): ReadonlyArray<TranslationProvider> {
		const defaultId = this.defaultEngine?.id;
		return this.enabledEngines.filter((engine) => engine.id !== defaultId);
	}

	get hasMultipleEnabled(): boolean {
		return this.enabledEngines.length > 1;
	}

	setEnabled(engineId: string, enabled: boolean): void {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine) return;
		engine.enabled = enabled;
		if (!enabled && this.defaultEngineId === engineId) {
			void this.persistDefault(null);
		}
	}

	setDefaultEngine(engineId: string): Promise<void> {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine) return Promise.resolve();
		if (!engine.enabled) {
			engine.enabled = true;
		}
		return this.persistDefault(engineId);
	}

	clearDefaultEngine(): void {
		void this.persistDefault(null);
	}

	addCustomEngine(name: string, urlTemplate: string): string {
		const id = `custom_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
		this.engines.push({
			id,
			name,
			urlTemplate,
			enabled: true,
			isBuiltIn: false,
		});
		return id;
	}

	removeCustomEngine(engineId: string): void {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine || engine.isBuiltIn) return;
		this.engines = this.engines.filter((entry) => entry.id !== engineId);
		if (this.defaultEngineId === engineId) {
			void this.persistDefault(null);
		}
	}

	updateCustomEngine(engineId: string, name: string, urlTemplate: string): void {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine || engine.isBuiltIn) return;
		engine.name = name;
		engine.urlTemplate = urlTemplate;
	}

	buildSearchUrl(engineId: string, query: string): string {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine) return '';
		return engine.urlTemplate.replace(/\{query\}/gu, encodeURIComponent(query));
	}

	private async persistDefault(engineId: string | null): Promise<void> {
		try {
			const current = UserSettings.getSubPreference('searchEngines');
			const next = create(SearchEngineSettingsSchema, {
				textSearchEngineId: current?.textSearchEngineId,
				reverseImageSearchEngineId: current?.reverseImageSearchEngineId,
				translationProviderId: engineId ?? undefined,
			});
			await UserSettings.setSubPreference('searchEngines', next);
		} catch (error) {
			logger.error('Failed to persist default translation provider', error);
		}
	}
}

export default new Translation();
