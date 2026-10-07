// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import UserSettings from '@app/features/user/state/UserSettings';
import {create} from '@bufbuild/protobuf';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

export interface SearchEngine {
	id: string;
	name: string;
	urlTemplate: string;
	enabled: boolean;
	isBuiltIn: boolean;
}

const logger = new Logger('SearchEngine');
const BUILT_IN_SEARCH_ENGINES: ReadonlyArray<Omit<SearchEngine, 'enabled'>> = [
	{id: 'google', name: 'Google', urlTemplate: 'https://www.google.com/search?q={query}', isBuiltIn: true},
	{id: 'bing', name: 'Bing', urlTemplate: 'https://www.bing.com/search?q={query}', isBuiltIn: true},
	{id: 'duckduckgo', name: 'DuckDuckGo', urlTemplate: 'https://duckduckgo.com/?q={query}', isBuiltIn: true},
	{id: 'yahoo', name: 'Yahoo', urlTemplate: 'https://search.yahoo.com/search?p={query}', isBuiltIn: true},
	{id: 'ecosia', name: 'Ecosia', urlTemplate: 'https://www.ecosia.org/search?q={query}', isBuiltIn: true},
	{id: 'brave', name: 'Brave Search', urlTemplate: 'https://search.brave.com/search?q={query}', isBuiltIn: true},
	{
		id: 'startpage',
		name: 'Startpage',
		urlTemplate: 'https://www.startpage.com/do/dsearch?query={query}',
		isBuiltIn: true,
	},
	{id: 'yandex', name: 'Yandex', urlTemplate: 'https://yandex.com/search/?text={query}', isBuiltIn: true},
	{
		id: 'wikipedia',
		name: 'Wikipedia',
		urlTemplate: 'https://en.wikipedia.org/w/index.php?search={query}',
		isBuiltIn: true,
	},
	{
		id: 'youtube',
		name: 'YouTube',
		urlTemplate: 'https://www.youtube.com/results?search_query={query}',
		isBuiltIn: true,
	},
	{id: 'github', name: 'GitHub', urlTemplate: 'https://github.com/search?q={query}', isBuiltIn: true},
	{id: 'reddit', name: 'Reddit', urlTemplate: 'https://www.reddit.com/search/?q={query}', isBuiltIn: true},
];
export const SUGGESTED_DEFAULT_SEARCH_ENGINE_ID = 'google';

export function createDefaultSearchEngines(): Array<SearchEngine> {
	return BUILT_IN_SEARCH_ENGINES.map((engine) => ({
		...engine,
		enabled: engine.id === SUGGESTED_DEFAULT_SEARCH_ENGINE_ID,
	}));
}

class SearchEngines {
	engines: Array<SearchEngine> = createDefaultSearchEngines();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => makePersistent(this, 'SearchEngine', ['engines'], {version: 2}));
	}

	get enabledEngines(): ReadonlyArray<SearchEngine> {
		return this.engines.filter((engine) => engine.enabled);
	}

	get defaultEngineId(): string | null {
		const value = UserSettings.getSubPreference('searchEngines')?.textSearchEngineId;
		return typeof value === 'string' && value.length > 0 ? value : null;
	}

	get defaultEngine(): SearchEngine | null {
		const id = this.defaultEngineId;
		if (id == null) return null;
		const engine = this.engines.find((entry) => entry.id === id && entry.enabled);
		return engine ?? null;
	}

	get effectiveDefaultEngine(): SearchEngine | null {
		if (this.defaultEngine) return this.defaultEngine;
		const enabled = this.enabledEngines;
		return enabled.length === 1 ? enabled[0] : null;
	}

	get hasUserPreference(): boolean {
		return this.defaultEngine != null;
	}

	get nonDefaultEnabledEngines(): ReadonlyArray<SearchEngine> {
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
				textSearchEngineId: engineId ?? undefined,
				reverseImageSearchEngineId: current?.reverseImageSearchEngineId,
				translationProviderId: current?.translationProviderId,
			});
			await UserSettings.setSubPreference('searchEngines', next);
		} catch (error) {
			logger.error('Failed to persist default web-search engine', error);
		}
	}
}

export default new SearchEngines();
