// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import UserSettings from '@app/features/user/state/UserSettings';
import {create} from '@bufbuild/protobuf';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

interface ReverseImageSearchEngine {
	id: string;
	name: string;
	urlTemplate: string;
	enabled: boolean;
	isBuiltIn: boolean;
}

const logger = new Logger('ReverseImageSearch');
const BUILT_IN_ENGINES: ReadonlyArray<Omit<ReverseImageSearchEngine, 'enabled'>> = [
	{
		id: 'google_lens',
		name: 'Google Lens',
		urlTemplate: 'https://lens.google.com/uploadbyurl?url={url}',
		isBuiltIn: true,
	},
	{
		id: 'yandex',
		name: 'Yandex',
		urlTemplate: 'https://yandex.com/images/search?rpt=imageview&url={url}',
		isBuiltIn: true,
	},
	{
		id: 'bing',
		name: 'Bing',
		urlTemplate: 'https://www.bing.com/images/searchbyimage?cbir=sbi&imgurl={url}',
		isBuiltIn: true,
	},
	{
		id: 'tineye',
		name: 'TinEye',
		urlTemplate: 'https://tineye.com/search?url={url}',
		isBuiltIn: true,
	},
	{
		id: 'saucenao',
		name: 'SauceNAO',
		urlTemplate: 'https://saucenao.com/search.php?url={url}',
		isBuiltIn: true,
	},
	{
		id: 'iqdb',
		name: 'IQDB',
		urlTemplate: 'https://iqdb.org/?url={url}',
		isBuiltIn: true,
	},
];
const SUGGESTED_DEFAULT_REVERSE_IMAGE_SEARCH_ENGINE_ID = 'google_lens';

function createDefaultReverseImageSearchEngines(): Array<ReverseImageSearchEngine> {
	return BUILT_IN_ENGINES.map((engine) => ({
		...engine,
		enabled: engine.id === SUGGESTED_DEFAULT_REVERSE_IMAGE_SEARCH_ENGINE_ID,
	}));
}

class ReverseImageSearch {
	engines: Array<ReverseImageSearchEngine> = createDefaultReverseImageSearchEngines();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => makePersistent(this, 'ReverseImageSearch', ['engines'], {version: 2}));
	}

	get enabledEngines(): ReadonlyArray<ReverseImageSearchEngine> {
		return this.engines.filter((engine) => engine.enabled);
	}

	get defaultEngineId(): string | null {
		const value = UserSettings.getSubPreference('searchEngines')?.reverseImageSearchEngineId;
		return typeof value === 'string' && value.length > 0 ? value : null;
	}

	get defaultEngine(): ReverseImageSearchEngine | null {
		const id = this.defaultEngineId;
		if (id == null) return null;
		const engine = this.engines.find((entry) => entry.id === id && entry.enabled);
		return engine ?? null;
	}

	get effectiveDefaultEngine(): ReverseImageSearchEngine | null {
		if (this.defaultEngine) return this.defaultEngine;
		const enabled = this.enabledEngines;
		return enabled.length === 1 ? enabled[0] : null;
	}

	get nonDefaultEnabledEngines(): ReadonlyArray<ReverseImageSearchEngine> {
		const defaultId = this.defaultEngine?.id;
		return this.enabledEngines.filter((engine) => engine.id !== defaultId);
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

	buildSearchUrl(engineId: string, imageUrl: string): string {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine) return '';
		return engine.urlTemplate.replace(/\{url\}/gu, encodeURIComponent(imageUrl));
	}

	private async persistDefault(engineId: string | null): Promise<void> {
		try {
			const current = UserSettings.getSubPreference('searchEngines');
			const next = create(SearchEngineSettingsSchema, {
				textSearchEngineId: current?.textSearchEngineId,
				reverseImageSearchEngineId: engineId ?? undefined,
				translationProviderId: current?.translationProviderId,
			});
			await UserSettings.setSubPreference('searchEngines', next);
		} catch (error) {
			logger.error('Failed to persist default reverse-image-search engine', error);
		}
	}
}

export default new ReverseImageSearch();
