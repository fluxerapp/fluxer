// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import UserSettings from '@app/features/user/state/UserSettings';
import {create} from '@bufbuild/protobuf';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

interface UrlTemplateProvider {
	id: string;
	name: string;
	urlTemplate: string;
	enabled: boolean;
	isBuiltIn: boolean;
}

type DefaultProviderField = 'textSearchEngineId' | 'reverseImageSearchEngineId' | 'translationProviderId';

interface UrlTemplateProviderStoreConfig {
	name: string;
	persist: (this: UrlTemplateProviderStore) => Promise<void>;
	builtIns: ReadonlyArray<Omit<UrlTemplateProvider, 'enabled' | 'isBuiltIn'>>;
	suggestedDefaultId: string;
	defaultField: DefaultProviderField;
	placeholder: RegExp;
	persistFailureMessage: string;
}

export class UrlTemplateProviderStore {
	engines: Array<UrlTemplateProvider>;
	private readonly config: UrlTemplateProviderStoreConfig;
	private readonly logger: Logger;

	constructor(config: UrlTemplateProviderStoreConfig) {
		this.config = config;
		this.logger = new Logger(config.name);
		this.engines = config.builtIns.map((engine) => ({
			...engine,
			isBuiltIn: true,
			enabled: engine.id === config.suggestedDefaultId,
		}));
		makeAutoObservable<this, 'config' | 'logger'>(this, {config: false, logger: false}, {autoBind: true});
		initializeStore(this, () => config.persist.call(this));
	}

	get enabledEngines(): ReadonlyArray<UrlTemplateProvider> {
		return this.engines.filter((engine) => engine.enabled);
	}

	get defaultEngineId(): string | null {
		const value = UserSettings.getSubPreference('searchEngines')?.[this.config.defaultField];
		return typeof value === 'string' && value.length > 0 ? value : null;
	}

	get defaultEngine(): UrlTemplateProvider | null {
		const id = this.defaultEngineId;
		if (id == null) return null;
		const engine = this.engines.find((entry) => entry.id === id && entry.enabled);
		return engine ?? null;
	}

	get effectiveDefaultEngine(): UrlTemplateProvider | null {
		if (this.defaultEngine) return this.defaultEngine;
		const enabled = this.enabledEngines;
		return enabled.length === 1 ? enabled[0] : null;
	}

	get nonDefaultEnabledEngines(): ReadonlyArray<UrlTemplateProvider> {
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

	buildSearchUrl(engineId: string, query: string): string {
		const engine = this.engines.find((entry) => entry.id === engineId);
		if (!engine) return '';
		return engine.urlTemplate.replace(this.config.placeholder, encodeURIComponent(query));
	}

	private async persistDefault(engineId: string | null): Promise<void> {
		try {
			const current = UserSettings.getSubPreference('searchEngines');
			const next = create(SearchEngineSettingsSchema, {
				textSearchEngineId: current?.textSearchEngineId,
				reverseImageSearchEngineId: current?.reverseImageSearchEngineId,
				translationProviderId: current?.translationProviderId,
				[this.config.defaultField]: engineId ?? undefined,
			});
			await UserSettings.setSubPreference('searchEngines', next);
		} catch (error) {
			this.logger.error(this.config.persistFailureMessage, error);
		}
	}
}
