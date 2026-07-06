// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GifProvider} from '@app/features/app/state/GifProviderConfig';
import {normalizeGifProviderInfo} from '@app/features/app/state/GifProviderConfig';
import RuntimeConfig, {
	normalizeAppPublicConfig,
	normalizeInstanceCommunity,
	normalizeInstanceRegistration,
	normalizeInstanceSso,
	normalizeInstanceServices,
} from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MS_PER_HOUR, MS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import type {
	InstanceAppPublic,
	InstanceCaptcha,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceEndpoints,
	InstanceFeatures,
	InstanceRegistration,
	InstanceServices,
	InstanceSso as InstanceSsoConfig,
} from '@fluxer/instance_bootstrap/src/Types';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('InstanceConfig');
const CONFIG_REFRESH_INTERVAL_MS = 30 * MS_PER_MINUTE;
const CONFIG_STALE_THRESHOLD_MS = MS_PER_HOUR;

export interface InstanceConfig {
	domain: string;
	fetchedAt: number;
	apiCodeVersion: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	registration: InstanceRegistration;
	community: InstanceCommunity;
	services: InstanceServices;
	gif: {
		provider: GifProvider;
		displayName: string;
		attributionRequired: boolean;
	};
	sso: InstanceSsoConfig | null;
	limits: LimitConfigSnapshot;
	push: {
		public_vapid_key: string | null;
	} | null;
	appPublic: InstanceAppPublic;
}

class InstanceConfigs {
	instanceConfigs: Map<string, InstanceConfig> = new Map();
	localInstanceDomain: string | null = null;
	private refreshIntervalId: number | null = null;
	private pendingFetches: Map<string, Promise<InstanceConfig>> = new Map();
	private configValidators: Map<string, {etag: string | null; lastModified: string | null}> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.startPeriodicRefresh();
	}

	private startPeriodicRefresh(): void {
		if (this.refreshIntervalId !== null) {
			return;
		}
		this.refreshIntervalId = window.setInterval(() => {
			this.refreshAllConfigs().catch((err) => {
				logger.warn('Periodic config refresh failed:', err);
			});
		}, CONFIG_REFRESH_INTERVAL_MS);
	}

	stopPeriodicRefresh(): void {
		if (this.refreshIntervalId !== null) {
			clearInterval(this.refreshIntervalId);
			this.refreshIntervalId = null;
		}
	}

	async fetchInstanceConfig(domain: string, forceRefresh = false): Promise<InstanceConfig> {
		const normalizedDomain = domain.toLowerCase();
		if (!forceRefresh) {
			const cached = this.instanceConfigs.get(normalizedDomain);
			if (cached && !this.isConfigStale(cached)) {
				logger.debug('Using cached config for:', normalizedDomain);
				return cached;
			}
		}
		const existingFetch = this.pendingFetches.get(normalizedDomain);
		if (existingFetch) {
			logger.debug('Waiting for existing fetch for:', normalizedDomain);
			return existingFetch;
		}
		const fetchPromise = this.doFetchInstanceConfig(normalizedDomain);
		this.pendingFetches.set(normalizedDomain, fetchPromise);
		try {
			return await fetchPromise;
		} finally {
			this.pendingFetches.delete(normalizedDomain);
		}
	}

	private async doFetchInstanceConfig(domain: string): Promise<InstanceConfig> {
		logger.debug('Fetching config for:', domain);
		const wellKnownUrl = `https://${domain}/.well-known/fluxer`;
		const cached = this.instanceConfigs.get(domain);
		const validators = cached ? this.configValidators.get(domain) : undefined;
		const headers: Record<string, string> = {
			Accept: 'application/json',
		};
		if (validators?.etag) {
			headers['If-None-Match'] = validators.etag;
		}
		if (validators?.lastModified) {
			headers['If-Modified-Since'] = validators.lastModified;
		}
		const response = await fetch(wellKnownUrl, {
			method: 'GET',
			headers,
		});
		if (response.status === 304 && cached) {
			const refreshed: InstanceConfig = {...cached, fetchedAt: Date.now()};
			runInAction(() => {
				this.instanceConfigs.set(domain, refreshed);
			});
			logger.debug('Config not modified for:', domain);
			return refreshed;
		}
		if (!response.ok) {
			throw new Error(`Failed to fetch instance config for ${domain}: ${response.status} ${response.statusText}`);
		}
		const data = (await response.json()) as InstanceDiscoveryResponse;
		const limits = this.processLimitsFromApi(data.limits);
		const gifProviderInfo = normalizeGifProviderInfo({
			provider: data.gif?.provider,
			attributionRequired: data.gif?.attribution_required,
		});
		const config: InstanceConfig = {
			domain,
			fetchedAt: Date.now(),
			apiCodeVersion: data.api_code_version,
			endpoints: data.endpoints,
			captcha: data.captcha,
			features: data.features,
			gif: {
				provider: gifProviderInfo.name,
				displayName: gifProviderInfo.displayName,
				attributionRequired: gifProviderInfo.attributionRequired,
			},
			sso: normalizeInstanceSso(data.sso),
			registration: normalizeInstanceRegistration(data.registration),
			community: normalizeInstanceCommunity(data.community),
			services: normalizeInstanceServices(data.services),
			limits,
			push: data.push ?? null,
			appPublic: normalizeAppPublicConfig(data.app_public),
		};
		runInAction(() => {
			this.instanceConfigs.set(domain, config);
			this.configValidators.set(domain, {
				etag: response.headers.get('etag'),
				lastModified: response.headers.get('last-modified'),
			});
		});
		logger.debug('Cached config for:', domain);
		return config;
	}

	getInstanceConfig(domain: string): InstanceConfig | null {
		const normalizedDomain = domain.toLowerCase();
		const cached = this.instanceConfigs.get(normalizedDomain);
		if (cached && this.isConfigStale(cached)) {
			this.fetchInstanceConfig(normalizedDomain, true).catch((err) => {
				logger.warn('Background config refresh failed for:', normalizedDomain, err);
			});
		}
		return cached ?? null;
	}

	getLocalInstanceConfig(): InstanceConfig | null {
		const domain = RuntimeConfig.localInstanceDomain;
		if (!domain) {
			return null;
		}
		const existing = this.instanceConfigs.get(domain.toLowerCase());
		if (existing) {
			return existing;
		}
		return {
			domain,
			fetchedAt: Date.now(),
			apiCodeVersion: RuntimeConfig.apiCodeVersion,
			endpoints: {
				api: RuntimeConfig.apiEndpoint,
				api_client: RuntimeConfig.apiEndpoint,
				api_public: RuntimeConfig.apiPublicEndpoint,
				gateway: RuntimeConfig.gatewayEndpoint,
				media: RuntimeConfig.mediaEndpoint,
				static_cdn: RuntimeConfig.staticCdnEndpoint,
				marketing: RuntimeConfig.marketingEndpoint,
				admin: RuntimeConfig.adminEndpoint,
				invite: RuntimeConfig.inviteEndpoint,
				gift: RuntimeConfig.giftEndpoint,
				webapp: RuntimeConfig.webAppEndpoint,
			},
			captcha: {
				provider: RuntimeConfig.captchaProvider,
				hcaptcha_site_key: RuntimeConfig.hcaptchaSiteKey,
				turnstile_site_key: RuntimeConfig.turnstileSiteKey,
			},
			features: RuntimeConfig.features,
			gif: {
				provider: RuntimeConfig.gifProvider,
				displayName: RuntimeConfig.gifProviderDisplayName,
				attributionRequired: RuntimeConfig.gifAttributionRequired,
			},
			sso: normalizeInstanceSso(RuntimeConfig.sso),
			registration: RuntimeConfig.registration,
			community: RuntimeConfig.community,
			services: RuntimeConfig.services,
			limits: RuntimeConfig.limits,
			push: {public_vapid_key: RuntimeConfig.publicPushVapidKey},
			appPublic: normalizeAppPublicConfig(RuntimeConfig.appPublic),
		};
	}

	getLimitsForInstance(domain: string): LimitConfigSnapshot | null {
		const config = this.getInstanceConfig(domain);
		return config?.limits ?? null;
	}

	async refreshAllConfigs(): Promise<void> {
		const domains = Array.from(this.instanceConfigs.keys());
		logger.debug('Refreshing configs for', domains.length, 'instances');
		const refreshPromises = domains.map(async (domain) => {
			try {
				await this.fetchInstanceConfig(domain, true);
			} catch (err) {
				logger.warn('Failed to refresh config for:', domain, err);
			}
		});
		await Promise.allSettled(refreshPromises);
	}

	async onGatewayReady(domain: string): Promise<void> {
		try {
			await this.fetchInstanceConfig(domain, true);
			logger.debug('Refreshed config on gateway ready for:', domain);
		} catch (err) {
			logger.warn('Failed to refresh config on gateway ready for:', domain, err);
		}
	}

	clearInstanceConfig(domain: string): void {
		const normalizedDomain = domain.toLowerCase();
		this.instanceConfigs.delete(normalizedDomain);
		this.configValidators.delete(normalizedDomain);
		logger.debug('Cleared config for:', normalizedDomain);
	}

	clearAllConfigs(): void {
		this.instanceConfigs.clear();
		this.configValidators.clear();
		logger.debug('Cleared all instance configs');
	}

	private isConfigStale(config: InstanceConfig): boolean {
		return Date.now() - config.fetchedAt > CONFIG_STALE_THRESHOLD_MS;
	}

	private processLimitsFromApi(limits: LimitConfigSnapshot | LimitConfigWireFormat | undefined): LimitConfigSnapshot {
		if (!limits) {
			return this.createEmptyLimitConfig();
		}
		if ('defaultsHash' in limits && limits.version === 2) {
			return expandWireFormat(limits);
		}
		return limits as LimitConfigSnapshot;
	}

	private createEmptyLimitConfig(): LimitConfigSnapshot {
		return {
			version: 1,
			traitDefinitions: [],
			rules: [],
		};
	}
}

export default new InstanceConfigs();
