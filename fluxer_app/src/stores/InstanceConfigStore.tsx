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

import {Logger} from '@app/lib/Logger';
import type {
	GifProvider,
	InstanceAppPublic,
	InstanceCaptcha,
	InstanceEndpoints,
	InstanceFeatures,
	InstanceSsoConfig,
} from '@app/stores/RuntimeConfigStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import {MS_PER_HOUR, MS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('InstanceConfigStore');

const CONFIG_REFRESH_INTERVAL_MS = 30 * MS_PER_MINUTE;
const CONFIG_STALE_THRESHOLD_MS = MS_PER_HOUR;

export interface FederationConfig {
	enabled: boolean;
	version: number;
}

export interface InstancePublicKey {
	id: string;
	algorithm: 'x25519';
	public_key_base64: string;
}

export interface OAuth2Config {
	authorization_endpoint: string;
	token_endpoint: string;
	userinfo_endpoint: string;
	scopes_supported: Array<string>;
}

export interface InstanceConfig {
	domain: string;
	fetchedAt: number;
	apiCodeVersion: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif: {provider: GifProvider};
	sso: InstanceSsoConfig | null;
	limits: LimitConfigSnapshot;
	push: {public_vapid_key: string | null} | null;
	appPublic: InstanceAppPublic;
	federation: FederationConfig | null;
	publicKey: InstancePublicKey | null;
	oauth2: OAuth2Config | null;
}

interface InstanceDiscoveryResponse {
	api_code_version: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif?: {provider: GifProvider};
	sso?: InstanceSsoConfig;
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
	push?: {public_vapid_key: string | null};
	app_public: InstanceAppPublic;
	federation?: FederationConfig;
	public_key?: InstancePublicKey;
	oauth2?: OAuth2Config;
}

class InstanceConfigStore {
	instanceConfigs: Map<string, InstanceConfig> = new Map();
	localInstanceDomain: string | null = null;

	private refreshIntervalId: number | null = null;
	private pendingFetches: Map<string, Promise<InstanceConfig>> = new Map();

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
		const response = await fetch(wellKnownUrl, {
			method: 'GET',
			headers: {
				Accept: 'application/json',
			},
		});

		if (!response.ok) {
			throw new Error(`Failed to fetch instance config for ${domain}: ${response.status} ${response.statusText}`);
		}

		const data = (await response.json()) as InstanceDiscoveryResponse;

		const limits = this.processLimitsFromApi(data.limits);
		const gifProvider: GifProvider = data.gif?.provider === 'tenor' ? 'tenor' : 'klipy';

		const config: InstanceConfig = {
			domain,
			fetchedAt: Date.now(),
			apiCodeVersion: data.api_code_version,
			endpoints: data.endpoints,
			captcha: data.captcha,
			features: data.features,
			gif: {provider: gifProvider},
			sso: data.sso ?? null,
			limits,
			push: data.push ?? null,
			appPublic: data.app_public,
			federation: data.federation ?? null,
			publicKey: data.public_key ?? null,
			oauth2: data.oauth2 ?? null,
		};

		runInAction(() => {
			this.instanceConfigs.set(domain, config);
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
		const domain = RuntimeConfigStore.localInstanceDomain;
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
			apiCodeVersion: RuntimeConfigStore.apiCodeVersion,
			endpoints: {
				api: RuntimeConfigStore.apiEndpoint,
				api_client: RuntimeConfigStore.apiEndpoint,
				api_public: RuntimeConfigStore.apiPublicEndpoint,
				gateway: RuntimeConfigStore.gatewayEndpoint,
				media: RuntimeConfigStore.mediaEndpoint,
				static_cdn: RuntimeConfigStore.staticCdnEndpoint,
				marketing: RuntimeConfigStore.marketingEndpoint,
				admin: RuntimeConfigStore.adminEndpoint,
				invite: RuntimeConfigStore.inviteEndpoint,
				gift: RuntimeConfigStore.giftEndpoint,
				webapp: RuntimeConfigStore.webAppEndpoint,
			},
			captcha: {
				provider: RuntimeConfigStore.captchaProvider,
				hcaptcha_site_key: RuntimeConfigStore.hcaptchaSiteKey,
				turnstile_site_key: RuntimeConfigStore.turnstileSiteKey,
			},
			features: RuntimeConfigStore.features,
			gif: {provider: RuntimeConfigStore.gifProvider},
			sso: RuntimeConfigStore.sso,
			limits: RuntimeConfigStore.limits,
			push: {public_vapid_key: RuntimeConfigStore.publicPushVapidKey},
			appPublic: {
				sentry_dsn: RuntimeConfigStore.sentryDsn,
			},
			federation: null,
			publicKey: null,
			oauth2: null,
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
		logger.debug('Cleared config for:', normalizedDomain);
	}

	clearAllConfigs(): void {
		this.instanceConfigs.clear();
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

export default new InstanceConfigStore();
