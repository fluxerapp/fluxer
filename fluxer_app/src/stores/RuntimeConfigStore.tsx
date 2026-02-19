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

import Config from '@app/Config';
import type {HttpRequestConfig} from '@app/lib/HttpClient';
import HttpClient from '@app/lib/HttpClient';
import {makePersistent} from '@app/lib/MobXPersistence';
import relayClient from '@app/lib/RelayClient';
import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

export interface InstanceFeatures {
	sms_mfa_enabled: boolean;
	voice_enabled: boolean;
	stripe_enabled: boolean;
	self_hosted: boolean;
	manual_review_enabled: boolean;
}

export interface InstanceSsoConfig {
	enabled: boolean;
	enforced: boolean;
	display_name: string | null;
	redirect_uri: string;
}

export interface InstanceEndpoints {
	api: string;
	api_client?: string;
	api_public?: string;
	gateway: string;
	media: string;
	static_cdn: string;
	marketing: string;
	admin: string;
	invite: string;
	gift: string;
	webapp: string;
}

export interface InstanceCaptcha {
	provider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptcha_site_key: string | null;
	turnstile_site_key: string | null;
}

export interface InstancePush {
	public_vapid_key: string | null;
}

export interface InstanceAppPublic {
	sentry_dsn: string;
}

export type GifProvider = 'klipy' | 'tenor';

export interface InstanceDiscoveryResponse {
	api_code_version: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif?: {provider: GifProvider};
	sso?: InstanceSsoConfig;
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
	push?: InstancePush;
	app_public: InstanceAppPublic;
}

export interface RuntimeConfigSnapshot {
	apiEndpoint: string;
	apiPublicEndpoint: string;
	gatewayEndpoint: string;
	mediaEndpoint: string;
	staticCdnEndpoint: string;
	marketingEndpoint: string;
	adminEndpoint: string;
	inviteEndpoint: string;
	giftEndpoint: string;
	webAppEndpoint: string;
	gifProvider: GifProvider;
	captchaProvider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptchaSiteKey: string | null;
	turnstileSiteKey: string | null;
	apiCodeVersion: number;
	features: InstanceFeatures;
	sso: InstanceSsoConfig | null;
	publicPushVapidKey: string | null;
	limits: LimitConfigSnapshot;
	sentryDsn: string;
	relayDirectoryUrl: string | null;
}

type InitState = 'initializing' | 'ready' | 'error';

class RuntimeConfigStore {
	private _initState: InitState = 'initializing';
	private _initError: Error | null = null;

	private _initPromise: Promise<void>;
	private _resolveInit!: () => void;
	private _rejectInit!: (err: Error) => void;

	private _connectSeq = 0;

	apiEndpoint: string = '';
	apiPublicEndpoint: string = '';
	gatewayEndpoint: string = '';
	mediaEndpoint: string = '';
	staticCdnEndpoint: string = '';
	marketingEndpoint: string = '';
	adminEndpoint: string = '';
	inviteEndpoint: string = '';
	giftEndpoint: string = '';
	webAppEndpoint: string = '';

	gifProvider: GifProvider = 'klipy';

	captchaProvider: 'hcaptcha' | 'turnstile' | 'none' = 'none';
	hcaptchaSiteKey: string | null = null;
	turnstileSiteKey: string | null = null;

	apiCodeVersion: number = API_CODE_VERSION;
	features: InstanceFeatures = {
		sms_mfa_enabled: false,
		voice_enabled: false,
		stripe_enabled: false,
		self_hosted: false,
		manual_review_enabled: false,
	};
	sso: InstanceSsoConfig | null = null;
	publicPushVapidKey: string | null = null;
	limits: LimitConfigSnapshot = this.createEmptyLimitConfig();
	currentDefaultsHash: string | null = null;

	sentryDsn: string = '';

	relayDirectoryUrl: string | null = Config.PUBLIC_RELAY_DIRECTORY_URL;

	get relayModeEnabled(): boolean {
		return this.relayDirectoryUrl != null;
	}

	constructor() {
		this._initPromise = new Promise<void>((resolve, reject) => {
			this._resolveInit = resolve;
			this._rejectInit = reject;
		});

		makeAutoObservable(this, {}, {autoBind: true});

		this.initialize().catch(() => {});

		reaction(
			() => this.apiEndpoint,
			(endpoint) => {
				if (endpoint) {
					HttpClient.setBaseUrl(endpoint, this.apiCodeVersion);
					this.updateTargetInstanceDomain(endpoint);
				}
			},
			{fireImmediately: true},
		);

		reaction(
			() => this.relayDirectoryUrl,
			(directoryUrl) => {
				HttpClient.setRelayDirectoryUrl(directoryUrl);
				if (directoryUrl) {
					relayClient.setRelayDirectoryUrl(directoryUrl);
				}
			},
			{fireImmediately: true},
		);
	}

	private updateTargetInstanceDomain(endpoint: string): void {
		try {
			const url = new URL(endpoint);
			HttpClient.setTargetInstanceDomain(url.hostname);
		} catch {
			HttpClient.setTargetInstanceDomain(null);
		}
	}

	private async initialize(): Promise<void> {
		try {
			await makePersistent(this, 'runtimeConfig', [
				'apiEndpoint',
				'apiPublicEndpoint',
				'gatewayEndpoint',
				'mediaEndpoint',
				'staticCdnEndpoint',
				'marketingEndpoint',
				'adminEndpoint',
				'inviteEndpoint',
				'giftEndpoint',
				'webAppEndpoint',
				'gifProvider',
				'captchaProvider',
				'hcaptchaSiteKey',
				'turnstileSiteKey',
				'apiCodeVersion',
				'features',
				'sso',
				'publicPushVapidKey',
				'limits',
				'currentDefaultsHash',
				'sentryDsn',
				'relayDirectoryUrl',
			]);

			const bootstrapEndpoint = this.apiEndpoint || Config.PUBLIC_BOOTSTRAP_API_ENDPOINT;

			await this.connectToEndpoint(bootstrapEndpoint);

			runInAction(() => {
				this._initState = 'ready';
				this._initError = null;
			});

			this._resolveInit();
		} catch (error) {
			const err = error instanceof Error ? error : new Error(String(error));
			runInAction(() => {
				this._initState = 'error';
				this._initError = err;
			});
			this._rejectInit(err);
		}
	}

	waitForInit(): Promise<void> {
		return this._initPromise;
	}

	get initialized(): boolean {
		return this._initState === 'ready';
	}

	get initError(): Error | null {
		return this._initError;
	}

	applySnapshot(snapshot: RuntimeConfigSnapshot): void {
		this.apiEndpoint = snapshot.apiEndpoint;
		this.apiPublicEndpoint = snapshot.apiPublicEndpoint;
		this.gatewayEndpoint = snapshot.gatewayEndpoint;
		this.mediaEndpoint = snapshot.mediaEndpoint;
		this.staticCdnEndpoint = snapshot.staticCdnEndpoint;
		this.marketingEndpoint = snapshot.marketingEndpoint;
		this.adminEndpoint = snapshot.adminEndpoint;
		this.inviteEndpoint = snapshot.inviteEndpoint;
		this.giftEndpoint = snapshot.giftEndpoint;
		this.webAppEndpoint = snapshot.webAppEndpoint;

		this.gifProvider = snapshot.gifProvider;

		this.captchaProvider = snapshot.captchaProvider;
		this.hcaptchaSiteKey = snapshot.hcaptchaSiteKey;
		this.turnstileSiteKey = snapshot.turnstileSiteKey;

		this.apiCodeVersion = snapshot.apiCodeVersion;
		this.features = snapshot.features;
		this.sso = snapshot.sso;
		this.publicPushVapidKey = snapshot.publicPushVapidKey;
		this.limits = this.normalizeLimits(snapshot.limits ?? this.createEmptyLimitConfig());
		this.currentDefaultsHash = null;

		this.sentryDsn = snapshot.sentryDsn;

		this.relayDirectoryUrl = snapshot.relayDirectoryUrl;
	}
	getSnapshot(): RuntimeConfigSnapshot {
		return {
			apiEndpoint: this.apiEndpoint,
			apiPublicEndpoint: this.apiPublicEndpoint,
			gatewayEndpoint: this.gatewayEndpoint,
			mediaEndpoint: this.mediaEndpoint,
			staticCdnEndpoint: this.staticCdnEndpoint,
			marketingEndpoint: this.marketingEndpoint,
			adminEndpoint: this.adminEndpoint,
			inviteEndpoint: this.inviteEndpoint,
			giftEndpoint: this.giftEndpoint,
			webAppEndpoint: this.webAppEndpoint,
			gifProvider: this.gifProvider,
			captchaProvider: this.captchaProvider,
			hcaptchaSiteKey: this.hcaptchaSiteKey,
			turnstileSiteKey: this.turnstileSiteKey,
			apiCodeVersion: this.apiCodeVersion,
			features: {...this.features},
			sso: this.sso ? {...this.sso} : null,
			publicPushVapidKey: this.publicPushVapidKey,
			limits: this.cloneLimits(this.limits),
			sentryDsn: this.sentryDsn,
			relayDirectoryUrl: this.relayDirectoryUrl,
		};
	}
	private createEmptyLimitConfig(): LimitConfigSnapshot {
		return {
			version: 1,
			traitDefinitions: [],
			rules: [],
		};
	}

	private cloneLimits(limits: LimitConfigSnapshot): LimitConfigSnapshot {
		return JSON.parse(JSON.stringify(limits));
	}

	private normalizeLimits(limits?: LimitConfigSnapshot): LimitConfigSnapshot {
		const cloned = this.cloneLimits(limits ?? this.createEmptyLimitConfig());
		return {
			...cloned,
			traitDefinitions: cloned.traitDefinitions ?? [],
			rules: cloned.rules ?? [],
		};
	}

	private processLimitsFromApi(limits: LimitConfigSnapshot | LimitConfigWireFormat | undefined): LimitConfigSnapshot {
		if (limits && 'defaultsHash' in limits && limits.version === 2) {
			const expanded = expandWireFormat(limits);
			this.currentDefaultsHash = limits.defaultsHash;
			return this.normalizeLimits(expanded);
		}
		this.currentDefaultsHash = null;
		return this.normalizeLimits(limits as LimitConfigSnapshot | undefined);
	}

	async withSnapshot<T>(snapshot: RuntimeConfigSnapshot, fn: () => Promise<T>): Promise<T> {
		const before = this.getSnapshot();
		this.applySnapshot(snapshot);

		try {
			return await fn();
		} finally {
			this.applySnapshot(before);
		}
	}

	async resetToDefaults(): Promise<void> {
		await this.connectToEndpoint(Config.PUBLIC_BOOTSTRAP_API_ENDPOINT);
	}

	async connectToEndpoint(input: string): Promise<void> {
		const connectId = ++this._connectSeq;

		const apiEndpoint = this.normalizeEndpoint(input);
		const wellKnownUrl = this.buildWellKnownUrl(apiEndpoint);

		const request: HttpRequestConfig = {url: wellKnownUrl};

		const response = await HttpClient.get<InstanceDiscoveryResponse>(request);

		if (connectId !== this._connectSeq) {
			return;
		}

		if (!response.ok) {
			throw new Error(`Failed to reach ${wellKnownUrl} (${response.status})`);
		}

		this.updateFromInstance(response.body);
	}

	private buildWellKnownUrl(apiEndpoint: string): string {
		try {
			const url = new URL(apiEndpoint);
			url.pathname = '/.well-known/fluxer';
			return url.toString();
		} catch {
			return `${apiEndpoint.replace(/\/api\/?$/, '')}/.well-known/fluxer`;
		}
	}

	private normalizeEndpoint(input: string): string {
		const trimmed = input['trim']();
		if (!trimmed) {
			throw new Error('API endpoint is required');
		}

		let candidate = trimmed;

		if (candidate.startsWith('/')) {
			candidate = `${window.location.origin}${candidate}`;
		} else if (!/^[a-zA-Z][a-zA-Z0-9+\-.]*:\/\//.test(candidate)) {
			candidate = `https://${candidate}`;
		}

		const url = new URL(candidate);
		if (url.pathname === '' || url.pathname === '/') {
			url.pathname = '/api';
		}
		url.pathname = url.pathname.replace(/\/+$/, '');
		return url.toString();
	}

	private updateFromInstance(instance: InstanceDiscoveryResponse): void {
		this.assertCodeVersion(instance.api_code_version);

		const apiEndpoint = instance.endpoints.api_client ?? instance.endpoints.api;
		const apiPublicEndpoint = instance.endpoints.api_public ?? apiEndpoint;
		const sso = instance.sso ?? null;
		const gifProvider: GifProvider = instance.gif?.provider === 'tenor' ? 'tenor' : 'klipy';

		runInAction(() => {
			this.apiEndpoint = apiEndpoint;
			this.apiPublicEndpoint = apiPublicEndpoint;

			this.gatewayEndpoint = instance.endpoints.gateway;
			this.mediaEndpoint = instance.endpoints.media;
			this.staticCdnEndpoint = instance.endpoints.static_cdn;
			this.marketingEndpoint = instance.endpoints.marketing;
			this.adminEndpoint = instance.endpoints.admin;
			this.inviteEndpoint = instance.endpoints.invite;
			this.giftEndpoint = instance.endpoints.gift;
			this.webAppEndpoint = instance.endpoints.webapp;

			this.gifProvider = gifProvider;

			this.captchaProvider = instance.captcha.provider;
			this.hcaptchaSiteKey = instance.captcha.hcaptcha_site_key;
			this.turnstileSiteKey = instance.captcha.turnstile_site_key;

			this.apiCodeVersion = instance.api_code_version;
			this.features = instance.features;
			this.sso = sso;
			this.publicPushVapidKey = instance.push?.public_vapid_key ?? null;
			this.limits = this.processLimitsFromApi(instance.limits);

			if (instance.app_public) {
				this.sentryDsn = instance.app_public.sentry_dsn;
			}
		});
	}
	private assertCodeVersion(instanceVersion: number): void {
		if (instanceVersion < API_CODE_VERSION) {
			throw new Error(
				`Incompatible server (code version ${instanceVersion}); this client requires ${API_CODE_VERSION}.`,
			);
		}
	}

	get webAppBaseUrl(): string {
		if (this.webAppEndpoint) {
			return this.webAppEndpoint.replace(/\/$/, '');
		}

		try {
			const url = new URL(this.apiEndpoint);
			if (url.pathname.endsWith('/api')) {
				url.pathname = url.pathname.slice(0, -4) || '/';
			}
			return url.toString().replace(/\/$/, '');
		} catch {
			return this.apiEndpoint.replace(/\/api$/, '');
		}
	}

	isSelfHosted(): boolean {
		return DeveloperOptionsStore.selfHostedModeOverride || this.features.self_hosted;
	}

	get marketingHost(): string {
		try {
			return new URL(this.marketingEndpoint).host;
		} catch {
			return '';
		}
	}

	get inviteHost(): string {
		try {
			return new URL(this.inviteEndpoint).host;
		} catch {
			return '';
		}
	}

	get giftHost(): string {
		try {
			return new URL(this.giftEndpoint).host;
		} catch {
			return '';
		}
	}

	get inviteUrlBase(): string {
		try {
			const url = new URL(this.inviteEndpoint);
			const path = url.pathname !== '/' ? url.pathname.replace(/\/$/, '') : '';
			return `${url.host}${path}`;
		} catch {
			return '';
		}
	}

	get giftUrlBase(): string {
		try {
			const url = new URL(this.giftEndpoint);
			const path = url.pathname !== '/' ? url.pathname.replace(/\/$/, '') : '';
			return `${url.host}${path}`;
		} catch {
			return '';
		}
	}

	get localInstanceDomain(): string {
		try {
			const url = new URL(this.apiEndpoint);
			return url.hostname;
		} catch {
			return 'localhost';
		}
	}
}

export function describeApiEndpoint(endpoint: string): string {
	try {
		const url = new URL(endpoint);
		const path = url.pathname === '/api' ? '' : url.pathname;
		return `${url.host}${path}`;
	} catch {
		return endpoint;
	}
}

export default new RuntimeConfigStore();
