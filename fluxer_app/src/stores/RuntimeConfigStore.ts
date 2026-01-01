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

import {makeAutoObservable, reaction, runInAction} from 'mobx';
import Config from '~/Config';
import {API_CODE_VERSION} from '~/Constants';
import type {HttpRequestConfig} from '~/lib/HttpClient';
import HttpClient from '~/lib/HttpClient';
import {makePersistent} from '~/lib/MobXPersistence';
import {wrapUrlWithElectronApiProxy} from '~/utils/ApiProxyUtils';
import DeveloperOptionsStore from './DeveloperOptionsStore';

const DEFAULT_FLUXER_GATEWAY_HOSTS = new Set(['gateway.fluxer.app', 'gateway.canary.fluxer.app']);

export interface InstanceFeatures {
	sms_mfa_enabled: boolean;
	voice_enabled: boolean;
	stripe_enabled: boolean;
	self_hosted: boolean;
}

export interface InstanceEndpoints {
	api: string;
	api_client?: string;
	api_public?: string;
	gateway: string;
	media: string;
	cdn: string;
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

export interface InstanceDiscoveryResponse {
	api_code_version: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	push?: InstancePush;
}

export interface RuntimeConfigSnapshot {
	apiEndpoint: string;
	apiPublicEndpoint: string;
	gatewayEndpoint: string;
	mediaEndpoint: string;
	cdnEndpoint: string;
	marketingEndpoint: string;
	adminEndpoint: string;
	inviteEndpoint: string;
	giftEndpoint: string;
	webAppEndpoint: string;
	captchaProvider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptchaSiteKey: string | null;
	turnstileSiteKey: string | null;
	apiCodeVersion: number;
	features: InstanceFeatures;
	publicPushVapidKey: string | null;
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
	cdnEndpoint: string = '';
	marketingEndpoint: string = '';
	adminEndpoint: string = '';
	inviteEndpoint: string = '';
	giftEndpoint: string = '';
	webAppEndpoint: string = '';

	captchaProvider: 'hcaptcha' | 'turnstile' | 'none' = 'none';
	hcaptchaSiteKey: string | null = null;
	turnstileSiteKey: string | null = null;

	apiCodeVersion: number = API_CODE_VERSION;
	features: InstanceFeatures = {
		sms_mfa_enabled: false,
		voice_enabled: false,
		stripe_enabled: false,
		self_hosted: false,
	};
	publicPushVapidKey: string | null = null;

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
					HttpClient.setBaseUrl(endpoint, Config.PUBLIC_API_VERSION);
				}
			},
			{fireImmediately: true},
		);
	}

	private async initialize(): Promise<void> {
		try {
			await makePersistent(this, 'runtimeConfig', [
				'apiEndpoint',
				'apiPublicEndpoint',
				'gatewayEndpoint',
				'mediaEndpoint',
				'cdnEndpoint',
				'marketingEndpoint',
				'adminEndpoint',
				'inviteEndpoint',
				'giftEndpoint',
				'webAppEndpoint',
				'captchaProvider',
				'hcaptchaSiteKey',
				'turnstileSiteKey',
				'apiCodeVersion',
				'features',
				'publicPushVapidKey',
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
		this.cdnEndpoint = snapshot.cdnEndpoint;
		this.marketingEndpoint = snapshot.marketingEndpoint;
		this.adminEndpoint = snapshot.adminEndpoint;
		this.inviteEndpoint = snapshot.inviteEndpoint;
		this.giftEndpoint = snapshot.giftEndpoint;
		this.webAppEndpoint = snapshot.webAppEndpoint;

		this.captchaProvider = snapshot.captchaProvider;
		this.hcaptchaSiteKey = snapshot.hcaptchaSiteKey;
		this.turnstileSiteKey = snapshot.turnstileSiteKey;

		this.apiCodeVersion = snapshot.apiCodeVersion;
		this.features = snapshot.features;
		this.publicPushVapidKey = snapshot.publicPushVapidKey;
	}

	getSnapshot(): RuntimeConfigSnapshot {
		return {
			apiEndpoint: this.apiEndpoint,
			apiPublicEndpoint: this.apiPublicEndpoint,
			gatewayEndpoint: this.gatewayEndpoint,
			mediaEndpoint: this.mediaEndpoint,
			cdnEndpoint: this.cdnEndpoint,
			marketingEndpoint: this.marketingEndpoint,
			adminEndpoint: this.adminEndpoint,
			inviteEndpoint: this.inviteEndpoint,
			giftEndpoint: this.giftEndpoint,
			webAppEndpoint: this.webAppEndpoint,
			captchaProvider: this.captchaProvider,
			hcaptchaSiteKey: this.hcaptchaSiteKey,
			turnstileSiteKey: this.turnstileSiteKey,
			apiCodeVersion: this.apiCodeVersion,
			features: {...this.features},
			publicPushVapidKey: this.publicPushVapidKey,
		};
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
		const instanceUrl = `${apiEndpoint}/instance`;

		const requestUrl = wrapUrlWithElectronApiProxy(instanceUrl);
		const request: HttpRequestConfig = {url: requestUrl};

		const response = await HttpClient.get<InstanceDiscoveryResponse>(request);

		if (connectId !== this._connectSeq) {
			return;
		}

		if (!response.ok) {
			throw new Error(`Failed to reach ${instanceUrl} (${response.status})`);
		}

		this.updateFromInstance(response.body);
	}

	private normalizeEndpoint(input: string): string {
		const trimmed = input.trim();
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

		runInAction(() => {
			this.apiEndpoint = apiEndpoint;
			this.apiPublicEndpoint = apiPublicEndpoint;

			this.gatewayEndpoint = instance.endpoints.gateway;
			this.mediaEndpoint = instance.endpoints.media;
			this.cdnEndpoint = instance.endpoints.cdn;
			this.marketingEndpoint = instance.endpoints.marketing;
			this.adminEndpoint = instance.endpoints.admin;
			this.inviteEndpoint = instance.endpoints.invite;
			this.giftEndpoint = instance.endpoints.gift;
			this.webAppEndpoint = instance.endpoints.webapp;

			this.captchaProvider = instance.captcha.provider;
			this.hcaptchaSiteKey = instance.captcha.hcaptcha_site_key;
			this.turnstileSiteKey = instance.captcha.turnstile_site_key;

			this.apiCodeVersion = instance.api_code_version;
			this.features = instance.features;
			this.publicPushVapidKey = instance.push?.public_vapid_key ?? null;
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

	isThirdPartyGateway(): boolean {
		if (!this.gatewayEndpoint) return false;
		try {
			const url = new URL(this.gatewayEndpoint);
			return !DEFAULT_FLUXER_GATEWAY_HOSTS.has(url.hostname);
		} catch {
			return false;
		}
	}

	hasElectronWsProxy(): boolean {
		return typeof window.electron?.getWsProxyUrl === 'function';
	}

	private getWsProxyBaseUrl(): URL | null {
		if (!this.hasElectronWsProxy()) {
			return null;
		}

		const raw = window.electron?.getWsProxyUrl();
		if (!raw) return null;

		try {
			return new URL(raw);
		} catch {
			return null;
		}
	}

	wrapGatewayUrlWithProxy(url: string): string {
		if (!this.isThirdPartyGateway()) {
			return url;
		}

		const proxy = this.getWsProxyBaseUrl();
		if (!proxy) {
			return url;
		}

		proxy.searchParams.set('target', url);
		return proxy.toString();
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
