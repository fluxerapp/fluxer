// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_GIF_PROVIDER_INFO,
	type GifProvider,
	type GifProviderInfo,
	type GifProviderInfoInput,
	normalizeGifProviderInfo,
} from '@app/features/app/state/GifProviderConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {http} from '@app/features/platform/transport/RestTransport';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import type {
	InstanceAppPublic,
	InstanceCaptcha,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceFeatures,
	InstanceRegistration,
	InstanceServices,
	InstanceSso as InstanceSsoConfig,
} from '@fluxer/instance_bootstrap/src/Types';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

export type {
	InstanceCaptcha,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceFeatures,
	InstanceRegistration,
	InstanceServices,
	InstanceSsoConfig,
};
export type {GifProvider, GifProviderInfo};

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
	gifProviderDisplayName: string;
	gifAttributionRequired: boolean;
	captchaProvider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptchaSiteKey: string | null;
	turnstileSiteKey: string | null;
	apiCodeVersion: number;
	features: InstanceFeatures;
	sso: InstanceSsoConfig | null;
	registration?: InstanceRegistration;
	community?: InstanceCommunity;
	services?: InstanceServices;
	publicPushVapidKey: string | null;
	limits: LimitConfigSnapshot;
	appPublic: InstanceAppPublic;
}

function runtimeInstanceKey(snapshot: RuntimeConfigSnapshot): string | null {
	try {
		const endpoint = snapshot.apiEndpoint.trim();
		const url = new URL(endpoint);
		if (
			(url.protocol !== 'https:' && url.protocol !== 'http:') ||
			url.username ||
			url.password ||
			url.search ||
			url.hash
		) {
			return null;
		}
		const path = url.pathname.replace(/\/+$/u, '');
		return `${url.origin.toLowerCase()}${path}`;
	} catch {
		return null;
	}
}

export function runtimeConfigSnapshotsAreSameInstance(
	left: RuntimeConfigSnapshot | undefined,
	right: RuntimeConfigSnapshot,
): boolean {
	if (!left) {
		return false;
	}
	const leftKey = runtimeInstanceKey(left);
	const rightKey = runtimeInstanceKey(right);
	return leftKey !== null && leftKey === rightKey;
}

const DEFAULT_INSTANCE_FEATURES: InstanceFeatures = {
	voice_enabled: false,
	stripe_enabled: false,
	self_hosted: false,
	presigned_attachment_uploads: false,
	emails_enabled: false,
};

export const DEFAULT_INSTANCE_REGISTRATION: InstanceRegistration = {
	mode: 'open',
	admin_registration_urls_enabled: true,
};

export const DEFAULT_INSTANCE_COMMUNITY: InstanceCommunity = {
	single_community: false,
	single_community_guild_id: null,
	direct_messages_disabled: false,
};

export function normalizeInstanceCommunity(community?: InstanceCommunity | null): InstanceCommunity {
	return {
		...DEFAULT_INSTANCE_COMMUNITY,
		...(community ?? {}),
	};
}

export const DEFAULT_INSTANCE_SERVICES: InstanceServices = {
	gif_enabled: true,
	youtube_enabled: false,
	bluesky_enabled: false,
};

export function normalizeInstanceServices(services?: InstanceServices | null): InstanceServices {
	return {
		...DEFAULT_INSTANCE_SERVICES,
		...(services ?? {}),
	};
}

export const DEFAULT_APP_PUBLIC_CONFIG: InstanceAppPublic = {
	branding: {
		product_name: 'Fluxer',
		icon_url: null,
		symbol_url: null,
		logo_url: null,
		wordmark_url: null,
		favicon_url: null,
		theme_color: null,
	},
	setup: {
		configured: false,
		admin_url: null,
	},
	legal: {
		terms_url: null,
		privacy_url: null,
	},
	registration: {
		collect_date_of_birth: true,
	},
};

export function normalizeInstanceRegistration(registration?: InstanceRegistration | null): InstanceRegistration {
	return {
		...DEFAULT_INSTANCE_REGISTRATION,
		...(registration ?? {}),
	};
}

function getInlinedInstance(): InstanceDiscoveryResponse {
	const bootstrap = typeof window !== 'undefined' ? window.__FLUXER_BOOTSTRAP__ : undefined;
	if (!bootstrap) {
		throw new Error('window.__FLUXER_BOOTSTRAP__ is missing — app must be served by fluxer_app_proxy');
	}
	return bootstrap.instance;
}

let lastAppliedDocumentProductName = DEFAULT_APP_PUBLIC_CONFIG.branding.product_name;

function escapeRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
}

function upsertDocumentLink(rel: string, href: string): void {
	if (typeof document === 'undefined') return;
	const selector = `link[rel="${rel}"][data-fluxer-branding="true"]`;
	const existing = document.head.querySelector<HTMLLinkElement>(selector);
	const link = existing ?? document.createElement('link');
	link.rel = rel;
	link.href = href;
	link.dataset.fluxerBranding = 'true';
	if (!existing) {
		document.head.appendChild(link);
	}
}

function removeDocumentLink(rel: string): void {
	if (typeof document === 'undefined') return;
	document.head.querySelectorAll<HTMLLinkElement>(`link[rel="${rel}"][data-fluxer-branding="true"]`).forEach((link) => {
		link.remove();
	});
}

function upsertDocumentMeta(name: string, content: string): void {
	if (typeof document === 'undefined') return;
	const brandedMeta = document.head.querySelector<HTMLMetaElement>(`meta[name="${name}"][data-fluxer-branding="true"]`);
	const meta = brandedMeta ?? document.createElement('meta');
	meta.name = name;
	meta.content = content;
	meta.dataset.fluxerBranding = 'true';
	if (brandedMeta) return;
	const precedingMeta = document.head.querySelector<HTMLMetaElement>(`meta[name="${name}"]`);
	document.head.insertBefore(meta, precedingMeta);
}

function removeDocumentMeta(name: string): void {
	if (typeof document === 'undefined') return;
	document.head
		.querySelectorAll<HTMLMetaElement>(`meta[name="${name}"][data-fluxer-branding="true"]`)
		.forEach((meta) => {
			meta.remove();
		});
}

function applyDocumentTitleProductName(previousProductName: string, nextProductName: string): void {
	if (typeof document === 'undefined' || previousProductName === nextProductName) return;
	const productPrefix = new RegExp(
		`^(\\(\\d+\\)\\s+|\\u2022\\s+)?${escapeRegExp(previousProductName)}(?=$| \\| )`,
		'u',
	);
	if (productPrefix.test(document.title)) {
		document.title = document.title.replace(productPrefix, (_match, prefix: string | undefined) => {
			return `${prefix ?? ''}${nextProductName}`;
		});
	}
}

function applyDocumentBranding(appPublic: InstanceAppPublic): void {
	if (typeof document === 'undefined') return;
	const productName = appPublic.branding.product_name.trim() || DEFAULT_APP_PUBLIC_CONFIG.branding.product_name;
	applyDocumentTitleProductName(lastAppliedDocumentProductName, productName);
	lastAppliedDocumentProductName = productName;
	upsertDocumentMeta('application-name', productName);
	upsertDocumentMeta('apple-mobile-web-app-title', productName);
	const faviconUrl = appPublic.branding.favicon_url ?? appPublic.branding.icon_url;
	if (faviconUrl) {
		upsertDocumentLink('icon', faviconUrl);
	} else {
		removeDocumentLink('icon');
	}
	if (appPublic.branding.theme_color) {
		upsertDocumentMeta('theme-color', appPublic.branding.theme_color);
	} else {
		removeDocumentMeta('theme-color');
	}
}

export function normalizeAppPublicConfig(appPublic?: Partial<InstanceAppPublic> | null): InstanceAppPublic {
	return {
		branding: {
			...DEFAULT_APP_PUBLIC_CONFIG.branding,
			...(appPublic?.branding ?? {}),
		},
		setup: {
			...DEFAULT_APP_PUBLIC_CONFIG.setup,
			...(appPublic?.setup ?? {}),
		},
		legal: {
			...DEFAULT_APP_PUBLIC_CONFIG.legal,
			...(appPublic?.legal ?? {}),
		},
		registration: {
			...DEFAULT_APP_PUBLIC_CONFIG.registration,
			...(appPublic?.registration ?? {}),
		},
	};
}

class RuntimeConfig {
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
	gifProvider: GifProvider = DEFAULT_GIF_PROVIDER_INFO.name;
	gifProviderDisplayName: string = DEFAULT_GIF_PROVIDER_INFO.displayName;
	gifAttributionRequired: boolean = DEFAULT_GIF_PROVIDER_INFO.attributionRequired;
	captchaProvider: 'hcaptcha' | 'turnstile' | 'none' = 'none';
	hcaptchaSiteKey: string | null = null;
	turnstileSiteKey: string | null = null;
	apiCodeVersion: number = API_CODE_VERSION;
	features: InstanceFeatures = {...DEFAULT_INSTANCE_FEATURES};
	sso: InstanceSsoConfig | null = null;
	registration: InstanceRegistration = {...DEFAULT_INSTANCE_REGISTRATION};
	community: InstanceCommunity = {...DEFAULT_INSTANCE_COMMUNITY};
	services: InstanceServices = {...DEFAULT_INSTANCE_SERVICES};
	publicPushVapidKey: string | null = null;
	limits: LimitConfigSnapshot = this.createEmptyLimitConfig();
	currentDefaultsHash: string | null = null;
	appPublic: InstanceAppPublic = normalizeAppPublicConfig();

	constructor() {
		this.updateFromInstance(getInlinedInstance());
		makeAutoObservable(this, {}, {autoBind: true});
		reaction(
			() => this.apiEndpoint,
			(endpoint) => {
				if (endpoint) {
					http.configure({baseUrl: endpoint, apiVersion: this.apiCodeVersion});
				}
			},
			{fireImmediately: true},
		);
	}

	waitForInit(): Promise<void> {
		return Promise.resolve();
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
			gifProviderDisplayName: this.gifProviderDisplayName,
			gifAttributionRequired: this.gifAttributionRequired,
			captchaProvider: this.captchaProvider,
			hcaptchaSiteKey: this.hcaptchaSiteKey,
			turnstileSiteKey: this.turnstileSiteKey,
			apiCodeVersion: this.apiCodeVersion,
			features: {...this.features},
			sso: this.sso ? {...this.sso} : null,
			registration: {...this.registration},
			community: {...this.community},
			services: {...this.services},
			publicPushVapidKey: this.publicPushVapidKey,
			limits: this.cloneLimits(this.limits),
			appPublic: normalizeAppPublicConfig(this.appPublic),
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

	applyAdminInstanceConfig(config: InstanceConfigResponse): void {
		const appPublic = normalizeAppPublicConfig({
			branding: config.app_public.branding,
			setup: {
				...config.app_public.setup,
				admin_url: this.appPublic.setup.admin_url,
			},
			legal: config.app_public.legal,
			registration: config.app_public.registration,
		});
		runInAction(() => {
			this.features = {
				...this.features,
				self_hosted: config.self_hosted,
			};
			this.registration = normalizeInstanceRegistration(config.registration);
			this.community = normalizeInstanceCommunity({
				single_community: config.policy.single_community_enabled,
				single_community_guild_id: config.policy.single_community_enabled
					? config.policy.single_community_guild_id
					: null,
				direct_messages_disabled: config.policy.direct_messages_disabled,
			});
			this.services = normalizeInstanceServices({
				gif_enabled: config.policy.services_resolved.gif_enabled,
				youtube_enabled: config.policy.services_resolved.youtube_enabled,
				bluesky_enabled: config.policy.services_resolved.bluesky_enabled,
			});
			this.appPublic = appPublic;
		});
		applyDocumentBranding(appPublic);
	}

	private updateFromInstance(instance: InstanceDiscoveryResponse): void {
		this.assertCodeVersion(instance.api_code_version);
		const apiEndpoint = instance.endpoints.api_client ?? instance.endpoints.api;
		const apiPublicEndpoint = instance.endpoints.api_public ?? apiEndpoint;
		const sso = instance.sso ?? null;
		const appPublic = normalizeAppPublicConfig(instance.app_public);
		const gifProviderInfo = normalizeGifProviderInfo({
			provider: instance.gif?.provider,
			attributionRequired: instance.gif?.attribution_required,
		});
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
			this.gifProvider = gifProviderInfo.name;
			this.gifProviderDisplayName = gifProviderInfo.displayName;
			this.gifAttributionRequired = gifProviderInfo.attributionRequired;
			this.captchaProvider = instance.captcha.provider;
			this.hcaptchaSiteKey = instance.captcha.hcaptcha_site_key;
			this.turnstileSiteKey = instance.captcha.turnstile_site_key;
			this.apiCodeVersion = instance.api_code_version;
			this.features = {
				...DEFAULT_INSTANCE_FEATURES,
				...instance.features,
			};
			this.sso = sso;
			this.registration = normalizeInstanceRegistration(instance.registration);
			this.community = normalizeInstanceCommunity(instance.community);
			this.services = normalizeInstanceServices(instance.services);
			this.publicPushVapidKey = instance.push?.public_vapid_key ?? null;
			this.limits = this.processLimitsFromApi(instance.limits);
			this.appPublic = appPublic;
		});
		applyDocumentBranding(appPublic);
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
		return DeveloperOptions.selfHostedModeOverride || this.features.self_hosted;
	}

	get emailsEnabled(): boolean {
		return this.features.emails_enabled;
	}

	get productName(): string {
		return this.appPublic.branding.product_name.trim() || DEFAULT_APP_PUBLIC_CONFIG.branding.product_name;
	}

	get iconUrl(): string | null {
		return this.appPublic.branding.icon_url;
	}

	get symbolUrl(): string | null {
		return this.appPublic.branding.symbol_url ?? this.iconUrl;
	}

	get logoUrl(): string | null {
		return this.appPublic.branding.logo_url ?? this.iconUrl;
	}

	get wordmarkUrl(): string | null {
		return this.appPublic.branding.wordmark_url;
	}

	get faviconUrl(): string | null {
		return this.appPublic.branding.favicon_url ?? this.iconUrl;
	}

	get themeColor(): string | null {
		return this.appPublic.branding.theme_color;
	}

	get termsUrl(): string | null {
		return this.appPublic.legal.terms_url;
	}

	get privacyUrl(): string | null {
		return this.appPublic.legal.privacy_url;
	}

	get collectDateOfBirthOnRegistration(): boolean {
		return this.appPublic.registration.collect_date_of_birth;
	}

	get setupAdminUrl(): string | null {
		return this.appPublic.setup.admin_url;
	}

	requiresSelfHostedSetup(): boolean {
		return this.isSelfHosted() && !this.appPublic.setup.configured;
	}

	get singleCommunityEnabled(): boolean {
		return this.community.single_community;
	}

	get singleCommunityGuildId(): string | null {
		return this.community.single_community ? this.community.single_community_guild_id : null;
	}

	get directMessagesDisabled(): boolean {
		return this.community.direct_messages_disabled;
	}

	get gifEnabled(): boolean {
		return this.services.gif_enabled;
	}

	get blueskyConnectionsEnabled(): boolean {
		return this.services.bluesky_enabled;
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

	applyGifProviderHeaders(input: GifProviderInfoInput): void {
		const info = normalizeGifProviderInfo(input);
		if (
			this.gifProvider === info.name &&
			this.gifProviderDisplayName === info.displayName &&
			this.gifAttributionRequired === info.attributionRequired
		) {
			return;
		}
		runInAction(() => {
			this.gifProvider = info.name;
			this.gifProviderDisplayName = info.displayName;
			this.gifAttributionRequired = info.attributionRequired;
		});
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

export default new RuntimeConfig();
