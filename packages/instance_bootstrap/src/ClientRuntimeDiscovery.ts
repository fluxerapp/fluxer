// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type InstanceDiscoveryDocument,
	type InstanceGeneration,
	InvalidInstanceDiscoveryDocumentError,
} from '@fluxer/instance_bootstrap/src/Discovery';
import type {
	InstanceAgePolicy,
	InstanceAppPublic,
	InstanceCommunity,
	InstanceDomainMigration,
	InstanceFeatures,
	InstanceGif,
	InstancePush,
	InstanceRegistration,
	InstanceServices,
	InstanceSso,
} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';

const DEFAULT_PREMIUM_PRODUCT_NAME = 'Plutonium';

export interface ClientRuntimeDiscoveryEndpoints {
	readonly discoveryApiEndpoint: string;
	readonly apiEndpoint: string;
	readonly apiPublicEndpoint: string;
	readonly gatewayEndpoint: string;
	readonly mediaEndpoint: string;
	readonly uploadRelayEndpoint: string | null;
	readonly staticCdnEndpoint: string;
	readonly marketingEndpoint: string;
	readonly docsEndpoint: string | null;
	readonly adminEndpoint: string;
	readonly inviteEndpoint: string;
	readonly giftEndpoint: string;
	readonly webAppEndpoint: string;
}

export interface ClientRuntimeDiscovery {
	readonly generation: InstanceGeneration;
	readonly apiCodeVersion: number;
	readonly endpoints: ClientRuntimeDiscoveryEndpoints;
	readonly features: InstanceFeatures;
	readonly gif: InstanceGif;
	readonly sso: InstanceSso | null;
	readonly registration: InstanceRegistration;
	readonly community: InstanceCommunity;
	readonly services: InstanceServices;
	readonly limits: LimitConfigSnapshot | LimitConfigWireFormat;
	readonly push: InstancePush;
	readonly appPublic: InstanceAppPublic;
	readonly agePolicy: InstanceAgePolicy | null;
	readonly domainMigration: InstanceDomainMigration | null;
}

export function projectClientRuntimeDiscovery(document: InstanceDiscoveryDocument): ClientRuntimeDiscovery {
	return {
		generation: document.generation,
		apiCodeVersion: requireValue(document.apiCodeVersion, 'api_code_version'),
		endpoints: projectEndpoints(document),
		features: projectFeatures(document.features),
		gif: requireValue(document.gif, 'gif'),
		sso: document.sso,
		registration: requireValue(document.registration, 'registration'),
		community: requireValue(document.community, 'community'),
		services: requireValue(document.services, 'services'),
		limits: requireValue(document.limits, 'limits'),
		push: projectPush(document.push),
		appPublic: projectAppPublic(document.appPublic),
		agePolicy: document.agePolicy,
		domainMigration: document.domainMigration,
	};
}

function projectEndpoints(document: InstanceDiscoveryDocument): ClientRuntimeDiscoveryEndpoints {
	const endpoints = document.endpoints;
	return {
		discoveryApiEndpoint: endpoints.api,
		apiEndpoint: requireValue(endpoints.api_client, 'endpoints.api_client'),
		apiPublicEndpoint: requireValue(endpoints.api_public, 'endpoints.api_public'),
		gatewayEndpoint: endpoints.gateway,
		mediaEndpoint: requireValue(endpoints.media, 'endpoints.media'),
		uploadRelayEndpoint: endpoints.upload_relay,
		staticCdnEndpoint: requireValue(endpoints.static_cdn, 'endpoints.static_cdn'),
		marketingEndpoint: requireValue(endpoints.marketing, 'endpoints.marketing'),
		docsEndpoint: endpoints.docs,
		adminEndpoint: requireValue(endpoints.admin, 'endpoints.admin'),
		inviteEndpoint: requireValue(endpoints.invite, 'endpoints.invite'),
		giftEndpoint: requireValue(endpoints.gift, 'endpoints.gift'),
		webAppEndpoint: requireValue(endpoints.webapp, 'endpoints.webapp'),
	};
}

function projectFeatures(features: Readonly<Partial<InstanceFeatures>>): InstanceFeatures {
	return {
		...features,
		voice_enabled: requireDefined(features.voice_enabled, 'features.voice_enabled'),
		stripe_enabled: requireDefined(features.stripe_enabled, 'features.stripe_enabled'),
		self_hosted: requireDefined(features.self_hosted, 'features.self_hosted'),
		presigned_attachment_uploads: requireDefined(
			features.presigned_attachment_uploads,
			'features.presigned_attachment_uploads',
		),
		emails_enabled: requireDefined(features.emails_enabled, 'features.emails_enabled'),
		premium_enabled: features.premium_enabled ?? !features.self_hosted,
		stripe_serviceable: features.stripe_serviceable ?? false,
		phone_verification_enabled: false,
	};
}

function projectPush(push: Readonly<Partial<InstancePush>>): InstancePush {
	return {
		...push,
		public_vapid_key: requireDefined(push.public_vapid_key, 'push.public_vapid_key'),
	};
}

function projectAppPublic(appPublic: InstanceDiscoveryDocument['appPublic']): InstanceAppPublic {
	const value = requireValue(appPublic, 'app_public');
	return {
		branding: {
			product_name: requireDefined(value.branding.product_name, 'app_public.branding.product_name'),
			icon_url: requireDefined(value.branding.icon_url, 'app_public.branding.icon_url'),
			symbol_url: requireDefined(value.branding.symbol_url, 'app_public.branding.symbol_url'),
			logo_url: requireDefined(value.branding.logo_url, 'app_public.branding.logo_url'),
			wordmark_url: requireDefined(value.branding.wordmark_url, 'app_public.branding.wordmark_url'),
			favicon_url: requireDefined(value.branding.favicon_url, 'app_public.branding.favicon_url'),
			theme_color: requireDefined(value.branding.theme_color, 'app_public.branding.theme_color'),
			status_page_url: value.branding.status_page_url ?? null,
			status_page_incident_history_url: value.branding.status_page_incident_history_url ?? null,
			premium_product_name: value.branding.premium_product_name ?? DEFAULT_PREMIUM_PRODUCT_NAME,
			premium_info_url: value.branding.premium_info_url ?? null,
		},
		setup: {
			configured: requireDefined(value.setup.configured, 'app_public.setup.configured'),
			admin_url: requireDefined(value.setup.admin_url, 'app_public.setup.admin_url'),
		},
		legal: {
			terms_url: requireDefined(value.legal.terms_url, 'app_public.legal.terms_url'),
			privacy_url: requireDefined(value.legal.privacy_url, 'app_public.legal.privacy_url'),
		},
		registration: {
			collect_date_of_birth: requireDefined(
				value.registration.collect_date_of_birth,
				'app_public.registration.collect_date_of_birth',
			),
		},
	};
}

function requireValue<T>(value: T | null, path: string): T {
	if (value === null) {
		throw new InvalidInstanceDiscoveryDocumentError(`${path} is required by the client runtime`);
	}
	return value;
}

function requireDefined<T>(value: T | undefined, path: string): T {
	if (value === undefined) {
		throw new InvalidInstanceDiscoveryDocumentError(`${path} is required by the client runtime`);
	}
	return value;
}
