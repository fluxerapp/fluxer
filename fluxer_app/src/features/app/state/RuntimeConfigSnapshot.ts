// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GifProvider} from '@app/features/app/state/GifProviderConfig';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {LIMIT_KEYS, type LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {
	ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MAX,
	ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MIN,
	AGE_POLICY_GEOS_MAX,
	LIMIT_RULES_MAX,
	MAX_BACKGROUND_GATEWAY_CONNECTIONS_MAX,
	MAX_BACKGROUND_GATEWAY_CONNECTIONS_MIN,
	parseInstanceDomainMigration,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {CanonicalNetworkProtocol} from '@fluxer/instance_bootstrap/src/NetworkOrigin';
import {
	AGE_POLICY_ACTIONS,
	type InstanceAgePolicy,
	type InstanceAgePolicyGeo,
	type InstanceAppPublic,
	type InstanceCommunity,
	type InstanceDomainMigration,
	type InstanceFeatures,
	type InstanceRegistration,
	type InstanceServices,
	type InstanceSso,
	REGISTRATION_MODES,
} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot, LimitFilter, LimitRule} from '@fluxer/limits/src/LimitTypes';

const LIMIT_KEYS_SET = new Set<string>(LIMIT_KEYS);
const DEFAULT_PREMIUM_PRODUCT_NAME = 'Plutonium';

export interface RuntimeConfigIdentity {
	readonly apiEndpoint: string;
}

export interface RuntimeConfigSnapshot extends RuntimeConfigIdentity {
	readonly apiPublicEndpoint: string;
	readonly gatewayEndpoint: string;
	readonly mediaEndpoint: string;
	readonly staticCdnEndpoint: string;
	readonly marketingEndpoint: string;
	readonly adminEndpoint: string;
	readonly inviteEndpoint: string;
	readonly giftEndpoint: string;
	readonly webAppEndpoint: string;
	readonly uploadRelayEndpoint: string | null;
	readonly gifProvider: GifProvider;
	readonly gifProviderDisplayName: string;
	readonly gifAttributionRequired: boolean;
	readonly apiCodeVersion: number;
	readonly features: InstanceFeatures;
	readonly sso: InstanceSso | null;
	readonly registration: InstanceRegistration;
	readonly community: InstanceCommunity;
	readonly services: InstanceServices;
	readonly publicPushVapidKey: string | null;
	readonly limits: LimitConfigSnapshot;
	readonly appPublic: InstanceAppPublic;
	readonly agePolicy: InstanceAgePolicy | null;
	readonly domainMigration: InstanceDomainMigration | null;
}

export class InvalidRuntimeConfigSnapshotError extends Error {
	readonly path: string;

	constructor(path: string, requirement: string) {
		super(`Runtime snapshot ${path} ${requirement}`);
		this.name = 'InvalidRuntimeConfigSnapshotError';
		this.path = path;
	}
}

function invalid(path: string, requirement: string): never {
	throw new InvalidRuntimeConfigSnapshotError(path, requirement);
}

function readRecord(value: unknown, path: string): Record<string, unknown> {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		invalid(path, 'must be an object');
	}
	return value as Record<string, unknown>;
}

function readBoolean(source: Record<string, unknown>, key: string, path: string): boolean {
	const value = source[key];
	if (typeof value !== 'boolean') {
		invalid(path, 'must be a boolean');
	}
	return value;
}

function readOptionalBoolean(source: Record<string, unknown>, key: string, path: string): boolean | undefined {
	if (!Object.hasOwn(source, key)) {
		return undefined;
	}
	return readBoolean(source, key, path);
}

function readNonEmptyString(source: Record<string, unknown>, key: string, path: string): string {
	const value = source[key];
	if (typeof value !== 'string' || value.trim().length === 0) {
		invalid(path, 'must be a non-empty string');
	}
	return value;
}

function readNullableNonEmptyString(source: Record<string, unknown>, key: string, path: string): string | null {
	const value = source[key];
	if (value === null) {
		return null;
	}
	return readNonEmptyString(source, key, path);
}

function readOptionalText(source: Record<string, unknown>, key: string): string | null {
	const value = source[key];
	if (typeof value !== 'string' || value.trim().length === 0) {
		return null;
	}
	return value;
}

function readPositiveInteger(source: Record<string, unknown>, key: string, path: string): number {
	const value = source[key];
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value <= 0) {
		invalid(path, 'must be a positive safe integer');
	}
	return value;
}

function readBoundedInteger(
	source: Record<string, unknown>,
	key: string,
	path: string,
	minimum: number,
	maximum: number,
): number {
	const value = source[key];
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < minimum || value > maximum) {
		invalid(path, `must be a safe integer from ${minimum.toString()} through ${maximum.toString()}`);
	}
	return value;
}

function readOptionalBoundedInteger(
	source: Record<string, unknown>,
	key: string,
	path: string,
	minimum: number,
	maximum: number,
): number | undefined {
	if (!Object.hasOwn(source, key)) {
		return undefined;
	}
	return readBoundedInteger(source, key, path, minimum, maximum);
}

function readLiteral<TValue extends string>(
	source: Record<string, unknown>,
	key: string,
	path: string,
	values: ReadonlyArray<TValue>,
): TValue {
	const value = source[key];
	const matched = values.find((candidate) => candidate === value);
	if (matched === undefined) {
		invalid(path, `must be one of ${values.join(', ')}`);
	}
	return matched;
}

function readEndpoint(source: Record<string, unknown>, key: string, path: string, kind: InstanceEndpointKind): string {
	const normalized = normalizeInstanceEndpoint(source[key], kind);
	if (normalized === null || normalized.startsWith('/')) {
		invalid(path, 'must be an absolute network endpoint');
	}
	return normalized;
}

function readNullableEndpoint(source: Record<string, unknown>, key: string, path: string): string | null {
	const value = source[key];
	if (value === null) {
		return null;
	}
	const normalized = normalizeInstanceEndpoint(value, InstanceEndpointKind.SERVICE);
	if (normalized === null) {
		invalid(path, 'must be null or an absolute network endpoint');
	}
	return normalized;
}

function readNullableAbsoluteHttpUrl(source: Record<string, unknown>, key: string, path: string): string | null {
	const value = source[key];
	if (value === null) {
		return null;
	}
	if (typeof value !== 'string') {
		invalid(path, 'must be null or an absolute HTTP(S) URL');
	}
	const trimmed = value.trim();
	let url: URL;
	try {
		url = new URL(trimmed);
	} catch {
		invalid(path, 'must be null or an absolute HTTP(S) URL');
	}
	if (url.protocol !== CanonicalNetworkProtocol.HTTP && url.protocol !== CanonicalNetworkProtocol.HTTPS) {
		invalid(path, 'must be null or an absolute HTTP(S) URL');
	}
	return trimmed;
}

function readStringArray(value: unknown, path: string): Array<string> {
	if (!Array.isArray(value) || value.some((entry) => typeof entry !== 'string')) {
		invalid(path, 'must be an array of strings');
	}
	return [...value];
}

function readFeatures(value: unknown): InstanceFeatures {
	const source = readRecord(value, 'features');
	const features: InstanceFeatures = {
		voice_enabled: readBoolean(source, 'voice_enabled', 'features.voice_enabled'),
		stripe_enabled: readBoolean(source, 'stripe_enabled', 'features.stripe_enabled'),
		self_hosted: readBoolean(source, 'self_hosted', 'features.self_hosted'),
		presigned_attachment_uploads: readBoolean(
			source,
			'presigned_attachment_uploads',
			'features.presigned_attachment_uploads',
		),
		emails_enabled: readBoolean(source, 'emails_enabled', 'features.emails_enabled'),
		premium_enabled: false,
		stripe_serviceable: readOptionalBoolean(source, 'stripe_serviceable', 'features.stripe_serviceable') ?? false,
		phone_verification_enabled: false,
	};
	features.premium_enabled =
		readOptionalBoolean(source, 'premium_enabled', 'features.premium_enabled') ?? !features.self_hosted;
	const desktopModulesEnabled = readOptionalBoolean(
		source,
		'desktop_modules_enabled',
		'features.desktop_modules_enabled',
	);
	if (desktopModulesEnabled !== undefined) {
		features.desktop_modules_enabled = desktopModulesEnabled;
	}
	const deletionGracePeriod = readOptionalBoundedInteger(
		source,
		'account_deletion_grace_period_hours',
		'features.account_deletion_grace_period_hours',
		ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MIN,
		ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MAX,
	);
	if (deletionGracePeriod !== undefined) {
		features.account_deletion_grace_period_hours = deletionGracePeriod;
	}
	if (source.account_identity !== undefined) {
		features.account_identity = readLiteral(
			source,
			'account_identity',
			'features.account_identity',
			Object.values(AccountIdentityModes),
		);
	}
	if (source.tag_style !== undefined) {
		features.tag_style = readLiteral(source, 'tag_style', 'features.tag_style', Object.values(TagStyles));
	}
	const maxBackgroundConnections = readOptionalBoundedInteger(
		source,
		'max_background_gateway_connections',
		'features.max_background_gateway_connections',
		MAX_BACKGROUND_GATEWAY_CONNECTIONS_MIN,
		MAX_BACKGROUND_GATEWAY_CONNECTIONS_MAX,
	);
	if (maxBackgroundConnections !== undefined) {
		features.max_background_gateway_connections = maxBackgroundConnections;
	}
	return features;
}

function readSso(value: unknown): InstanceSso | null {
	if (value === null) {
		return null;
	}
	const source = readRecord(value, 'sso');
	const sso: InstanceSso = {
		enabled: readBoolean(source, 'enabled', 'sso.enabled'),
		enforced: readBoolean(source, 'enforced', 'sso.enforced'),
		display_name: readNullableNonEmptyString(source, 'display_name', 'sso.display_name'),
		redirect_uri: readEndpoint(source, 'redirect_uri', 'sso.redirect_uri', InstanceEndpointKind.SERVICE),
	};
	const available = readOptionalBoolean(source, 'available', 'sso.available');
	if (available !== undefined) {
		sso.available = available;
	}
	return sso;
}

function readRegistration(value: unknown): InstanceRegistration {
	const source = readRecord(value, 'registration');
	return {
		mode: readLiteral(source, 'mode', 'registration.mode', REGISTRATION_MODES),
		admin_registration_urls_enabled: readBoolean(
			source,
			'admin_registration_urls_enabled',
			'registration.admin_registration_urls_enabled',
		),
	};
}

function readCommunity(value: unknown): InstanceCommunity {
	const source = readRecord(value, 'community');
	return {
		single_community: readBoolean(source, 'single_community', 'community.single_community'),
		single_community_guild_id: readNullableNonEmptyString(
			source,
			'single_community_guild_id',
			'community.single_community_guild_id',
		),
		direct_messages_disabled: readBoolean(source, 'direct_messages_disabled', 'community.direct_messages_disabled'),
		guild_create_access: readOptionalBoolean(source, 'guild_create_access', 'community.guild_create_access') ?? true,
	};
}

function readServices(value: unknown): InstanceServices {
	const source = readRecord(value, 'services');
	return {
		gif_enabled: readBoolean(source, 'gif_enabled', 'services.gif_enabled'),
		youtube_enabled: readBoolean(source, 'youtube_enabled', 'services.youtube_enabled'),
		bluesky_enabled: readBoolean(source, 'bluesky_enabled', 'services.bluesky_enabled'),
	};
}

function readLimitFilter(value: unknown, path: string): LimitFilter {
	const source = readRecord(value, path);
	const filter: LimitFilter = {};
	if (Object.hasOwn(source, 'traits')) {
		filter.traits = readStringArray(source.traits, `${path}.traits`);
	}
	if (Object.hasOwn(source, 'guildFeatures')) {
		filter.guildFeatures = readStringArray(source.guildFeatures, `${path}.guildFeatures`);
	}
	return filter;
}

function readLimitValues(value: unknown, path: string): Partial<Record<LimitKey, number>> {
	const source = readRecord(value, path);
	const result: Partial<Record<LimitKey, number>> = {};
	for (const [key, entry] of Object.entries(source)) {
		if (!LIMIT_KEYS_SET.has(key)) {
			continue;
		}
		if (typeof entry !== 'number' || !Number.isFinite(entry)) {
			invalid(`${path}.${key}`, 'must be a finite number');
		}
		result[key as LimitKey] = entry;
	}
	return result;
}

function readLimitRule(value: unknown, index: number): LimitRule {
	const path = `limits.rules[${index.toString()}]`;
	const source = readRecord(value, path);
	const rule: LimitRule = {
		id: readNonEmptyString(source, 'id', `${path}.id`),
		limits: readLimitValues(source.limits, `${path}.limits`),
	};
	if (Object.hasOwn(source, 'filters')) {
		rule.filters = readLimitFilter(source.filters, `${path}.filters`);
	}
	if (Object.hasOwn(source, 'modifiedFields')) {
		const modifiedFields = readStringArray(source.modifiedFields, `${path}.modifiedFields`);
		rule.modifiedFields = modifiedFields.filter((key): key is LimitKey => LIMIT_KEYS_SET.has(key));
	}
	return rule;
}

function readLimits(value: unknown): LimitConfigSnapshot {
	const source = readRecord(value, 'limits');
	const traitDefinitions = readStringArray(source.traitDefinitions, 'limits.traitDefinitions');
	if (!Array.isArray(source.rules) || source.rules.length > LIMIT_RULES_MAX) {
		invalid('limits.rules', `must be an array with at most ${LIMIT_RULES_MAX.toString()} entries`);
	}
	const snapshot: LimitConfigSnapshot = {
		traitDefinitions,
		rules: source.rules.map(readLimitRule),
	};
	if (Object.hasOwn(source, 'version')) {
		snapshot.version = readPositiveInteger(source, 'version', 'limits.version');
	}
	return snapshot;
}

function readAppPublic(value: unknown): InstanceAppPublic {
	const source = readRecord(value, 'appPublic');
	const branding = readRecord(source.branding, 'appPublic.branding');
	const setup = readRecord(source.setup, 'appPublic.setup');
	const legal = readRecord(source.legal, 'appPublic.legal');
	const registration = readRecord(source.registration, 'appPublic.registration');
	return {
		branding: {
			product_name: readNonEmptyString(branding, 'product_name', 'appPublic.branding.product_name'),
			icon_url: readNullableAbsoluteHttpUrl(branding, 'icon_url', 'appPublic.branding.icon_url'),
			symbol_url: readNullableAbsoluteHttpUrl(branding, 'symbol_url', 'appPublic.branding.symbol_url'),
			logo_url: readNullableAbsoluteHttpUrl(branding, 'logo_url', 'appPublic.branding.logo_url'),
			wordmark_url: readNullableAbsoluteHttpUrl(branding, 'wordmark_url', 'appPublic.branding.wordmark_url'),
			favicon_url: readNullableAbsoluteHttpUrl(branding, 'favicon_url', 'appPublic.branding.favicon_url'),
			theme_color: readNullableNonEmptyString(branding, 'theme_color', 'appPublic.branding.theme_color'),
			status_page_url: readOptionalText(branding, 'status_page_url'),
			status_page_incident_history_url: readOptionalText(branding, 'status_page_incident_history_url'),
			premium_product_name: readOptionalText(branding, 'premium_product_name') ?? DEFAULT_PREMIUM_PRODUCT_NAME,
			premium_info_url: Object.hasOwn(branding, 'premium_info_url')
				? readNullableAbsoluteHttpUrl(branding, 'premium_info_url', 'appPublic.branding.premium_info_url')
				: null,
		},
		setup: {
			configured: readBoolean(setup, 'configured', 'appPublic.setup.configured'),
			admin_url: readNullableAbsoluteHttpUrl(setup, 'admin_url', 'appPublic.setup.admin_url'),
		},
		legal: {
			terms_url: readNullableAbsoluteHttpUrl(legal, 'terms_url', 'appPublic.legal.terms_url'),
			privacy_url: readNullableAbsoluteHttpUrl(legal, 'privacy_url', 'appPublic.legal.privacy_url'),
		},
		registration: {
			collect_date_of_birth: readBoolean(
				registration,
				'collect_date_of_birth',
				'appPublic.registration.collect_date_of_birth',
			),
		},
	};
}

function readAgePolicyGeo(value: unknown, index: number): InstanceAgePolicyGeo {
	const path = `agePolicy.geos[${index.toString()}]`;
	const source = readRecord(value, path);
	return {
		country_code: readNonEmptyString(source, 'country_code', `${path}.country_code`),
		region_code: readNullableNonEmptyString(source, 'region_code', `${path}.region_code`),
		action: readLiteral(source, 'action', `${path}.action`, AGE_POLICY_ACTIONS),
		card_verification_available: readBoolean(
			source,
			'card_verification_available',
			`${path}.card_verification_available`,
		),
	};
}

function readAgePolicy(value: unknown): InstanceAgePolicy | null {
	if (value === null) {
		return null;
	}
	const source = readRecord(value, 'agePolicy');
	if (!Array.isArray(source.geos) || source.geos.length > AGE_POLICY_GEOS_MAX) {
		invalid('agePolicy.geos', `must be an array with at most ${AGE_POLICY_GEOS_MAX.toString()} entries`);
	}
	return {geos: source.geos.map(readAgePolicyGeo)};
}

export function requireRuntimeConfigSnapshot(value: unknown): RuntimeConfigSnapshot {
	const source = readRecord(value, 'root');
	return {
		apiEndpoint: readEndpoint(source, 'apiEndpoint', 'apiEndpoint', InstanceEndpointKind.SERVICE),
		apiPublicEndpoint: readEndpoint(source, 'apiPublicEndpoint', 'apiPublicEndpoint', InstanceEndpointKind.SERVICE),
		gatewayEndpoint: readEndpoint(source, 'gatewayEndpoint', 'gatewayEndpoint', InstanceEndpointKind.GATEWAY),
		mediaEndpoint: readEndpoint(source, 'mediaEndpoint', 'mediaEndpoint', InstanceEndpointKind.SERVICE),
		staticCdnEndpoint: readEndpoint(source, 'staticCdnEndpoint', 'staticCdnEndpoint', InstanceEndpointKind.SERVICE),
		marketingEndpoint: readEndpoint(source, 'marketingEndpoint', 'marketingEndpoint', InstanceEndpointKind.SERVICE),
		adminEndpoint: readEndpoint(source, 'adminEndpoint', 'adminEndpoint', InstanceEndpointKind.SERVICE),
		inviteEndpoint: readEndpoint(source, 'inviteEndpoint', 'inviteEndpoint', InstanceEndpointKind.SERVICE),
		giftEndpoint: readEndpoint(source, 'giftEndpoint', 'giftEndpoint', InstanceEndpointKind.SERVICE),
		webAppEndpoint: readEndpoint(source, 'webAppEndpoint', 'webAppEndpoint', InstanceEndpointKind.WEBAPP),
		uploadRelayEndpoint: readNullableEndpoint(source, 'uploadRelayEndpoint', 'uploadRelayEndpoint'),
		gifProvider: readNonEmptyString(source, 'gifProvider', 'gifProvider'),
		gifProviderDisplayName: readNonEmptyString(source, 'gifProviderDisplayName', 'gifProviderDisplayName'),
		gifAttributionRequired: readBoolean(source, 'gifAttributionRequired', 'gifAttributionRequired'),
		apiCodeVersion: readPositiveInteger(source, 'apiCodeVersion', 'apiCodeVersion'),
		features: readFeatures(source.features),
		sso: readSso(source.sso),
		registration: readRegistration(source.registration),
		community: readCommunity(source.community),
		services: readServices(source.services),
		publicPushVapidKey: readNullableNonEmptyString(source, 'publicPushVapidKey', 'publicPushVapidKey'),
		limits: readLimits(source.limits),
		appPublic: readAppPublic(source.appPublic),
		agePolicy: readAgePolicy(source.agePolicy),
		domainMigration: parseInstanceDomainMigration(source.domainMigration),
	};
}
