// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {MAX_CONNECTION_PROVIDERS} from '@fluxer/constants/src/ConnectionConstants';
import {z} from 'zod';

type ConnectionProviderKind = 'oidc' | 'oauth2';

export interface StoredConnectionProvider {
	id: string;
	name: string;
	enabled: boolean;
	kind: ConnectionProviderKind;
	issuer: string | null;
	authorization_url: string | null;
	token_url: string | null;
	userinfo_url: string | null;
	client_id: string;
	client_secret: string;
	scope: string;
	name_claim: string | null;
	id_path: string | null;
	name_path: string | null;
	profile_url_template: string | null;
	icon_url: string | null;
}

export interface ConnectionProviderEntry {
	provider: StoredConnectionProvider;
	fingerprint: string;
}

export type ConnectionProviderIndex = ReadonlyMap<string, ConnectionProviderEntry>;

interface OAuthConnectionIdentifier {
	providerId: string;
	fingerprint: string;
	subject: string;
}

const NullableString = z.string().nullable().default(null);

const StoredConnectionProviderSchema = z.object({
	id: z.string().regex(/^[a-z0-9][a-z0-9_-]{0,31}$/),
	name: z.string().min(1),
	enabled: z.boolean(),
	kind: z.enum(['oidc', 'oauth2']),
	issuer: NullableString,
	authorization_url: NullableString,
	token_url: NullableString,
	userinfo_url: NullableString,
	client_id: z.string().min(1),
	client_secret: z.string().min(1),
	scope: z.string().min(1),
	name_claim: NullableString,
	id_path: NullableString,
	name_path: NullableString,
	profile_url_template: NullableString,
	icon_url: NullableString,
});

const IDENTIFIER_PATTERN = /^([a-z0-9][a-z0-9_-]{0,31}):([0-9a-f]{8}):(.+)$/s;
const MAX_SUBJECT_LENGTH = 200;
const MAX_ACCOUNT_NAME_LENGTH = 64;
const UNSAFE_TEXT_CHARACTER = /[\p{Cc}\u2028\u2029\u202A-\u202E\u2066-\u2069]/u;
const UNSAFE_TEXT_CHARACTERS = new RegExp(UNSAFE_TEXT_CHARACTER.source, 'gu');

export function parseStoredConnectionProviders(raw: string | null): Array<StoredConnectionProvider> {
	if (raw === null) return [];
	let value: unknown;
	try {
		value = JSON.parse(raw);
	} catch {
		Logger.error('Stored connection providers are not valid JSON');
		return [];
	}
	if (!Array.isArray(value)) return [];
	const providers: Array<StoredConnectionProvider> = [];
	for (const entry of value.slice(0, MAX_CONNECTION_PROVIDERS)) {
		const parsed = StoredConnectionProviderSchema.safeParse(entry);
		if (!parsed.success) {
			Logger.error('Stored connection provider has invalid fields and is skipped');
			continue;
		}
		if (providers.some((provider) => provider.id === parsed.data.id)) continue;
		providers.push(parsed.data);
	}
	return providers;
}

function connectionProviderIdentityOrigin(
	provider: Pick<StoredConnectionProvider, 'kind' | 'issuer' | 'userinfo_url'>,
): string {
	if (provider.kind === 'oidc') return (provider.issuer ?? '').replace(/\/+$/, '');
	return URL.parse(provider.userinfo_url ?? '')?.origin ?? '';
}

export function connectionProviderFingerprint(
	provider: Pick<StoredConnectionProvider, 'kind' | 'issuer' | 'userinfo_url'>,
): string {
	return createHash('sha256').update(connectionProviderIdentityOrigin(provider)).digest('hex').slice(0, 8);
}

export function buildConnectionProviderIndex(
	providers: ReadonlyArray<StoredConnectionProvider>,
): ConnectionProviderIndex {
	return new Map(
		providers.map((provider) => [provider.id, {provider, fingerprint: connectionProviderFingerprint(provider)}]),
	);
}

export function buildOAuthConnectionIdentifier(entry: ConnectionProviderEntry, subject: string): string {
	return `${entry.provider.id}:${entry.fingerprint}:${subject}`;
}

export function parseOAuthConnectionIdentifier(identifier: string): OAuthConnectionIdentifier | null {
	const match = IDENTIFIER_PATTERN.exec(identifier);
	if (!match) return null;
	return {providerId: match[1]!, fingerprint: match[2]!, subject: match[3]!};
}

export function findAvailableConnectionProvider(
	identifier: string,
	providers: ConnectionProviderIndex,
): {entry: ConnectionProviderEntry; parsed: OAuthConnectionIdentifier} | null {
	const parsed = parseOAuthConnectionIdentifier(identifier);
	if (!parsed) return null;
	const entry = providers.get(parsed.providerId);
	if (!entry?.provider.enabled || entry.fingerprint !== parsed.fingerprint) return null;
	return {entry, parsed};
}

export function connectionCallbackUrl(providerId: string): string {
	return `${Config.endpoints.apiPublic.replace(/\/+$/, '')}/connections/oauth/${providerId}/callback`;
}

export function normalizeConnectionSubject(value: unknown): string | null {
	const subject = typeof value === 'number' && Number.isSafeInteger(value) ? String(value) : value;
	if (typeof subject !== 'string' || subject.length === 0 || subject.length > MAX_SUBJECT_LENGTH) return null;
	return UNSAFE_TEXT_CHARACTER.test(subject) ? null : subject;
}

export function normalizeConnectionAccountName(value: unknown, subject: string): string {
	if (typeof value !== 'string') return subject.slice(0, MAX_ACCOUNT_NAME_LENGTH);
	const name = [...value.replace(UNSAFE_TEXT_CHARACTERS, '').trim()].slice(0, MAX_ACCOUNT_NAME_LENGTH).join('').trim();
	return name.length > 0 ? name : subject.slice(0, MAX_ACCOUNT_NAME_LENGTH);
}

export function readJsonPath(value: unknown, path: string): unknown {
	let current = value;
	for (const segment of path.split('.')) {
		if (Array.isArray(current) && /^\d+$/.test(segment)) {
			current = current[Number(segment)];
		} else if (current !== null && typeof current === 'object' && !Array.isArray(current)) {
			current = Object.hasOwn(current, segment) ? (current as Record<string, unknown>)[segment] : undefined;
		} else {
			return undefined;
		}
	}
	return current;
}

export function renderProfileUrl(template: string | null, subject: string, name: string): string | null {
	if (!template) return null;
	const url = URL.parse(
		template.replaceAll('{id}', encodeURIComponent(subject)).replaceAll('{name}', encodeURIComponent(name)),
	);
	return url?.protocol === 'https:' ? url.toString() : null;
}
