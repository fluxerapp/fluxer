// SPDX-License-Identifier: AGPL-3.0-or-later

import {connectionProviderFingerprint, type StoredConnectionProvider} from '@app/api/connection/ConnectionProviders';
import {isDisplayableLabel, isHttpsSsoUrl, validateSsoPublicOutboundUrl} from '@app/api/instance/SsoConfigValidation';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ConnectionProviderUpdate} from '@fluxer/schema/src/domains/admin/AdminSchemas';

export type ValidatedConnectionProvider = Omit<StoredConnectionProvider, 'client_secret'> & {
	client_secret: string | undefined;
};

const PLACEHOLDER_PATTERN = /\{(id|name)\}/g;

function emptyToNull(value: string | null | undefined): string | null {
	const trimmed = value?.trim();
	return trimmed ? trimmed : null;
}

async function validateUrl(
	value: string | null | undefined,
	field: string,
	{required, saved, checkReachable}: {required: boolean; saved: string | null; checkReachable: boolean},
): Promise<string | null> {
	const url = emptyToNull(value);
	if (url === null) {
		if (required) throw InputValidationError.create(field, 'This URL is required for this kind of provider');
		return null;
	}
	if (checkReachable && url !== saved) return validateSsoPublicOutboundUrl(url, field);
	if (!isHttpsSsoUrl(url)) throw InputValidationError.create(field, 'Use an HTTPS URL');
	return url;
}

function validateIconUrl(value: string | null | undefined, field: string): string | null {
	const url = emptyToNull(value);
	if (url === null) return null;
	if (URL.parse(url)?.protocol !== 'https:') throw InputValidationError.create(field, 'The icon must be an HTTPS URL');
	return url;
}

function validateProfileUrlTemplate(value: string | null | undefined, field: string): string | null {
	const template = emptyToNull(value);
	if (template === null) return null;
	const filled = URL.parse(template.replace(PLACEHOLDER_PATTERN, 'x'));
	const authority = template.slice('https://'.length).split(/[/?#]/, 1)[0] ?? '';
	if (
		!template.startsWith('https://') ||
		filled?.protocol !== 'https:' ||
		filled.username ||
		filled.password ||
		authority.includes('{') ||
		/\{(?!id\}|name\})/.test(template)
	) {
		throw InputValidationError.create(
			field,
			'The profile link must be an HTTPS URL, with {id} or {name} only after the host',
		);
	}
	return template;
}

async function validateProvider(
	provider: ConnectionProviderUpdate,
	index: number,
	saved: StoredConnectionProvider | undefined,
): Promise<ValidatedConnectionProvider> {
	const field = (name: string) => `integrations.connection_providers.${index}.${name}`;
	const url = (name: 'issuer' | 'authorization_url' | 'token_url' | 'userinfo_url', required: boolean) =>
		validateUrl(provider[name], field(name), {
			required,
			saved: saved?.[name] ?? null,
			checkReachable: provider.enabled,
		});
	if (!isDisplayableLabel(provider.name)) {
		throw InputValidationError.create(field('name'), 'The name has characters that cannot be shown');
	}
	const scopes = provider.scope.split(/\s+/).filter(Boolean);
	const oidc = provider.kind === 'oidc';
	if (oidc && !scopes.includes('openid')) {
		throw InputValidationError.create(field('scope'), 'OpenID Connect providers need the openid scope');
	}
	if (!oidc && !emptyToNull(provider.id_path)) {
		throw InputValidationError.create(field('id_path'), 'Set the path to the account id in the user object');
	}
	return {
		id: provider.id,
		name: provider.name,
		enabled: provider.enabled,
		kind: provider.kind,
		issuer: oidc ? await url('issuer', true) : null,
		authorization_url: await url('authorization_url', !oidc),
		token_url: await url('token_url', !oidc),
		userinfo_url: await url('userinfo_url', !oidc),
		client_id: provider.client_id,
		client_secret: provider.client_secret,
		scope: scopes.join(' '),
		name_claim: oidc ? emptyToNull(provider.name_claim) : null,
		id_path: oidc ? null : emptyToNull(provider.id_path),
		name_path: oidc ? null : emptyToNull(provider.name_path),
		profile_url_template: validateProfileUrlTemplate(provider.profile_url_template, field('profile_url_template')),
		icon_url: validateIconUrl(provider.icon_url, field('icon_url')),
	};
}

export async function validateConnectionProviders(
	providers: ReadonlyArray<ConnectionProviderUpdate>,
	current: ReadonlyArray<StoredConnectionProvider>,
): Promise<Array<ValidatedConnectionProvider>> {
	const seen = new Set<string>();
	const validated: Array<ValidatedConnectionProvider> = [];
	for (const [index, provider] of providers.entries()) {
		if (seen.has(provider.id)) {
			throw InputValidationError.create(
				`integrations.connection_providers.${index}.id`,
				'Each provider needs its own id',
			);
		}
		seen.add(provider.id);
		validated.push(
			await validateProvider(
				provider,
				index,
				current.find((saved) => saved.id === provider.id),
			),
		);
	}
	return validated;
}

export function mergeConnectionProviderSecrets(
	next: ReadonlyArray<ValidatedConnectionProvider>,
	current: ReadonlyArray<StoredConnectionProvider>,
): Array<StoredConnectionProvider> {
	return next.map((provider, index) => {
		if (provider.client_secret !== undefined) return {...provider, client_secret: provider.client_secret};
		const saved = current.find((candidate) => candidate.id === provider.id);
		if (
			!saved?.client_secret ||
			saved.token_url !== provider.token_url ||
			connectionProviderFingerprint(saved) !== connectionProviderFingerprint(provider)
		) {
			throw InputValidationError.create(
				`integrations.connection_providers.${index}.client_secret`,
				'Enter the client secret for this provider',
			);
		}
		return {...provider, client_secret: saved.client_secret};
	});
}
