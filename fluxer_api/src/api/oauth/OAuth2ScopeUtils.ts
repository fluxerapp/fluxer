// SPDX-License-Identifier: AGPL-3.0-or-later

import {type OAuth2Scope, OAuth2Scopes} from '@fluxer/constants/src/OAuth2Constants';

const OAUTH2_SCOPE_SET = new Set<string>(OAuth2Scopes);
const OAUTH2_SCOPE_ORDER: ReadonlyArray<string> = OAuth2Scopes;

export function parseOAuth2Scope(scope: string): Array<string> {
	return scope.split(/[\s+]+/).filter(Boolean);
}

export function isOAuth2Scope(scope: string): scope is OAuth2Scope {
	return OAUTH2_SCOPE_SET.has(scope);
}

export function filterOAuth2Scopes(scopes: Iterable<string>): Array<OAuth2Scope> {
	return [...scopes].filter(isOAuth2Scope);
}

export function filterOAuth2ScopeSet(scopes: Iterable<string>): Set<string> {
	return new Set(filterOAuth2Scopes(scopes));
}

export function sortOAuth2Scopes(scopes: Iterable<string>): Array<string> {
	return [...scopes].sort((a, b) => {
		const ai = OAUTH2_SCOPE_ORDER.indexOf(a);
		const bi = OAUTH2_SCOPE_ORDER.indexOf(b);
		if (ai === -1 && bi === -1) return a.localeCompare(b);
		if (ai === -1) return 1;
		if (bi === -1) return -1;
		return ai - bi;
	});
}
