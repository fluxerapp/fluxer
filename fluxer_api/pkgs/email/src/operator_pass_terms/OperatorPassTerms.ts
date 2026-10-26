// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OPERATOR_PASS_TERMS_TEXT,
	type OperatorPassTermsBlock,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTermsText';

export const OPERATOR_PASS_TERMS_VERSION = '2026-10-26';

export type OperatorPassTermsLanguage = keyof typeof OPERATOR_PASS_TERMS_TEXT;

const OPERATOR_PASS_TERMS_PATH = '/operator-pass-terms';

export const OPERATOR_PASS_TRADER_EMAIL = 'support@fluxer.com';

const TITLES: Record<OperatorPassTermsLanguage, string> = {
	'en-US': 'Fluxer Operator Pass terms',
	'sv-SE': 'Villkor för Fluxer Operator Pass',
};

const VERSION_LABELS: Record<OperatorPassTermsLanguage, string> = {
	'en-US': 'Version',
	'sv-SE': 'Version',
};

const TRADER_LINES: Record<OperatorPassTermsLanguage, ReadonlyArray<string>> = {
	'en-US': [
		'Fluxer Platform AB, organization number 559537-3993',
		'c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden',
		OPERATOR_PASS_TRADER_EMAIL,
	],
	'sv-SE': [
		'Fluxer Platform AB, organisationsnummer 559537-3993',
		'c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sverige',
		OPERATOR_PASS_TRADER_EMAIL,
	],
};

const MARKDOWN_LINK = /\[([^\]]+)\]\(([^)]+)\)/gu;
const ANGLE_LINK = /<([^>\s]+@[^>\s]+)>/gu;

export function resolveOperatorPassTermsLanguage(locale: string | null | undefined): OperatorPassTermsLanguage {
	return locale?.toLowerCase().startsWith('sv') ? 'sv-SE' : 'en-US';
}

export function getOperatorPassTermsTitle(language: OperatorPassTermsLanguage): string {
	return TITLES[language];
}

export function getOperatorPassTraderLines(language: OperatorPassTermsLanguage): ReadonlyArray<string> {
	return TRADER_LINES[language];
}

export function getOperatorPassTermsUrl(marketingBaseUrl: string): string {
	return `${marketingBaseUrl.replace(/\/+$/u, '')}${OPERATOR_PASS_TERMS_PATH}`;
}

export function toPlainTermsText(text: string, marketingBaseUrl: string): string {
	const base = marketingBaseUrl.replace(/\/+$/u, '');
	return text
		.replace(MARKDOWN_LINK, (_match, label: string, href: string) => {
			const url = href.startsWith('/') ? `${base}${href}` : href;
			const bareUrl = url.replace(/^https?:\/\//u, '');
			return label === bareUrl || label === url ? url : `${label} (${url})`;
		})
		.replace(ANGLE_LINK, '$1');
}

export function getOperatorPassTermsBlocks(language: OperatorPassTermsLanguage): ReadonlyArray<OperatorPassTermsBlock> {
	return OPERATOR_PASS_TERMS_TEXT[language];
}

export function getOperatorPassTermsVersionLabel(language: OperatorPassTermsLanguage): string {
	return `${VERSION_LABELS[language]} ${OPERATOR_PASS_TERMS_VERSION}`;
}
