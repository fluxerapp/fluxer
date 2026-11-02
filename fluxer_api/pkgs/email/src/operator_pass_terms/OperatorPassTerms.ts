// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OPERATOR_PASS_TERMS_TEXT,
	type OperatorPassTermsBlock,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTermsText';

export const OPERATOR_PASS_TERMS_VERSION = '2026-11-01';

export type OperatorPassTermsLanguage = keyof typeof OPERATOR_PASS_TERMS_TEXT;

const OPERATOR_PASS_TERMS_BY_VERSION: Readonly<Record<string, typeof OPERATOR_PASS_TERMS_TEXT>> = {
	[OPERATOR_PASS_TERMS_VERSION]: OPERATOR_PASS_TERMS_TEXT,
};

const OPERATOR_PASS_TERMS_PATH = '/operator-pass-terms';

export const OPERATOR_PASS_TRADER_EMAIL = 'support@fluxer.com';

export const OPERATOR_PASS_TRADER_PHONE = '+46 79 101 18 18';

const TITLES: Record<OperatorPassTermsLanguage, string> = {
	'en-US': 'Fluxer Operator Pass terms',
	'sv-SE': 'Villkor för Fluxer Operator Pass',
	'pt-BR': 'Termos do Operator Pass do Fluxer',
};

const VERSION_LABELS: Record<OperatorPassTermsLanguage, string> = {
	'en-US': 'Version',
	'sv-SE': 'Version',
	'pt-BR': 'Versão',
};

const SELLER_ADDRESSES: Record<OperatorPassTermsLanguage, string> = {
	'en-US': 'c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden',
	'sv-SE': 'c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sverige',
	'pt-BR': 'c/o Embassy House, Östgötagatan 12, 116 25 Estocolmo, Suécia',
};

const TRADER_LINES: Record<OperatorPassTermsLanguage, ReadonlyArray<string>> = {
	'en-US': [
		'Fluxer Platform AB, organization number 559537-3993, registered office (säte) Haninge, Sweden',
		SELLER_ADDRESSES['en-US'],
		`Phone ${OPERATOR_PASS_TRADER_PHONE}`,
		OPERATOR_PASS_TRADER_EMAIL,
	],
	'sv-SE': [
		'Fluxer Platform AB, organisationsnummer 559537-3993, säte Haninge',
		SELLER_ADDRESSES['sv-SE'],
		`Telefon ${OPERATOR_PASS_TRADER_PHONE}`,
		OPERATOR_PASS_TRADER_EMAIL,
	],
	'pt-BR': [
		'Fluxer Platform AB, número de registro sueco 559537-3993, sede em Haninge, Suécia',
		SELLER_ADDRESSES['pt-BR'],
		`Telefone ${OPERATOR_PASS_TRADER_PHONE}`,
		OPERATOR_PASS_TRADER_EMAIL,
	],
};

export function getOperatorPassSellerAddress(locale: string | null | undefined): string {
	const normalized = locale?.toLowerCase() ?? '';
	if (normalized.startsWith('sv')) return SELLER_ADDRESSES['sv-SE'];
	if (normalized.startsWith('pt')) return SELLER_ADDRESSES['pt-BR'];
	return SELLER_ADDRESSES['en-US'];
}

const MARKDOWN_LINK = /\[([^\]]+)\]\(([^)]+)\)/gu;
const ANGLE_LINK = /<([^>\s]+@[^>\s]+)>/gu;

export function resolveOperatorPassTermsLanguage(
	locale: string | null | undefined,
	withdrawalCountry: string | null = null,
): OperatorPassTermsLanguage {
	if (withdrawalCountry === 'BR') return 'pt-BR';
	const normalized = locale?.toLowerCase() ?? '';
	if (normalized.startsWith('sv')) return 'sv-SE';
	return 'en-US';
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

export function hasOperatorPassTermsVersion(version: string): boolean {
	return Object.hasOwn(OPERATOR_PASS_TERMS_BY_VERSION, version);
}

export function getOperatorPassTermsBlocks(
	language: OperatorPassTermsLanguage,
	version: string,
): ReadonlyArray<OperatorPassTermsBlock> {
	if (!hasOperatorPassTermsVersion(version)) {
		throw new Error(`No Operator Pass terms text for version ${version}`);
	}
	return OPERATOR_PASS_TERMS_BY_VERSION[version][language];
}

export function getOperatorPassTermsVersionLabel(language: OperatorPassTermsLanguage, version: string): string {
	return `${VERSION_LABELS[language]} ${version}`;
}
