// SPDX-License-Identifier: AGPL-3.0-or-later

export type SectionLabel = 'API' | 'Self-hosting' | 'Downloads';

export interface Section {
	readonly label: SectionLabel;
	readonly href: string;
}

export const sections: ReadonlyArray<Section> = [
	{label: 'API', href: '/http-api/'},
	{label: 'Self-hosting', href: '/operator/'},
	{label: 'Downloads', href: '/downloads/overview/'},
];

export function isHome(id: string): boolean {
	return id === '' || id === 'index';
}

export function isNotFound(id: string): boolean {
	return id === '404';
}

export function sectionOf(id: string): SectionLabel | undefined {
	if (isHome(id) || isNotFound(id)) {
		return undefined;
	}
	if (id === 'operator' || id.startsWith('operator/')) {
		return 'Self-hosting';
	}
	if (id.startsWith('downloads/')) {
		return 'Downloads';
	}
	return 'API';
}

export function withBase(href: string): string {
	if (!href.startsWith('/')) {
		return href;
	}
	return `${import.meta.env.BASE_URL.replace(/\/$/, '')}${href}`;
}

export function sectionOfHref(href: string): SectionLabel | undefined {
	const base = import.meta.env.BASE_URL.replace(/\/$/, '');
	const path = href.startsWith(base) ? href.slice(base.length) : href;
	return sectionOf(path.replace(/^\/+|\/+$/g, ''));
}
