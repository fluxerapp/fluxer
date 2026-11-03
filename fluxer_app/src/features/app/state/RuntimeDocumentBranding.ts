// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_APP_SHELL_BRANDING} from '@app/features/app/state/AppShellBranding';
import {getServedDocumentBranding} from '@app/features/app/state/ServedDocumentBranding';
import {setDocumentTitleProductName} from '@app/features/window/hooks/useFluxerDocumentTitle';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';

const LIGHT_SCHEME_MEDIA = '(prefers-color-scheme: light)';
const DARK_SCHEME_MEDIA = '(prefers-color-scheme: dark)';

type IconSlot = 'light' | 'dark';

export interface DocumentFavicons {
	readonly light: string | null;
	readonly dark: string | null;
}

function brandedIcon(slot: IconSlot): HTMLLinkElement | null {
	return document.head.querySelector<HTMLLinkElement>(`link[rel="icon"][data-fluxer-branding="${slot}"]`);
}

function setIcon(slot: IconSlot, href: string, media: string | null): void {
	const existing = brandedIcon(slot);
	const link = existing ?? document.createElement('link');
	link.rel = 'icon';
	link.href = href;
	if (media === null) {
		link.removeAttribute('media');
	} else {
		link.media = media;
	}
	link.dataset.fluxerBranding = slot;
	if (existing === null) {
		document.head.appendChild(link);
	}
}

function removeIcon(slot: IconSlot): void {
	brandedIcon(slot)?.remove();
}

function suspendDefaultIcons(): void {
	for (const link of document.head.querySelectorAll<HTMLLinkElement>('link[rel="icon"]:not([data-fluxer-branding])')) {
		link.dataset.fluxerDefaultRel = 'icon';
		link.rel = '';
	}
}

function restoreDefaultIcons(): void {
	for (const link of document.head.querySelectorAll<HTMLLinkElement>('link[data-fluxer-default-rel="icon"]')) {
		link.rel = 'icon';
		delete link.dataset.fluxerDefaultRel;
	}
}

function defaultLightIconHref(): string | null {
	for (const link of document.head.querySelectorAll<HTMLLinkElement>('link[data-fluxer-default-rel="icon"]')) {
		if (link.getAttribute('media') !== DARK_SCHEME_MEDIA) return link.getAttribute('href');
	}
	return null;
}

function applyFavicons({light, dark}: DocumentFavicons): void {
	if (light === null && dark === null) {
		removeIcon('light');
		removeIcon('dark');
		restoreDefaultIcons();
		return;
	}
	suspendDefaultIcons();
	const lightHref = light ?? defaultLightIconHref();
	if (lightHref === null) {
		removeIcon('light');
	} else {
		setIcon('light', lightHref, dark === null ? null : LIGHT_SCHEME_MEDIA);
	}
	if (dark === null) {
		removeIcon('dark');
		return;
	}
	setIcon('dark', dark, DARK_SCHEME_MEDIA);
	const lightLink = brandedIcon('light');
	const darkLink = brandedIcon('dark');
	if (lightLink !== null && darkLink !== null) {
		document.head.insertBefore(darkLink, lightLink);
	}
}

function mediaMatches(media: string): boolean {
	if (media === '' || typeof window.matchMedia !== 'function') return true;
	return window.matchMedia(media).matches;
}

export function findActiveIconLink(): HTMLLinkElement | null {
	const links = document.head.querySelectorAll<HTMLLinkElement>('link');
	for (let index = links.length - 1; index >= 0; index--) {
		const link = links[index];
		if (/(^|\s)icon(\s|$)/i.test(link.getAttribute('rel') ?? '') && mediaMatches(link.getAttribute('media') ?? '')) {
			return link;
		}
	}
	return null;
}

function brandedMeta(name: string): HTMLMetaElement | null {
	return document.head.querySelector<HTMLMetaElement>(`meta[name="${name}"][data-fluxer-branding="true"]`);
}

function setMeta(name: string, content: string): void {
	const existing = brandedMeta(name);
	const meta = existing ?? document.createElement('meta');
	meta.name = name;
	meta.content = content;
	meta.dataset.fluxerBranding = 'true';
	if (existing === null) {
		document.head.insertBefore(meta, document.head.querySelector<HTMLMetaElement>(`meta[name="${name}"]`));
	}
}

function removeMeta(name: string): void {
	brandedMeta(name)?.remove();
}

export function getDocumentFavicons(appPublic: InstanceAppPublic | null): DocumentFavicons {
	if (appPublic === null) return {light: DEFAULT_APP_SHELL_BRANDING.faviconUrl, dark: null};
	return {
		light: appPublic.branding.favicon_url ?? appPublic.branding.icon_url,
		dark: appPublic.branding.favicon_dark_url,
	};
}

export function applyRuntimeDocumentBranding(appPublic: InstanceAppPublic): void {
	if (typeof document === 'undefined') return;
	const productName = appPublic.branding.product_name;
	setDocumentTitleProductName(productName);
	setMeta('application-name', productName);
	setMeta('apple-mobile-web-app-title', productName);
	applyFavicons(getDocumentFavicons(appPublic));
	if (appPublic.branding.theme_color === null) {
		removeMeta('theme-color');
	} else {
		setMeta('theme-color', appPublic.branding.theme_color);
	}
}

export function applyServedDocumentBranding(): void {
	if (typeof document === 'undefined') return;
	setDocumentTitleProductName(getServedDocumentBranding().productName);
	removeMeta('application-name');
	removeMeta('apple-mobile-web-app-title');
	removeMeta('theme-color');
	applyFavicons({light: null, dark: null});
}
