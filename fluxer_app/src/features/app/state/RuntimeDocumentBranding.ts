// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_APP_SHELL_BRANDING} from '@app/features/app/state/AppShellBranding';
import {setDocumentTitleProductName} from '@app/features/window/hooks/useFluxerDocumentTitle';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';

function brandedLink(rel: string): HTMLLinkElement | null {
	return document.head.querySelector<HTMLLinkElement>(`link[rel="${rel}"][data-fluxer-branding="true"]`);
}

function setLink(rel: string, href: string): void {
	const existing = brandedLink(rel);
	const link = existing ?? document.createElement('link');
	link.rel = rel;
	link.href = href;
	link.dataset.fluxerBranding = 'true';
	if (existing === null) {
		document.head.appendChild(link);
	}
}

function removeLink(rel: string): void {
	brandedLink(rel)?.remove();
}

function suspendDefaultLinks(rel: string): void {
	for (const link of document.head.querySelectorAll<HTMLLinkElement>(
		`link[rel="${rel}"]:not([data-fluxer-branding])`,
	)) {
		link.dataset.fluxerDefaultRel = rel;
		link.rel = '';
	}
}

function restoreDefaultLinks(rel: string): void {
	for (const link of document.head.querySelectorAll<HTMLLinkElement>(`link[data-fluxer-default-rel="${rel}"]`)) {
		link.rel = rel;
		delete link.dataset.fluxerDefaultRel;
	}
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

function applyDocumentBranding(productName: string, faviconUrl: string | null, themeColor: string | null): void {
	if (typeof document === 'undefined') return;
	setDocumentTitleProductName(productName);
	setMeta('application-name', productName);
	setMeta('apple-mobile-web-app-title', productName);
	if (faviconUrl === null) {
		removeLink('icon');
		restoreDefaultLinks('icon');
	} else {
		suspendDefaultLinks('icon');
		setLink('icon', faviconUrl);
	}
	if (themeColor === null) {
		removeMeta('theme-color');
	} else {
		setMeta('theme-color', themeColor);
	}
}

export function getDocumentFaviconUrl(appPublic: InstanceAppPublic | null): string | null {
	if (appPublic === null) return DEFAULT_APP_SHELL_BRANDING.faviconUrl;
	return appPublic.branding.favicon_url ?? appPublic.branding.icon_url;
}

export function applyRuntimeDocumentBranding(appPublic: InstanceAppPublic): void {
	applyDocumentBranding(
		appPublic.branding.product_name,
		getDocumentFaviconUrl(appPublic),
		appPublic.branding.theme_color,
	);
}

export function applyDefaultAppShellDocumentBranding(): void {
	applyDocumentBranding(
		DEFAULT_APP_SHELL_BRANDING.productName,
		getDocumentFaviconUrl(null),
		DEFAULT_APP_SHELL_BRANDING.themeColor,
	);
}
