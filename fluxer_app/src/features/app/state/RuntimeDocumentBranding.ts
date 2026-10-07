// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_APP_SHELL_BRANDING} from '@app/features/app/state/AppShellBranding';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';

let lastProductName = DEFAULT_APP_SHELL_BRANDING.productName;

function escapeRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
}

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

function replaceTitleProductName(nextProductName: string): void {
	if (lastProductName === nextProductName) return;
	const prefix = new RegExp(`^(\\(\\d+\\)\\s+|\\u2022\\s+)?${escapeRegExp(lastProductName)}(?=$| \\| )`, 'u');
	if (prefix.test(document.title)) {
		document.title = document.title.replace(prefix, (_match, notificationPrefix: string | undefined) => {
			return `${notificationPrefix ?? ''}${nextProductName}`;
		});
	}
}

function applyDocumentBranding(productName: string, faviconUrl: string | null, themeColor: string | null): void {
	if (typeof document === 'undefined') return;
	replaceTitleProductName(productName);
	lastProductName = productName;
	setMeta('application-name', productName);
	setMeta('apple-mobile-web-app-title', productName);
	if (faviconUrl === null) {
		removeLink('icon');
	} else {
		setLink('icon', faviconUrl);
	}
	if (themeColor === null) {
		removeMeta('theme-color');
	} else {
		setMeta('theme-color', themeColor);
	}
}

export function applyRuntimeDocumentBranding(appPublic: InstanceAppPublic): void {
	applyDocumentBranding(
		appPublic.branding.product_name,
		appPublic.branding.favicon_url ?? appPublic.branding.icon_url,
		appPublic.branding.theme_color,
	);
}

export function applyDefaultAppShellDocumentBranding(): void {
	applyDocumentBranding(
		DEFAULT_APP_SHELL_BRANDING.productName,
		DEFAULT_APP_SHELL_BRANDING.faviconUrl,
		DEFAULT_APP_SHELL_BRANDING.themeColor,
	);
}
