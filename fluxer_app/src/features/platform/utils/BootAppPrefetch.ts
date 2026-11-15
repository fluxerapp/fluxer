// SPDX-License-Identifier: AGPL-3.0-or-later

const BOOT_APP_PRELOADS_ELEMENT_ID = 'fluxer-boot-app-preloads';

interface BootAppPreload {
	readonly href: string;
	readonly as: string;
	readonly integrity?: string;
}

function readBootAppPreloads(): ReadonlyArray<BootAppPreload> {
	const element = document.getElementById(BOOT_APP_PRELOADS_ELEMENT_ID);
	if (element?.textContent == null) {
		return [];
	}
	try {
		const parsed: unknown = JSON.parse(element.textContent);
		return Array.isArray(parsed) ? (parsed as ReadonlyArray<BootAppPreload>) : [];
	} catch {
		return [];
	}
}

function prefersReducedData(): boolean {
	const connection = (navigator as Navigator & {connection?: {saveData?: boolean}}).connection;
	return connection?.saveData === true;
}

export function prefetchBootApp(): void {
	if (prefersReducedData()) {
		return;
	}
	const existing = new Set(
		Array.from(
			document.querySelectorAll<HTMLLinkElement>('link[rel="preload"], link[rel="prefetch"]'),
			(link) => link.href,
		),
	);
	for (const preload of readBootAppPreloads()) {
		const href = new URL(preload.href, document.baseURI).href;
		if (existing.has(href)) {
			continue;
		}
		const link = document.createElement('link');
		link.rel = 'prefetch';
		link.as = preload.as;
		link.href = href;
		if (preload.integrity != null) {
			link.crossOrigin = 'anonymous';
			link.integrity = preload.integrity;
		}
		document.head.appendChild(link);
	}
}
