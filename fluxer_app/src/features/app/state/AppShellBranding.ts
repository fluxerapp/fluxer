// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';

export interface AppShellBranding {
	readonly productName: string;
	readonly iconUrl: string | null;
	readonly symbolUrl: string | null;
	readonly logoUrl: string | null;
	readonly wordmarkUrl: string | null;
	readonly faviconUrl: string | null;
	readonly themeColor: string | null;
}

export const DEFAULT_APP_SHELL_BRANDING: AppShellBranding = Object.freeze({
	productName: PRODUCT_NAME,
	iconUrl: null,
	symbolUrl: null,
	logoUrl: null,
	wordmarkUrl: null,
	faviconUrl: null,
	themeColor: null,
});

export function resolveAppShellBranding(snapshot: RuntimeConfigSnapshot | null): AppShellBranding {
	if (snapshot === null) {
		return DEFAULT_APP_SHELL_BRANDING;
	}
	const branding = snapshot.appPublic.branding;
	return {
		productName: branding.product_name,
		iconUrl: branding.icon_url,
		symbolUrl: branding.symbol_url ?? branding.icon_url,
		logoUrl: branding.logo_url ?? branding.icon_url,
		wordmarkUrl: branding.wordmark_url,
		faviconUrl: branding.favicon_url ?? branding.icon_url,
		themeColor: branding.theme_color,
	};
}
