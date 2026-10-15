// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

const UPSTREAM_PRODUCT_NAME = 'Fluxer';

let cachedProductName: string | null = null;

export function getDefaultProductName(): string {
	return Config.instance.branding.productName || UPSTREAM_PRODUCT_NAME;
}

export function getInstanceProductName(): string {
	const productName = cachedProductName ?? getDefaultProductName();
	if (Config.instance.selfHosted && isUnbrandedProductName(productName)) {
		return new URL(Config.endpoints.webApp).hostname;
	}
	return productName;
}

export function setCachedProductName(productName: string | null): void {
	cachedProductName = productName;
}

function isUnbrandedProductName(productName: string): boolean {
	const normalized = productName.trim().toLowerCase();
	return normalized === '' || normalized === UPSTREAM_PRODUCT_NAME.toLowerCase();
}
