// SPDX-License-Identifier: AGPL-3.0-or-later

function getBootstrapProductName(): string {
	if (typeof window === 'undefined') {
		return 'Fluxer';
	}
	const productName = window.__FLUXER_BOOTSTRAP__?.instance.app_public?.branding?.product_name?.trim();
	return productName || 'Fluxer';
}

function getBootstrapPremiumProductName(): string {
	if (typeof window === 'undefined') {
		return 'Plutonium';
	}
	const premiumProductName = window.__FLUXER_BOOTSTRAP__?.instance.app_public?.branding?.premium_product_name?.trim();
	return premiumProductName || 'Plutonium';
}

export const PRODUCT_NAME = getBootstrapProductName();
export const PREMIUM_PRODUCT_NAME = getBootstrapPremiumProductName();
export const PREMIUM_PRODUCT_FULL_NAME = `${PRODUCT_NAME} ${PREMIUM_PRODUCT_NAME}`;
