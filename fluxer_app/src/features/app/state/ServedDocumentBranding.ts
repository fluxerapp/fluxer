// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';

export interface ServedDocumentBranding {
	readonly productName: string;
	readonly themeColor: string | null;
	readonly customCss: string | null;
}

const SERVED_PRODUCT_NAME_SELECTOR = 'meta[name="apple-mobile-web-app-title"]:not([data-fluxer-branding])';
const SERVED_THEME_COLOR_STYLE_ID = 'fluxer-instance-theme-color';
const SERVED_CUSTOM_CSS_STYLE_ID = 'fluxer-instance-custom-css';

let served: ServedDocumentBranding | null = null;

function readServedDocumentBranding(): ServedDocumentBranding {
	if (typeof document === 'undefined') {
		return {productName: PRODUCT_NAME, themeColor: null, customCss: null};
	}
	const productName = document.head
		.querySelector<HTMLMetaElement>(SERVED_PRODUCT_NAME_SELECTOR)
		?.getAttribute('content')
		?.trim();
	const themeColor = document.getElementById(SERVED_THEME_COLOR_STYLE_ID)?.getAttribute('data-color')?.trim();
	const customCss = document.getElementById(SERVED_CUSTOM_CSS_STYLE_ID)?.textContent?.trim();
	return {
		productName: productName || PRODUCT_NAME,
		themeColor: themeColor || null,
		customCss: customCss || null,
	};
}

export function getServedDocumentBranding(): ServedDocumentBranding {
	served ??= readServedDocumentBranding();
	return served;
}
