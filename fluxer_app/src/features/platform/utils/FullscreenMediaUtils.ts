// SPDX-License-Identifier: AGPL-3.0-or-later

import {getExtendedDocument} from '@app/features/platform/types/Browser';

const EMBED_MEDIA_SELECTOR = '[data-embed-media="true"]';
const MEDIA_FULLSCREEN_ROOT_SELECTOR = '[data-media-fullscreen-root="true"]';

export function getFullscreenElement(): Element | null {
	const doc = getExtendedDocument();
	return (
		document.fullscreenElement ||
		doc.webkitFullscreenElement ||
		doc.mozFullScreenElement ||
		doc.msFullscreenElement ||
		null
	);
}

function isFullscreenMediaElement(element: Element | null): boolean {
	if (!element) return false;
	if (typeof HTMLVideoElement !== 'undefined' && element instanceof HTMLVideoElement) return true;
	return element.matches(EMBED_MEDIA_SELECTOR) || element.matches(MEDIA_FULLSCREEN_ROOT_SELECTOR);
}

export function isDocumentFullscreenMedia(): boolean {
	return isFullscreenMediaElement(getFullscreenElement());
}
