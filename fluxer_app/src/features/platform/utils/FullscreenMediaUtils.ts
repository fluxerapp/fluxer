// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getExtendedDocument,
	supportsMozRequestFullScreen,
	supportsMsRequestFullscreen,
	supportsWebkitRequestFullscreen,
} from '@app/features/platform/types/Browser';
import type {ExtendedHTMLElement} from '@app/types/browser.d';

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

export function supportsFullscreenAPI(): boolean {
	const doc = getExtendedDocument();
	return !!(
		document.fullscreenEnabled ||
		doc.webkitFullscreenEnabled ||
		doc.mozFullScreenEnabled ||
		doc.msFullscreenEnabled
	);
}

export async function requestFullscreen(element: HTMLElement): Promise<void> {
	if (element.requestFullscreen) {
		await element.requestFullscreen();
	} else if (supportsWebkitRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.webkitRequestFullscreen!();
	} else if (supportsMozRequestFullScreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.mozRequestFullScreen!();
	} else if (supportsMsRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.msRequestFullscreen!();
	}
}

export async function exitFullscreenAPI(): Promise<void> {
	const doc = getExtendedDocument();
	if (document.exitFullscreen) {
		await document.exitFullscreen();
	} else if (doc.webkitExitFullscreen) {
		await doc.webkitExitFullscreen();
	} else if (doc.mozCancelFullScreen) {
		await doc.mozCancelFullScreen();
	} else if (doc.msExitFullscreen) {
		await doc.msExitFullscreen();
	}
}
