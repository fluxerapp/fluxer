/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {
	ExtendedDocument,
	ExtendedHTMLElement,
	ExtendedHTMLVideoElement,
	ExtendedWindow,
	LegacyDocumentSelection,
} from './browser.d';

export type {ExtendedDocument, ExtendedHTMLElement, ExtendedHTMLVideoElement, ExtendedWindow, LegacyDocumentSelection};

export function isLegacyDocument(_document: Document): _document is Document & LegacyDocumentSelection {
	return 'selection' in _document && (_document as any).selection !== undefined;
}

export function supportsWebkitFullscreen(_document: Document): _document is ExtendedDocument {
	return 'webkitFullscreenElement' in _document;
}

export function supportsMozFullscreen(_document: Document): _document is ExtendedDocument {
	return 'mozFullScreenElement' in _document;
}

export function supportsMsFullscreen(_document: Document): _document is ExtendedDocument {
	return 'msFullscreenElement' in _document;
}

export function supportsWebkitRequestFullscreen(_element: HTMLElement): _element is ExtendedHTMLElement {
	return 'webkitRequestFullscreen' in _element;
}

export function supportsMozRequestFullScreen(_element: HTMLElement): _element is ExtendedHTMLElement {
	return 'mozRequestFullScreen' in _element;
}

export function supportsMsRequestFullscreen(_element: HTMLElement): _element is ExtendedHTMLElement {
	return 'msRequestFullscreen' in _element;
}

export function supportsDisablePictureInPicture(
	_video: HTMLVideoElement,
): _video is HTMLVideoElement & {disablePictureInPicture?: boolean} {
	return 'disablePictureInPicture' in _video;
}

export function supportsShowSaveFilePicker(
	_window: Window,
): _window is Window & {showSaveFilePicker?: (options: SaveFilePickerOptions) => Promise<FileSystemFileHandle>} {
	return 'showSaveFilePicker' in _window;
}

export function supportsRequestIdleCallback(
	_window: Window,
): _window is Window & {requestIdleCallback?: (callback: IdleRequestCallback, options?: IdleRequestOptions) => number} {
	return 'requestIdleCallback' in _window;
}

export function getExtendedDocument(): ExtendedDocument {
	return document as ExtendedDocument;
}

export function getExtendedWindow(): ExtendedWindow {
	return window as unknown as ExtendedWindow;
}
