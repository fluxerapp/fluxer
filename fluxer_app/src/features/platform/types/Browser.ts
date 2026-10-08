// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ExtendedDocument,
	ExtendedHTMLElement,
	ExtendedWindow,
	LegacyDocumentSelection,
} from '@app/types/browser.d';

export function isLegacyDocument(_document: Document): _document is Document & LegacyDocumentSelection {
	return 'selection' in _document && typeof _document.selection === 'object' && _document.selection !== null;
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

export function supportsDisablePictureInPicture(_video: HTMLVideoElement): _video is HTMLVideoElement & {
	disablePictureInPicture?: boolean;
} {
	return 'disablePictureInPicture' in _video;
}

export function supportsShowSaveFilePicker(_window: Window): _window is Window & {
	showSaveFilePicker?: (options: SaveFilePickerOptions) => Promise<FileSystemFileHandle>;
} {
	return 'showSaveFilePicker' in _window;
}

export function supportsRequestIdleCallback(_window: Window): _window is Window & {
	requestIdleCallback?: (callback: IdleRequestCallback, options?: IdleRequestOptions) => number;
} {
	return 'requestIdleCallback' in _window;
}

export function getExtendedDocument(): ExtendedDocument {
	return document as ExtendedDocument;
}

function isExtendedWindow(_window: Window): _window is ExtendedWindow {
	return typeof _window === 'object' && _window !== null;
}

export function getExtendedWindow(): ExtendedWindow {
	if (isExtendedWindow(window)) {
		return window;
	}
	throw new Error('Expected window to be an ExtendedWindow');
}
