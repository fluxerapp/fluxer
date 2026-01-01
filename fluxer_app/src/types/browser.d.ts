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

export interface LegacyDocumentSelection {
	selection?: {
		type: string;
		createRange(): {
			text: string;
		};
	};
}

export interface ExtendedDocument extends Document {
	webkitFullscreenElement?: Element;
	mozFullScreenElement?: Element;
	msFullscreenElement?: Element;
	webkitFullscreenEnabled?: boolean;
	mozFullScreenEnabled?: boolean;
	msFullscreenEnabled?: boolean;
	webkitExitFullscreen?: () => Promise<void>;
	mozCancelFullScreen?: () => Promise<void>;
	msExitFullscreen?: () => Promise<void>;
}

export interface ExtendedHTMLElement extends HTMLElement {
	webkitRequestFullscreen?: () => Promise<void>;
	mozRequestFullScreen?: () => Promise<void>;
	msRequestFullscreen?: () => Promise<void>;
}

export interface ExtendedHTMLVideoElement extends HTMLVideoElement {
	disablePictureInPicture: boolean | undefined;
	webkitEnterFullscreen?: () => Promise<void>;
	webkitExitFullscreen?: () => Promise<void>;
	webkitDisplayingFullscreen?: boolean;
	webkitSupportsFullscreen?: boolean;
}

export interface ExtendedWindow extends Window {
	showSaveFilePicker?: (options: SaveFilePickerOptions) => Promise<FileSystemFileHandle>;
	requestIdleCallback?: (callback: IdleRequestCallback, options?: IdleRequestOptions) => number;
	[key: string]: unknown;
}
