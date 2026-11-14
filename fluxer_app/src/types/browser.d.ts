// SPDX-License-Identifier: AGPL-3.0-or-later

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
	showSaveFilePicker?: (options?: SaveFilePickerOptions) => Promise<FileSystemFileHandle>;
	[key: string]: unknown;
}
