// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_LOCAL_APP_SCHEME = 'fluxer-app';
export const DESKTOP_LOCAL_APP_PROTOCOL = `${DESKTOP_LOCAL_APP_SCHEME}:`;
export const DESKTOP_LOCAL_APP_HOST = 'app';
export const DESKTOP_LOCAL_APP_ORIGIN = `${DESKTOP_LOCAL_APP_PROTOCOL}//${DESKTOP_LOCAL_APP_HOST}`;
export const DESKTOP_LOCAL_APP_URL = `${DESKTOP_LOCAL_APP_ORIGIN}/`;

export const LOCAL_APP_API_PATH_PREFIX = '/api';
export const LOCAL_APP_REMOTE_PROXY_PATH_PREFIX = '/proxy';
export const LOCAL_APP_UPLOAD_RELAY_PATH_SUFFIX = '/relay';
export const LOCAL_APP_REMOTE_PROXY_URL_PARAMETER = 'url';
export const LOCAL_APP_UPLOAD_RELAY_TOKEN_PARAMETER = 't';

export const LOCAL_APP_UPLOAD_ID_HEADER = 'X-Fluxer-Local-Upload-Id';

export const LOCAL_APP_UPLOAD_PROGRESS_CHANNELS = Object.freeze({
	subscribe: 'desktop-local-app-upload:subscribe',
	progress: 'desktop-local-app-upload:progress',
} as const);

export interface DesktopLocalAppUploadProgress {
	readonly uploadId: string;
	readonly loaded: number;
	readonly total: number | null;
	readonly done: boolean;
	readonly failed: boolean;
}
