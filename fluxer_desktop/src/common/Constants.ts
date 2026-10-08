// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL, type BuildChannel} from '@electron/common/BuildChannel';
import {
	DESKTOP_LOCAL_APP_HOST,
	DESKTOP_LOCAL_APP_ORIGIN,
	DESKTOP_LOCAL_APP_SCHEME,
	DESKTOP_LOCAL_APP_URL,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

export const CHANNEL_APP_PROTOCOLS: Record<BuildChannel, string> = {
	stable: 'fluxer',
	canary: 'fluxer',
	development: 'fluxer-development',
};
export const APP_PROTOCOL = CHANNEL_APP_PROTOCOLS[BUILD_CHANNEL];
export const STABLE_APP_URL = 'https://web.fluxer.app';
export const CANARY_APP_URL = 'https://web.canary.fluxer.app';
const DEVELOPMENT_APP_URL = 'http://localhost:8088';
export const CHANNEL_APP_URLS: Record<BuildChannel, string> = {
	stable: STABLE_APP_URL,
	canary: CANARY_APP_URL,
	development: DEVELOPMENT_APP_URL,
};
export const LOCAL_DEVELOPMENT_INSTANCE_URL = BUILD_CHANNEL === 'development' ? DEVELOPMENT_APP_URL : null;
export const DOWNLOAD_PAGE_URLS: Record<BuildChannel, string> = {
	stable: 'https://fluxer.app/download',
	canary: 'https://canary.fluxer.app/download',
	development: 'http://localhost:8088/download',
};
export const STABLE_MIGRATED_APP_ORIGIN = 'https://fluxer.com';
export const CANARY_MIGRATED_APP_ORIGIN = 'https://canary.fluxer.com';
export const DESKTOP_APP_SCHEME = DESKTOP_LOCAL_APP_SCHEME;
export const DESKTOP_APP_HOST = DESKTOP_LOCAL_APP_HOST;
export const DESKTOP_APP_ORIGIN = DESKTOP_LOCAL_APP_ORIGIN;
export const DESKTOP_APP_URL = DESKTOP_LOCAL_APP_URL;
export const DESKTOP_APP_LANDING_URL = `${DESKTOP_LOCAL_APP_URL}channels/@me`;
export const DESKTOP_PREBOOT_THEME_CHANNEL = 'desktop-preboot-theme:report';
export const DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL = 'desktop-window:first-content-painted';
export const STATIC_CDN_URL = 'https://fluxerstatic.com';
export const DEFAULT_WINDOW_WIDTH = 1280;
export const DEFAULT_WINDOW_HEIGHT = 800;
export const MIN_WINDOW_WIDTH = 800;
export const MIN_WINDOW_HEIGHT = 600;
