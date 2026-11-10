// SPDX-License-Identifier: AGPL-3.0-or-later

export const BOOT_STATE_DATABASE_NAME = 'fluxer-boot-state';
export const BOOT_STATE_DATABASE_VERSION = 1;
export const BOOT_STATE_KEY_STORE = 'keys';
export const BOOT_STATE_STATE_STORE = 'states';
export const BOOT_STATE_MARKER_STORAGE_KEY = 'fluxer:boot-state:accounts';
export const BOOT_STATE_ACCOUNT_ID_PREFIX = 'fluxer-boot-state:v1:';
export const BOOT_STATE_ACCOUNT_ID_BYTES = 20;
export const BOOT_STATE_AAD_PREFIX = 'FXBS';
export const BOOT_STATE_COMPRESSION = 'deflate-raw';
export const BOOT_STATE_PREBOOT_GLOBAL = '__FLUXER_PREBOOT_BOOT_STATE__';
export const BOOT_STATE_MAX_ACCOUNTS = 10;
export const BOOT_STATE_MAX_AGE_MS = 30 * 24 * 60 * 60 * 1000;
export const BOOT_FRAME_AAD_PREFIX = 'FXBF';
export const BOOT_FRAME_ELEMENT_ID = 'fluxer-boot-frame';
export const BOOT_FRAME_SCROLL_ATTRIBUTE = 'data-boot-frame-scroll';
export const BOOT_FRAME_GLOBAL = '__FLUXER_BOOT_FRAME__';
export const BOOT_FRAME_SHOWN_MARK = 'fluxer:boot:frame-shown';
export const BOOT_FRAME_IMAGE_SOURCE_ATTRIBUTE = 'data-boot-frame-image-source';
