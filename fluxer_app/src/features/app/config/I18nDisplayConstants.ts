// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import {formatAssetUploadExtensions, formatKnownAnimatedAssetExtensions} from '@fluxer/constants/src/AssetFormatPolicy';
import {THE_OTHER_PLATFORM} from '@fluxer/constants/src/ExternalPlatformConstants';

export {PRODUCT_NAME};
export const DESKTOP_ENTRY_NAME = 'Fluxer';
export const CANARY_DESKTOP_ENTRY_NAME = 'Fluxer Canary';
export const PAYMENT_PROVIDER_NAME = 'Stripe';
export const BLUESKY_PROVIDER_NAME = 'Bluesky';
export const APP_STORE_NAME = 'App Store';
export const GOOGLE_PLAY_NAME = 'Google Play';
export const PIX_PAYMENT_METHOD = 'Pix';
export const UPI_PAYMENT_METHOD = 'UPI';
export const SUPPORT_EMAIL = 'support@fluxer.com';
export const SUPPORT_EMAIL_MAILTO = `mailto:${SUPPORT_EMAIL}`;
export const I18N_WEBLATE_DOMAIN = 'weblate.fluxer.tools';
export const I18N_WEBLATE_URL = `https://${I18N_WEBLATE_DOMAIN}`;
export const EXAMPLE_DOMAIN = 'example.com';
export const EXAMPLE_URL = `https://${EXAMPLE_DOMAIN}`;
export const EXAMPLE_CALLBACK_URL = `${EXAMPLE_URL}/callback`;
export const EXAMPLE_EMAIL = 'name@example.com';
export const EXAMPLE_PERSONAL_EMAIL = 'marty@example.com';
export const EXAMPLE_REPORT_EMAIL = 'you@example.com';
export const EXAMPLE_REPORT_USER_TAG = 'username#1234';
export const EXAMPLE_REPORT_USERNAME = 'username';
export const EXAMPLE_INVITE_CODE = 'abcDEF12';
export const EXAMPLE_VERIFICATION_CODE = 'ABCD-1234';
export const EXAMPLE_FLUXER_TAG = 'Marty_McFly';
export const EXAMPLE_CUSTOM_URL_SLUG = 'your-custom-url';
export const EXAMPLE_BOT_NAME = 'BotName';
export const EXAMPLE_CHANNEL_NAME = 'new-channel';
export const EXAMPLE_GENERAL_CHANNEL_NAME = 'general';
export const EXAMPLE_USERNAME_MENTION = '@username';
export const EXAMPLE_FLUXER_TAG_FULL = 'Username#0000';
export const EXAMPLE_USERNAME = 'Username';
export const VISIONARY_LIFETIME_BADGE_LABEL = 'Visionary #42';
export const LINK_PREVIEW_EXAMPLE_URL = 'https://fluxer.app';
export const EXAMPLE_GIF_URLS = `${EXAMPLE_URL}/gif1.gif\n${EXAMPLE_URL}/gif2.gif`;
export const THE_OTHER_PLATFORM_TEMPLATE_EXAMPLE_URL = `https://${THE_OTHER_PLATFORM.toLowerCase()}.new/abcd1234`;
const DESKTOP_DOWNLOAD_URLS: Record<string, string> = {
	stable: 'https://fluxer.app/download',
	canary: 'https://canary.fluxer.app/download',
	development: 'http://localhost:8088/download',
};

export const DESKTOP_DOWNLOAD_URL =
	DESKTOP_DOWNLOAD_URLS[Config.PUBLIC_RELEASE_CHANNEL] ?? DESKTOP_DOWNLOAD_URLS.stable;
export const FLUXER_DOCS_DOMAIN = 'fluxer.dev';
export const FLUXER_DOCS_URL = `https://${FLUXER_DOCS_DOMAIN}`;
export const FLUXER_BLUESKY_HANDLE = '@fluxer.app';
export const YOUTUBE_PROVIDER_NAME = 'YouTube';
export const EVERYONE_MENTION = '@everyone';
export const HERE_MENTION = '@here';
export const SILENT_MENTION = '@silent';
export const ROLE_MENTION = '@role';
export const ROLES_MENTION = '@roles';
export const RECENT_MENTIONS_RETENTION_DAYS = 7;
export const SEND_MESSAGES_PERMISSION = 'SEND_MESSAGES';
export const ATTACH_FILES_PERMISSION = 'ATTACH_FILES';
export const ANIMATED_ICON_FEATURE = 'ANIMATED_ICON';
export const ANIMATED_BANNER_FEATURE = 'ANIMATED_BANNER';
export const STATIC_IMAGE_FORMATS = formatAssetUploadExtensions('avatar', {animatedAllowed: false});
export const STATIC_IMAGE_WITH_AVIF_FORMATS = formatAssetUploadExtensions('splash');
export const ANIMATED_IMAGE_FORMATS = formatAssetUploadExtensions('avatar');
export const ANIMATED_AVATAR_FORMATS = formatKnownAnimatedAssetExtensions('avatar');
export const AVIF_FORMAT_LABEL = 'AVIF';
export const IMAGE_MAX_SIZE_BYTES = 10 * 1024 * 1024;
export const BACKGROUND_MEDIA_MAX_SIZE_BYTES = 10 * 1024 * 1024;
export const CUSTOM_SOUND_MAX_SIZE_BYTES = 2 * 1024 * 1024;
export const AVATAR_RECOMMENDED_SIZE_LABEL = '512×512px';
export const BANNER_MINIMUM_SIZE_LABEL = '680×240px';
export const BANNER_ASPECT_RATIO_LABEL = '17:6';
export const WIDE_IMAGE_ASPECT_RATIO_LABEL = '16:9';
export const MFA_CODE_DIGIT_COUNT = 6;
