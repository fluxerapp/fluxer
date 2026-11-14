// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BannedFileShaRow} from '@app/api/database/types/AdminArchiveTypes';

export const BANNED_URLS_REFRESH_CHANNEL = 'banned_urls_refresh';
export const BANNED_URL_DOMAINS_REFRESH_CHANNEL = 'banned_url_domains_refresh';
export const BANNED_FILE_SHAS_REFRESH_CHANNEL = 'banned_file_shas_refresh';
export const BANNED_PHRASES_REFRESH_CHANNEL = 'banned_phrases_refresh';
export const BANNED_AVATAR_HASHES_REFRESH_CHANNEL = 'banned_avatar_hashes_refresh';
export const BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL = 'banned_profile_substrings_refresh';
export const ContentBlocklistSeverity = {
	ALLOW: 0,
	WARN: 1,
	BLOCK: 2,
	BLOCK_AND_REPORT: 3,
} as const;

export const ContentBlocklistCategory = {
	MANUAL: 'manual',
	URL_HAUS: 'urlhaus',
	PHISH_TANK: 'phishtank',
	GOOGLE_SAFE_BROWSING: 'google_safe_browsing',
	MALWARE_BAZAAR: 'malware_bazaar',
	NCMEC: 'ncmec',
	GIFCT: 'gifct',
	STOP_NCII: 'stop_ncii',
} as const;

export function isBlocklistFeedFileSha(row: Pick<BannedFileShaRow, 'category' | 'added_by'>): boolean {
	return row.added_by == null && row.category === ContentBlocklistCategory.MALWARE_BAZAAR;
}
