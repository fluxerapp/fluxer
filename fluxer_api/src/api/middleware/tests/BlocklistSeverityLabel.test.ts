// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {ContentBlocklistCategory, ContentBlocklistSeverity} from '@app/api/constants/ContentModeration';
import {bannedAvatarHashCache} from '@app/api/middleware/BannedAvatarHashCache';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {afterEach, describe, expect, it, vi} from 'vitest';

const SEVERITIES = [
	['allow', ContentBlocklistSeverity.ALLOW],
	['warn', ContentBlocklistSeverity.WARN],
	['block', ContentBlocklistSeverity.BLOCK],
	['block and report', ContentBlocklistSeverity.BLOCK_AND_REPORT],
	['unset', null],
] as const;

const FILE_SHA = 'a'.repeat(64);
const AVATAR_HASH = 'b'.repeat(8);

const rowMeta = {
	category: ContentBlocklistCategory.MANUAL,
	source_url: null,
	added_at: new Date(),
	added_by: 1n,
	notes: null,
};

describe('blocklist severity is a stored label', () => {
	afterEach(() => {
		vi.restoreAllMocks();
		urlBlocklistCache.resetForTesting();
		fileShaCache.resetForTesting();
		bannedAvatarHashCache.resetForTesting();
	});

	it.each(SEVERITIES)('a %s url row and url domain row still block', async (_label, severity) => {
		vi.spyOn(AdminRepository.prototype, 'loadAllBannedUrls').mockResolvedValue([
			{...rowMeta, url_canonical: 'https://blocked.example/path', severity},
		]);
		vi.spyOn(AdminRepository.prototype, 'loadAllBannedUrlDomains').mockResolvedValue([
			{...rowMeta, domain: 'blocked-domain.example', match_subdomains: true, severity},
		]);
		await urlBlocklistCache.refresh();
		expect(urlBlocklistCache.size.urls).toBe(1);
		expect(urlBlocklistCache.isUrlBanned('https://blocked.example/path')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see https://blocked.example/path')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see https://www.blocked-domain.example/x')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see https://allowed.example/path')).toBe(false);
	});

	it.each(SEVERITIES)('a %s file sha row still blocks', async (_label, severity) => {
		vi.spyOn(AdminRepository.prototype, 'loadAllBannedFileShas').mockResolvedValue([
			{...rowMeta, sha256_hex: FILE_SHA.toUpperCase(), content_type: 'image/png', severity},
		]);
		await fileShaCache.refresh();
		expect(fileShaCache.size).toBe(1);
		expect(fileShaCache.isBanned(FILE_SHA)).toBe(true);
		expect(fileShaCache.isBanned('c'.repeat(64))).toBe(false);
	});

	it.each(SEVERITIES)('a %s avatar hash row still blocks', async (_label, severity) => {
		vi.spyOn(AdminRepository.prototype, 'loadAllBannedAvatarHashes').mockResolvedValue([
			{...rowMeta, hash_short: AVATAR_HASH, severity},
		]);
		await bannedAvatarHashCache.refresh();
		expect(bannedAvatarHashCache.size).toBe(1);
		expect(bannedAvatarHashCache.contains(AVATAR_HASH)).toBe(true);
		expect(bannedAvatarHashCache.contains(`a_${AVATAR_HASH}`)).toBe(true);
		expect(bannedAvatarHashCache.contains('c'.repeat(8))).toBe(false);
	});
});
