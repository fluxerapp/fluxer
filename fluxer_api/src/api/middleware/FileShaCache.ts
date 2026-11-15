// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {Config} from '@app/api/Config';
import {BANNED_FILE_SHAS_REFRESH_CHANNEL, isBlocklistFeedFileSha} from '@app/api/constants/ContentModeration';
import {HashSetBlocklistCache} from '@app/api/middleware/HashSetBlocklistCache';

const adminRepository = new AdminRepository();
export const fileShaCache = new HashSetBlocklistCache({
	name: 'file-SHA blocklist cache',
	channel: BANNED_FILE_SHAS_REFRESH_CHANNEL,
	load: async () => {
		const includeFeedRows = Config.blocklistFeeds.enabled;
		return (await adminRepository.loadAllBannedFileShas())
			.filter((row) => includeFeedRows || !isBlocklistFeedFileSha(row))
			.map((row) => row.sha256_hex);
	},
	normalize: (value) => value.toLowerCase(),
	refreshedMessage: 'File-SHA blocklist cache refreshed',
});
