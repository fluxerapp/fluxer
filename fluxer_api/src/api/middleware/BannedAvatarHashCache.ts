// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_AVATAR_HASHES_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {stripAnimationPrefix} from '@app/api/infrastructure/AssetEntitlementUtils';
import {HashSetBlocklistCache} from '@app/api/middleware/HashSetBlocklistCache';

const adminRepository = new AdminRepository();
export const bannedAvatarHashCache = new HashSetBlocklistCache({
	name: 'avatar-hash blocklist cache',
	channel: BANNED_AVATAR_HASHES_REFRESH_CHANNEL,
	load: async () => (await adminRepository.loadAllBannedAvatarHashes()).map((row) => row.hash_short),
	normalize: (value) => stripAnimationPrefix(value.toLowerCase()),
	refreshedMessage: 'Avatar-hash blocklist cache refreshed',
});
