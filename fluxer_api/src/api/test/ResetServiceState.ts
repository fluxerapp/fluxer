// SPDX-License-Identifier: AGPL-3.0-or-later

import {resetSudoModeServiceForTesting} from '@app/api/auth/services/SudoModeService';
import {resetSsoRequestUrlPolicyForTesting} from '@app/api/instance/SsoConfigValidation';
import {resetGlobalLimitConfigServiceForTesting} from '@app/api/limits/LimitConfigService';
import {bannedAvatarHashCache} from '@app/api/middleware/BannedAvatarHashCache';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {resetServiceMiddlewareForTesting} from '@app/api/middleware/ServiceMiddleware';
import {resetServiceRegistryForTesting} from '@app/api/middleware/ServiceRegistry';
import {resetServiceSingletonsForTesting} from '@app/api/middleware/ServiceSingletons';
import {torExitListCache} from '@app/api/middleware/TorExitListCache';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {resetAdminSecretHashForTesting} from '@app/api/oauth/repositories/ApplicationRepository';
import {resetIpBanExemptionsForTesting} from '@app/api/risk/IpBanExemptions';
import {setThemeCssMaxBytesForTesting} from '@app/api/theme/ThemeService';
import {resetGeoipReadersForTesting} from '@pkgs/geoip/src/GeoipLookup';

export async function resetServiceStateForTesting(): Promise<void> {
	resetServiceRegistryForTesting();
	resetServiceSingletonsForTesting();
	resetServiceMiddlewareForTesting();
	resetIpBanExemptionsForTesting();
	resetGlobalLimitConfigServiceForTesting();
	resetSudoModeServiceForTesting();
	resetSsoRequestUrlPolicyForTesting();
	resetAdminSecretHashForTesting();
	setThemeCssMaxBytesForTesting(undefined);
	await ipBanCache.shutdown();
	ipBanCache.resetCaches();
	await torExitListCache.shutdown();
	torExitListCache.clearForTesting();
	urlBlocklistCache.resetForTesting();
	resetGeoipReadersForTesting();
	fileShaCache.resetForTesting();
	phraseBlocklistCache.resetForTesting();
	bannedAvatarHashCache.resetForTesting();
	profileSubstringBlocklistCache.resetForTesting();
}
