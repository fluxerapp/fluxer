// SPDX-License-Identifier: AGPL-3.0-or-later

import {resetGeoipReadersForTesting} from '@pkgs/geoip/src/GeoipLookup';
import {resetSudoModeServiceForTesting} from '../auth/services/SudoModeService';
import {resetSsoRequestUrlPolicyForTesting} from '../instance/SsoConfigValidation';
import {resetGlobalLimitConfigServiceForTesting} from '../limits/LimitConfigService';
import {bannedAvatarHashCache} from '../middleware/BannedAvatarHashCache';
import {fileShaCache} from '../middleware/FileShaCache';
import {ipBanCache} from '../middleware/IpBanMiddleware';
import {phraseBlocklistCache} from '../middleware/PhraseBlocklistCache';
import {profileSubstringBlocklistCache} from '../middleware/ProfileSubstringBlocklistCache';
import {resetServiceMiddlewareForTesting} from '../middleware/ServiceMiddleware';
import {resetServiceRegistryForTesting} from '../middleware/ServiceRegistry';
import {resetServiceSingletonsForTesting} from '../middleware/ServiceSingletons';
import {torExitListCache} from '../middleware/TorExitListCache';
import {urlBlocklistCache} from '../middleware/UrlBlocklistCache';
import {resetAdminSecretHashForTesting} from '../oauth/repositories/ApplicationRepository';
import {resetIpBanExemptionsForTesting} from '../risk/IpBanExemptions';
import {setThemeCssMaxBytesForTesting} from '../theme/ThemeService';

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
