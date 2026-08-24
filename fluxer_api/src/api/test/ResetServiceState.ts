// SPDX-License-Identifier: AGPL-3.0-or-later

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

export function resetServiceStateForTesting(): void {
	resetServiceRegistryForTesting();
	resetServiceSingletonsForTesting();
	resetServiceMiddlewareForTesting();
	resetIpBanExemptionsForTesting();
	resetGlobalLimitConfigServiceForTesting();
	resetSudoModeServiceForTesting();
	resetSsoRequestUrlPolicyForTesting();
	resetAdminSecretHashForTesting();
	setThemeCssMaxBytesForTesting(undefined);
	ipBanCache.shutdown();
	ipBanCache.resetCaches();
	torExitListCache.shutdown();
	torExitListCache.clearForTesting();
	urlBlocklistCache.resetForTesting();
	fileShaCache.resetForTesting();
	phraseBlocklistCache.resetForTesting();
	bannedAvatarHashCache.resetForTesting();
	profileSubstringBlocklistCache.resetForTesting();
}
