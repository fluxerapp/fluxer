// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import PremiumState from '@app/features/premium/state/PremiumState';
import {areGiftPurchasesAvailable, arePremiumPurchasesAvailable} from '@app/features/premium/utils/PremiumUtils';
import Users from '@app/features/user/state/Users';
import {useEffect, useRef} from 'react';

export function useCanSendGift(active = true): boolean {
	const currentUserId = Users.currentUser?.id ?? null;
	const premiumState =
		currentUserId != null && PremiumState.loadedForUserId === currentUserId ? PremiumState.state : null;
	const needsPricing =
		active &&
		currentUserId != null &&
		RuntimeConfig.isSelfHosted() &&
		arePremiumPurchasesAvailable() &&
		premiumState == null &&
		!PremiumState.loading;
	const requestedRef = useRef(false);
	useEffect(() => {
		if (!needsPricing || requestedRef.current) return;
		requestedRef.current = true;
		PremiumCommands.refreshPremiumState().catch(() => {});
	}, [needsPricing]);
	return areGiftPurchasesAvailable(premiumState?.pricing.localized);
}
