// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Accounts from '@app/features/auth/state/Accounts';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {
	consumeCompletedPremiumCheckoutReturnIntent,
	getCurrentPremiumActive,
	getPendingPremiumCheckoutReturnIntent,
} from '@app/features/premium/utils/PremiumCheckoutReturnIntent';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import Users from '@app/features/user/state/Users';
import WindowState from '@app/features/window/state/Window';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef} from 'react';

const REFRESH_COOLDOWN_MS = 2000;

interface ActivePremiumCheckoutReturnWatcherProps {
	readonly currentUserId: string;
	readonly showPremiumFeatures: boolean;
}

const ActivePremiumCheckoutReturnWatcher = observer(function ActivePremiumCheckoutReturnWatcher({
	currentUserId,
	showPremiumFeatures,
}: ActivePremiumCheckoutReturnWatcherProps) {
	const isPremium = getCurrentPremiumActive();
	const isFocused = WindowState.focused;
	const isVisible = WindowState.visible;
	const refreshInFlightRef = useRef(false);
	const lastRefreshAtRef = useRef(0);
	const maybeRefreshPremiumState = useCallback(() => {
		if (!showPremiumFeatures) return;
		if (!getPendingPremiumCheckoutReturnIntent()) return;
		const now = Date.now();
		if (refreshInFlightRef.current || now - lastRefreshAtRef.current < REFRESH_COOLDOWN_MS) return;
		lastRefreshAtRef.current = now;
		refreshInFlightRef.current = true;
		void PremiumCommands.refreshPremiumState()
			.catch(() => undefined)
			.finally(() => {
				refreshInFlightRef.current = false;
			});
	}, [currentUserId, showPremiumFeatures]);
	useEffect(() => {
		if (!showPremiumFeatures) return;
		if (!isFocused || !isVisible) return;
		maybeRefreshPremiumState();
	}, [isFocused, isVisible, maybeRefreshPremiumState, showPremiumFeatures]);
	useEffect(() => {
		if (!showPremiumFeatures) return;
		if (!isPremium || !consumeCompletedPremiumCheckoutReturnIntent()) return;
		PlutoniumPageCommands.openPlutoniumPage();
	}, [isPremium, currentUserId, showPremiumFeatures]);
	return null;
});

export const PremiumCheckoutReturnWatcher = observer(function PremiumCheckoutReturnWatcher() {
	const currentUserId = Users.currentUser?.id ?? null;
	const currentAccountKey = Accounts.currentAccountKey;
	const runtimeSnapshot = RuntimeConfig.getSnapshotOrNull();
	if (currentAccountKey === null || currentUserId === null || runtimeSnapshot === null) {
		return null;
	}
	return (
		<ActivePremiumCheckoutReturnWatcher
			currentUserId={currentUserId}
			showPremiumFeatures={shouldShowPremiumFeatures()}
			data-flx="premium.premium-checkout-return-watcher.active-premium-checkout-return-watcher"
		/>
	);
});
