// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {
	consumeCompletedPremiumCheckoutReturnIntent,
	getCurrentPremiumActive,
	getPendingPremiumCheckoutReturnIntent,
} from '@app/features/premium/utils/PremiumCheckoutReturnIntent';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import WindowState from '@app/features/window/state/Window';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef} from 'react';

const REFRESH_COOLDOWN_MS = 2000;

export const PremiumCheckoutReturnWatcher = observer(() => {
	const currentUserId = Users.currentUser?.id ?? null;
	const isPremium = getCurrentPremiumActive();
	const isFocused = WindowState.focused;
	const isVisible = WindowState.visible;
	const showPremiumFeatures = shouldShowPremiumFeatures();
	const refreshInFlightRef = useRef(false);
	const lastRefreshAtRef = useRef(0);
	const maybeRefreshPremiumState = useCallback(() => {
		if (!showPremiumFeatures) return;
		if (!currentUserId || !getPendingPremiumCheckoutReturnIntent()) return;
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
		ModalCommands.popAll();
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal initialTab="plutonium" data-flx="premium.checkout-return-watcher.user-settings-modal" />
			)),
		);
		ComponentBus.dispatchOrBuffer('USER_SETTINGS_TAB_SELECT', {tab: 'plutonium'});
	}, [isPremium, currentUserId, showPremiumFeatures]);
	return null;
});
