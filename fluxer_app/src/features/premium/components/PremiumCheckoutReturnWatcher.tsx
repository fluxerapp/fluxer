// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {offerAuthenticatorCalmly} from '@app/features/auth/components/modals/AuthenticatorOfferModal';
import Accounts from '@app/features/auth/state/Accounts';
import {deliverPurchasedChatGift, hasPendingChatGiftSend} from '@app/features/gift/state/ChatGiftSend';
import * as OperatorPassModalCommands from '@app/features/premium/commands/OperatorPassModalCommands';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import OperatorPassCheckoutSession from '@app/features/premium/state/OperatorPassCheckoutSession';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {isOperatorPassAvailable} from '@app/features/premium/utils/OperatorPassUtils';
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
const CHAT_GIFT_POLL_INTERVAL_MS = 3000;
const CHAT_GIFT_POLL_ATTEMPTS = 40;

interface ActivePremiumCheckoutReturnWatcherProps {
	readonly currentUserId: string;
	readonly showPremiumFeatures: boolean;
}

const ActivePremiumCheckoutReturnWatcher = observer(function ActivePremiumCheckoutReturnWatcher({
	currentUserId,
	showPremiumFeatures,
}: ActivePremiumCheckoutReturnWatcherProps) {
	const isPremium = getCurrentPremiumActive();
	const operatorPassRun = OperatorPassCheckoutSession.run;
	const operatorPassArrived =
		operatorPassRun !== null && (OperatorPassHoldings.count ?? 0) > operatorPassRun.startedPassCount;
	const isFocused = WindowState.focused;
	const isVisible = WindowState.visible;
	const refreshInFlightRef = useRef(false);
	const lastRefreshAtRef = useRef(0);
	const maybeRefreshPremiumState = useCallback(() => {
		if (!showPremiumFeatures) return;
		const intent = getPendingPremiumCheckoutReturnIntent();
		if (!intent) return;
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
		if (!isPremium) return;
		const intent = consumeCompletedPremiumCheckoutReturnIntent();
		if (!intent) return;
		PlutoniumPageCommands.openPlutoniumPage();
		void offerAuthenticatorCalmly();
	}, [isPremium, currentUserId, showPremiumFeatures]);
	useEffect(() => {
		if (!isFocused || !isVisible || !hasPendingChatGiftSend()) return;
		let attempts = 0;
		const timer = setInterval(() => {
			attempts += 1;
			void deliverPurchasedChatGift().then((done) => {
				if (done || attempts >= CHAT_GIFT_POLL_ATTEMPTS || !hasPendingChatGiftSend()) clearInterval(timer);
			});
		}, CHAT_GIFT_POLL_INTERVAL_MS);
		return () => clearInterval(timer);
	}, [isFocused, isVisible]);
	useEffect(() => {
		if (!operatorPassArrived || !isOperatorPassAvailable()) return;
		OperatorPassModalCommands.reopenCheckoutModalAfterReturn();
	}, [operatorPassArrived]);
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
