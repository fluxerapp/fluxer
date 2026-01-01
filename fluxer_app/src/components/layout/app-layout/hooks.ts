/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import {clearPendingBulkDeletionNagbarDismissal} from '~/actions/NagbarActionCreators';
import AppStorage from '~/lib/AppStorage';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import NagbarStore from '~/stores/NagbarStore';
import UserStore from '~/stores/UserStore';
import {isDesktop} from '~/utils/NativeUtils';
import * as NotificationUtils from '~/utils/NotificationUtils';
import {isStandalonePwa} from '~/utils/PwaUtils';
import {type AppLayoutState, type NagbarConditions, type NagbarState, NagbarType, UPDATE_DISMISS_KEY} from './types';

export const useAppLayoutState = (): AppLayoutState => {
	const [isStandalone, setIsStandalone] = React.useState(isStandalonePwa());

	React.useEffect(() => {
		const checkStandalone = () => {
			setIsStandalone(isStandalonePwa());
		};

		checkStandalone();

		document.documentElement.classList.toggle('is-standalone', isStandalone);

		return () => {
			document.documentElement.classList.remove('is-standalone');
		};
	}, [isStandalone]);

	return {isStandalone};
};

export const useNagbarConditions = (): NagbarConditions => {
	const user = UserStore.currentUser;
	const nagbarState = NagbarStore;
	const premiumOverrideType = DeveloperOptionsStore.premiumTypeOverride;
	const premiumWillCancel = user?.premiumWillCancel ?? false;
	const isMockPremium = premiumOverrideType != null && premiumOverrideType > 0;
	const previousPendingBulkDeletionKeyRef = React.useRef<string | null>(null);

	React.useEffect(() => {
		const handleVersionUpdateAvailable = () => {
			if (nagbarState.forceUpdateAvailable) {
				return;
			}

			const dismissedUntilStr = AppStorage.getItem(UPDATE_DISMISS_KEY);
			if (dismissedUntilStr) {
				const dismissedUntil = Number.parseInt(dismissedUntilStr, 10);
				if (Date.now() < dismissedUntil) {
					return;
				}
				localStorage.removeItem(UPDATE_DISMISS_KEY);
			}
		};

		window.addEventListener('version-update-available', handleVersionUpdateAvailable as EventListener);

		return () => {
			window.removeEventListener('version-update-available', handleVersionUpdateAvailable as EventListener);
		};
	}, [nagbarState.forceUpdateAvailable]);

	const shouldShowDesktopNotification = (() => {
		if (!NotificationUtils.hasNotification()) return false;
		if (typeof Notification !== 'undefined') {
			if (Notification.permission === 'granted') return false;
			if (Notification.permission === 'denied') return false;
		}
		return true;
	})();

	const canShowPremiumGracePeriod = (() => {
		if (nagbarState.forceHidePremiumGracePeriod) return false;
		if (nagbarState.forcePremiumGracePeriod) return true;
		if (!user?.premiumUntil || user.premiumType === 2 || premiumWillCancel) return false;
		const now = new Date();
		const expiryDate = new Date(user.premiumUntil);
		const gracePeriodMs = 3 * 24 * 60 * 60 * 1000;
		const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
		const isInGracePeriod = now > expiryDate && now <= graceEndDate;
		return isInGracePeriod;
	})();

	const canShowPremiumExpired = (() => {
		if (nagbarState.forceHidePremiumExpired) return false;
		if (nagbarState.forcePremiumExpired) return true;
		if (!user?.premiumUntil || user.premiumType === 2 || premiumWillCancel) return false;
		const now = new Date();
		const expiryDate = new Date(user.premiumUntil);
		const gracePeriodMs = 3 * 24 * 60 * 60 * 1000;
		const expiredStateDurationMs = 30 * 24 * 60 * 60 * 1000;
		const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
		const expiredStateEndDate = new Date(expiryDate.getTime() + expiredStateDurationMs);
		const isExpired = now > graceEndDate;
		const showExpiredState = isExpired && now <= expiredStateEndDate;
		return showExpiredState;
	})();

	const canShowGiftInventory = (() => {
		if (nagbarState.forceHideGiftInventory) return false;
		if (nagbarState.forceGiftInventory) return true;
		return Boolean(user?.hasUnreadGiftInventory && !nagbarState.giftInventoryDismissed);
	})();

	const canShowPremiumOnboarding = (() => {
		if (nagbarState.forceHidePremiumOnboarding) return false;
		if (nagbarState.forcePremiumOnboarding) return true;
		if (isMockPremium) return false;
		return Boolean(
			user?.isPremium() && !user?.hasDismissedPremiumOnboarding && !nagbarState.premiumOnboardingDismissed,
		);
	})();

	const isNativeDesktop = isDesktop();
	const isMobileDevice = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
	const isDesktopBrowser = !isNativeDesktop && !isMobileDevice;

	const canShowDesktopDownload = (() => {
		if (nagbarState.forceHideDesktopDownload) return false;
		if (nagbarState.forceDesktopDownload) return true;
		return isDesktopBrowser && !nagbarState.desktopDownloadDismissed;
	})();

	const canShowMobileDownload = (() => {
		if (nagbarState.forceHideMobileDownload) return false;
		if (nagbarState.forceMobileDownload) return true;
		return Boolean(!isMobileDevice && user?.usedMobileClient === false && !nagbarState.mobileDownloadDismissed);
	})();

	const pendingBulkDeletion = user?.getPendingBulkMessageDeletion();
	const pendingBulkDeletionKey = pendingBulkDeletion?.scheduledAt.toISOString() ?? null;

	React.useEffect(() => {
		const previousKey = previousPendingBulkDeletionKeyRef.current;
		if (previousKey && previousKey !== pendingBulkDeletionKey) {
			clearPendingBulkDeletionNagbarDismissal(previousKey);
		}

		previousPendingBulkDeletionKeyRef.current = pendingBulkDeletionKey;
	}, [pendingBulkDeletionKey]);

	const hasPendingBulkMessageDeletion = Boolean(
		pendingBulkDeletion && !nagbarState.hasPendingBulkDeletionDismissed(pendingBulkDeletionKey),
	);

	return {
		userIsUnclaimed: nagbarState.forceHideUnclaimedAccount
			? false
			: nagbarState.forceUnclaimedAccount
				? true
				: Boolean(user && !user.isClaimed()),
		userNeedsVerification: nagbarState.forceHideEmailVerification
			? false
			: nagbarState.forceEmailVerification
				? true
				: Boolean(user?.isClaimed() && !user.verified),
		canShowDesktopNotification: nagbarState.forceHideDesktopNotification
			? false
			: nagbarState.forceDesktopNotification
				? true
				: shouldShowDesktopNotification && !nagbarState.desktopNotificationDismissed,
		canShowPremiumGracePeriod,
		canShowPremiumExpired,
		canShowPremiumOnboarding,
		canShowGiftInventory,
		canShowDesktopDownload,
		canShowMobileDownload,
		hasPendingBulkMessageDeletion,
	};
};

export const useActiveNagbars = (conditions: NagbarConditions): Array<NagbarState> => {
	return React.useMemo(() => {
		const undismissibleTypes = new Set<NagbarType>([
			NagbarType.UNCLAIMED_ACCOUNT,
			NagbarType.EMAIL_VERIFICATION,
			NagbarType.BULK_DELETE_PENDING,
		]);

		const nagbars: Array<NagbarState> = [
			{
				type: NagbarType.BULK_DELETE_PENDING,
				priority: 0,
				visible: conditions.hasPendingBulkMessageDeletion,
			},
			{
				type: NagbarType.UNCLAIMED_ACCOUNT,
				priority: 1,
				visible: conditions.userIsUnclaimed,
			},
			{
				type: NagbarType.EMAIL_VERIFICATION,
				priority: 2,
				visible: conditions.userNeedsVerification,
			},
			{
				type: NagbarType.PREMIUM_GRACE_PERIOD,
				priority: 3,
				visible: conditions.canShowPremiumGracePeriod,
			},
			{
				type: NagbarType.PREMIUM_EXPIRED,
				priority: 4,
				visible: conditions.canShowPremiumExpired,
			},
			{
				type: NagbarType.PREMIUM_ONBOARDING,
				priority: 5,
				visible: conditions.canShowPremiumOnboarding,
			},
			{
				type: NagbarType.DESKTOP_NOTIFICATION,
				priority: 7,
				visible: conditions.canShowDesktopNotification,
			},
			{
				type: NagbarType.GIFT_INVENTORY,
				priority: 8,
				visible: conditions.canShowGiftInventory,
			},
			{
				type: NagbarType.DESKTOP_DOWNLOAD,
				priority: 9,
				visible: conditions.canShowDesktopDownload,
			},
			{
				type: NagbarType.MOBILE_DOWNLOAD,
				priority: 10,
				visible: conditions.canShowMobileDownload,
			},
		];

		const visibleNagbars = nagbars.filter((nagbar) => nagbar.visible).sort((a, b) => a.priority - b.priority);

		const undismissibleNagbars = visibleNagbars.filter((nagbar) => undismissibleTypes.has(nagbar.type));
		const regularNagbars = visibleNagbars.filter((nagbar) => !undismissibleTypes.has(nagbar.type));

		const selectedRegularNagbars = regularNagbars.length > 0 ? [regularNagbars[0]] : [];

		return [...undismissibleNagbars, ...selectedRegularNagbars].sort((a, b) => a.priority - b.priority);
	}, [conditions]);
};
