// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {isAutoRedirectExemptPath} from '@app/app/router/RouterConstants';
import {KeyboardModeListener} from '@app/features/app/components/layout/KeyboardModeListener';
import {MobileBottomNav} from '@app/features/app/components/layout/MobileBottomNav';
import {SelfHostedSetupWizardGate} from '@app/features/app/components/setup/SelfHostedSetupWizardGate';
import {AppSkeleton} from '@app/features/app/components/skeleton/AppSkeleton';
import {useLoadingSkeletonRetention} from '@app/features/app/hooks/useLoadingSkeletonRetention';
import {useSkeletonLayoutMemoryCapture} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {
	shouldShowLoadingSkeleton as getShouldShowLoadingSkeleton,
	shouldShowMobileBottomNav,
} from '@app/features/app/state/AppShellPresentationPolicy';
import {isClientBooting} from '@app/features/app/state/ClientReadiness';
import Initialization from '@app/features/app/state/Initialization';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {getDefaultLandingPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import {navigateToWithMobileHistory} from '@app/features/navigation/utils/MobileNavigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import * as PushSubscriptionService from '@app/features/platform/push/PushSubscriptionService';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useHolidayClasses} from '@app/features/theme/hooks/useHolidayClasses';
import Location from '@app/features/ui/state/Location';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {isInstalledPwa} from '@app/features/ui/utils/PwaUtils';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import Users from '@app/features/user/state/Users';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('RootComponent');
export const RootComponent: React.FC<{children?: React.ReactNode}> = observer(({children}) => {
	useHolidayClasses();
	const location = useLocation();
	const isAuthenticated = Authentication.isAuthenticated;
	const requiresSelfHostedSetup = RuntimeConfig.requiresSelfHostedSetup();
	const mobileLayoutState = MobileLayout;
	const [hasRestoredLocation, setHasRestoredLocation] = useState(false);
	const [effectiveSkeletonPathname, setEffectiveSkeletonPathname] = useState<string | null>(null);
	const currentUser = Users.currentUser;
	const [hasHandledNotificationNav, setHasHandledNotificationNav] = useState(false);
	const previousMobileLayoutStateRef = useRef(mobileLayoutState.enabled);
	const lastMobileHistoryBuildRef = useRef<{ts: number; path: string} | null>(null);
	const lastNotificationNavRef = useRef<{ts: number; key: string} | null>(null);
	const isLocationStateHydrated = Location.isHydrated;
	const canNavigateToProtectedRoutes = Initialization.canNavigateToProtectedRoutes;
	const pendingRedirectRef = useRef<string | null>(null);
	const hasStartedRestoreRef = useRef(false);
	const pathname = location.pathname;
	const isHandoff = isHandoffRequest(location.searchParams);
	const isAutoRedirectExemptRoute = isAutoRedirectExemptPath(pathname);
	const shouldSkipAutoRedirect = isAutoRedirectExemptRoute || (pathname === Routes.LOGIN && isHandoff);
	const isStandaloneRoute = useMemo(() => {
		return (
			pathname.startsWith(Routes.LOGIN) ||
			pathname.startsWith(Routes.REGISTER) ||
			pathname.startsWith(Routes.FORGOT_PASSWORD) ||
			pathname.startsWith(Routes.RESET_PASSWORD) ||
			pathname.startsWith(Routes.VERIFY_EMAIL) ||
			pathname.startsWith(Routes.AUTHORIZE_IP) ||
			pathname.startsWith(Routes.EMAIL_REVERT) ||
			pathname.startsWith(Routes.OAUTH_AUTHORIZE) ||
			pathname.startsWith(Routes.SSO_CALLBACK) ||
			pathname.startsWith(Routes.REPORT) ||
			pathname.startsWith(Routes.PREMIUM_CALLBACK) ||
			pathname.startsWith(Routes.AGE_VERIFICATION_CALLBACK) ||
			pathname.startsWith(Routes.CONNECTION_CALLBACK) ||
			pathname === '/__notfound' ||
			pathname.startsWith('/invite/') ||
			pathname.startsWith('/gift/') ||
			pathname.startsWith('/theme/')
		);
	}, [pathname]);
	const shouldShowSelfHostedSetup = requiresSelfHostedSetup && !isStandaloneRoute;
	const shouldBypassGateway = requiresSelfHostedSetup || isStandaloneRoute;
	const authToken = Authentication.authToken;
	const shouldShowLoadingSkeleton = getShouldShowLoadingSkeleton({
		booting: isClientBooting(),
		isAuthenticated,
		isAuthSessionInitialized: SessionManager.isInitialized,
		shouldBypassGateway,
	});
	useSkeletonLayoutMemoryCapture(
		isAuthenticated && Initialization.isReady && !shouldBypassGateway && !shouldShowLoadingSkeleton,
	);
	const shouldDismissTransientOverlays = !isAuthenticated || shouldShowLoadingSkeleton;
	const {isLoadingSkeletonRetained, handleLoadingSkeletonTransitionEnd} = useLoadingSkeletonRetention({
		shouldShowLoadingSkeleton,
	});
	let pendingRestorableSkeletonPathname: string | null = null;
	if (
		isAuthenticated &&
		isLocationStateHydrated &&
		!AccountManager.isSwitching &&
		pathname === Routes.ME &&
		!hasRestoredLocation
	) {
		const lastLocation = Location.getLastLocation();
		if (lastLocation != null && lastLocation !== pathname) {
			pendingRestorableSkeletonPathname = lastLocation;
		}
	}
	useLayoutEffect(() => {
		if (
			!isAuthenticated ||
			!isLocationStateHydrated ||
			AccountManager.isSwitching ||
			pathname !== Routes.ME ||
			hasRestoredLocation
		) {
			return;
		}
		const lastLocation = Location.getLastLocation();
		if (lastLocation && lastLocation !== pathname) {
			setEffectiveSkeletonPathname(lastLocation);
		}
	}, [hasRestoredLocation, isAuthenticated, isLocationStateHydrated, pathname]);
	useLayoutEffect(() => {
		if (effectiveSkeletonPathname != null && pathname !== Routes.ME) {
			setEffectiveSkeletonPathname(null);
		}
	}, [effectiveSkeletonPathname, pathname]);
	useLayoutEffect(() => {
		if (!shouldShowLoadingSkeleton && !isLoadingSkeletonRetained && effectiveSkeletonPathname != null) {
			setEffectiveSkeletonPathname(null);
		}
	}, [effectiveSkeletonPathname, isLoadingSkeletonRetained, shouldShowLoadingSkeleton]);
	useEffect(() => {
		if (!isAuthenticated) return;
		void PushSubscriptionService.cleanupNativeDesktopWebPushSubscriptions('desktop-authenticated');
	}, [isAuthenticated]);
	const normalizeInternalUrl = useCallback((rawUrl: string): string => {
		try {
			const u = new URL(rawUrl, window.location.origin);
			if (u.origin === window.location.origin) {
				return u.pathname + u.search + u.hash;
			}
			return rawUrl;
		} catch {
			return rawUrl;
		}
	}, []);
	useLayoutEffect(() => {
		if (!shouldDismissTransientOverlays) return;
		UserProfileCommands.closeUserProfileSurfaces();
	}, [shouldDismissTransientOverlays]);
	useEffect(() => {
		if (!SessionManager.isInitialized) return;
		if (AccountManager.isSwitching) return;
		const isAuth = Authentication.isAuthenticated;
		if (isAuth && isStandaloneRoute) return;
		if (shouldBypassGateway) {
			if (isAuth) {
				if (!shouldSkipAutoRedirect) {
					RouterUtils.replaceWith(getDefaultLandingPath());
				}
				return;
			}
			if (GatewayConnection.isConnected || GatewayConnection.isConnecting || GatewayConnection.socket) {
				GatewayConnection.logout();
			}
			return;
		}
		if (!isAuth) {
			const current = pathname + window.location.search;
			if (!pendingRedirectRef.current) {
				pendingRedirectRef.current = current;
			}
			RouterUtils.replaceWith(setPathQueryParams(Routes.LOGIN, {redirect_to: pendingRedirectRef.current}));
			return;
		}
		if (isAuth && Initialization.isLoading) {
			void AuthenticationCommands.ensureSessionStarted();
		}
	}, [
		SessionManager.isInitialized,
		authToken,
		AccountManager.isSwitching,
		Authentication.isAuthenticated,
		GatewayConnection.isConnected,
		GatewayConnection.isConnecting,
		Initialization.isLoading,
		shouldBypassGateway,
		shouldSkipAutoRedirect,
		pendingRedirectRef,
	]);
	useEffect(() => {
		if (!Authentication.isAuthenticated) return;
		const target = pendingRedirectRef.current;
		if (!target) return;
		const current = location.pathname + window.location.search;
		if (current !== target) {
			RouterUtils.replaceWith(target);
		}
		pendingRedirectRef.current = null;
	}, [Authentication.isAuthenticated, location.pathname]);
	useEffect(() => {
		if (
			!isAuthenticated ||
			hasRestoredLocation ||
			hasStartedRestoreRef.current ||
			!canNavigateToProtectedRoutes ||
			!isLocationStateHydrated
		) {
			return;
		}
		if (location.pathname === Routes.HOME) {
			return;
		}
		hasStartedRestoreRef.current = true;
		setHasRestoredLocation(true);
		const lastLocation = Location.getLastLocation();
		if (lastLocation && lastLocation !== location.pathname && location.pathname === Routes.ME) {
			navigateToWithMobileHistory(lastLocation, mobileLayoutState.enabled);
		} else if (mobileLayoutState.enabled) {
			const p = location.pathname;
			if ((Routes.isDMRoute(p) && p !== Routes.ME) || (Routes.isGuildChannelRoute(p) && p.split('/').length === 4)) {
				navigateToWithMobileHistory(p, true);
				setHasHandledNotificationNav(true);
			}
		}
	}, [
		isAuthenticated,
		hasRestoredLocation,
		mobileLayoutState.enabled,
		isLocationStateHydrated,
		canNavigateToProtectedRoutes,
		location.pathname,
	]);
	useEffect(() => {
		const shouldSaveLocation = Routes.isChannelRoute(location.pathname) || Routes.isSpecialPage(location.pathname);
		if (isAuthenticated && shouldSaveLocation) {
			Location.saveLocation(location.pathname);
		}
	}, [isAuthenticated, location.pathname]);
	useEffect(() => {
		if (!isAuthenticated || !hasRestoredLocation) return;
		const previousMobileLayoutState = previousMobileLayoutStateRef.current;
		if (previousMobileLayoutState === mobileLayoutState.enabled) return;
		previousMobileLayoutStateRef.current = mobileLayoutState.enabled;
		if (mobileLayoutState.enabled) {
			const currentPath = location.pathname;
			const now = Date.now();
			const last = lastMobileHistoryBuildRef.current;
			if (last && last.path === currentPath && now - last.ts < 1500) {
				return;
			}
			lastMobileHistoryBuildRef.current = {ts: now, path: currentPath};
			if (
				(Routes.isDMRoute(currentPath) && currentPath !== Routes.ME) ||
				(Routes.isGuildChannelRoute(currentPath) && currentPath.split('/').length === 4)
			) {
				if (Routes.isDMRoute(currentPath) && currentPath !== Routes.ME) {
					RouterUtils.replaceWith(Routes.ME);
					setTimeout(() => RouterUtils.transitionTo(currentPath), 0);
				} else if (Routes.isGuildChannelRoute(currentPath) && currentPath.split('/').length === 4) {
					const parts = currentPath.split('/');
					const guildId = parts[2];
					const guildPath = Routes.guildChannel(guildId);
					RouterUtils.replaceWith(guildPath);
					setTimeout(() => RouterUtils.transitionTo(currentPath), 0);
				}
			}
		}
	}, [isAuthenticated, hasRestoredLocation, mobileLayoutState.enabled, location.pathname]);
	const navigateWithHistoryStack = useCallback(
		(url: string) => {
			navigateToWithMobileHistory(url, mobileLayoutState.enabled);
		},
		[mobileLayoutState.enabled],
	);
	useEffect(() => {
		if (!isAuthenticated) return;
		const handleNotificationNavigate = (event: MessageEvent) => {
			if (event.data?.type === 'NOTIFICATION_CLICK_NAVIGATE') {
				const rawUrl = typeof event.data.url === 'string' ? event.data.url : null;
				if (!rawUrl) return;
				const targetUserId =
					typeof event.data.targetUserId === 'string' ? (event.data.targetUserId as string) : undefined;
				const normalizedUrl = normalizeInternalUrl(rawUrl);
				const key = `${targetUserId ?? ''}:${normalizedUrl}`;
				const now = Date.now();
				const last = lastNotificationNavRef.current;
				if (last && last.key === key && now - last.ts < 1500) {
					return;
				}
				lastNotificationNavRef.current = {ts: now, key};
				void (async () => {
					if (targetUserId && targetUserId !== AccountManager.currentUserId && AccountManager.canSwitchAccounts) {
						try {
							await AccountManager.switchToAccount(targetUserId);
						} catch (error) {
							logger.error('Failed to switch account for notification', error);
						}
					}
					if (mobileLayoutState.enabled) {
						navigateWithHistoryStack(normalizedUrl);
					} else {
						RouterUtils.transitionTo(normalizedUrl);
					}
					setHasHandledNotificationNav(true);
				})();
				return;
			}
			if (event.data?.type === 'PUSH_SUBSCRIPTION_CHANGE') {
				if (PushSubscriptionService.isWebPushBlockedForNativeDesktop()) {
					void PushSubscriptionService.cleanupNativeDesktopWebPushSubscriptions('service-worker-message');
				} else if (isInstalledPwa()) {
					void PushSubscriptionService.registerPushSubscription();
				}
				return;
			}
		};
		if (!hasHandledNotificationNav) {
			const urlParams = location.searchParams;
			if (urlParams.get('fromNotification') === '1') {
				const newParams = new URLSearchParams(urlParams);
				newParams.delete('fromNotification');
				const cleanUrl = new URL(location.pathname, window.location.origin);
				cleanUrl.search = newParams.toString();
				const cleanPath = `${cleanUrl.pathname}${cleanUrl.search}${cleanUrl.hash}`;
				if (mobileLayoutState.enabled) {
					navigateWithHistoryStack(cleanPath);
				} else {
					RouterUtils.transitionTo(cleanPath);
				}
				setHasHandledNotificationNav(true);
			}
		}
		navigator.serviceWorker?.addEventListener('message', handleNotificationNavigate);
		return () => {
			navigator.serviceWorker?.removeEventListener('message', handleNotificationNavigate);
		};
	}, [
		isAuthenticated,
		mobileLayoutState.enabled,
		hasHandledNotificationNav,
		location,
		navigateWithHistoryStack,
		normalizeInternalUrl,
	]);
	const showBottomNav = shouldShowMobileBottomNav({
		mobileLayoutEnabled: mobileLayoutState.enabled,
		pathname,
	});
	if (shouldShowSelfHostedSetup) {
		return <SelfHostedSetupWizardGate data-flx="app.router.root-component.self-hosted-setup-wizard-gate" />;
	}
	return (
		<>
			{!shouldShowLoadingSkeleton && (
				<>
					<KeyboardModeListener data-flx="app.router.root-component.keyboard-mode-listener" />
					{children}
					{showBottomNav && currentUser && (
						<MobileBottomNav currentUser={currentUser} data-flx="app.router.root-component.mobile-bottom-nav" />
					)}
				</>
			)}
			{(shouldShowLoadingSkeleton || isLoadingSkeletonRetained) && (
				<AppSkeleton
					key={`loading-skeleton:${AccountManager.currentUserId ?? 'unauthenticated'}`}
					effectivePathname={effectiveSkeletonPathname ?? pendingRestorableSkeletonPathname ?? pathname}
					isExiting={!shouldShowLoadingSkeleton}
					onTransitionEnd={handleLoadingSkeletonTransitionEnd}
					data-flx="app.router.root-component.app-skeleton"
				/>
			)}
		</>
	);
});
