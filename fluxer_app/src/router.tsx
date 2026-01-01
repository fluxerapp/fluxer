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

import {i18n} from '@lingui/core';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ThemeActionCreators from '~/actions/ThemeActionCreators';
import {ChannelTypes, ME} from '~/Constants';
import {AppBadge} from '~/components/AppBadge';
import {ChannelIndexPage} from '~/components/channel/ChannelIndexPage';
import {ChannelLayout} from '~/components/channel/ChannelLayout';
import {DMLayout} from '~/components/channel/dm/DMLayout';
import {AppLayout} from '~/components/layout/AppLayout';
import {AuthLayout} from '~/components/layout/AuthLayout';
import {FavoritesLayout} from '~/components/layout/FavoritesLayout';
import {GuildLayout} from '~/components/layout/GuildLayout';
import {GuildsLayout} from '~/components/layout/GuildsLayout';
import {KeyboardModeListener} from '~/components/layout/KeyboardModeListener';
import {MobileBottomNav} from '~/components/layout/MobileBottomNav';
import {SplashScreen} from '~/components/layout/SplashScreen';
import {BookmarksBottomSheet} from '~/components/modals/BookmarksBottomSheet';
import {StatusChangeBottomSheet} from '~/components/modals/StatusChangeBottomSheet';
import AuthorizeIPPage from '~/components/pages/AuthorizeIPPage';
import EmailRevertPage from '~/components/pages/EmailRevertPage';
import ForgotPasswordPage from '~/components/pages/ForgotPasswordPage';
import GiftLoginPage from '~/components/pages/GiftLoginPage';
import GiftRegisterPage from '~/components/pages/GiftRegisterPage';
import InviteLoginPage from '~/components/pages/InviteLoginPage';
import InviteRegisterPage from '~/components/pages/InviteRegisterPage';
import LoginPage from '~/components/pages/LoginPage';
import {NotFoundPage} from '~/components/pages/NotFoundPage';
import {NotificationsPage} from '~/components/pages/NotificationsPage';
import OAuthAuthorizePage from '~/components/pages/OAuthAuthorizePage';
import PendingVerificationPage from '~/components/pages/PendingVerificationPage';
import PremiumCallbackPage from '~/components/pages/PremiumCallbackPage';
import RegisterPage from '~/components/pages/RegisterPage';
import {ReportPage} from '~/components/pages/ReportPage';
import ResetPasswordPage from '~/components/pages/ResetPasswordPage';
import ThemeLoginPage from '~/components/pages/ThemeLoginPage';
import ThemeRegisterPage from '~/components/pages/ThemeRegisterPage';
import VerifyEmailPage from '~/components/pages/VerifyEmailPage';
import {YouPage} from '~/components/pages/YouPage';
import {
	createRootRoute,
	createRoute,
	createRouter,
	Redirect,
	type RouteConfig,
	useLocation,
	useParams,
} from '~/lib/router';
import SessionManager from '~/lib/SessionManager';
import {Routes} from '~/Routes';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import AccountManager from '~/stores/AccountManager';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import ConnectionStore from '~/stores/ConnectionStore';
import InitializationStore from '~/stores/InitializationStore';
import LocationStore from '~/stores/LocationStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import NavigationStore from '~/stores/NavigationStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserStore from '~/stores/UserStore';
import {compareChannelPosition, filterViewableChannels} from '~/utils/channelShared';
import {navigateToWithMobileHistory} from '~/utils/MobileNavigation';
import {isInstalledPwa} from '~/utils/PwaUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import RuntimeConfigStore from './stores/RuntimeConfigStore';

const AUTO_REDIRECT_EXEMPT_PATHS = new Set<string>([
	Routes.RESET_PASSWORD,
	Routes.AUTHORIZE_IP,
	Routes.EMAIL_REVERT,
	Routes.VERIFY_EMAIL,
	Routes.OAUTH_AUTHORIZE,
	Routes.REPORT,
]);

const GuildChannelRouter: React.FC<{guildId: string; children?: React.ReactNode}> = observer(({guildId, children}) => {
	const location = useLocation();

	React.useEffect(() => {
		if (guildId === ME || location.pathname === Routes.ME) {
			return;
		}

		if (MobileLayoutStore.enabled) {
			return;
		}

		if (location.pathname.startsWith('/channels/') && !location.pathname.startsWith(Routes.ME)) {
			if (location.pathname.split('/').length === 3) {
				const pathSegments = location.pathname.split('/');
				const currentGuildId = pathSegments[2];

				if (currentGuildId !== guildId) {
					return;
				}

				const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guildId);

				if (selectedChannelId) {
					const channel = ChannelStore.getChannel(selectedChannelId);
					if (channel && channel.guildId === guildId) {
						RouterUtils.replaceWith(Routes.guildChannel(guildId, selectedChannelId));
					} else {
						const channels = ChannelStore.getGuildChannels(guildId);
						const viewableChannels = filterViewableChannels(channels).sort(compareChannelPosition);

						if (viewableChannels.length > 0) {
							const firstChannel = viewableChannels[0];
							RouterUtils.replaceWith(Routes.guildChannel(guildId, firstChannel.id));
						}
					}
				} else {
					const channels = ChannelStore.getGuildChannels(guildId);
					const viewableChannels = filterViewableChannels(channels).sort(compareChannelPosition);

					if (viewableChannels.length > 0) {
						const firstChannel = viewableChannels[0];
						RouterUtils.replaceWith(Routes.guildChannel(guildId, firstChannel.id));
					}
				}
			}
		}
	}, [guildId, location.pathname, MobileLayoutStore.enabled]);

	if (guildId === ME || location.pathname === Routes.ME) {
		return null;
	}

	return <GuildLayout>{children}</GuildLayout>;
});

const RootComponent: React.FC<{children?: React.ReactNode}> = observer(({children}) => {
	const location = useLocation();
	const isAuthenticated = AuthenticationStore.isAuthenticated;
	const mobileLayoutState = MobileLayoutStore;
	const [hasRestoredLocation, setHasRestoredLocation] = React.useState(false);
	const currentUser = UserStore.currentUser;
	const [hasHandledNotificationNav, setHasHandledNotificationNav] = React.useState(false);
	const [previousMobileLayoutState, setPreviousMobileLayoutState] = React.useState(mobileLayoutState.enabled);
	const lastMobileHistoryBuildRef = React.useRef<{ts: number; path: string} | null>(null);
	const isLocationStoreHydrated = LocationStore.isHydrated;
	const canNavigateToProtectedRoutes = InitializationStore.canNavigateToProtectedRoutes;

	const hasStartedRestoreRef = React.useRef(false);
	const pathname = location.pathname;
	const isDesktopHandoff = location.searchParams.get('desktop_handoff') === '1';
	const isAutoRedirectExemptRoute = AUTO_REDIRECT_EXEMPT_PATHS.has(pathname);
	const shouldSkipAutoRedirect = isAutoRedirectExemptRoute || (pathname === Routes.LOGIN && isDesktopHandoff);

	const isAuthRoute = React.useMemo(() => {
		return (
			pathname.startsWith(Routes.LOGIN) ||
			pathname.startsWith(Routes.REGISTER) ||
			pathname.startsWith(Routes.FORGOT_PASSWORD) ||
			pathname.startsWith(Routes.RESET_PASSWORD) ||
			pathname.startsWith(Routes.VERIFY_EMAIL) ||
			pathname.startsWith(Routes.AUTHORIZE_IP) ||
			pathname.startsWith(Routes.EMAIL_REVERT) ||
			pathname.startsWith(Routes.OAUTH_AUTHORIZE) ||
			pathname.startsWith(Routes.REPORT) ||
			pathname.startsWith('/invite/') ||
			pathname.startsWith('/gift/') ||
			pathname.startsWith('/theme/')
		);
	}, [pathname]);

	const shouldBypassGateway = isAuthRoute && pathname !== Routes.PENDING_VERIFICATION;
	const authToken = AuthenticationStore.authToken;

	React.useEffect(() => {
		if (!SessionManager.isInitialized) {
			return;
		}

		if (AccountManager.isSwitching) {
			return;
		}

		const isAuth = AuthenticationStore.isAuthenticated;

		if (shouldBypassGateway) {
			if (isAuth) {
				if (!shouldSkipAutoRedirect) {
					RouterUtils.replaceWith(Routes.ME);
				}
				return;
			}
			if (ConnectionStore.isConnected || ConnectionStore.isConnecting || ConnectionStore.socket) {
				ConnectionStore.logout();
			}
			return;
		}

		if (!isAuth) {
			const current = pathname + window.location.search;
			RouterUtils.replaceWith(`${Routes.LOGIN}?redirect_to=${encodeURIComponent(current)}`);
			return;
		}

		if (isAuth && InitializationStore.isLoading) {
			void AuthenticationActionCreators.ensureSessionStarted();
		}
	}, [
		SessionManager.isInitialized,
		authToken,
		AccountManager.isSwitching,
		AuthenticationStore.isAuthenticated,
		ConnectionStore.isConnected,
		ConnectionStore.isConnecting,
		InitializationStore.isLoading,
		shouldBypassGateway,
		shouldSkipAutoRedirect,
	]);

	React.useEffect(() => {
		if (
			!isAuthenticated ||
			hasRestoredLocation ||
			hasStartedRestoreRef.current ||
			!canNavigateToProtectedRoutes ||
			!isLocationStoreHydrated
		) {
			return;
		}

		if (location.pathname === Routes.HOME) {
			return;
		}

		hasStartedRestoreRef.current = true;
		setHasRestoredLocation(true);

		const lastLocation = LocationStore.getLastLocation();
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
		isLocationStoreHydrated,
		canNavigateToProtectedRoutes,
		location.pathname,
	]);

	React.useEffect(() => {
		if (!isAuthenticated || !hasRestoredLocation) return;

		if (previousMobileLayoutState !== mobileLayoutState.enabled) {
			setPreviousMobileLayoutState(mobileLayoutState.enabled);

			if (mobileLayoutState.enabled) {
				const currentPath = location.pathname;
				if (
					(Routes.isDMRoute(currentPath) && currentPath !== Routes.ME) ||
					(Routes.isGuildChannelRoute(currentPath) && currentPath.split('/').length === 4)
				) {
					navigateToWithMobileHistory(currentPath, true);
				}
			}
		}
	}, [isAuthenticated, hasRestoredLocation, mobileLayoutState.enabled, previousMobileLayoutState, location.pathname]);

	React.useEffect(() => {
		const shouldSaveLocation = Routes.isChannelRoute(location.pathname) || Routes.isSpecialPage(location.pathname);

		if (isAuthenticated && shouldSaveLocation) {
			LocationStore.saveLocation(location.pathname);
		}
	}, [isAuthenticated, location.pathname]);

	React.useEffect(() => {
		if (!isAuthenticated || !hasRestoredLocation) return;

		if (previousMobileLayoutState !== mobileLayoutState.enabled) {
			setPreviousMobileLayoutState(mobileLayoutState.enabled);

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
		}
	}, [isAuthenticated, hasRestoredLocation, mobileLayoutState.enabled, previousMobileLayoutState, location.pathname]);

	const navigateWithHistoryStack = React.useCallback(
		(url: string) => {
			navigateToWithMobileHistory(url, mobileLayoutState.enabled);
		},
		[mobileLayoutState.enabled],
	);

	React.useEffect(() => {
		if (!isAuthenticated || !mobileLayoutState.enabled) return;

		const handleNotificationNavigate = (event: MessageEvent) => {
			if (event.data?.type === 'NOTIFICATION_CLICK_NAVIGATE') {
				if (hasHandledNotificationNav) {
					return;
				}

				const url = event.data.url;
				const targetUserId = event.data.targetUserId as string | undefined;

				void (async () => {
					if (targetUserId && targetUserId !== AccountManager.currentUserId && AccountManager.canSwitchAccounts) {
						try {
							await AccountManager.switchToAccount(targetUserId);
						} catch (error) {
							console.error('Failed to switch account for notification', error);
						}
					}

					navigateWithHistoryStack(url);
					setHasHandledNotificationNav(true);
				})();

				return;
			}

			if (event.data?.type === 'PUSH_SUBSCRIPTION_CHANGE') {
				if (isInstalledPwa()) {
					void PushSubscriptionService.registerPushSubscription();
				}
			}
		};

		if (!hasHandledNotificationNav) {
			const urlParams = location.searchParams;
			if (urlParams.get('fromNotification') === '1') {
				const newParams = new URLSearchParams(urlParams);
				newParams.delete('fromNotification');
				const cleanPath = location.pathname + (newParams.toString() ? `?${newParams.toString()}` : '');

				navigateWithHistoryStack(cleanPath);
				setHasHandledNotificationNav(true);
			}
		}

		navigator.serviceWorker?.addEventListener('message', handleNotificationNavigate);

		return () => {
			navigator.serviceWorker?.removeEventListener('message', handleNotificationNavigate);
		};
	}, [isAuthenticated, mobileLayoutState.enabled, hasHandledNotificationNav, location, navigateWithHistoryStack]);

	React.useEffect(() => {
		if (currentUser?.pendingManualVerification) {
			if (
				pathname !== Routes.PENDING_VERIFICATION &&
				!pathname.startsWith('/login') &&
				!pathname.startsWith('/register')
			) {
				RouterUtils.replaceWith(Routes.PENDING_VERIFICATION);
			}
		}
	}, [currentUser, pathname]);

	const showBottomNav =
		mobileLayoutState.enabled &&
		(location.pathname === Routes.ME ||
			Routes.isFavoritesRoute(location.pathname) ||
			location.pathname === Routes.NOTIFICATIONS ||
			location.pathname === Routes.YOU ||
			(Routes.isGuildChannelRoute(location.pathname) && location.pathname.split('/').length === 3));

	if (isAuthenticated && !canNavigateToProtectedRoutes && !shouldBypassGateway) {
		return <SplashScreen />;
	}

	return (
		<>
			<KeyboardModeListener />
			{children}
			{showBottomNav && currentUser && <MobileBottomNav currentUser={currentUser} />}
		</>
	);
});

const rootRoute = createRootRoute({
	layout: ({children}) => <>{children}</>,
});

const notFoundRoute = createRoute({
	id: '__notFound',
	path: '/__notfound',
	component: () => <NotFoundPage />,
});

const homeRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'home',
	path: '/',
	onEnter: () => new Redirect(Routes.ME),
});

const authLayoutRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'authLayout',
	layout: ({children}) => <AuthLayout>{children}</AuthLayout>,
});

const loginRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'login',
	path: '/login',
	onEnter: () => {
		if (AuthenticationStore.isAuthenticated) {
			const search = window.location.search;
			const qp = new URLSearchParams(search);
			const isDesktopHandoff = qp.get('desktop_handoff') === '1';
			if (isDesktopHandoff) {
				return undefined;
			}
			const redirectTo = qp.get('redirect_to');
			return new Redirect(redirectTo || Routes.ME);
		}
		return undefined;
	},
	component: () => <LoginPage />,
});

const registerRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'register',
	path: '/register',
	onEnter: () => {
		if (AuthenticationStore.isAuthenticated) {
			const search = window.location.search;
			const qp = new URLSearchParams(search);
			const redirectTo = qp.get('redirect_to');
			return new Redirect(redirectTo || Routes.ME);
		}
		return undefined;
	},
	component: () => <RegisterPage />,
});

const oauthAuthorizeRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'oauthAuthorize',
	path: Routes.OAUTH_AUTHORIZE,
	onEnter: () => {
		if (!AuthenticationStore.isAuthenticated) {
			const current = window.location.pathname + window.location.search;
			return new Redirect(`${Routes.LOGIN}?redirect_to=${encodeURIComponent(current)}`);
		}
		return undefined;
	},
	component: () => <OAuthAuthorizePage />,
});

const inviteRegisterRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'inviteRegister',
	path: '/invite/:code',
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const code = ctx.params.code;
			if (code) {
				InviteActionCreators.openAcceptModal(code);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <InviteRegisterPage />,
});

const inviteLoginRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'inviteLogin',
	path: '/invite/:code/login',
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const code = ctx.params.code;
			if (code) {
				InviteActionCreators.openAcceptModal(code);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <InviteLoginPage />,
});

const giftRegisterRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'giftRegister',
	path: '/gift/:code',
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const code = ctx.params.code;
			if (code) {
				GiftActionCreators.openAcceptModal(code);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <GiftRegisterPage />,
});

const giftLoginRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'giftLogin',
	path: '/gift/:code/login',
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const code = ctx.params.code;
			if (code) {
				GiftActionCreators.openAcceptModal(code);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <GiftLoginPage />,
});

const forgotPasswordRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'forgotPassword',
	path: Routes.FORGOT_PASSWORD,
	onEnter: () => {
		if (AuthenticationStore.isAuthenticated) {
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <ForgotPasswordPage />,
});

const resetPasswordRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'resetPassword',
	path: Routes.RESET_PASSWORD,
	component: () => <ResetPasswordPage />,
});

const emailRevertRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'emailRevert',
	path: Routes.EMAIL_REVERT,
	component: () => <EmailRevertPage />,
});

const verifyEmailRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'verifyEmail',
	path: Routes.VERIFY_EMAIL,
	component: () => <VerifyEmailPage />,
});

const authorizeIPRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'authorizeIP',
	path: Routes.AUTHORIZE_IP,
	component: () => <AuthorizeIPPage />,
});

const pendingVerificationRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'pendingVerification',
	path: Routes.PENDING_VERIFICATION,
	component: () => <PendingVerificationPage />,
});

const reportRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'report',
	path: Routes.REPORT,
	component: () => <ReportPage />,
});

const appLayoutRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'appLayout',
	onEnter: () => {
		if (!SessionManager.isInitialized) {
			return undefined;
		}
		if (!AuthenticationStore.isAuthenticated) {
			const current = window.location.pathname + window.location.search;
			return new Redirect(`${Routes.LOGIN}?redirect_to=${encodeURIComponent(current)}`);
		}
		return undefined;
	},
	layout: ({children}) => (
		<>
			<AppBadge />
			<AppLayout>{children}</AppLayout>
		</>
	),
});

const guildsLayoutRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'guildsLayout',
	layout: ({children}) => <GuildsLayout>{children}</GuildsLayout>,
});

const notificationsRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'notifications',
	path: Routes.NOTIFICATIONS,
	component: () => {
		const [bookmarksSheetOpen, setBookmarksSheetOpen] = React.useState(false);

		return (
			<>
				<NotificationsPage onBookmarksClick={() => setBookmarksSheetOpen(true)} />
				<BookmarksBottomSheet isOpen={bookmarksSheetOpen} onClose={() => setBookmarksSheetOpen(false)} />
			</>
		);
	},
});

const youRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'you',
	path: Routes.YOU,
	component: () => {
		const [statusSheetOpen, setStatusSheetOpen] = React.useState(false);

		return (
			<>
				<YouPage onAvatarClick={() => setStatusSheetOpen(true)} />
				<StatusChangeBottomSheet isOpen={statusSheetOpen} onClose={() => setStatusSheetOpen(false)} />
			</>
		);
	},
});

const premiumCallbackRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'premiumCallback',
	path: Routes.PREMIUM_CALLBACK,
	component: () => <PremiumCallbackPage />,
});

const themeRegisterRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'themeRegister',
	path: Routes.THEME_REGISTER,
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const themeId = ctx.params.themeId;
			if (themeId) {
				ThemeActionCreators.openAcceptModal(themeId, i18n);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <ThemeRegisterPage />,
});

const themeLoginRoute = createRoute({
	getParentRoute: () => authLayoutRoute,
	id: 'themeLogin',
	path: Routes.THEME_LOGIN,
	onEnter: (ctx) => {
		if (AuthenticationStore.isAuthenticated) {
			const themeId = ctx.params.themeId;
			if (themeId) {
				ThemeActionCreators.openAcceptModal(themeId, i18n);
			}
			return new Redirect(Routes.ME);
		}
		return undefined;
	},
	component: () => <ThemeLoginPage />,
});

const bookmarksRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'bookmarks',
	path: Routes.BOOKMARKS,
	component: () => <DMLayout />,
});

const mentionsRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'mentions',
	path: Routes.MENTIONS,
	component: () => <DMLayout />,
});

const meRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'me',
	path: '/channels/@me',
	component: observer(() => {
		const isMobileLayout = MobileLayoutStore.enabled;

		React.useEffect(() => {
			if (!isMobileLayout && SelectedChannelStore.selectedChannelIds.has(ME)) {
				SelectedChannelStore.clearGuildSelection(ME);
			}
		}, [isMobileLayout]);

		return <DMLayout />;
	}),
});

const favoritesRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'favorites',
	path: '/channels/@favorites',
	layout: ({children}) => <FavoritesLayout>{children}</FavoritesLayout>,
});

const favoritesChannelRoute = createRoute({
	getParentRoute: () => favoritesRoute,
	id: 'favoritesChannel',
	path: '/channels/@favorites/:channelId',
	component: () => (
		<ChannelLayout>
			<ChannelIndexPage />
		</ChannelLayout>
	),
});

const channelsRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'channels',
	path: '/channels/:guildId',
	layout: ({children}) => {
		const params = useParams() as {guildId: string};
		const {guildId} = params;

		if (guildId === ME) {
			return <DMLayout>{children}</DMLayout>;
		}

		return guildId ? <GuildChannelRouter guildId={guildId}>{children}</GuildChannelRouter> : null;
	},
});

const channelRoute = createRoute({
	getParentRoute: () => channelsRoute,
	id: 'channel',
	path: '/channels/:guildId/:channelId',
	onEnter: (ctx) => {
		const {guildId, channelId} = ctx.params;
		const channel = ChannelStore.getChannel(channelId);
		if (channel && (channel.type === ChannelTypes.GUILD_CATEGORY || channel.type === ChannelTypes.GUILD_LINK)) {
			return new Redirect(Routes.guildChannel(guildId));
		}
		return undefined;
	},
	component: () => (
		<ChannelLayout>
			<ChannelIndexPage />
		</ChannelLayout>
	),
});

const messageRoute = createRoute({
	getParentRoute: () => channelRoute,
	id: 'message',
	path: '/channels/:guildId/:channelId/:messageId',
	onEnter: (ctx) => {
		const {guildId, channelId} = ctx.params;
		const channel = ChannelStore.getChannel(channelId);
		if (channel && (channel.type === ChannelTypes.GUILD_CATEGORY || channel.type === ChannelTypes.GUILD_LINK)) {
			return new Redirect(Routes.guildChannel(guildId));
		}
		return undefined;
	},
	component: () => (
		<ChannelLayout>
			<ChannelIndexPage />
		</ChannelLayout>
	),
});

const authRouteTree = authLayoutRoute.addChildren([
	loginRoute,
	registerRoute,
	oauthAuthorizeRoute,
	inviteRegisterRoute,
	inviteLoginRoute,
	themeRegisterRoute,
	themeLoginRoute,
	forgotPasswordRoute,
	resetPasswordRoute,
	emailRevertRoute,
	verifyEmailRoute,
	authorizeIPRoute,
	pendingVerificationRoute,
	reportRoute,
	...(RuntimeConfigStore.isSelfHosted() ? [] : [giftRegisterRoute, giftLoginRoute]),
]);

const routeTree = rootRoute.addChildren([
	homeRoute,
	notFoundRoute,
	authRouteTree,
	appLayoutRoute.addChildren([
		notificationsRoute,
		youRoute,
		premiumCallbackRoute,
		guildsLayoutRoute.addChildren([
			bookmarksRoute,
			mentionsRoute,
			meRoute,
			favoritesRoute.addChildren([favoritesChannelRoute]),
			channelsRoute.addChildren([channelRoute.addChildren([messageRoute])]),
		]),
	]),
]);

const routes: Array<RouteConfig> = routeTree.build();

const rootComponentRoute = routes.find((r) => r.id === '__root');
if (rootComponentRoute) {
	rootComponentRoute.layout = (props) => <RootComponent>{props.children}</RootComponent>;
}

export const router = createRouter({
	routes,
	history: RouterUtils.getHistory() ?? undefined,
	notFoundRouteId: '__notFound',
	scrollRestoration: 'top',
});

NavigationStore.initialize(router);
