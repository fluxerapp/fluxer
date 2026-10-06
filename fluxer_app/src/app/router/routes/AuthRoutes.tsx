// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {rootRoute} from '@app/app/router/routes/RootRoutes';
import {AuthLayout} from '@app/features/app/components/layout/AuthLayout';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	type AuthRoutePage,
	createAuthRoutePage,
	createNamedAuthRoutePage,
} from '@app/features/auth/flow/AuthLoadableRoutePage';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import Authentication from '@app/features/auth/state/Authentication';
import {safeRedirectTarget, safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {createRoute} from '@app/features/platform/components/router/RouterBuilder';
import type {RouteConfig, RouteContext} from '@app/features/platform/components/router/RouterTypes';
import {Redirect} from '@app/features/platform/components/router/RouterTypes';
import SessionManager from '@app/features/platform/state/AuthSession';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {i18n} from '@lingui/core';

const AuthorizeIPPage = createAuthRoutePage(
	'AuthorizeIPPage',
	() => import('@app/features/auth/components/pages/AuthorizeIPPage'),
);
const EmailRevertPage = createAuthRoutePage(
	'EmailRevertPage',
	() => import('@app/features/auth/components/pages/EmailRevertPage'),
);
const ForgotPasswordPage = createAuthRoutePage(
	'ForgotPasswordPage',
	() => import('@app/features/auth/components/pages/ForgotPasswordPage'),
);
const LoginPage = createAuthRoutePage('LoginPage', () => import('@app/features/auth/components/pages/LoginPage'));
const OAuthAuthorizePage = createAuthRoutePage(
	'OAuthAuthorizePage',
	() => import('@app/features/auth/components/pages/OAuthAuthorizePage'),
);
const RecoverAccountPage = createAuthRoutePage(
	'RecoverAccountPage',
	() => import('@app/features/auth/components/pages/RecoverAccountPage'),
);
const RegisterPage = createAuthRoutePage(
	'RegisterPage',
	() => import('@app/features/auth/components/pages/RegisterPage'),
);
const ResetPasswordPage = createAuthRoutePage(
	'ResetPasswordPage',
	() => import('@app/features/auth/components/pages/ResetPasswordPage'),
);
const SsoCallbackPage = createAuthRoutePage(
	'SsoCallbackPage',
	() => import('@app/features/auth/components/pages/SsoCallbackPage'),
);
const VerifyEmailPage = createAuthRoutePage(
	'VerifyEmailPage',
	() => import('@app/features/auth/components/pages/VerifyEmailPage'),
);
const GiftLoginPage = createAuthRoutePage(
	'GiftLoginPage',
	() => import('@app/features/expressions/components/pages/GiftLoginPage'),
);
const GiftRegisterPage = createAuthRoutePage(
	'GiftRegisterPage',
	() => import('@app/features/expressions/components/pages/GiftRegisterPage'),
);
const InviteLoginPage = createAuthRoutePage(
	'InviteLoginPage',
	() => import('@app/features/invite/components/pages/InviteLoginPage'),
);
const InviteRegisterPage = createAuthRoutePage(
	'InviteRegisterPage',
	() => import('@app/features/invite/components/pages/InviteRegisterPage'),
);
const ReportPage = createNamedAuthRoutePage('ReportPage', async () => {
	const module = await import('@app/features/moderation/components/pages/ReportPage');
	return module.ReportPage;
});
const ThemeLoginPage = createAuthRoutePage(
	'ThemeLoginPage',
	() => import('@app/features/theme/components/pages/ThemeLoginPage'),
);
const ThemeRegisterPage = createAuthRoutePage(
	'ThemeRegisterPage',
	() => import('@app/features/theme/components/pages/ThemeRegisterPage'),
);

const currentRedirectTarget = (fallback: string): string => {
	const qp = new URLSearchParams(window.location.search);
	return safeRedirectTargetOrFallback(qp.get('redirect_to'), fallback);
};

const resolveToPath = (to: Redirect['to']): string => {
	if (typeof to === 'string') {
		return to;
	}
	const url = new URL(to.to, window.location.origin);
	if (to.search) {
		url.search = '';
		for (const [k, v] of Object.entries(to.search)) {
			if (v === undefined) continue;
			if (v === null) {
				url.searchParams.set(k, '');
			} else {
				url.searchParams.set(k, String(v));
			}
		}
	}
	if (to.hash) {
		url.hash = to.hash.startsWith('#') ? to.hash : `#${to.hash}`;
	}
	return url.pathname + url.search + url.hash;
};

type AuthRedirectHandler = (ctx: RouteContext) => Redirect | undefined;

const redirectWhenEmailsDisabled: RouteConfig['onEnter'] = () => {
	if (!RuntimeConfig.emailsEnabled) {
		return new Redirect(Routes.LOGIN);
	}
	return undefined;
};

const redirectWhenPasswordResetUnavailable: RouteConfig['onEnter'] = () => {
	if (!RuntimeConfig.emailsEnabled && !RuntimeConfig.usesUsernameSignIn) {
		return new Redirect(Routes.LOGIN);
	}
	return undefined;
};

const whenAuthenticated = (handler: AuthRedirectHandler) => {
	return (ctx: RouteContext): Redirect | undefined => {
		const execute = (): Redirect | undefined => handler(ctx);
		if (SessionManager.isInitialized) {
			return Authentication.isAuthenticated ? execute() : undefined;
		}
		void SessionManager.initialize().then(() => {
			if (Authentication.isAuthenticated) {
				const res = execute();
				if (res instanceof Redirect) {
					RouterUtils.replaceWith(resolveToPath(res.to));
				}
			}
		});
		return undefined;
	};
};
const authLayoutRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'authLayout',
	layout: ({children}) => <AuthLayout data-flx="app.router.auth-routes.layout.auth-layout">{children}</AuthLayout>,
});

interface AuthPageRouteOptions {
	id: string;
	path: string;
	page: AuthRoutePage;
	dataFlx: string;
	onEnter?: RouteConfig['onEnter'];
}

function createAuthPageRoute({id, path, page: Page, dataFlx, onEnter}: AuthPageRouteOptions) {
	return createRoute({
		getParentRoute: () => authLayoutRoute,
		id,
		path,
		onEnter,
		preload: Page.preload,
		component: () => <Page data-flx={dataFlx} />,
	});
}

function createAuthRedirectRoute(id: string, path: string) {
	return createRoute({
		getParentRoute: () => authLayoutRoute,
		id,
		path,
		onEnter: () => new Redirect(Routes.ME),
	});
}

const loginRoute = createAuthPageRoute({
	id: 'login',
	path: '/login',
	page: LoginPage,
	dataFlx: 'app.router.auth-routes.login-page',
	onEnter: whenAuthenticated(() => {
		const search = window.location.search;
		const qp = new URLSearchParams(search);
		if (isHandoffRequest(qp)) {
			return undefined;
		}
		const redirectTo = safeRedirectTarget(qp.get('redirect_to'));
		return new Redirect(redirectTo || Routes.ME);
	}),
});
const ssoCallbackRoute = createAuthPageRoute({
	id: 'ssoCallback',
	path: Routes.SSO_CALLBACK,
	page: SsoCallbackPage,
	dataFlx: 'app.router.auth-routes.sso-callback-page',
});
const inviteBaseRoute = createAuthRedirectRoute('inviteBase', '/invite');
const giftBaseRoute = createAuthRedirectRoute('giftBase', '/gift');
const themeBaseRoute = createAuthRedirectRoute('themeBase', '/theme');
const registerRoute = createAuthPageRoute({
	id: 'register',
	path: '/register',
	page: RegisterPage,
	dataFlx: 'app.router.auth-routes.register-page',
});
const oauthAuthorizeRoute = createAuthPageRoute({
	id: 'oauthAuthorize',
	path: Routes.OAUTH_AUTHORIZE,
	page: OAuthAuthorizePage,
	dataFlx: 'app.router.auth-routes.o-auth-authorize-page',
	onEnter: () => {
		const current = window.location.pathname + window.location.search;
		if (!SessionManager.isInitialized) {
			void SessionManager.initialize().then(() => {
				if (!Authentication.isAuthenticated) {
					RouterUtils.replaceWith(setPathQueryParams(Routes.LOGIN, {redirect_to: current}));
				}
			});
			return undefined;
		}
		if (!Authentication.isAuthenticated) {
			return new Redirect(setPathQueryParams(Routes.LOGIN, {redirect_to: current}));
		}
		return undefined;
	},
});
const inviteRegisterRoute = createAuthPageRoute({
	id: 'inviteRegister',
	path: '/invite/:code',
	page: InviteRegisterPage,
	dataFlx: 'app.router.auth-routes.invite-register-page',
	onEnter: whenAuthenticated((ctx) => {
		const code = ctx.params['code'];
		if (code) {
			void import('@app/features/invite/commands/InviteCommands').then((commands) => {
				commands.openAcceptModal(code);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});
const inviteLoginRoute = createAuthPageRoute({
	id: 'inviteLogin',
	path: '/invite/:code/login',
	page: InviteLoginPage,
	dataFlx: 'app.router.auth-routes.invite-login-page',
	onEnter: whenAuthenticated((ctx) => {
		const qp = new URLSearchParams(window.location.search);
		if (isHandoffRequest(qp)) {
			return undefined;
		}
		const code = ctx.params['code'];
		if (code) {
			void import('@app/features/invite/commands/InviteCommands').then((commands) => {
				commands.openAcceptModal(code);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});
const giftRegisterRoute = createAuthPageRoute({
	id: 'giftRegister',
	path: '/gift/:code',
	page: GiftRegisterPage,
	dataFlx: 'app.router.auth-routes.gift-register-page',
	onEnter: whenAuthenticated((ctx) => {
		const code = ctx.params['code'];
		if (code) {
			void import('@app/features/gift/commands/GiftCommands').then((commands) => {
				commands.openAcceptModal(code);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});
const giftLoginRoute = createAuthPageRoute({
	id: 'giftLogin',
	path: '/gift/:code/login',
	page: GiftLoginPage,
	dataFlx: 'app.router.auth-routes.gift-login-page',
	onEnter: whenAuthenticated((ctx) => {
		const qp = new URLSearchParams(window.location.search);
		if (isHandoffRequest(qp)) {
			return undefined;
		}
		const code = ctx.params['code'];
		if (code) {
			void import('@app/features/gift/commands/GiftCommands').then((commands) => {
				commands.openAcceptModal(code);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});
const forgotPasswordRoute = createAuthPageRoute({
	id: 'forgotPassword',
	path: Routes.FORGOT_PASSWORD,
	page: ForgotPasswordPage,
	dataFlx: 'app.router.auth-routes.forgot-password-page',
	onEnter: (ctx) => {
		if (RuntimeConfig.usesUsernameSignIn) {
			return new Redirect(Routes.RECOVER_ACCOUNT);
		}
		if (!RuntimeConfig.emailsEnabled) {
			return new Redirect(Routes.LOGIN);
		}
		return whenAuthenticated(() => new Redirect(Routes.ME))(ctx);
	},
});
const recoverAccountRoute = createAuthPageRoute({
	id: 'recoverAccount',
	path: Routes.RECOVER_ACCOUNT,
	page: RecoverAccountPage,
	dataFlx: 'app.router.auth-routes.recover-account-page',
	onEnter: (ctx) => {
		if (!RuntimeConfig.usesUsernameSignIn) {
			return new Redirect(Routes.LOGIN);
		}
		return whenAuthenticated(() => new Redirect(Routes.ME))(ctx);
	},
});
const resetPasswordRoute = createAuthPageRoute({
	id: 'resetPassword',
	path: Routes.RESET_PASSWORD,
	page: ResetPasswordPage,
	dataFlx: 'app.router.auth-routes.reset-password-page',
	onEnter: redirectWhenPasswordResetUnavailable,
});
const emailRevertRoute = createAuthPageRoute({
	id: 'emailRevert',
	path: Routes.EMAIL_REVERT,
	page: EmailRevertPage,
	dataFlx: 'app.router.auth-routes.email-revert-page',
	onEnter: redirectWhenEmailsDisabled,
});
const verifyEmailRoute = createAuthPageRoute({
	id: 'verifyEmail',
	path: Routes.VERIFY_EMAIL,
	page: VerifyEmailPage,
	dataFlx: 'app.router.auth-routes.verify-email-page',
	onEnter: redirectWhenEmailsDisabled,
});
const authorizeIPRoute = createAuthPageRoute({
	id: 'authorizeIP',
	path: Routes.AUTHORIZE_IP,
	page: AuthorizeIPPage,
	dataFlx: 'app.router.auth-routes.authorize-ip-page',
});
const pendingRoute = createAuthRedirectRoute('pending', Routes.PENDING);
const reportRoute = createAuthPageRoute({
	id: 'report',
	path: Routes.REPORT,
	page: ReportPage,
	dataFlx: 'app.router.auth-routes.report-page',
});
const themeRegisterRoute = createAuthPageRoute({
	id: 'themeRegister',
	path: Routes.THEME_REGISTER,
	page: ThemeRegisterPage,
	dataFlx: 'app.router.auth-routes.theme-register-page',
	onEnter: whenAuthenticated((ctx) => {
		const themeId = ctx.params.themeId;
		if (themeId) {
			void import('@app/features/theme/commands/ThemeCommands').then((commands) => {
				commands.openAcceptModal(themeId, i18n);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});
const themeLoginRoute = createAuthPageRoute({
	id: 'themeLogin',
	path: Routes.THEME_LOGIN,
	page: ThemeLoginPage,
	dataFlx: 'app.router.auth-routes.theme-login-page',
	onEnter: whenAuthenticated((ctx) => {
		const qp = new URLSearchParams(window.location.search);
		if (isHandoffRequest(qp)) {
			return undefined;
		}
		const themeId = ctx.params.themeId;
		if (themeId) {
			void import('@app/features/theme/commands/ThemeCommands').then((commands) => {
				commands.openAcceptModal(themeId, i18n);
			});
		}
		return new Redirect(currentRedirectTarget(Routes.ME));
	}),
});

export const authRouteTree = authLayoutRoute.addChildren([
	loginRoute,
	ssoCallbackRoute,
	registerRoute,
	oauthAuthorizeRoute,
	inviteBaseRoute,
	giftBaseRoute,
	themeBaseRoute,
	inviteRegisterRoute,
	inviteLoginRoute,
	themeRegisterRoute,
	themeLoginRoute,
	forgotPasswordRoute,
	recoverAccountRoute,
	resetPasswordRoute,
	emailRevertRoute,
	verifyEmailRoute,
	authorizeIPRoute,
	pendingRoute,
	reportRoute,
	...(shouldShowPremiumFeatures() ? [giftRegisterRoute, giftLoginRoute] : []),
]);
