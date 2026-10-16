// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthRouteRedirectPolicy from '@app/app/router/policy/AuthRouteRedirectPolicy';
import * as ShortLinkRouteEnterPolicy from '@app/app/router/policy/ShortLinkRouteEnterPolicy';
import {rootRoute} from '@app/app/router/routes/RootRoutes';
import * as ShortLinkAcceptModalOpener from '@app/app/router/ShortLinkAcceptModalOpener';
import {AuthLayout} from '@app/features/app/components/layout/AuthLayout';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {type AuthRoutePage, createAuthRoutePage} from '@app/features/auth/flow/AuthLoadableRoutePage';
import {createRoute, type RouteBuilder} from '@app/features/platform/components/router/RouterBuilder';
import {NotFound, Redirect, type RouteContext} from '@app/features/platform/components/router/RouterTypes';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';

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
const SSOCallbackPage = createAuthRoutePage(
	'SSOCallbackPage',
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
const ReportPage = createAuthRoutePage('ReportPage', async () => ({
	default: (await import('@app/features/moderation/components/pages/ReportPage')).ReportPage,
}));
const ThemeLoginPage = createAuthRoutePage(
	'ThemeLoginPage',
	() => import('@app/features/theme/components/pages/ThemeLoginPage'),
);
const ThemeRegisterPage = createAuthRoutePage(
	'ThemeRegisterPage',
	() => import('@app/features/theme/components/pages/ThemeRegisterPage'),
);

const authLayoutRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'authLayout',
	layout: ({children}) => <AuthLayout data-flx="app.router.auth-routes.layout.auth-layout">{children}</AuthLayout>,
});

type AuthPageEnterHandler = (context: RouteContext) => Redirect | NotFound | undefined;

interface AuthPageRouteOptions {
	id: string;
	path: string;
	page: AuthRoutePage;
	dataFlx: string;
	onEnter?: AuthPageEnterHandler;
}

function createAuthPageRoute({id, path, page: Page, dataFlx, onEnter}: AuthPageRouteOptions): RouteBuilder {
	return createRoute({
		getParentRoute: () => authLayoutRoute,
		id,
		path,
		onEnter,
		preload: Page.preload,
		component: () => <Page data-flx={dataFlx} />,
	});
}

function createAuthRedirectRoute(id: string, path: string): RouteBuilder {
	return createRoute({
		getParentRoute: () => authLayoutRoute,
		id,
		path,
		onEnter: () => new Redirect(Routes.ME),
	});
}

interface ShortLinkRoutePaths {
	readonly register: string;
	readonly login: string;
}

interface ShortLinkAuthRouteOptions {
	id: string;
	dataFlxPrefix: string;
	paths: ShortLinkRoutePaths;
	registerPage: AuthRoutePage;
	loginPage: AuthRoutePage;
	handlers: {readonly onRegisterEnter: AuthPageEnterHandler; readonly onLoginEnter: AuthPageEnterHandler};
}

function createShortLinkAuthRoutes({
	id,
	dataFlxPrefix,
	paths,
	registerPage,
	loginPage,
	handlers,
}: ShortLinkAuthRouteOptions): Array<RouteBuilder> {
	return [
		createAuthPageRoute({
			id: `${id}Register`,
			path: paths.register,
			page: registerPage,
			dataFlx: `${dataFlxPrefix}-register-page`,
			onEnter: handlers.onRegisterEnter,
		}),
		createAuthPageRoute({
			id: `${id}Login`,
			path: paths.login,
			page: loginPage,
			dataFlx: `${dataFlxPrefix}-login-page`,
			onEnter: handlers.onLoginEnter,
		}),
	];
}

const inviteRouteEnterHandlers = ShortLinkRouteEnterPolicy.createHandlers({
	paramName: 'code',
	openAcceptModal: ShortLinkAcceptModalOpener.openInvite,
});
const giftShortLinkHandlers = ShortLinkRouteEnterPolicy.createHandlers({
	paramName: 'code',
	openAcceptModal: ShortLinkAcceptModalOpener.openGift,
});

function whenGiftingAvailable(handler: AuthRouteRedirectPolicy.AuthRouteEnterHandler): AuthPageEnterHandler {
	return (context: RouteContext): Redirect | NotFound | undefined => {
		if (RuntimeConfig.getSnapshotOrNull() !== null && !shouldShowPremiumFeatures()) {
			return new NotFound();
		}
		return handler(context);
	};
}

const giftRouteEnterHandlers = {
	onRegisterEnter: whenGiftingAvailable(giftShortLinkHandlers.onRegisterEnter),
	onLoginEnter: whenGiftingAvailable(giftShortLinkHandlers.onLoginEnter),
};
const themeRouteEnterHandlers = ShortLinkRouteEnterPolicy.createHandlers({
	paramName: 'themeId',
	openAcceptModal: ShortLinkAcceptModalOpener.openTheme,
});

const loginRoute = createAuthPageRoute({
	id: 'login',
	path: '/login',
	page: LoginPage,
	dataFlx: 'app.router.auth-routes.login-page',
	onEnter: AuthRouteRedirectPolicy.whenAuthenticated(AuthRouteRedirectPolicy.resolveAuthenticatedLoginEntry),
});
const ssoCallbackRoute = createAuthPageRoute({
	id: 'ssoCallback',
	path: Routes.SSO_CALLBACK,
	page: SSOCallbackPage,
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
	onEnter: AuthRouteRedirectPolicy.requireAuthentication,
});
const inviteRoutes = createShortLinkAuthRoutes({
	id: 'invite',
	dataFlxPrefix: 'app.router.auth-routes.invite',
	paths: {register: Routes.INVITE_REGISTER, login: Routes.INVITE_LOGIN},
	registerPage: InviteRegisterPage,
	loginPage: InviteLoginPage,
	handlers: inviteRouteEnterHandlers,
});
const giftRoutes = createShortLinkAuthRoutes({
	id: 'gift',
	dataFlxPrefix: 'app.router.auth-routes.gift',
	paths: {register: Routes.GIFT_REGISTER, login: Routes.GIFT_LOGIN},
	registerPage: GiftRegisterPage,
	loginPage: GiftLoginPage,
	handlers: giftRouteEnterHandlers,
});
const forgotPasswordRoute = createAuthPageRoute({
	id: 'forgotPassword',
	path: Routes.FORGOT_PASSWORD,
	page: ForgotPasswordPage,
	dataFlx: 'app.router.auth-routes.forgot-password-page',
	onEnter: AuthRouteRedirectPolicy.resolveForgotPasswordEntry,
});
const recoverAccountRoute = createAuthPageRoute({
	id: 'recoverAccount',
	path: Routes.RECOVER_ACCOUNT,
	page: RecoverAccountPage,
	dataFlx: 'app.router.auth-routes.recover-account-page',
	onEnter: AuthRouteRedirectPolicy.resolveRecoverAccountEntry,
});
const resetPasswordRoute = createAuthPageRoute({
	id: 'resetPassword',
	path: Routes.RESET_PASSWORD,
	page: ResetPasswordPage,
	dataFlx: 'app.router.auth-routes.reset-password-page',
	onEnter: AuthRouteRedirectPolicy.resolvePasswordResetEntry,
});
const emailRevertRoute = createAuthPageRoute({
	id: 'emailRevert',
	path: Routes.EMAIL_REVERT,
	page: EmailRevertPage,
	dataFlx: 'app.router.auth-routes.email-revert-page',
	onEnter: AuthRouteRedirectPolicy.resolveEmailFeatureEntry,
});
const verifyEmailRoute = createAuthPageRoute({
	id: 'verifyEmail',
	path: Routes.VERIFY_EMAIL,
	page: VerifyEmailPage,
	dataFlx: 'app.router.auth-routes.verify-email-page',
	onEnter: AuthRouteRedirectPolicy.resolveEmailFeatureEntry,
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
	onEnter: themeRouteEnterHandlers.onRegisterEnter,
});
const themeLoginRoute = createAuthPageRoute({
	id: 'themeLogin',
	path: Routes.THEME_LOGIN,
	page: ThemeLoginPage,
	dataFlx: 'app.router.auth-routes.theme-login-page',
	onEnter: themeRouteEnterHandlers.onLoginEnter,
});

export const authRouteTree = authLayoutRoute.addChildren([
	loginRoute,
	ssoCallbackRoute,
	registerRoute,
	oauthAuthorizeRoute,
	inviteBaseRoute,
	giftBaseRoute,
	themeBaseRoute,
	...inviteRoutes,
	...giftRoutes,
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
]);
