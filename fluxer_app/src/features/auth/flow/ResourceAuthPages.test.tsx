// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {TokenResponse} from '@app/features/auth/commands/AuthenticationCommands';
import {
	type ResourceAuthPageConfig,
	ResourceLoginPage,
	ResourceRegisterPage,
} from '@app/features/auth/flow/ResourceAuthPages';
import type {ReactNode} from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	snapshot: {
		apiEndpoint: 'https://instance.example/api',
		apiCodeVersion: 7,
		features: {},
		registration: {mode: 'open', admin_registration_urls_enabled: false},
	} as RuntimeConfigSnapshot,
	reset: vi.fn(),
	completeLogin: vi.fn(),
	search: '',
	loginProps: null as any,
	registerProps: null as any,
	ssoProps: null as any,
	carouselProps: null as any,
}));
vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@lingui/react/macro', () => ({useLingui: () => ({i18n: {_: (d: any) => d.message ?? d.id}})}));
vi.mock('@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow', () => ({
	isHandoffRequest: (params: URLSearchParams) => params.has('desktop_handoff'),
}));
vi.mock('mobx-react-lite', () => ({observer: (component: unknown) => component}));
vi.mock('@app/features/i18n/utils/CommonMessageDescriptors', () => ({REGISTER_DESCRIPTOR: {message: 'Register'}}));
vi.mock('@app/features/auth/commands/AuthenticationCommands', () => ({
	completeLogin: mocks.completeLogin,
	authResponseUserToUserData: () => ({id: 'user'}),
}));
vi.mock('@app/features/auth/state/AuthRuntimeTarget', () => ({useAuthRuntimeTarget: () => ({reset: mocks.reset})}));
vi.mock('@app/features/platform/components/router/RouterReact', () => ({useLocation: () => ({search: mocks.search})}));
vi.mock('@app/features/window/hooks/useFluxerDocumentTitle', () => ({useFluxerDocumentTitle: () => {}}));
vi.mock('@app/features/auth/flow/useAuthPresentation', () => ({useAuthPresentation: () => {}}));
vi.mock('@app/features/auth/state/AuthLayoutContext', () => ({AuthCardVariant: {STANDARD: 'standard'}}));
vi.mock('@app/features/auth/flow/AuthRuntimeTargetGate', () => ({
	AuthRuntimeTargetGate: ({children}: {children: (snapshot: RuntimeConfigSnapshot) => ReactNode}) =>
		children(mocks.snapshot),
}));
vi.mock('@app/features/auth/flow/AuthLoadingState', () => ({
	AuthLoadingState: () => <span data-flx="auth.flow.resource-auth-pages-test.auth-loading-state.span">loading</span>,
}));
vi.mock('@app/features/auth/flow/AuthErrorState', () => ({
	AuthErrorState: () => <span data-flx="auth.flow.resource-auth-pages-test.auth-error-state.span">error</span>,
}));
vi.mock('@app/features/auth/flow/AuthRuntimeTargetResetAction', () => ({AuthRuntimeTargetResetAction: () => null}));
vi.mock('@app/features/auth/flow/AuthLoginLayout', () => ({
	AuthLoginLayout: (props: any) => {
		mocks.loginProps = props;
		return props.extraTopContent;
	},
}));
vi.mock('@app/features/auth/flow/AuthLoginStepCarousel', () => ({
	AuthLoginStepCarousel: (props: any) => {
		mocks.carouselProps = props;
		return props.children;
	},
}));
vi.mock('@app/features/auth/flow/AuthRegisterFormCore', () => ({
	AuthRegisterFormCore: (props: any) => {
		mocks.registerProps = props;
		return null;
	},
}));
vi.mock('@app/features/auth/flow/AuthRouterLink', () => ({AuthRouterLink: () => null}));
vi.mock('@app/features/auth/flow/AuthBottomLink', () => ({AuthBottomLink: () => null}));
vi.mock('@app/features/auth/flow/DesktopDeepLinkPrompt', () => ({
	DesktopDeepLinkPrompt: (props: any) => (
		<span data-flx="auth.flow.resource-auth-pages-test.desktop-deep-link-prompt.span">
			{props.preferLogin ? 'prefer-login' : 'default'}
		</span>
	),
}));
vi.mock('@app/features/auth/flow/AuthSsoPanel', () => ({
	resolveAuthPanelSso: (snapshot: any) => snapshot.sso,
	AuthSsoPanel: (props: any) => {
		mocks.ssoProps = props;
		return null;
	},
}));

function config(overrides: Partial<ResourceAuthPageConfig<string>> = {}): ResourceAuthPageConfig<string> {
	return {
		kind: 'gift',
		dataFlxPrefix: (mode) => `gift.${mode}`,
		useResource: () => ({loading: false, error: false, resource: 'gift'}),
		renderHeader: (resource) => (
			<span data-flx="auth.flow.resource-auth-pages-test.render-header.span">{resource}</span>
		),
		documentTitle: () => 'Claim gift',
		errorTitle: () => 'Not found',
		errorText: 'Not found',
		registerPath: (id) => `/gift/${id}/register`,
		loginPath: (id) => `/gift/${id}/login`,
		redirectPath: (id, redirect) => redirect ?? `/gift/${id}/register`,
		fallbackRedirectPath: () => '/',
		registerRedirectPath: () => '/',
		onComplete: vi.fn(),
		submitLabel: 'Create account',
		...overrides,
	};
}
beforeEach(() => {
	vi.clearAllMocks();
	mocks.search = '';
	mocks.loginProps = mocks.registerProps = mocks.ssoProps = mocks.carouselProps = null;
	delete (mocks.snapshot as any).sso;
	(mocks.snapshot as any).registration = {mode: 'open', admin_registration_urls_enabled: false};
});
describe('resource auth guards', () => {
	it('blocks redeemed resources before login, registration or enforced SSO', () => {
		(mocks.snapshot as any).sso = {enabled: true, enforced: true};
		const blocked = config({
			renderBlockingState: () => (
				<span data-flx="auth.flow.resource-auth-pages-test.render-blocking-state.span">redeemed</span>
			),
		});
		expect(
			renderToStaticMarkup(
				<ResourceLoginPage id="code" config={blocked} data-flx="auth.flow.resource-auth-pages-test.code" />,
			),
		).toContain('redeemed');
		expect(
			renderToStaticMarkup(
				<ResourceRegisterPage id="code" config={blocked} data-flx="auth.flow.resource-auth-pages-test.code--2" />,
			),
		).toContain('redeemed');
		expect(mocks.loginProps).toBeNull();
		expect(mocks.registerProps).toBeNull();
		expect(mocks.ssoProps).toBeNull();
	});
	it('keeps loading and missing resources outside authentication', () => {
		renderToStaticMarkup(
			<ResourceLoginPage
				id="code"
				config={config({useResource: () => ({loading: true, error: false, resource: null})})}
				data-flx="auth.flow.resource-auth-pages-test.code--3"
			/>,
		);
		renderToStaticMarkup(
			<ResourceRegisterPage
				id="code"
				config={config({useResource: () => ({loading: false, error: true, resource: null})})}
				data-flx="auth.flow.resource-auth-pages-test.code--4"
			/>,
		);
		expect(mocks.loginProps).toBeNull();
		expect(mocks.registerProps).toBeNull();
	});
	it('preserves safe redirects, gift login preference, MFA fallback and async completion order', async () => {
		mocks.search = '?redirect_to=%2Fchannels%2F%40me';
		let finish!: () => void;
		const accept = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finish = resolve;
				}),
		);
		const html = renderToStaticMarkup(
			<ResourceLoginPage
				id="code"
				config={config({preferLogin: true, onComplete: accept, fallbackRedirectPath: () => '/gift/code'})}
				data-flx="auth.flow.resource-auth-pages-test.code--5"
			/>,
		);
		expect(html).toContain('prefer-login');
		expect(mocks.loginProps.redirectPath).toBe('/channels/@me');
		expect(mocks.carouselProps.mfaFallbackRedirectPath).toBe('/gift/code');
		const pending = mocks.loginProps.onLoginComplete();
		expect(accept).toHaveBeenCalledWith('code', expect.anything(), mocks.snapshot);
		expect(mocks.reset).not.toHaveBeenCalled();
		finish();
		await pending;
		expect(mocks.reset).toHaveBeenCalledOnce();
	});
	it('finishes registration against the captured instance before accepting the resource', async () => {
		const accept = vi.fn();
		renderToStaticMarkup(
			<ResourceRegisterPage
				id="code"
				config={config({onComplete: accept})}
				data-flx="auth.flow.resource-auth-pages-test.code--6"
			/>,
		);
		const response = {token: 'token', user_id: 'user', user: {id: 'user'}} as TokenResponse;
		await mocks.registerProps.onRegister(response);
		expect(mocks.completeLogin).toHaveBeenCalledWith({
			token: 'token',
			userId: 'user',
			runtimeSnapshot: mocks.snapshot,
			userData: {id: 'user'},
		});
		expect(mocks.completeLogin.mock.invocationCallOrder[0]).toBeLessThan(accept.mock.invocationCallOrder[0]);
		expect(accept).toHaveBeenCalledWith('code', expect.anything(), mocks.snapshot);
		expect(mocks.registerProps.onAuthenticated).toBe(mocks.reset);
	});
	it('uses enforced SSO without mounting the registration form', () => {
		(mocks.snapshot as any).sso = {enabled: true, enforced: true};
		renderToStaticMarkup(
			<ResourceRegisterPage id="code" config={config()} data-flx="auth.flow.resource-auth-pages-test.code--7" />,
		);
		expect(mocks.ssoProps.runtimeSnapshot).toBe(mocks.snapshot);
		expect(mocks.ssoProps.redirectPath).toBe('/gift/code/register');
		expect(mocks.registerProps).toBeNull();
	});
	it('asks for an email and password on an approval instance', () => {
		(mocks.snapshot as any).registration = {mode: 'approval', admin_registration_urls_enabled: false};
		renderToStaticMarkup(
			<ResourceRegisterPage id="code" config={config()} data-flx="auth.flow.resource-auth-pages-test.code--8" />,
		);
		expect(mocks.registerProps.fields).toMatchObject({
			showEmail: true,
			showPassword: true,
			showPasswordConfirmation: true,
		});
	});
	it('keeps the minimal form on an open email instance', () => {
		renderToStaticMarkup(
			<ResourceRegisterPage id="code" config={config()} data-flx="auth.flow.resource-auth-pages-test.code--9" />,
		);
		expect(mocks.registerProps.fields).toEqual({showUsername: false});
	});
});
