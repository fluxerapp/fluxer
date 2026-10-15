// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {join} from 'node:path';
import type {AccountRestriction} from '@app/features/auth/state/AccountRestriction';
import type {EnforcementNotice, NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import type {ReactNode} from 'react';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	restriction: null as unknown,
	pathname: '/reset',
	clearAccountRestriction: vi.fn(),
	replaceWith: vi.fn(),
	pushModal: vi.fn(),
	popModal: vi.fn(),
	locale: 'en-US',
	mobile: false,
}));

function interpolate(message: string, values: Record<string, unknown> = {}): string {
	return message.replace(/\{(\w+)\}/g, (_match, name: string) => String(values[name]));
}

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children: ReactNode}) => children,
	useLingui: () => ({
		i18n: {
			get locale() {
				return mocks.locale;
			},
			_: (descriptor: {message: string}, values?: Record<string, unknown>) => interpolate(descriptor.message, values),
		},
	}),
}));
vi.mock('mobx-react-lite', () => ({observer: (component: unknown) => component}));
vi.mock('@app/features/auth/flow/AccountRestrictionScreen.module.css', () => ({
	default: new Proxy({}, {get: (_target, name) => String(name)}),
}));
vi.mock('@app/features/app/components/shared/ExternalLink', () => ({
	ExternalLink: ({href, children}: {href: string; children: ReactNode}) => (
		<a href={href} data-external-link="true" data-flx="auth.flow.account-restriction-screen-test.external-link">
			{children}
		</a>
	),
}));
vi.mock('@app/features/ui/button/Button', () => ({
	Button: ({onClick, children}: {onClick: () => void; children: ReactNode}) => (
		<button type="button" onClick={onClick} data-flx="auth.flow.account-restriction-screen-test.button">
			{children}
		</button>
	),
}));
vi.mock('@app/features/i18n/utils/CommonMessageDescriptors', () => ({
	BACK_TO_SIGN_IN_DESCRIPTOR: {message: 'Back to sign-in'},
	TRY_AGAIN_DESCRIPTOR: {message: 'Try again'},
}));
vi.mock('@app/features/auth/flow/useAuthPresentation', () => ({useAuthPresentation: () => {}}));
vi.mock('@app/features/ui/utils/MobileExperience', () => ({isMobileExperienceEnabled: () => mocks.mobile}));
vi.mock('@app/features/auth/state/AuthLayoutContext', () => ({AuthCardVariant: {STANDARD: 'standard'}}));
vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {
		get accountRestriction() {
			return mocks.restriction;
		},
	},
}));
vi.mock('@app/features/auth/commands/AuthenticationCommands', () => ({
	clearAccountRestriction: mocks.clearAccountRestriction,
}));
vi.mock('@app/features/navigation/utils/RouterUtils', () => ({replaceWith: mocks.replaceWith}));
vi.mock('@app/features/platform/components/router/RouterReact', () => ({
	useLocation: () => ({pathname: mocks.pathname}),
}));
vi.mock('@app/features/app/components/dialogs/Modal', () => ({
	Root: ({className, children}: {className: string; children: ReactNode}) => (
		<div role="dialog" className={className} data-flx="auth.flow.account-restriction-screen-test.root">
			{children}
		</div>
	),
	Header: ({title}: {title: string}) => <h1 data-flx="auth.flow.account-restriction-screen-test.header">{title}</h1>,
	Content: ({children, dir}: {children: ReactNode; dir?: string}) => (
		<div data-modal-content="true" dir={dir} data-flx="auth.flow.account-restriction-screen-test.content">
			{children}
		</div>
	),
	ContentLayout: ({children}: {children: ReactNode}) => children,
	Footer: ({children}: {children: ReactNode}) => children,
}));
vi.mock('@app/features/ui/commands/ModalCommands', () => ({
	push: mocks.pushModal,
	pop: mocks.popModal,
	modal: (render: () => ReactNode) => render,
}));

const {AccountRestrictionGate, AccountRestrictionScreen} = await import(
	'@app/features/auth/flow/AccountRestrictionScreen'
);
const {showNetworkBlockedModal} = await import('@app/features/auth/components/NetworkBlockedHandler');

const notice: EnforcementNotice = {
	decision_id: '1234567890123456789',
	kind: 'suspension',
	permanent: false,
	issued_at: '2026-10-01T10:00:00.000Z',
	ends_at: '2026-10-08T10:00:00.000Z',
	deletion_at: null,
	summary: 'We suspended your account until Oct 8, 2026, 10:00 AM UTC.',
	effects: ["You can't sign in or use Fluxer anywhere until then. Your account, messages and communities are kept."],
	rule: {title: 'Spam and service abuse', url: 'https://fluxer.com/guidelines#6-spam-and-service-abuse'},
	facts: [
		'We found that your account sent unwanted messages in bulk.',
		'3 messages in #general of Example, between Oct 1, 2026, 9:00 AM UTC and Oct 1, 2026, 9:05 AM UTC.',
	],
	statement: 'Please read the rules.',
	how_we_decided: ['We reviewed this after a report.', 'A person on our team reviewed this and made the decision.'],
	appeal: {email: 'appeals@fluxer.com', deadline: 'Nov 30, 2026', account_has_email: true},
	redress: [
		"If you're in the EU, you can also refer this decision to a certified out-of-court dispute settlement body. The European Commission keeps a [list of these bodies](https://digital-strategy.ec.europa.eu/en/policies/dsa-out-court-dispute-settlement).",
	],
};

const block: NetworkBlock = {
	ip_address: '203.0.113.7',
	appeal_email: 'support@fluxer.com',
	appeals_supported: true,
	ban_kind: 'permanent',
	expires_at: null,
	reason: null,
	product_name: 'Fluxer',
};

let root: Root | null = null;
let container: HTMLElement | null = null;

function render(node: ReactNode): HTMLElement {
	const target = container;
	if (target == null) {
		throw new Error('The test container is not mounted');
	}
	act(() => root?.render(node));
	return target;
}

function renderScreen(restriction: AccountRestriction, handlers = {onBackToSignIn: vi.fn(), onTryAgain: vi.fn()}) {
	const element = render(
		<AccountRestrictionScreen
			restriction={restriction}
			{...handlers}
			data-flx="auth.flow.account-restriction-screen-test.account-restriction-screen"
		/>,
	);
	return {element, ...handlers};
}

function text(element: HTMLElement): string {
	return (element.textContent ?? '').replace(/\s+/g, ' ');
}

function paragraphs(element: HTMLElement): Array<string> {
	return [...element.querySelectorAll('p')].map((paragraph) => text(paragraph));
}

function clickButton(element: HTMLElement, label: string): void {
	const button = [...element.querySelectorAll('button')].find((candidate) => candidate.textContent === label);
	if (button == null) {
		throw new Error(`No ${label} button`);
	}
	act(() => button.click());
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
	mocks.restriction = null;
	mocks.pathname = '/reset';
	mocks.locale = 'en-US';
	mocks.mobile = false;
	vi.clearAllMocks();
});

afterEach(() => {
	act(() => root?.unmount());
	container?.remove();
	root = null;
	container = null;
});

describe('account notice', () => {
	test('shows every section of the notice with a real mailto link', () => {
		const {element} = renderScreen({type: 'notice', notice});
		const content = text(element);
		expect(element.querySelector('h1')?.textContent).toBe('Account suspended');
		expect([...element.querySelectorAll('h2')].map((heading) => heading.textContent)).toEqual([
			'What happened',
			'Why',
			'How we decided',
			'How to appeal',
		]);
		for (const line of [notice.summary, ...notice.effects, ...notice.facts, ...notice.how_we_decided]) {
			expect(content).toContain(line);
		}
		expect(content).toContain('The European Commission keeps a list of these bodies.');
		expect(content).not.toContain('](');
		expect(content).toContain('Rule: Spam and service abuse');
		expect(content).toContain('Note from our team: Please read the rules.');
		expect(content).toContain(
			'If you think we got this wrong, email appeals@fluxer.com by Nov 30, 2026. Write from the email address on your account if you can, and include the decision ID. Appeals are free, and a person reviews each one.',
		);
		expect(content).toContain('Decision ID: 1234567890123456789');
		const links = [...element.querySelectorAll('a[data-external-link]')].map((link) => link.getAttribute('href'));
		expect(links).toEqual([
			'https://fluxer.com/guidelines#6-spam-and-service-abuse',
			'mailto:appeals@fluxer.com',
			'https://digital-strategy.ec.europa.eu/en/policies/dsa-out-court-dispute-settlement',
		]);
	});

	test('the primary button sits in the footer, and mobile gets its own card', () => {
		const {element} = renderScreen({type: 'notice', notice});
		const root = element.firstElementChild;
		expect(root?.lastElementChild?.className).toBe('footer');
		expect(root?.lastElementChild?.querySelector('button')?.textContent).toBe('Back to sign-in');
		expect(root?.className.split(' ')).not.toContain('surface');
		mocks.mobile = true;
		renderScreen({type: 'notice', notice});
		expect(container?.firstElementChild?.className.split(' ')).toContain('surface');
	});

	test('follows the direction of the locale', () => {
		mocks.locale = 'ar';
		const {element} = renderScreen({type: 'notice', notice});
		expect(element.firstElementChild?.getAttribute('dir')).toBe('rtl');
	});

	test('titles follow the kind and permanence', () => {
		renderScreen({type: 'notice', notice: {...notice, permanent: true, ends_at: null}});
		expect(container?.querySelector('h1')?.textContent).toBe('Account banned');
		renderScreen({type: 'notice', notice: {...notice, kind: 'closure', deletion_at: '2027-01-01T00:00:00.000Z'}});
		expect(container?.querySelector('h1')?.textContent).toBe('Account closed');
	});

	test('the root is selectable and the decision ID has no copy control', () => {
		const {element} = renderScreen({type: 'notice', notice});
		expect(element.firstElementChild?.className.split(' ')).toContain('selectable');
		expect([...element.querySelectorAll('button')].map((button) => button.textContent)).toEqual(['Back to sign-in']);
		expect(text(element)).not.toMatch(/cop(y|ied)/i);
		const css = readFileSync(join(process.cwd(), 'src/features/auth/flow/AccountRestrictionScreen.module.css'), 'utf8');
		expect(css).toMatch(
			/\.selectable,\s*\.selectable \* \{\s*user-select: text !important;\s*-webkit-user-select: text !important;/,
		);
	});

	test('an account with no email asks for the decision ID and username', () => {
		const {element} = renderScreen({
			type: 'notice',
			notice: {...notice, appeal: {email: 'appeals@fluxer.com', deadline: 'Nov 30, 2026', account_has_email: false}},
		});
		expect(text(element)).toContain(
			'If you think we got this wrong, email appeals@fluxer.com by Nov 30, 2026. Include the decision ID and your username. Appeals are free, and a person reviews each one.',
		);
	});

	test('a deadline that ends in a period does not double the full stop', () => {
		const {element} = renderScreen({
			type: 'notice',
			notice: {...notice, appeal: {email: 'appeals@fluxer.com', deadline: '14. pro 2026.', account_has_email: true}},
		});
		expect(text(element)).toContain('by 14. pro 2026. Write from');
	});

	test('a restriction from before decisions has no deadline and no decision ID', () => {
		const {element} = renderScreen({
			type: 'notice',
			notice: {
				...notice,
				decision_id: null,
				rule: null,
				statement: null,
				how_we_decided: [],
				facts: ["We don't have more detail on record for this decision."],
				appeal: {email: 'appeals@fluxer.com', deadline: null, account_has_email: true},
			},
		});
		const content = text(element);
		expect(content).toContain('If you think we got this wrong, email appeals@fluxer.com and include your username.');
		expect(content).not.toContain('Decision ID');
		expect(content).not.toContain('Rule:');
		expect(content).not.toContain('How we decided');
	});

	test('self-hosted notices have no appeal section and no email link', () => {
		const {element} = renderScreen({
			type: 'notice',
			notice: {...notice, rule: {title: 'Spam and service abuse', url: null}, appeal: null, redress: []},
		});
		const content = text(element);
		expect(content).not.toContain('How to appeal');
		expect(content).not.toContain('@');
		expect(content).toContain('Rule: Spam and service abuse');
		expect(element.querySelectorAll('a')).toHaveLength(0);
	});

	test('back to sign-in calls its handler', () => {
		const {element, onBackToSignIn} = renderScreen({type: 'notice', notice});
		clickButton(element, 'Back to sign-in');
		expect(onBackToSignIn).toHaveBeenCalledOnce();
	});
});

describe('network blocked', () => {
	test('without a reason shows the plain lead, the shared-network paragraph and the contact', () => {
		const {element} = renderScreen({type: 'network_blocked', block});
		expect(element.querySelector('h1')?.textContent).toBe('Network blocked');
		expect(paragraphs(element)).toEqual([
			'Fluxer blocked access from this network.',
			'If you share this network, for example at school, at work, on public Wi-Fi, through your mobile carrier or a VPN, this block may have nothing to do with you. The quickest fix is to switch networks, for example to mobile data instead of Wi-Fi, or to turn your VPN on or off.',
			'If you think this block is a mistake, email support@fluxer.com and include your IP address, 203.0.113.7.',
		]);
		expect(text(element)).not.toMatch(/sign in/i);
		expect(element.querySelector('a')?.getAttribute('href')).toBe('mailto:support@fluxer.com');
		expect(element.firstElementChild?.className.split(' ')).toContain('selectable');
	});

	test('with a reason leads with the rule', () => {
		const {element} = renderScreen({
			type: 'network_blocked',
			block: {...block, appeal_email: 'appeals@fluxer.com', reason: {title: 'Spam and service abuse', url: null}},
		});
		expect(paragraphs(element).slice(0, 2)).toEqual([
			'Fluxer blocked access from this network because of activity that broke its rules.',
			'Rule: Spam and service abuse',
		]);
	});

	test('a temporary block says when it ends', () => {
		const {element} = renderScreen({
			type: 'network_blocked',
			block: {...block, ban_kind: 'temporary_24h', expires_at: '2026-10-15T10:00:00.000Z'},
		});
		expect(text(element)).toContain('The block ends on Oct 15, 2026, 10:00 AM UTC.');
	});

	test('self-hosted blocks have no contact line', () => {
		const {element} = renderScreen({
			type: 'network_blocked',
			block: {...block, appeal_email: null, appeals_supported: false},
		});
		expect(text(element)).not.toContain('mistake');
		expect(element.querySelectorAll('a')).toHaveLength(0);
	});

	test('an unnamed self-hosted instance uses the name the server sent', () => {
		const {element} = renderScreen({
			type: 'network_blocked',
			block: {...block, appeal_email: null, appeals_supported: false, product_name: 'chat.example.org'},
		});
		expect(paragraphs(element)[0]).toBe('chat.example.org blocked access from this network.');
		expect(text(element)).not.toContain('Fluxer');
	});

	test('try again calls its handler', () => {
		const {element, onTryAgain} = renderScreen({type: 'network_blocked', block});
		clickButton(element, 'Try again');
		expect(onTryAgain).toHaveBeenCalledOnce();
	});
});

describe('gate', () => {
	test('renders its page when nothing is restricted', () => {
		const element = render(
			<AccountRestrictionGate data-flx="auth.flow.account-restriction-screen-test.gate">
				<p data-flx="auth.flow.account-restriction-screen-test.page">page</p>
			</AccountRestrictionGate>,
		);
		expect(text(element)).toBe('page');
	});

	test('replaces the page and returns to sign-in from another page', () => {
		mocks.restriction = {type: 'notice', notice};
		const element = render(
			<AccountRestrictionGate data-flx="auth.flow.account-restriction-screen-test.gate--2">
				<p data-flx="auth.flow.account-restriction-screen-test.page--2">page</p>
			</AccountRestrictionGate>,
		);
		expect(text(element)).not.toContain('page');
		clickButton(element, 'Back to sign-in');
		expect(mocks.clearAccountRestriction).toHaveBeenCalledOnce();
		expect(mocks.replaceWith).toHaveBeenCalledWith('/login');
	});

	test('try again on the sign-in page stays on it', () => {
		mocks.restriction = {type: 'network_blocked', block};
		mocks.pathname = '/login';
		const element = render(
			<AccountRestrictionGate data-flx="auth.flow.account-restriction-screen-test.gate--3">
				<p data-flx="auth.flow.account-restriction-screen-test.page--3">page</p>
			</AccountRestrictionGate>,
		);
		clickButton(element, 'Try again');
		expect(mocks.clearAccountRestriction).toHaveBeenCalledOnce();
		expect(mocks.replaceWith).not.toHaveBeenCalled();
	});
});

describe('signed-in network block modal', () => {
	test('opens once until someone tries again', () => {
		showNetworkBlockedModal(block);
		showNetworkBlockedModal(block);
		expect(mocks.pushModal).toHaveBeenCalledOnce();
		const renderModal = mocks.pushModal.mock.calls[0]?.[0] as () => ReactNode;
		const element = render(renderModal());
		expect(element.querySelector('[role="dialog"]')?.className).toBe('selectable');
		expect(element.querySelector('h1')?.textContent).toBe('Network blocked');
		expect(text(element)).toContain('Fluxer blocked access from this network.');
		clickButton(element, 'Try again');
		expect(mocks.popModal).toHaveBeenCalledOnce();
		showNetworkBlockedModal({...block, appeal_email: null, appeals_supported: false, product_name: 'chat.example.org'});
		expect(mocks.pushModal).toHaveBeenCalledTimes(2);
		const renderSelfHosted = mocks.pushModal.mock.calls[1]?.[0] as () => ReactNode;
		const selfHosted = render(renderSelfHosted());
		expect(text(selfHosted)).toContain('chat.example.org blocked access from this network.');
		expect(text(selfHosted)).not.toContain('Fluxer');
		clickButton(selfHosted, 'Try again');
	});

	test('follows the direction of the locale', () => {
		mocks.locale = 'he';
		showNetworkBlockedModal(block);
		const renderModal = mocks.pushModal.mock.calls[0]?.[0] as () => ReactNode;
		const element = render(renderModal());
		expect(element.querySelector('[data-modal-content]')?.getAttribute('dir')).toBe('rtl');
		clickButton(element, 'Try again');
	});
});
