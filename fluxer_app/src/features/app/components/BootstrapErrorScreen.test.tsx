// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReactNode} from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: ReactNode}) => children ?? null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));
vi.mock('@app/features/platform/state/ResetClientState', () => ({
	ResetClientStateReason: {RESET_APP_DATA: 'reset_app_data'},
	resetClientState: vi.fn(),
}));

const {BootstrapErrorScreen} = await import('@app/features/app/components/BootstrapErrorScreen');

const RESET_BUTTON = 'app.bootstrap-error-screen.button.reset';
const RETRY_BUTTON = 'app.bootstrap-error-screen.button.retry';
const COPY_DETAILS_BUTTON = 'app.bootstrap-error-screen.button.copy-details';

afterEach(() => {
	vi.restoreAllMocks();
});

test('an unreachable instance offers a retry and never a reset of app data', () => {
	const error = new Error(
		"Error invoking remote method 'desktop-runtime-config:resolve': DesktopRuntimeDiscoveryUnreachableError: No usable instance discovery document was served",
	);

	const markup = renderToStaticMarkup(<BootstrapErrorScreen error={error} />);

	expect(markup).toContain(RETRY_BUTTON);
	expect(markup).not.toContain(RESET_BUTTON);
	expect(markup).not.toContain('corrupted data');
});

test('an unreachable instance keeps the raw error out of the page and offers it as a copy', () => {
	const error = new Error(
		"Error invoking remote method 'desktop-runtime-config:resolve': DesktopRuntimeDiscoveryUnreachableError: connect ECONNREFUSED 127.0.0.1:9",
	);

	const markup = renderToStaticMarkup(<BootstrapErrorScreen error={error} />);

	expect(markup).not.toContain('DesktopRuntimeDiscoveryUnreachableError');
	expect(markup).not.toContain('ECONNREFUSED');
	expect(markup).toContain(COPY_DETAILS_BUTTON);
});

test('a failure while the device is offline never offers a reset of app data', () => {
	vi.spyOn(navigator, 'onLine', 'get').mockReturnValue(false);

	const markup = renderToStaticMarkup(<BootstrapErrorScreen error={new Error('Stored account record is malformed')} />);

	expect(markup).not.toContain(RESET_BUTTON);
});

test('a local startup failure still offers the reset of app data', () => {
	const markup = renderToStaticMarkup(<BootstrapErrorScreen error={new Error('Stored account record is malformed')} />);

	expect(markup).toContain(RETRY_BUTTON);
	expect(markup).toContain(RESET_BUTTON);
	expect(markup).toContain('Stored account record is malformed');
	expect(markup).not.toContain(COPY_DETAILS_BUTTON);
});
