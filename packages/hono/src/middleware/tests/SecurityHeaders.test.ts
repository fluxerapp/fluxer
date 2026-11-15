// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LOCKED_DOWN_PERMISSIONS_POLICY,
	STRICT_TRANSPORT_SECURITY_PRELOAD,
	securityHeaders,
} from '@fluxer/hono/src/middleware/SecurityHeaders';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

describe('SecurityHeaders Middleware', () => {
	test('sets the baseline security headers', async () => {
		const app = new Hono();
		app.use('*', securityHeaders({contentSecurityPolicy: "default-src 'none'"}));
		app.get('/', (c) => c.text('OK'));
		const response = await app.request('/');
		expect(response.headers.get('Strict-Transport-Security')).toBe(STRICT_TRANSPORT_SECURITY_PRELOAD);
		expect(response.headers.get('Content-Security-Policy')).toBe("default-src 'none'");
		expect(response.headers.get('X-Frame-Options')).toBe('DENY');
		expect(response.headers.get('X-Content-Type-Options')).toBe('nosniff');
		expect(response.headers.get('Referrer-Policy')).toBe('strict-origin-when-cross-origin');
		expect(response.headers.get('Permissions-Policy')).toBe(LOCKED_DOWN_PERMISSIONS_POLICY);
	});
	test('preserves route-specific CSP headers by default', async () => {
		const app = new Hono();
		app.use('*', securityHeaders({contentSecurityPolicy: "default-src 'none'"}));
		app.get('/', (c) => {
			c.header('Content-Security-Policy', "default-src 'self'; script-src 'nonce-test'");
			return c.html('<!doctype html>');
		});
		const response = await app.request('/');
		expect(response.headers.get('Content-Security-Policy')).toBe("default-src 'self'; script-src 'nonce-test'");
	});
});
