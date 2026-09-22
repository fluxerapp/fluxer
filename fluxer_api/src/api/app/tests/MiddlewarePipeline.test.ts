// SPDX-License-Identifier: AGPL-3.0-or-later

import {configureMiddleware} from '@app/api/app/MiddlewarePipeline';
import {TorExitMiddleware} from '@app/api/middleware/TorExitMiddleware';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';

function registeredHandlers(torExitBlockingEnabled: boolean): Array<unknown> {
	const routes = new Hono<HonoEnv>({strict: true});
	configureMiddleware(routes, {
		logger: new NoopLogger(),
		nodeEnv: 'test',
		corsOrigins: ['http://localhost:3000'],
		trustClientIpHeader: true,
		clientIpHeaderName: 'x-forwarded-for',
		maxInflightRequests: 100,
		torExitBlockingEnabled,
	});
	return routes.routes.map((route) => route.handler);
}

describe('tor exit blocking in the middleware pipeline', () => {
	it('registers the tor exit middleware when the switch is on', () => {
		expect(registeredHandlers(true)).toContain(TorExitMiddleware);
	});

	it('leaves the tor exit middleware unregistered when the switch is off', () => {
		expect(registeredHandlers(false)).not.toContain(TorExitMiddleware);
	});
});
