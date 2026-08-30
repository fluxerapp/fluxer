// SPDX-License-Identifier: AGPL-3.0-or-later

import {createLogger, Logger} from '@fluxer/logger/src/Logger';
import pino, {type Logger as PinoLogger} from 'pino';
import {afterEach, describe, expect, it, vi} from 'vitest';

function readDestination(logger: PinoLogger): unknown {
	return (logger as unknown as Record<symbol, unknown>)[pino.symbols.streamSym];
}

describe('Logger.child', () => {
	afterEach(() => {
		vi.restoreAllMocks();
		vi.unstubAllEnvs();
	});

	it('shares the parent destination instead of opening a new one', () => {
		const parent = createLogger('logger-test', {environment: 'production'});
		const child = parent.child({logger: 'ChildService'});
		expect(child).toBeInstanceOf(Logger);
		expect(readDestination(child.pino)).toBe(readDestination(parent.pino));
	});

	it('does not construct a throwaway pino root logger', () => {
		vi.stubEnv('FLUXER_ENV', 'production');
		const parent = createLogger('logger-test', {environment: 'production'});
		const destinationSpy = vi.spyOn(pino, 'destination');
		const child = parent.child({logger: 'ChildService'});
		expect(destinationSpy).not.toHaveBeenCalled();
		expect(child.pino.bindings()['logger']).toBe('ChildService');
	});

	it('keeps the parent bindings on the child', () => {
		const parent = createLogger('logger-test', {environment: 'production'});
		const child = parent.child({logger: 'ChildService'});
		expect(child.pino.bindings()['service']).toBe('logger-test');
		expect(child.pino.level).toBe(parent.pino.level);
	});
});
