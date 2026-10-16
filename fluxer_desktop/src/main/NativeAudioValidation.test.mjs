// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const {isValidWinEndpoints, resolveWindowsCaptureScope} = loadTsModule('@electron/main/NativeAudioValidation');

const SPEAKERS_ID = '{0.0.0.00000000}.{6f1e8a2c-0000-4000-8000-000000000001}';

describe('isValidWinEndpoints', () => {
	test('accepts the default entry and device ids', () => {
		assert.equal(isValidWinEndpoints(['default']), true);
		assert.equal(isValidWinEndpoints(['default', SPEAKERS_ID]), true);
		assert.equal(isValidWinEndpoints([`{${'a'.repeat(511)}`]), true);
	});

	test('rejects malformed selections', () => {
		assert.equal(isValidWinEndpoints(undefined), false);
		assert.equal(isValidWinEndpoints('default'), false);
		assert.equal(isValidWinEndpoints([]), false);
		assert.equal(isValidWinEndpoints(Array.from({length: 17}, (_, index) => `{${index}}`)), false);
		assert.equal(isValidWinEndpoints(['speakers']), false);
		assert.equal(isValidWinEndpoints(['{abc\0}']), false);
		assert.equal(isValidWinEndpoints([`{${'a'.repeat(512)}`]), false);
		assert.equal(isValidWinEndpoints([42]), false);
	});
});

describe('resolveWindowsCaptureScope', () => {
	const fullCapabilities = {
		process: true,
		system: true,
		processExclude: true,
		sessionMixer: true,
		endpointSelect: true,
	};

	test('device selection needs the session mixer and the endpoint capability', () => {
		assert.deepEqual(resolveWindowsCaptureScope('session-mixer', ['default'], fullCapabilities), {
			captureScope: 'session-mixer',
			endpoints: ['default'],
		});
		assert.throws(() =>
			resolveWindowsCaptureScope('session-mixer', ['default'], {...fullCapabilities, endpointSelect: false}),
		);
		assert.throws(() => resolveWindowsCaptureScope('session-mixer', ['default'], undefined));
		assert.throws(() => resolveWindowsCaptureScope('system', ['default'], fullCapabilities));
	});

	test('include-only hosts downgrade entire system audio to the session mixer', () => {
		assert.deepEqual(resolveWindowsCaptureScope('system', undefined, {...fullCapabilities, processExclude: false}), {
			captureScope: 'session-mixer',
		});
	});

	test('entire system audio keeps process-exclude loopback when it is available', () => {
		assert.deepEqual(resolveWindowsCaptureScope('system', undefined, fullCapabilities), {captureScope: 'system'});
		assert.deepEqual(resolveWindowsCaptureScope('process', undefined, fullCapabilities), {captureScope: 'process'});
	});
});
