// SPDX-License-Identifier: AGPL-3.0-or-later

import {BootstrapErrorKind, classifyBootstrapError} from '@app/features/app/components/BootstrapErrorKind';
import {InstanceDiscoveryUnreachableError} from '@fluxer/instance_bootstrap/src/Discovery';
import {expect, test} from 'vitest';

const DESKTOP_UNREACHABLE = new Error(
	"Error invoking remote method 'desktop-runtime-config:resolve': DesktopRuntimeDiscoveryUnreachableError: No usable instance discovery document was served for https://web.canary.fluxer.app/api: https://web.canary.fluxer.app/.well-known/fluxer (connect ECONNREFUSED 127.0.0.1:9)",
);

test('a desktop discovery that could not reach the instance is a connection failure', () => {
	expect(classifyBootstrapError(DESKTOP_UNREACHABLE, true)).toBe(BootstrapErrorKind.UNREACHABLE);
});

test('a web discovery that no candidate served is a connection failure', () => {
	const error = new InstanceDiscoveryUnreachableError([{url: 'https://one.example', reason: 'Failed to fetch'}]);

	expect(classifyBootstrapError(error, true)).toBe(BootstrapErrorKind.UNREACHABLE);
});

test('an unreachable instance wrapped as a cause or inside an aggregate is still a connection failure', () => {
	const wrapped = new Error('Failed to restore the session', {cause: DESKTOP_UNREACHABLE});
	const aggregate = new AggregateError([new Error('cancellation was rejected'), wrapped], 'Instance discovery failed');

	expect(classifyBootstrapError(wrapped, true)).toBe(BootstrapErrorKind.UNREACHABLE);
	expect(classifyBootstrapError(aggregate, true)).toBe(BootstrapErrorKind.UNREACHABLE);
});

test('any failure while the device is offline is a connection failure', () => {
	expect(classifyBootstrapError(new TypeError('Failed to fetch'), false)).toBe(BootstrapErrorKind.UNREACHABLE);
});

test('an instance that answered without a usable document, or a local failure, is not a connection failure', () => {
	const refused = new Error(
		"Error invoking remote method 'desktop-runtime-config:resolve': DesktopRuntimeDiscoveryFailedError: No usable instance discovery document was served for https://one.example/api: https://one.example/.well-known/fluxer (responded HTTP 404)",
	);

	expect(classifyBootstrapError(refused, true)).toBe(BootstrapErrorKind.FAILED);
	expect(classifyBootstrapError(new Error('Stored account record is malformed'), true)).toBe(BootstrapErrorKind.FAILED);
	expect(classifyBootstrapError('boom', true)).toBe(BootstrapErrorKind.FAILED);
});
