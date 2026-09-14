// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {implementationError} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Error} from '@fluxer/voice_engine_v2/src/protocol/types';

export function nativeZeroCopyRequiredError(
	resource: 'capture' | 'frame' | 'frameSink' | 'hardwareEncoder',
): VoiceEngineV2Error {
	assert.equal(typeof resource, 'string', 'nativeZeroCopyRequiredError resource must be a string');
	assert.ok(resource.length > 0, 'nativeZeroCopyRequiredError resource must not be empty');
	const label =
		resource === 'capture'
			? 'Native Electron capture'
			: resource === 'frame'
				? 'Native Electron capture frames'
				: resource === 'frameSink'
					? 'Native Electron frame sinks'
					: 'Native hardware encoder usage';
	return implementationError(`${label} require zero-copy transport`, 'zeroCopyScreenTransport');
}

export function unavailableZeroCopyTransportError(
	resource: 'capture' | 'frameSink' | 'hardwareEncoder',
): VoiceEngineV2Error {
	assert.equal(typeof resource, 'string', 'unavailableZeroCopyTransportError resource must be a string');
	assert.ok(resource.length > 0, 'unavailableZeroCopyTransportError resource must not be empty');
	const label =
		resource === 'capture'
			? 'native Electron capture'
			: resource === 'frameSink'
				? 'native Electron frame sinks'
				: 'native hardware encoders';
	return {
		code: 'unsupportedCapability',
		capability: 'zeroCopyScreenTransport',
		message: `Voice engine v2 implementation does not support zero-copy transport for ${label}`,
	};
}
