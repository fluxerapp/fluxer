// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {LimitConfigCodec} from '@fluxer/limits/src/LimitConfigCodec';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';

const defaultLimitConfigCodec = new LimitConfigCodec();

export function computeOverrides(
	fullLimits: Partial<Record<LimitKey, number>>,
	defaults: Record<LimitKey, number>,
): Partial<Record<LimitKey, number>> {
	return defaultLimitConfigCodec.computeOverrides(fullLimits, defaults);
}

export function computeWireFormat(config: LimitConfigSnapshot): LimitConfigWireFormat {
	return defaultLimitConfigCodec.toWireFormat(config);
}

export function expandWireFormat(wireFormat: LimitConfigWireFormat): LimitConfigSnapshot {
	return defaultLimitConfigCodec.fromWireFormat(wireFormat);
}
