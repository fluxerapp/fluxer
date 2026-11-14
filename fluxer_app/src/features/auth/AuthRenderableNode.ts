// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReactNode} from 'react';

export function isAuthRenderableNode(value: ReactNode): boolean {
	if (value == null) {
		return false;
	}
	if (typeof value === 'boolean') {
		return value;
	}
	if (typeof value === 'string') {
		return value.length > 0;
	}
	if (typeof value === 'number') {
		return value !== 0 && !Number.isNaN(value);
	}
	if (typeof value === 'bigint') {
		return value !== 0n;
	}
	return true;
}
