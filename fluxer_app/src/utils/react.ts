/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type React from 'react';

const FORWARD_REF_TYPE = Symbol.for('react.forward_ref');
const MEMO_TYPE = Symbol.for('react.memo');

interface ReactComponentType {
	prototype?: {
		isReactComponent?: boolean;
	};
}

interface ReactMemoType {
	$$typeof?: symbol;
	type?: unknown;
}

function typeSupportsRef(type: unknown): boolean {
	if (typeof type === 'string') {
		return true;
	}

	if (typeof type === 'function') {
		const componentType = type as ReactComponentType;
		return Boolean(componentType.prototype?.isReactComponent);
	}

	if (typeof type === 'object' && type !== null) {
		const memoType = type as ReactMemoType & Record<string, unknown>;
		const $$typeof = memoType.$$typeof;

		if ($$typeof === FORWARD_REF_TYPE) {
			return true;
		}

		if ($$typeof === MEMO_TYPE) {
			return typeSupportsRef(memoType.type);
		}
	}

	return false;
}

export function elementSupportsRef(element: React.ReactElement | null | undefined): boolean {
	if (!element) return false;
	return typeSupportsRef(element.type);
}
