// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ClassValue, clsx} from 'clsx';
import type React from 'react';

export const FLX_ELEMENT_CLASS_NAME = 'flx-element';

export function flxElementClassName(...values: Array<ClassValue>): string {
	return clsx(FLX_ELEMENT_CLASS_NAME, ...values);
}

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
