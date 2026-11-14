// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Placement} from '@floating-ui/react-dom';

const MIRRORED_SIDE: Record<string, string> = {left: 'right', right: 'left'};

export function toDocumentPlacement<T extends Placement>(placement: T, element: Element | null | undefined): T {
	const root =
		element?.ownerDocument?.documentElement ?? (typeof document === 'undefined' ? null : document.documentElement);
	if (root?.dir !== 'rtl') {
		return placement;
	}
	const [side, align] = placement.split('-');
	const mirrored = MIRRORED_SIDE[side];
	if (!mirrored) {
		return placement;
	}
	return (align ? `${mirrored}-${align}` : mirrored) as T;
}
