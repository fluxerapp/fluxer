// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PopoutPosition} from '@app/features/ui/popover';
import {createContext, useContext} from 'react';

export interface PopoutResizePositionOffset {
	x: number;
	y: number;
}

export interface PopoutResizePositionSession {
	updateOffset: (offset: PopoutResizePositionOffset) => void;
	finish: (offset: PopoutResizePositionOffset) => void;
}

export interface PopoutResizePositionController {
	begin: () => PopoutResizePositionSession;
	placement?: PopoutPosition;
}

export const PopoutResizePositionContext = createContext<PopoutResizePositionController | null>(null);

export function usePopoutResizePositionController(): PopoutResizePositionController {
	const controller = useContext(PopoutResizePositionContext);
	if (controller == null) {
		throw new Error('Resizable popout requires a positioning owner');
	}
	return controller;
}

export function usePopoutResizePlacement(): PopoutPosition | null {
	const controller = useContext(PopoutResizePositionContext);
	if (controller == null || controller.placement == null) return null;
	return controller.placement;
}
