// SPDX-License-Identifier: AGPL-3.0-or-later

import {createContext} from 'react';

export interface ModalStackContextValue {
	stackIndex: number;
	isVisible: boolean;
	needsBackdrop: boolean;
	isTopmost: boolean;
	restoreFocusOnClose: boolean;
}

export const UNSTACKED_MODAL_CONTEXT: ModalStackContextValue = {
	stackIndex: 0,
	isVisible: true,
	needsBackdrop: true,
	isTopmost: true,
	restoreFocusOnClose: true,
};

export const ModalStackContext = createContext<ModalStackContextValue>(UNSTACKED_MODAL_CONTEXT);
