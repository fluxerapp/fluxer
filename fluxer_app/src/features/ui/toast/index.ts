// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReactNode} from 'react';

type ToastType = 'success' | 'error' | 'info';
type ToastActivationEvent = React.MouseEvent | React.KeyboardEvent;

export interface ToastProps {
	type: ToastType;
	children: ReactNode;
	timeout?: number;
	onClick?: (event: ToastActivationEvent) => void;
	onTimeout?: () => void;
	onClose?: () => void;
}

export type ToastPropsExtended = ToastProps & {
	id: string;
	closeToast: (id: string) => void;
};
