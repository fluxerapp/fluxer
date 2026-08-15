// SPDX-License-Identifier: AGPL-3.0-or-later

import React, {useContext} from 'react';

export const PopoutKeyContext = React.createContext<PopoutKey | null>(null);

export function usePopoutKeyContext(): PopoutKey | null {
	return useContext(PopoutKeyContext);
}

export function usePopoutKey(): PopoutKey | null {
	return usePopoutKeyContext();
}

export type PopoutKey = string | number;
export type PopoutAnimationType = 'smooth' | 'none' | 'profile-slide' | 'profile-slide-inverted';
export type PopoutPosition =
	| 'top'
	| 'bottom'
	| 'left'
	| 'right'
	| 'top-start'
	| 'top-end'
	| 'bottom-start'
	| 'bottom-end'
	| 'left-start'
	| 'left-end'
	| 'right-start'
	| 'right-end';

export interface PopoutReferenceRect {
	x: number;
	y: number;
	width: number;
	height: number;
	top: number;
	right: number;
	bottom: number;
	left: number;
}

export interface Popout {
	key: PopoutKey;
	dependsOn?: PopoutKey;
	position: PopoutPosition;
	target: HTMLElement;
	frozenTargetRect?: PopoutReferenceRect;
	render: (props: {popoutKey: PopoutKey; onClose: () => void}) => React.ReactNode;
	zIndexBoost?: number;
	shouldAutoUpdate?: boolean;
	shouldReposition?: boolean;
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	animationType?: PopoutAnimationType;
	constrainHeight?: boolean;
	containerClass?: string;
	onOpen?: () => void;
	onClose?: () => void;
	onCloseRequest?: (event?: Event) => boolean;
	returnFocusRef?: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	lastPosition?: {
		x: number;
		y: number;
	};
	clickPos?: number;
	preventInvert?: boolean;
	disableBackdrop?: boolean;
	keepOpenOnTargetUnmount?: boolean;
	hoverMode?: boolean;
	returnFocusOnClose?: boolean;
	onContentMouseEnter?: () => void;
	onContentMouseLeave?: () => void;
}
