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

import React from 'react';

export const PopoutKeyContext = React.createContext<PopoutKey | null>(null);

export const usePopoutKeyContext = (): PopoutKey | null => {
	return React.useContext(PopoutKeyContext);
};

export type PopoutKey = string | number;

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

export interface Popout {
	key: PopoutKey;
	dependsOn?: PopoutKey;
	position: PopoutPosition;
	target: HTMLElement;
	render: (props: {popoutKey: PopoutKey; onClose: () => void}) => React.ReactNode;
	zIndexBoost?: number;
	shouldAutoUpdate?: boolean;
	shouldReposition?: boolean;
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	animationType?: 'smooth' | 'none';
	containerClass?: string;
	onOpen?: () => void;
	onClose?: () => void;
	onCloseRequest?: (event?: Event) => boolean;
	returnFocusRef?: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	lastPosition?: {x: number; y: number};
	clickPos?: number;
	preventInvert?: boolean;
	disableBackdrop?: boolean;
	hoverMode?: boolean;
	onContentMouseEnter?: () => void;
	onContentMouseLeave?: () => void;
}
