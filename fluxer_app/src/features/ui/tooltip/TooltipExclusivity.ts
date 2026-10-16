// SPDX-License-Identifier: AGPL-3.0-or-later

import {useLayoutEffect, useRef} from 'react';

type HideTooltip = () => void;

let nextTooltipId = 0;
let activeTooltipId: number | null = null;
const tooltipHideCallbacks = new Map<number, HideTooltip>();

function createExclusiveTooltipId(): number {
	nextTooltipId += 1;
	return nextTooltipId;
}

function registerExclusiveTooltip(id: number, hide: HideTooltip): () => void {
	tooltipHideCallbacks.set(id, hide);
	return () => {
		tooltipHideCallbacks.delete(id);
		if (activeTooltipId === id) {
			activeTooltipId = null;
		}
	};
}

function activateExclusiveTooltip(id: number): void {
	if (activeTooltipId !== null && activeTooltipId !== id) {
		tooltipHideCallbacks.get(activeTooltipId)?.();
	}
	activeTooltipId = id;
}

function deactivateExclusiveTooltip(id: number): void {
	if (activeTooltipId === id) {
		activeTooltipId = null;
	}
}

export function useExclusiveTooltip(isOpen: boolean, hide: HideTooltip): void {
	const idRef = useRef<number | null>(null);
	if (idRef.current === null) {
		idRef.current = createExclusiveTooltipId();
	}
	const id = idRef.current;

	useLayoutEffect(() => {
		const unregister = registerExclusiveTooltip(id, hide);
		if (isOpen) {
			activateExclusiveTooltip(id);
		} else {
			deactivateExclusiveTooltip(id);
		}
		return unregister;
	}, [hide, id, isOpen]);
}
