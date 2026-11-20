// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect} from 'react';
import {useDragDropManager} from 'react-dnd';

export function useStaleDragRecovery(active: boolean, onRecover: () => void): void {
	const dragDropManager = useDragDropManager();
	useEffect(() => {
		if (!active) return;
		const recover = () => {
			if (dragDropManager.getMonitor().isDragging()) {
				dragDropManager.getActions().endDrag();
			}
			onRecover();
		};
		window.addEventListener('pointerdown', recover, true);
		window.addEventListener('blur', recover);
		return () => {
			window.removeEventListener('pointerdown', recover, true);
			window.removeEventListener('blur', recover);
		};
	}, [active, dragDropManager, onRecover]);
}
