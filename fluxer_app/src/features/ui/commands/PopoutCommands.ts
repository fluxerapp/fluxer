// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Popout} from '@app/features/ui/popover';
import PopoutState from '@app/features/ui/state/Popout';

export function open(popout: Popout): void {
	PopoutState.open(popout);
}

export function close(key?: string | number): void {
	PopoutState.requestClose(key);
}

export function finishClose(key?: string | number): void {
	PopoutState.close(key);
}

export function closeAll(): void {
	PopoutState.closeAll();
}

export function closeAllForDocument(ownerDocument: Document): void {
	PopoutState.closeAllForDocument(ownerDocument);
}
