// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import type {DesktopPendingModuleUpdate} from '@fluxer/desktop_ipc/src/ModuleContract';

const logger = createChildLogger('ModuleUpdateGate');

type PendingModuleUpdateListener = (pending: DesktopPendingModuleUpdate | null) => void;

interface PendingModuleUpdateEntry {
	readonly update: DesktopPendingModuleUpdate;
	readonly apply: () => void;
	readonly discard: () => void;
}

interface ModuleUpdateGateState {
	entry: PendingModuleUpdateEntry | null;
	readonly listeners: Set<PendingModuleUpdateListener>;
}

const gateState: ModuleUpdateGateState = {
	entry: null,
	listeners: new Set(),
};

function deliver(listener: PendingModuleUpdateListener, pending: DesktopPendingModuleUpdate | null): void {
	try {
		listener(pending);
	} catch (error) {
		logger.error('A pending module update listener threw', error);
	}
}

function notify(pending: DesktopPendingModuleUpdate | null): void {
	for (const listener of Array.from(gateState.listeners)) {
		deliver(listener, pending);
	}
}

export function offerPendingModuleUpdate(entry: PendingModuleUpdateEntry): void {
	const superseded = gateState.entry;
	gateState.entry = entry;
	if (superseded != null) {
		superseded.discard();
	}
	notify(entry.update);
}

export function getPendingModuleUpdate(): DesktopPendingModuleUpdate | null {
	return gateState.entry?.update ?? null;
}

export function applyPendingModuleUpdate(): boolean {
	const entry = gateState.entry;
	if (entry == null) {
		return false;
	}
	gateState.entry = null;
	notify(null);
	entry.apply();
	return true;
}

export function discardPendingModuleUpdate(): void {
	const entry = gateState.entry;
	if (entry == null) {
		return;
	}
	gateState.entry = null;
	notify(null);
	entry.discard();
}

export function observePendingModuleUpdate(listener: PendingModuleUpdateListener): () => void {
	gateState.listeners.add(listener);
	deliver(listener, getPendingModuleUpdate());
	return () => {
		gateState.listeners.delete(listener);
	};
}
