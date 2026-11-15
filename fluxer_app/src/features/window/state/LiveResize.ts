// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

const LIVE_RESIZE_CLASS = 'window-live-resizing';

type Listener = () => void;

let resizing = false;
let started = false;
const listeners = new Set<Listener>();

function applyClass(active: boolean): void {
	if (typeof document === 'undefined') return;
	document.documentElement.classList.toggle(LIVE_RESIZE_CLASS, active);
}

function setResizing(active: boolean): void {
	if (resizing === active) return;
	resizing = active;
	applyClass(active);
	for (const listener of listeners) listener();
}

function start(): void {
	if (started) return;
	started = true;
	const electronApi = getElectronAPI();
	if (electronApi?.onWindowLiveResizeChange == null) return;
	electronApi.onWindowLiveResizeChange((active) => {
		setResizing(active);
	});
}

export function startLiveResizeTracking(): void {
	start();
}

export function subscribeToLiveResize(listener: Listener): () => void {
	start();
	listeners.add(listener);
	return () => {
		listeners.delete(listener);
	};
}

export function getLiveResizeSnapshot(): boolean {
	return resizing;
}
