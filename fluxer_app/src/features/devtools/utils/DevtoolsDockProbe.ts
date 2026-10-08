// SPDX-License-Identifier: AGPL-3.0-or-later

const DEVTOOLS_DOCK_THRESHOLD_PX = 160;

type DevtoolsDockOrientation = 'horizontal' | 'vertical';

export interface DevtoolsViewportSnapshot {
	outerWidth: number;
	innerWidth: number;
	outerHeight: number;
	innerHeight: number;
	legacyDebuggerPresent: boolean;
}

export interface DevtoolsDockState {
	open: boolean;
	orientation: DevtoolsDockOrientation | null;
}

type DevtoolsDockListener = (state: DevtoolsDockState) => void;

function widthGap(snapshot: DevtoolsViewportSnapshot): number {
	return snapshot.outerWidth - snapshot.innerWidth;
}

function heightGap(snapshot: DevtoolsViewportSnapshot): number {
	return snapshot.outerHeight - snapshot.innerHeight;
}

function hasLegacyDebugger(): boolean {
	try {
		return Boolean((window as {Firebug?: {chrome?: {isInitialized?: boolean}}}).Firebug?.chrome?.isInitialized);
	} catch {
		return false;
	}
}

function captureViewportSnapshot(): DevtoolsViewportSnapshot {
	return {
		outerWidth: window.outerWidth,
		innerWidth: window.innerWidth,
		outerHeight: window.outerHeight,
		innerHeight: window.innerHeight,
		legacyDebuggerPresent: hasLegacyDebugger(),
	};
}

function detectDevtoolsDockState(snapshot: DevtoolsViewportSnapshot): DevtoolsDockState {
	const wideGap = widthGap(snapshot) > DEVTOOLS_DOCK_THRESHOLD_PX;
	const tallGap = heightGap(snapshot) > DEVTOOLS_DOCK_THRESHOLD_PX;
	if (!(wideGap && tallGap) && (snapshot.legacyDebuggerPresent || wideGap || tallGap)) {
		return {
			open: true,
			orientation: wideGap ? 'vertical' : 'horizontal',
		};
	}
	return {
		open: false,
		orientation: null,
	};
}

export class DevtoolsDockProbe {
	private readonly listeners = new Set<DevtoolsDockListener>();
	private readonly readSnapshot: () => DevtoolsViewportSnapshot;
	private started = false;
	private state: DevtoolsDockState = {open: false, orientation: null};

	constructor(readSnapshot: () => DevtoolsViewportSnapshot = captureViewportSnapshot) {
		this.readSnapshot = readSnapshot;
	}

	start(): void {
		if (this.started || typeof window === 'undefined') {
			return;
		}
		this.started = true;
		window.addEventListener('resize', this.handleViewportChanged);
		window.addEventListener('orientationchange', this.handleViewportChanged);
		this.poll();
	}

	stop(): void {
		if (!this.started || typeof window === 'undefined') {
			return;
		}
		this.started = false;
		window.removeEventListener('resize', this.handleViewportChanged);
		window.removeEventListener('orientationchange', this.handleViewportChanged);
	}

	subscribe(listener: DevtoolsDockListener): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private handleViewportChanged = (): void => {
		this.poll();
	};

	private poll(): void {
		const nextState = detectDevtoolsDockState(this.readSnapshot());
		if (nextState.open === this.state.open && nextState.orientation === this.state.orientation) {
			return;
		}
		this.state = nextState;
		for (const listener of this.listeners) {
			listener(nextState);
		}
	}
}
