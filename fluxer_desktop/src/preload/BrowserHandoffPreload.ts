// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DesktopHandoffAPI,
	DesktopHandoffInstance,
	DesktopHandoffSession,
	DesktopHandoffStatusResult,
} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {DESKTOP_HANDOFF_CHANNELS} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';

interface BrowserHandoffInvoker {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
}

interface BrowserHandoffPreloadAPI {
	readonly desktopHandoff: DesktopHandoffAPI;
}

export function createBrowserHandoffPreloadAPI(renderer: BrowserHandoffInvoker): BrowserHandoffPreloadAPI {
	return Object.freeze({
		desktopHandoff: Object.freeze<DesktopHandoffAPI>({
			initiate: (instance: DesktopHandoffInstance) =>
				renderer.invoke(DESKTOP_HANDOFF_CHANNELS.initiate, instance) as Promise<DesktopHandoffSession>,
			status: (code: string) =>
				renderer.invoke(DESKTOP_HANDOFF_CHANNELS.status, code) as Promise<DesktopHandoffStatusResult>,
		}),
	});
}
