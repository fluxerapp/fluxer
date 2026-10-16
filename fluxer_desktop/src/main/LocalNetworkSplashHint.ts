// SPDX-License-Identifier: AGPL-3.0-or-later

import {isLocalNetworkUrl} from '@electron/common/LocalNetworkHost';

const LOCAL_NETWORK_HINT_DELAY_MS = 4000;

interface LocalNetworkSplashHintDependencies {
	readonly apiBaseUrl: string;
	readonly platform: NodeJS.Platform;
	readonly onHint: () => void;
	readonly delayMs?: number;
	readonly schedule?: (callback: () => void, delayMs: number) => NodeJS.Timeout;
	readonly cancel?: (timer: NodeJS.Timeout) => void;
}

export function shouldWatchForLocalNetworkPrompt(apiBaseUrl: string, platform: NodeJS.Platform): boolean {
	return platform === 'darwin' && isLocalNetworkUrl(apiBaseUrl);
}

export class LocalNetworkSplashHint {
	private readonly enabled: boolean;
	private readonly onHint: () => void;
	private readonly delayMs: number;
	private readonly schedule: (callback: () => void, delayMs: number) => NodeJS.Timeout;
	private readonly cancel: (timer: NodeJS.Timeout) => void;
	private timer: NodeJS.Timeout | null = null;

	public constructor(dependencies: LocalNetworkSplashHintDependencies) {
		this.enabled = shouldWatchForLocalNetworkPrompt(dependencies.apiBaseUrl, dependencies.platform);
		this.onHint = dependencies.onHint;
		this.delayMs = dependencies.delayMs ?? LOCAL_NETWORK_HINT_DELAY_MS;
		this.schedule = dependencies.schedule ?? ((callback, delayMs) => setTimeout(callback, delayMs));
		this.cancel = dependencies.cancel ?? ((timer) => clearTimeout(timer));
	}

	public armWhileChecking(checking: boolean): void {
		if (!this.enabled || !checking) {
			this.disarm();
			return;
		}
		if (this.timer != null) {
			return;
		}
		this.timer = this.schedule(() => {
			this.timer = null;
			this.onHint();
		}, this.delayMs);
		this.timer.unref?.();
	}

	public disarm(): void {
		if (this.timer == null) {
			return;
		}
		this.cancel(this.timer);
		this.timer = null;
	}
}
