// SPDX-License-Identifier: AGPL-3.0-or-later

import {isLocalNetworkUrl} from '@electron/common/LocalNetworkHost';

const LOCAL_NETWORK_HINT_DELAY_MS = 4000;

interface LocalNetworkSplashHintDependencies {
	readonly apiBaseUrl: string;
	readonly platform: NodeJS.Platform;
	readonly onHint: () => void;
}

function shouldWatchForLocalNetworkPrompt(apiBaseUrl: string, platform: NodeJS.Platform): boolean {
	return platform === 'darwin' && isLocalNetworkUrl(apiBaseUrl);
}

export class LocalNetworkSplashHint {
	private readonly enabled: boolean;
	private readonly onHint: () => void;
	private timer: NodeJS.Timeout | null = null;

	public constructor(dependencies: LocalNetworkSplashHintDependencies) {
		this.enabled = shouldWatchForLocalNetworkPrompt(dependencies.apiBaseUrl, dependencies.platform);
		this.onHint = dependencies.onHint;
	}

	public armWhileChecking(checking: boolean): void {
		if (!this.enabled || !checking) {
			this.disarm();
			return;
		}
		if (this.timer != null) {
			return;
		}
		this.timer = setTimeout(() => {
			this.timer = null;
			this.onHint();
		}, LOCAL_NETWORK_HINT_DELAY_MS);
		this.timer.unref();
	}

	public disarm(): void {
		if (this.timer == null) {
			return;
		}
		clearTimeout(this.timer);
		this.timer = null;
	}
}
