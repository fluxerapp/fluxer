// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';

export interface GatewayResumeListenerRequest {
	readonly socket: GatewaySocket;
	readonly isCurrent: () => boolean;
}

export class GatewayResumeListenerOwner {
	private onlineListener: (() => void) | null = null;
	private visibilityListener: (() => void) | null = null;
	private pageshowListener: ((event: PageTransitionEvent) => void) | null = null;

	install({socket, isCurrent}: GatewayResumeListenerRequest): void {
		this.dispose();

		this.onlineListener = () => {
			if (!isCurrent()) return;
			socket.handleNetworkStatusChange(true);
		};
		window.addEventListener('online', this.onlineListener);

		this.visibilityListener = () => {
			if (!isCurrent()) return;
			if (document.visibilityState !== 'visible') return;
			socket.probeAfterResume('visibilitychange', {accelerateReconnect: false});
		};
		document.addEventListener('visibilitychange', this.visibilityListener);

		this.pageshowListener = (event: PageTransitionEvent) => {
			if (!isCurrent()) return;
			if (event.persisted) {
				socket.forceReconnectFromResume('pageshow-bfcache');
				return;
			}
			socket.probeAfterResume('pageshow', {accelerateReconnect: false});
		};
		window.addEventListener('pageshow', this.pageshowListener);
	}

	dispose(): void {
		if (this.onlineListener != null) {
			window.removeEventListener('online', this.onlineListener);
			this.onlineListener = null;
		}
		if (this.visibilityListener != null) {
			document.removeEventListener('visibilitychange', this.visibilityListener);
			this.visibilityListener = null;
		}
		if (this.pageshowListener != null) {
			window.removeEventListener('pageshow', this.pageshowListener);
			this.pageshowListener = null;
		}
	}
}
