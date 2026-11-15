// SPDX-License-Identifier: AGPL-3.0-or-later

import {SignalClient, SignalConnectionState, type SignalOptions} from 'livekit-client';

export class VoiceMeshSignalClient extends SignalClient {
	private connected = false;

	constructor(private readonly onInterestChanged: () => void) {
		super();
	}

	override get currentState(): SignalConnectionState {
		return this.connected ? SignalConnectionState.CONNECTED : SignalConnectionState.DISCONNECTED;
	}

	open(options: SignalOptions): void {
		this.connectOptions = options;
		this.connected = true;
	}

	override close(): Promise<void> {
		this.connected = false;
		return Promise.resolve();
	}

	override sendUpdateSubscription(): Promise<void> {
		this.onInterestChanged();
		return Promise.resolve();
	}

	override sendUpdateTrackSettings(): void {
		this.onInterestChanged();
	}

	override sendMuteTrack(): Promise<void> {
		return Promise.resolve();
	}

	override sendUpdateLocalAudioTrack(): Promise<void> {
		return Promise.resolve();
	}

	override sendUpdateSubscriptionPermissions(): Promise<void> {
		return Promise.resolve();
	}

	override sendUpdateVideoLayers(): Promise<void> {
		return Promise.resolve();
	}

	override sendLeave(): Promise<void> {
		return Promise.resolve();
	}
}
