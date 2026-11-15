// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	getVoiceConnectionContextFromMediaEngine,
	syncLocalVoiceStateWithServer,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {asPinnableVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';

type VoiceEngineV2AppMediaSource = 'camera' | 'screen_share';
type VoiceEngineV2AppMediaStateReason =
	| 'user'
	| 'server'
	| 'track_event'
	| 'disconnect'
	| 'room_disconnect'
	| 'cleanup'
	| 'logout';

interface VoiceEngineV2AppMediaStateUpdateOptions {
	sendUpdate: boolean;
	forceSync?: boolean;
}

interface VoiceEngineV2AppMediaStateOptions {
	forceSync?: boolean;
	sendUpdate?: boolean;
	reason: VoiceEngineV2AppMediaStateReason;
}

const logger = new Logger('VoiceEngineV2AppMediaStateAdapter');

function deriveStateOptions(options: VoiceEngineV2AppMediaStateUpdateOptions): VoiceEngineV2AppMediaStateOptions {
	const reason: VoiceEngineV2AppMediaStateReason = options.sendUpdate ? 'user' : 'server';
	return {forceSync: options.forceSync === true, reason, sendUpdate: options.sendUpdate};
}

class VoiceEngineV2AppMediaStateAdapter {
	private suppressNextSync: Record<VoiceEngineV2AppMediaSource, boolean> = {
		camera: false,
		screen_share: false,
	};

	applyCameraState(enabled: boolean, options: VoiceEngineV2AppMediaStateUpdateOptions): void {
		assert.equal(typeof enabled, 'boolean', 'camera state must be a boolean');
		assert.equal(typeof options.sendUpdate, 'boolean', 'camera state sendUpdate must be a boolean');
		assert.ok(
			typeof options.forceSync === 'boolean' || options.forceSync === undefined,
			'camera state forceSync must be a boolean when present',
		);
		this.applyRequestedState('camera', enabled, deriveStateOptions(options));
	}

	applyScreenShareState(enabled: boolean, options: VoiceEngineV2AppMediaStateUpdateOptions): void {
		assert.equal(typeof enabled, 'boolean', 'screen-share state must be a boolean');
		assert.equal(typeof options.sendUpdate, 'boolean', 'screen-share state sendUpdate must be a boolean');
		assert.ok(
			typeof options.forceSync === 'boolean' || options.forceSync === undefined,
			'screen-share state forceSync must be a boolean when present',
		);
		this.applyRequestedState('screen_share', enabled, deriveStateOptions(options));
	}

	handleLocalTrackStateChange(source: unknown, isPublished: boolean): boolean {
		const mapped = this.mapSource(source);
		if (!mapped) return false;
		const suppressed = this.consumeSuppression(mapped);
		const changed = this.updateLocalState(mapped, isPublished);
		if (suppressed || !changed) {
			if (suppressed) {
				logger.debug('Suppressed local track sync', {source: mapped, isPublished});
			}
			return changed;
		}
		this.syncState(mapped, isPublished);
		return changed;
	}

	resetLocalMediaState(reason: VoiceEngineV2AppMediaStateReason): void {
		const suppress =
			reason === 'disconnect' || reason === 'room_disconnect' || reason === 'cleanup' || reason === 'logout';
		this.suppressNextSync.camera = suppress;
		this.suppressNextSync.screen_share = suppress;
		this.applyRequestedState('camera', false, {reason, sendUpdate: false});
		this.applyRequestedState('screen_share', false, {reason, sendUpdate: false});
	}

	private applyRequestedState(
		source: VoiceEngineV2AppMediaSource,
		enabled: boolean,
		options: VoiceEngineV2AppMediaStateOptions,
	): void {
		const {forceSync = false, sendUpdate = true, reason} = options;
		if (reason === 'user') {
			this.suppressNextSync[source] = false;
		}
		if (!sendUpdate) {
			this.suppressNextSync[source] = true;
		}
		const changed = this.updateLocalState(source, enabled);
		if (!sendUpdate || (!changed && !forceSync)) return;
		this.syncState(source, enabled);
	}

	private updateLocalState(source: VoiceEngineV2AppMediaSource, enabled: boolean): boolean {
		if (source === 'camera') {
			const current = LocalVoiceState.getSelfVideo();
			if (current === enabled) return false;
			LocalVoiceState.updateSelfVideo(enabled);
			return true;
		}
		const current = LocalVoiceState.getSelfStream();
		if (current === enabled) return false;
		LocalVoiceState.updateSelfStream(enabled);
		return true;
	}

	private syncState(source: VoiceEngineV2AppMediaSource, enabled: boolean): void {
		if (!this.canSync()) return;
		if (source === 'camera') {
			syncLocalVoiceStateWithServer({self_video: enabled});
			return;
		}
		syncLocalVoiceStateWithServer({self_stream: enabled});
	}

	private canSync(): boolean {
		const connection = getVoiceConnectionContextFromMediaEngine();
		if (!connection || connection.disconnecting) return false;
		return Boolean(connection.channelId && connection.connectionId);
	}

	private mapSource(source: unknown): VoiceEngineV2AppMediaSource | null {
		const pinnedSource = asPinnableVoiceTrackSource(source);
		if (pinnedSource === VoiceTrackSource.Camera) return 'camera';
		if (pinnedSource === VoiceTrackSource.ScreenShare) return 'screen_share';
		return null;
	}

	private consumeSuppression(source: VoiceEngineV2AppMediaSource): boolean {
		if (!this.suppressNextSync[source]) return false;
		this.suppressNextSync[source] = false;
		return true;
	}
}

const voiceEngineV2AppMediaStateAdapter = new VoiceEngineV2AppMediaStateAdapter();

export default voiceEngineV2AppMediaStateAdapter;
