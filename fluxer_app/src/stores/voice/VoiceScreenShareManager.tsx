/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Room, ScreenShareCaptureOptions, TrackPublishOptions} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import {Logger} from '~/lib/Logger';
import {Platform} from '~/lib/Platform';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import {ScreenRecordingPermissionDeniedError} from '~/utils/errors/ScreenRecordingPermissionDeniedError';
import {ensureNativePermission} from '~/utils/NativePermissions';
import {isDesktop, isNativeMacOS} from '~/utils/NativeUtils';
import {SoundType} from '~/utils/SoundUtils';

const logger = new Logger('VoiceScreenShareManager');

interface VoiceStateSync {
	syncVoiceState: (partial: {self_stream?: boolean}) => void;
	updateLocalParticipant: () => void;
}

class VoiceScreenShareManager {
	isScreenSharePending = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getIsScreenSharePending(): boolean {
		return this.isScreenSharePending;
	}

	async setScreenShareEnabled(
		room: Room | null,
		enabled: boolean,
		sync: VoiceStateSync,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		if (Platform.OS !== 'web') {
			logger.warn('[setScreenShareEnabled] Screen share not supported on native');
			return;
		}

		const {sendUpdate = true, ...restOptions} = options || {};
		const participant = room?.localParticipant;

		if (!participant) {
			logger.warn('[setScreenShareEnabled] No participant');
			return;
		}

		if (this.isScreenSharePending) {
			logger.debug('[setScreenShareEnabled] Already pending, ignoring request');
			return;
		}

		if (enabled && isDesktop() && isNativeMacOS()) {
			const denied = MediaPermissionStore.isScreenRecordingExplicitlyDenied();
			if (denied) {
				logger.warn('[setScreenShareEnabled] Screen recording permission explicitly denied');
				throw new ScreenRecordingPermissionDeniedError();
			}

			const nativeResult = await ensureNativePermission('screen');
			if (nativeResult === 'denied') {
				logger.warn('[setScreenShareEnabled] Screen recording permission denied');
				MediaPermissionStore.markScreenRecordingExplicitlyDenied();
				throw new ScreenRecordingPermissionDeniedError();
			}

			if (nativeResult === 'granted') {
				MediaPermissionStore.updateScreenRecordingPermissionGranted();
			}
		}

		const applyState = (value: boolean) => {
			LocalVoiceStateStore.updateSelfStream(value);
			if (sendUpdate) sync.syncVoiceState({self_stream: value});
		};

		if (!enabled) applyState(false);

		runInAction(() => {
			this.isScreenSharePending = true;
		});

		try {
			await participant.setScreenShareEnabled(enabled, restOptions, publishOptions);
			if (enabled) applyState(true);

			sync.updateLocalParticipant();

			if (enabled) {
				SoundActionCreators.playSound(SoundType.ScreenShareStart);
			} else {
				SoundActionCreators.playSound(SoundType.ScreenShareStop);
			}
			logger.info('[setScreenShareEnabled] Success', {enabled});
		} catch (e) {
			if (e instanceof Error && (e.name === 'AbortError' || e.name === 'NotAllowedError')) {
				logger.debug('[setScreenShareEnabled] User cancelled or permission denied', {name: e.name});
				const actual = participant.isScreenShareEnabled;
				applyState(actual);
				return;
			}

			logger.error('[setScreenShareEnabled] Failed', {enabled, error: e});
			const actual = participant.isScreenShareEnabled;
			applyState(actual);

			if (actual) {
				SoundActionCreators.playSound(SoundType.ScreenShareStart);
			} else {
				SoundActionCreators.playSound(SoundType.ScreenShareStop);
			}
		} finally {
			runInAction(() => {
				this.isScreenSharePending = false;
			});
		}
	}

	async toggleScreenShareFromKeybind(room: Room | null, sync: VoiceStateSync): Promise<void> {
		const current = LocalVoiceStateStore.getSelfStream();
		await this.setScreenShareEnabled(room, !current, sync);
	}

	resetStreamTracking(): void {
		runInAction(() => {
			this.isScreenSharePending = false;
		});
	}
}

export default new VoiceScreenShareManager();
