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

import type {Room} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';
import {ScreenShareSubscriptionManager} from './ScreenShareSubscriptionManager';
import {VideoSubscriptionManager} from './VideoSubscriptionManager';

const logger = new Logger('VoiceSubscriptionManager');

export type VideoQualityLevel = 'low' | 'medium' | 'high';

class VoiceSubscriptionManager {
	private room: Room | null = null;
	private videoManager: VideoSubscriptionManager;
	private screenShareManager: ScreenShareSubscriptionManager;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.videoManager = new VideoSubscriptionManager();
		this.screenShareManager = new ScreenShareSubscriptionManager();
		logger.debug('[VoiceSubscriptionManager] Initialized');
	}

	setRoom(room: Room | null): void {
		if (this.room === room) return;

		if (this.room) {
			this.cleanup();
		}

		runInAction(() => {
			this.room = room;
		});

		this.videoManager.setRoom(room);
		this.screenShareManager.setRoom(room);

		if (room) {
			logger.info('[setRoom] Room set', {participantCount: room.remoteParticipants.size});
		} else {
			logger.info('[setRoom] Room cleared');
		}
	}

	cleanup(): void {
		logger.debug('[cleanup] Cleaning up all subscriptions');
		this.videoManager.cleanup();
		this.screenShareManager.cleanup();
		logger.info('[cleanup] All subscriptions cleaned up');
	}

	subscribeToVideo(
		participantIdentity: string,
		element: HTMLElement | null,
		initialQuality: VideoQualityLevel = 'low',
	): void {
		this.videoManager.subscribe(participantIdentity, element, initialQuality);
	}

	unsubscribeFromVideo(participantIdentity: string): void {
		this.videoManager.unsubscribe(participantIdentity);
	}

	setVideoEnabled(participantIdentity: string, enabled: boolean): void {
		this.videoManager.setEnabled(participantIdentity, enabled);
	}

	setVideoQuality(participantIdentity: string, quality: VideoQualityLevel): void {
		this.videoManager.setQuality(participantIdentity, quality);
	}

	subscribeToScreenShare(
		participantIdentity: string,
		element: HTMLElement | null,
		context: 'focused' | 'carousel' | 'hidden' = 'carousel',
	): void {
		this.screenShareManager.subscribe(participantIdentity, element, context);
	}

	unsubscribeFromScreenShare(participantIdentity: string): void {
		this.screenShareManager.unsubscribe(participantIdentity);
	}

	setScreenShareContext(participantIdentity: string, context: 'focused' | 'carousel' | 'hidden'): void {
		this.screenShareManager.setContext(participantIdentity, context);
	}

	isVideoSubscribed(participantIdentity: string): boolean {
		return this.videoManager.isSubscribed(participantIdentity);
	}

	isScreenShareSubscribed(participantIdentity: string): boolean {
		return this.screenShareManager.isSubscribed(participantIdentity);
	}

	getVideoQuality(participantIdentity: string): VideoQualityLevel | null {
		return this.videoManager.getQuality(participantIdentity);
	}

	getScreenShareContext(participantIdentity: string): 'focused' | 'carousel' | 'hidden' | null {
		return this.screenShareManager.getContext(participantIdentity);
	}
}

const instance = new VoiceSubscriptionManager();
(window as typeof window & {_voiceSubscriptionManager?: VoiceSubscriptionManager})._voiceSubscriptionManager = instance;
export default instance;
