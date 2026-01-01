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

import type {RemoteParticipant, RemoteTrackPublication, Room} from 'livekit-client';
import {Track, VideoQuality} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';
import type {VideoQualityLevel} from './VoiceSubscriptionManager';

const logger = new Logger('VideoSubscriptionManager');

interface VideoSubscriptionState {
	subscribed: boolean;
	enabled: boolean;
	quality: VideoQualityLevel;
	isIntersecting: boolean;
	observer: IntersectionObserver | null;
}

const qualityMap: Record<VideoQualityLevel, VideoQuality> = {
	low: VideoQuality.LOW,
	medium: VideoQuality.MEDIUM,
	high: VideoQuality.HIGH,
};

export class VideoSubscriptionManager {
	private room: Room | null = null;
	private states = new Map<string, VideoSubscriptionState>();
	private readonly intersectionOptions: IntersectionObserverInit = {
		root: null,
		rootMargin: '50px',
		threshold: [0, 0.1],
	};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setRoom(room: Room | null): void {
		this.room = room;
	}

	cleanup(): void {
		for (const state of this.states.values()) {
			state.observer?.disconnect();
		}
		runInAction(() => {
			this.states.clear();
		});
	}

	subscribe(participantIdentity: string, element: HTMLElement | null, initialQuality: VideoQualityLevel = 'low'): void {
		if (!this.room) {
			logger.warn('[subscribe] No room available');
			return;
		}

		const participant = this.room.remoteParticipants.get(participantIdentity);
		if (!participant) {
			logger.warn('[subscribe] Participant not found', {participantIdentity});
			return;
		}

		const existingState = this.states.get(participantIdentity);
		if (existingState) {
			logger.debug('[subscribe] Already subscribed', {participantIdentity});
			if (element && element !== existingState.observer?.root) {
				this.updateObserver(participantIdentity, element);
			}
			return;
		}

		logger.info('[subscribe] Subscribing to video', {participantIdentity, quality: initialQuality});

		const cameraPublication = this.findCameraPublication(participant);
		if (!cameraPublication) {
			logger.debug('[subscribe] No camera publication found', {participantIdentity});
			return;
		}

		try {
			cameraPublication.setSubscribed(true);
			this.applyQuality(cameraPublication, initialQuality);

			const observer = element ? this.createObserver(participantIdentity, element) : null;

			runInAction(() => {
				this.states.set(participantIdentity, {
					subscribed: true,
					enabled: false,
					quality: initialQuality,
					isIntersecting: false,
					observer,
				});
			});

			logger.debug('[subscribe] Video subscribed successfully', {participantIdentity});
		} catch (error) {
			logger.error('[subscribe] Failed to subscribe', {participantIdentity, error});
		}
	}

	unsubscribe(participantIdentity: string): void {
		const state = this.states.get(participantIdentity);
		if (!state) {
			logger.debug('[unsubscribe] Not subscribed', {participantIdentity});
			return;
		}

		logger.info('[unsubscribe] Unsubscribing from video', {participantIdentity});

		state.observer?.disconnect();

		if (this.room) {
			const participant = this.room.remoteParticipants.get(participantIdentity);
			if (participant) {
				const cameraPublication = this.findCameraPublication(participant);
				if (cameraPublication) {
					try {
						cameraPublication.setSubscribed(false);
						logger.debug('[unsubscribe] Track unsubscribed', {participantIdentity});
					} catch (error) {
						logger.error('[unsubscribe] Failed to unsubscribe', {participantIdentity, error});
					}
				}
			}
		}

		runInAction(() => {
			this.states.delete(participantIdentity);
		});

		logger.info('[unsubscribe] Video unsubscribed successfully', {participantIdentity});
	}

	setEnabled(participantIdentity: string, enabled: boolean): void {
		const state = this.states.get(participantIdentity);
		if (!state) {
			logger.debug('[setEnabled] Not subscribed', {participantIdentity});
			return;
		}

		if (state.enabled === enabled) {
			return;
		}

		logger.debug('[setEnabled] Setting video enabled state', {participantIdentity, enabled});

		if (this.room) {
			const participant = this.room.remoteParticipants.get(participantIdentity);
			if (participant) {
				const cameraPublication = this.findCameraPublication(participant);
				if (cameraPublication) {
					try {
						cameraPublication.setEnabled(enabled);
						runInAction(() => {
							state.enabled = enabled;
						});
						logger.debug('[setEnabled] Track enabled state updated', {participantIdentity, enabled});
					} catch (error) {
						logger.error('[setEnabled] Failed to set enabled state', {participantIdentity, enabled, error});
					}
				}
			}
		}
	}

	setQuality(participantIdentity: string, quality: VideoQualityLevel): void {
		const state = this.states.get(participantIdentity);
		if (!state) {
			logger.debug('[setQuality] Not subscribed', {participantIdentity});
			return;
		}

		if (state.quality === quality) {
			return;
		}

		logger.debug('[setQuality] Setting video quality', {participantIdentity, quality});

		if (this.room) {
			const participant = this.room.remoteParticipants.get(participantIdentity);
			if (participant) {
				const cameraPublication = this.findCameraPublication(participant);
				if (cameraPublication) {
					this.applyQuality(cameraPublication, quality);
					runInAction(() => {
						state.quality = quality;
					});
					logger.debug('[setQuality] Quality updated', {participantIdentity, quality});
				}
			}
		}
	}

	isSubscribed(participantIdentity: string): boolean {
		return this.states.get(participantIdentity)?.subscribed ?? false;
	}

	getQuality(participantIdentity: string): VideoQualityLevel | null {
		return this.states.get(participantIdentity)?.quality ?? null;
	}

	private findCameraPublication(participant: RemoteParticipant): RemoteTrackPublication | undefined {
		for (const pub of participant.videoTrackPublications.values()) {
			if (pub.source === Track.Source.Camera) {
				return pub;
			}
		}
		return undefined;
	}

	private applyQuality(publication: RemoteTrackPublication, quality: VideoQualityLevel): void {
		try {
			const lkQuality = qualityMap[quality];
			publication.setVideoQuality(lkQuality);
			logger.debug('[applyQuality] Quality set', {quality, lkQuality});
		} catch (error) {
			logger.error('[applyQuality] Failed to set quality', {quality, error});
		}
	}

	private createObserver(participantIdentity: string, element: HTMLElement): IntersectionObserver {
		const observer = new IntersectionObserver((entries) => {
			for (const entry of entries) {
				const isIntersecting = entry.isIntersecting;
				const state = this.states.get(participantIdentity);
				if (!state) continue;

				runInAction(() => {
					state.isIntersecting = isIntersecting;
				});

				this.setEnabled(participantIdentity, isIntersecting);

				logger.debug('[observer] Intersection changed', {participantIdentity, isIntersecting});
			}
		}, this.intersectionOptions);

		observer.observe(element);
		return observer;
	}

	private updateObserver(participantIdentity: string, element: HTMLElement): void {
		const state = this.states.get(participantIdentity);
		if (!state) return;

		state.observer?.disconnect();

		const observer = this.createObserver(participantIdentity, element);

		runInAction(() => {
			state.observer = observer;
		});

		logger.debug('[updateObserver] Observer updated', {participantIdentity});
	}
}
