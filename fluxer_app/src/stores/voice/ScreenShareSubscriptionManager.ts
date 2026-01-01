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

const logger = new Logger('ScreenShareSubscriptionManager');

interface ScreenShareSubscriptionState {
	subscribed: boolean;
	enabled: boolean;
	quality: VideoQualityLevel;
	context: 'focused' | 'carousel' | 'hidden';
	isIntersecting: boolean;
	observer: IntersectionObserver | null;
}

const qualityMap: Record<VideoQualityLevel, VideoQuality> = {
	low: VideoQuality.LOW,
	medium: VideoQuality.MEDIUM,
	high: VideoQuality.HIGH,
};

export class ScreenShareSubscriptionManager {
	private room: Room | null = null;
	private states = new Map<string, ScreenShareSubscriptionState>();
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

	subscribe(
		participantIdentity: string,
		element: HTMLElement | null,
		context: 'focused' | 'carousel' | 'hidden' = 'carousel',
	): void {
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
			if (existingState.context !== context) {
				this.setContext(participantIdentity, context);
			}
			if (element && element !== existingState.observer?.root) {
				this.updateObserver(participantIdentity, element);
			}
			return;
		}

		logger.info('[subscribe] Subscribing to screen share', {participantIdentity, context});

		const screenSharePublication = this.findScreenSharePublication(participant);
		if (!screenSharePublication) {
			logger.debug('[subscribe] No screen share publication found', {participantIdentity});
			return;
		}

		const quality = this.getQualityForContext(context);
		const enabled = context !== 'hidden';

		try {
			screenSharePublication.setSubscribed(true);
			if (enabled) {
				screenSharePublication.setEnabled(true);
			} else {
				screenSharePublication.setEnabled(false);
			}
			this.applyQuality(screenSharePublication, quality);

			const observer = element ? this.createObserver(participantIdentity, element) : null;

			runInAction(() => {
				this.states.set(participantIdentity, {
					subscribed: true,
					enabled,
					quality,
					context,
					isIntersecting: false,
					observer,
				});
			});

			logger.debug('[subscribe] Screen share subscribed successfully', {participantIdentity, context});
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

		logger.info('[unsubscribe] Unsubscribing from screen share', {participantIdentity});

		state.observer?.disconnect();

		if (this.room) {
			const participant = this.room.remoteParticipants.get(participantIdentity);
			if (participant) {
				const screenSharePublication = this.findScreenSharePublication(participant);
				if (screenSharePublication) {
					try {
						screenSharePublication.setSubscribed(false);
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

		logger.info('[unsubscribe] Screen share unsubscribed successfully', {participantIdentity});
	}

	setContext(participantIdentity: string, context: 'focused' | 'carousel' | 'hidden'): void {
		const state = this.states.get(participantIdentity);
		if (!state) {
			logger.debug('[setContext] Not subscribed', {participantIdentity});
			return;
		}

		if (state.context === context) {
			return;
		}

		logger.debug('[setContext] Setting screen share context', {participantIdentity, context});

		const quality = this.getQualityForContext(context);
		const enabled = context !== 'hidden';

		if (this.room) {
			const participant = this.room.remoteParticipants.get(participantIdentity);
			if (participant) {
				const screenSharePublication = this.findScreenSharePublication(participant);
				if (screenSharePublication) {
					try {
						screenSharePublication.setEnabled(enabled);
						this.applyQuality(screenSharePublication, quality);

						runInAction(() => {
							state.context = context;
							state.enabled = enabled;
							state.quality = quality;
						});

						logger.debug('[setContext] Context updated', {participantIdentity, context, quality, enabled});
					} catch (error) {
						logger.error('[setContext] Failed to update context', {participantIdentity, context, error});
					}
				}
			}
		}
	}

	isSubscribed(participantIdentity: string): boolean {
		return this.states.get(participantIdentity)?.subscribed ?? false;
	}

	getContext(participantIdentity: string): 'focused' | 'carousel' | 'hidden' | null {
		return this.states.get(participantIdentity)?.context ?? null;
	}

	private findScreenSharePublication(participant: RemoteParticipant): RemoteTrackPublication | undefined {
		for (const pub of participant.videoTrackPublications.values()) {
			if (pub.source === Track.Source.ScreenShare) {
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

	private getQualityForContext(context: 'focused' | 'carousel' | 'hidden'): VideoQualityLevel {
		switch (context) {
			case 'focused':
				return 'high';
			case 'carousel':
				return 'medium';
			case 'hidden':
				return 'low';
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

				if (state.context !== 'focused') {
					if (this.room) {
						const participant = this.room.remoteParticipants.get(participantIdentity);
						if (participant) {
							const pub = this.findScreenSharePublication(participant);
							if (pub) {
								try {
									pub.setEnabled(isIntersecting);
									runInAction(() => {
										state.enabled = isIntersecting;
									});
								} catch (error) {
									logger.error('[observer] Failed to set enabled', {participantIdentity, error});
								}
							}
						}
					}
				}

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
