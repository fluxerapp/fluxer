// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {Store} from '@app/features/voice/engine/Store';
import {
	selectVoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphSubscriptionCommand,
	type VoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphSubscriptionEvent,
	type VoiceMediaGraphVideoQuality,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	getScreenShareWatchFailureForPublicationOperation,
	type ScreenSharePublicationOperation,
	ScreenShareWatchErrorCode,
} from '@app/features/voice/state/ScreenShareWatchFailures';
import type {RemoteParticipant, RemoteTrackPublication, Room} from 'livekit-client';
import {VideoQuality} from 'livekit-client';

const logger = new Logger('VideoSubscriptionManager');

const qualityMap: Record<VoiceMediaGraphVideoQuality, VideoQuality> = {
	low: VideoQuality.LOW,
	medium: VideoQuality.MEDIUM,
	high: VideoQuality.HIGH,
};

export class VideoSubscriptionManager extends Store {
	private room: Room | null = null;
	private observers = new Map<string, IntersectionObserver>();
	private readonly intersectionOptions: IntersectionObserverInit = {
		root: null,
		rootMargin: '50px',
		threshold: [0, 0.1],
	};

	setRoom(room: Room | null): void {
		this.update(() => {
			this.room = room;
		});
	}

	cleanup(): void {
		this.transition({type: 'subscription.cleanup', source: VoiceTrackSource.Camera});
	}

	subscribeToParticipant(
		participantIdentity: string,
		element: HTMLElement | null,
		initialQuality: VoiceMediaGraphVideoQuality = 'low',
	): void {
		if (!this.room) {
			logger.warn('No room available');
			return;
		}
		const participant = this.room.remoteParticipants.get(participantIdentity) ?? null;
		if (!participant) {
			logger.warn('Participant not found', {participantIdentity});
			return;
		}
		logger.info('Subscribing to video', {participantIdentity, quality: initialQuality});
		const cameraPublication = this.findCameraPublication(participant);
		if (!cameraPublication) {
			logger.debug('No camera publication found', {participantIdentity});
		}
		const existingState = this.getSubscriptionEntry(participantIdentity);
		this.transition({
			type: 'subscription.subscribe',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			hasPublication: cameraPublication != null,
			observedElement: element,
			quality: existingState ? undefined : initialQuality,
		});
		logger.debug('Video subscribe intent recorded', {participantIdentity});
	}

	unsubscribeFromParticipant(participantIdentity: string): void {
		if (!this.getSubscriptionEntry(participantIdentity)) {
			logger.debug('Not subscribed', {participantIdentity});
			return;
		}
		logger.info('Unsubscribing from video', {participantIdentity});
		this.transition({type: 'subscription.unsubscribe', participantIdentity, source: VoiceTrackSource.Camera});
		logger.info('Video unsubscribed successfully', {participantIdentity});
	}

	setEnabled(participantIdentity: string, enabled: boolean): void {
		const state = this.getSubscriptionEntry(participantIdentity);
		if (!state) {
			logger.debug('Not subscribed', {participantIdentity});
			return;
		}
		if (state.enabled === enabled) {
			return;
		}
		logger.debug('Setting video enabled state', {participantIdentity, enabled});
		this.transition({
			type: 'subscription.setEnabled',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			hasPublication: this.hasCameraPublicationForIdentity(participantIdentity),
			enabled,
		});
	}

	setQuality(participantIdentity: string, quality: VoiceMediaGraphVideoQuality): void {
		const state = this.getSubscriptionEntry(participantIdentity);
		if (!state) {
			logger.debug('Not subscribed', {participantIdentity});
			return;
		}
		if (state.quality === quality) {
			return;
		}
		logger.debug('Setting video quality', {participantIdentity, quality});
		this.transition({
			type: 'subscription.setQuality',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			hasPublication: this.hasCameraPublicationForIdentity(participantIdentity),
			quality,
		});
	}

	isSubscribed(participantIdentity: string): boolean {
		return this.getSubscriptionEntry(participantIdentity)?.subscribed ?? false;
	}

	getQuality(participantIdentity: string): VoiceMediaGraphVideoQuality | null {
		return this.getSubscriptionEntry(participantIdentity)?.quality ?? null;
	}

	reattachAfterPublish(participantIdentity: string): void {
		const state = this.getSubscriptionEntry(participantIdentity);
		if (!state?.subscribed || !this.room) return;
		this.transition({
			type: 'subscription.reattachAfterPublish',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			hasPublication: this.hasCameraPublicationForIdentity(participantIdentity),
		});
	}

	private findCameraPublication(participant: RemoteParticipant | null | undefined): RemoteTrackPublication | undefined {
		if (!participant) return undefined;
		for (const pub of participant.videoTrackPublications.values()) {
			if (asVoiceTrackSource(pub.source) === VoiceTrackSource.Camera) {
				return pub;
			}
		}
		return undefined;
	}

	private findCameraPublicationForIdentity(participantIdentity: string): RemoteTrackPublication | undefined {
		return this.findCameraPublication(this.room?.remoteParticipants.get(participantIdentity) ?? null);
	}

	private hasCameraPublicationForIdentity(participantIdentity: string): boolean {
		return this.findCameraPublicationForIdentity(participantIdentity) != null;
	}

	private runPublicationOperation(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		operation: ScreenSharePublicationOperation,
		apply: () => void,
	): boolean {
		try {
			apply();
			return true;
		} catch (error) {
			logger.error('Video publication command failed', {
				participantIdentity,
				trackSid: publication.trackSid,
				operation,
				error,
			});
			const failure = getScreenShareWatchFailureForPublicationOperation(operation);
			this.reportCommandFailed(participantIdentity, failure.code, failure.reason);
			return false;
		}
	}

	private reportActualChanged(
		participantIdentity: string,
		changes: {
			subscribed?: boolean | null;
			enabled?: boolean | null;
			quality?: VoiceMediaGraphVideoQuality | null;
			trackSid?: string | null;
		},
	): void {
		voiceMediaGraphStore.transition({
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			at: voiceMediaGraphStore.nowMs(),
			...changes,
		});
	}

	private reportCommandFailed(participantIdentity: string, code: number, reason: string): void {
		voiceMediaGraphStore.transition({
			type: 'subscription.commandFailed',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			at: voiceMediaGraphStore.nowMs(),
			code,
			reason,
		});
	}

	private reportPublicationObserved(participantIdentity: string, trackSid: string | null): void {
		voiceMediaGraphStore.transition({
			type: 'publication.observed',
			participantIdentity,
			source: VoiceTrackSource.Camera,
			trackSid,
			at: voiceMediaGraphStore.nowMs(),
		});
	}

	private applyQuality(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		quality: VoiceMediaGraphVideoQuality,
	): boolean {
		return this.runPublicationOperation(participantIdentity, publication, 'setVideoQuality', () => {
			publication.setVideoQuality(qualityMap[quality]);
		});
	}

	private createObserver(participantIdentity: string, element: HTMLElement): IntersectionObserver {
		const observer = new IntersectionObserver((entries) => {
			for (const entry of entries) {
				const isIntersecting = entry.isIntersecting;
				if (!this.getSubscriptionEntry(participantIdentity)) continue;
				this.transition({
					type: 'subscription.intersection',
					participantIdentity,
					source: VoiceTrackSource.Camera,
					hasPublication: this.hasCameraPublicationForIdentity(participantIdentity),
					isIntersecting,
				});
				logger.debug('Intersection changed', {participantIdentity, isIntersecting});
			}
		}, this.intersectionOptions);
		observer.observe(element);
		return observer;
	}

	private attachObserver(participantIdentity: string, element: HTMLElement): void {
		try {
			this.observers.set(participantIdentity, this.createObserver(participantIdentity, element));
		} catch (error) {
			logger.error('Failed to attach intersection observer', {participantIdentity, error});
			this.reportCommandFailed(
				participantIdentity,
				ScreenShareWatchErrorCode.ObserverAttachFailed,
				'observer-attach-failed',
			);
		}
	}

	private detachObserver(participantIdentity: string): void {
		const observer = this.observers.get(participantIdentity);
		this.observers.delete(participantIdentity);
		if (!observer) return;
		try {
			observer.disconnect();
		} catch (error) {
			logger.error('Failed to detach intersection observer', {participantIdentity, error});
			this.reportCommandFailed(
				participantIdentity,
				ScreenShareWatchErrorCode.ObserverDetachFailed,
				'observer-detach-failed',
			);
		}
	}

	private getSubscriptionEntry(participantIdentity: string): VoiceMediaGraphSubscriptionEntry | null {
		return selectVoiceMediaGraphSubscriptionEntry(
			voiceMediaGraphStore.getGraphSnapshot(),
			participantIdentity,
			VoiceTrackSource.Camera,
		);
	}

	applyReconciledCommand(command: VoiceMediaGraphSubscriptionCommand): void {
		this.applyCommand(command);
	}

	private transition(event: VoiceMediaGraphSubscriptionEvent): void {
		let commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
		this.update(() => {
			commands = voiceMediaGraphStore.takeSubscriptionCommands(event);
		});
		for (const command of commands) {
			this.applyCommand(command);
		}
	}

	private applyCommand(command: VoiceMediaGraphSubscriptionCommand): void {
		if (command.source !== VoiceTrackSource.Camera) return;
		switch (command.type) {
			case 'subscribePublication':
			case 'resubscribePublication':
				this.subscribePublication(command.participantIdentity, command.enabled, command.quality);
				break;
			case 'unsubscribePublication':
				this.unsubscribePublication(command.participantIdentity);
				break;
			case 'setPublicationEnabled':
				this.setPublicationEnabled(command.participantIdentity, command.enabled);
				break;
			case 'setPublicationQuality':
				this.setPublicationQuality(command.participantIdentity, command.quality);
				break;
			case 'observeElement':
				this.attachObserver(command.participantIdentity, command.element as HTMLElement);
				break;
			case 'disconnectObserver':
				this.detachObserver(command.participantIdentity);
				break;
		}
	}

	private subscribePublication(
		participantIdentity: string,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
	): void {
		const publication = this.findCameraPublicationForIdentity(participantIdentity);
		if (!publication) {
			this.transition({type: 'subscription.publicationMissing', participantIdentity, source: VoiceTrackSource.Camera});
			return;
		}
		this.reportPublicationObserved(participantIdentity, publication.trackSid ?? null);
		const subscribedApplied = this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
			publication.setSubscribed(true);
		});
		const enabledApplied = this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
			publication.setEnabled(enabled);
		});
		const qualityApplied = this.applyQuality(participantIdentity, publication, quality);
		if (!subscribedApplied || !enabledApplied || !qualityApplied) return;
		this.reportActualChanged(participantIdentity, {
			subscribed: true,
			enabled,
			quality,
			trackSid: publication.trackSid ?? null,
		});
		logger.debug('Video subscribed successfully', {participantIdentity});
	}

	private unsubscribePublication(participantIdentity: string): void {
		const publication = this.findCameraPublicationForIdentity(participantIdentity);
		if (!publication) return;
		const applied = this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
			publication.setSubscribed(false);
		});
		if (!applied) return;
		this.reportActualChanged(participantIdentity, {subscribed: false});
		logger.debug('Track unsubscribed', {participantIdentity});
	}

	private setPublicationEnabled(participantIdentity: string, enabled: boolean): void {
		const publication = this.findCameraPublicationForIdentity(participantIdentity);
		if (!publication) {
			this.transition({type: 'subscription.publicationMissing', participantIdentity, source: VoiceTrackSource.Camera});
			return;
		}
		const applied = this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
			publication.setEnabled(enabled);
		});
		if (!applied) return;
		this.reportActualChanged(participantIdentity, {enabled});
		logger.debug('Track enabled state updated', {participantIdentity, enabled});
	}

	private setPublicationQuality(participantIdentity: string, quality: VoiceMediaGraphVideoQuality): void {
		const publication = this.findCameraPublicationForIdentity(participantIdentity);
		if (!publication) {
			this.transition({type: 'subscription.publicationMissing', participantIdentity, source: VoiceTrackSource.Camera});
			return;
		}
		if (!this.applyQuality(participantIdentity, publication, quality)) return;
		this.reportActualChanged(participantIdentity, {quality});
		logger.debug('Quality updated', {participantIdentity, quality});
	}
}
