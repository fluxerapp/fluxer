// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {Store} from '@app/features/voice/engine/Store';
import {
	selectVoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphSubscriptionCommand,
	type VoiceMediaGraphSubscriptionContext,
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

const logger = new Logger('ScreenShareSubscriptionManager');
const SCREEN_SHARE_REPUBLISH_RESUBSCRIBE_DELAY_MS = 100;

const qualityMap: Record<VoiceMediaGraphVideoQuality, VideoQuality> = {
	low: VideoQuality.LOW,
	medium: VideoQuality.MEDIUM,
	high: VideoQuality.HIGH,
};

export class ScreenShareSubscriptionManager extends Store {
	private room: Room | null = null;
	private observers = new Map<string, IntersectionObserver>();
	private reattachPublicationTargets = new Map<string, RemoteTrackPublication>();
	private resubscribePulseTokens = new Map<string, number>();
	private resubscribePulseSequence = 0;
	private pendingResubscribePulses = new Set<string>();
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
		this.transition({type: 'subscription.cleanup', source: VoiceTrackSource.ScreenShare});
	}

	subscribeToParticipant(
		participantIdentity: string,
		element: HTMLElement | null,
		context: VoiceMediaGraphSubscriptionContext = 'carousel',
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
		logger.info('Subscribing to screen share', {participantIdentity, context});
		const screenSharePublication = this.findScreenSharePublication(participant);
		if (!screenSharePublication) {
			logger.debug('No screen share publication found', {participantIdentity});
		}
		this.transition({
			type: 'subscription.subscribe',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			hasPublication: screenSharePublication != null,
			observedElement: element,
			context,
		});
		logger.debug('Screen share subscribe intent recorded', {participantIdentity, context});
	}

	unsubscribeFromParticipant(participantIdentity: string): void {
		if (!this.getSubscriptionEntry(participantIdentity)) {
			logger.debug('Not subscribed', {participantIdentity});
			return;
		}
		logger.info('Unsubscribing from screen share', {participantIdentity});
		this.transition({type: 'subscription.unsubscribe', participantIdentity, source: VoiceTrackSource.ScreenShare});
		logger.info('Screen share unsubscribed successfully', {participantIdentity});
	}

	setContext(participantIdentity: string, context: VoiceMediaGraphSubscriptionContext): void {
		const state = this.getSubscriptionEntry(participantIdentity);
		if (!state) {
			logger.debug('Not subscribed', {participantIdentity});
			return;
		}
		if (state.context === context) {
			return;
		}
		logger.debug('Setting screen share context', {participantIdentity, context});
		this.transition({
			type: 'subscription.setContext',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			hasPublication: this.hasScreenSharePublicationForIdentity(participantIdentity),
			context,
		});
	}

	isSubscribed(participantIdentity: string): boolean {
		return this.getSubscriptionEntry(participantIdentity)?.subscribed ?? false;
	}

	getContext(participantIdentity: string): VoiceMediaGraphSubscriptionContext | null {
		return this.getSubscriptionEntry(participantIdentity)?.context ?? null;
	}

	reattachAfterPublish(participantIdentity: string, publication?: RemoteTrackPublication): void {
		const state = this.getSubscriptionEntry(participantIdentity);
		if (!state?.subscribed || !this.room) return;
		logger.info('Reattaching screen share subscription after republish', {
			participantIdentity,
			context: state.context,
			enabled: state.enabled,
			trackSid: publication?.trackSid ?? null,
		});
		if (publication) {
			this.reattachPublicationTargets.set(participantIdentity, publication);
		}
		try {
			this.transition({
				type: 'subscription.reattachAfterPublish',
				participantIdentity,
				source: VoiceTrackSource.ScreenShare,
				hasPublication: publication != null || this.hasScreenSharePublicationForIdentity(participantIdentity),
				forceResubscribe: true,
			});
		} finally {
			this.reattachPublicationTargets.delete(participantIdentity);
		}
	}

	private findScreenSharePublication(
		participant: RemoteParticipant | null | undefined,
	): RemoteTrackPublication | undefined {
		if (!participant) return undefined;
		const selected = ScreenSharePublicationMigration.selectScreenSharePublication(participant);
		if (selected) return selected;
		for (const pub of participant.videoTrackPublications.values()) {
			if (asVoiceTrackSource(pub.source) === VoiceTrackSource.ScreenShare) {
				return pub;
			}
		}
		return undefined;
	}

	private findScreenSharePublicationForIdentity(participantIdentity: string): RemoteTrackPublication | undefined {
		return this.findScreenSharePublication(this.getParticipant(participantIdentity));
	}

	private hasScreenSharePublicationForIdentity(participantIdentity: string): boolean {
		return this.findScreenSharePublicationForIdentity(participantIdentity) != null;
	}

	private getParticipant(participantIdentity: string): RemoteParticipant | null {
		return this.room?.remoteParticipants.get(participantIdentity) ?? null;
	}

	private getActiveScreenSharePublications(participant: RemoteParticipant | null): Array<RemoteTrackPublication> {
		const managedPublications = ScreenSharePublicationMigration.getManagedScreenSharePublications(participant);
		if (managedPublications.length > 0) return managedPublications;
		const fallbackPublication = this.findScreenSharePublication(participant);
		return fallbackPublication ? [fallbackPublication] : [];
	}

	private getAllScreenSharePublications(participant: RemoteParticipant | null): Array<RemoteTrackPublication> {
		if (!participant) return [];
		return Array.from(participant.videoTrackPublications.values()).filter(
			(publication): publication is RemoteTrackPublication =>
				asVoiceTrackSource(publication.source) === VoiceTrackSource.ScreenShare,
		);
	}

	private withActiveScreenSharePublications(
		participantIdentity: string,
		apply: (publications: Array<RemoteTrackPublication>, participant: RemoteParticipant | null) => void,
	): void {
		const participant = this.getParticipant(participantIdentity);
		const publications = this.getActiveScreenSharePublications(participant);
		if (publications.length === 0) {
			this.reportPublicationLost(participantIdentity);
			this.transition({
				type: 'subscription.publicationMissing',
				participantIdentity,
				source: VoiceTrackSource.ScreenShare,
			});
			return;
		}
		this.reportPublicationObserved(participantIdentity, publications[0]?.trackSid ?? null);
		apply(publications, participant);
	}

	private getTargetScreenSharePublications(
		participantIdentity: string,
		participant: RemoteParticipant | null,
	): Array<RemoteTrackPublication> {
		const target = this.reattachPublicationTargets.get(participantIdentity);
		if (target) return [target];
		return this.getActiveScreenSharePublications(participant);
	}

	private isSubscriptionStillWanted(participantIdentity: string): boolean {
		return this.getSubscriptionEntry(participantIdentity)?.subscribed ?? false;
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
			logger.error('Screen share publication command failed', {
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
			source: VoiceTrackSource.ScreenShare,
			at: voiceMediaGraphStore.nowMs(),
			...changes,
		});
	}

	private reportCommandFailed(participantIdentity: string, code: number, reason: string): void {
		voiceMediaGraphStore.transition({
			type: 'subscription.commandFailed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: voiceMediaGraphStore.nowMs(),
			code,
			reason,
		});
	}

	private reportPublicationObserved(participantIdentity: string, trackSid: string | null): void {
		voiceMediaGraphStore.transition({
			type: 'publication.observed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			trackSid,
			at: voiceMediaGraphStore.nowMs(),
		});
	}

	private reportPublicationLost(participantIdentity: string): void {
		voiceMediaGraphStore.transition({
			type: 'publication.lost',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
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
					source: VoiceTrackSource.ScreenShare,
					hasPublication: this.hasScreenSharePublicationForIdentity(participantIdentity),
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
			VoiceTrackSource.ScreenShare,
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
		if (command.source !== VoiceTrackSource.ScreenShare) return;
		switch (command.type) {
			case 'subscribePublication':
				this.subscribePublication(command.participantIdentity, command.enabled, command.quality);
				break;
			case 'resubscribePublication':
				this.resubscribePublication(command.participantIdentity, command.enabled, command.quality);
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

	private applySubscribeOperations(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
	): boolean {
		const subscribedApplied = this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
			publication.setSubscribed(true);
		});
		const enabledApplied = this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
			publication.setEnabled(enabled);
		});
		const qualityApplied = this.applyQuality(participantIdentity, publication, quality);
		return subscribedApplied && enabledApplied && qualityApplied;
	}

	private subscribePublication(
		participantIdentity: string,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
	): void {
		if (this.pendingResubscribePulses.has(participantIdentity)) {
			logger.debug('Skipping subscribe while resubscribe pulse is pending', {participantIdentity});
			return;
		}
		this.withActiveScreenSharePublications(participantIdentity, (publications, participant) => {
			let applied = true;
			for (const publication of publications) {
				applied = this.applySubscribeOperations(participantIdentity, publication, enabled, quality) && applied;
			}
			for (const publication of ScreenSharePublicationMigration.getScreenSharePublicationsToDisable(participant)) {
				this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
					publication.setSubscribed(false);
				});
			}
			if (!applied) return;
			this.reportActualChanged(participantIdentity, {
				enabled,
				quality,
				trackSid: publications[0]?.trackSid ?? null,
			});
			logger.debug('Screen share subscribe command applied', {participantIdentity});
		});
	}

	private resubscribePublication(
		participantIdentity: string,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
	): void {
		const publications = this.getTargetScreenSharePublications(
			participantIdentity,
			this.getParticipant(participantIdentity),
		);
		if (publications.length === 0) {
			this.reportPublicationLost(participantIdentity);
			this.transition({
				type: 'subscription.publicationMissing',
				participantIdentity,
				source: VoiceTrackSource.ScreenShare,
			});
			return;
		}
		this.reportPublicationObserved(participantIdentity, publications[0]?.trackSid ?? null);
		const pulseToken = this.beginResubscribePulse(participantIdentity);
		for (const publication of publications) {
			this.startResubscribePulse(participantIdentity, publication, enabled, quality, pulseToken);
		}
	}

	private beginResubscribePulse(participantIdentity: string): number {
		this.resubscribePulseSequence += 1;
		this.resubscribePulseTokens.set(participantIdentity, this.resubscribePulseSequence);
		this.pendingResubscribePulses.add(participantIdentity);
		return this.resubscribePulseSequence;
	}

	private startResubscribePulse(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
		pulseToken: number,
	): void {
		const enabledApplied = this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
			publication.setEnabled(false);
		});
		const subscribedApplied = this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
			publication.setSubscribed(false);
		});
		if (enabledApplied && subscribedApplied) {
			this.reportActualChanged(participantIdentity, {subscribed: false, enabled: false});
		}
		globalThis.setTimeout(() => {
			this.completeResubscribePulse(participantIdentity, publication, enabled, quality, pulseToken);
		}, SCREEN_SHARE_REPUBLISH_RESUBSCRIBE_DELAY_MS);
		logger.debug('Screen share resubscribe pulse started after republish', {
			participantIdentity,
			trackSid: publication.trackSid,
		});
	}

	private completeResubscribePulse(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		enabled: boolean,
		quality: VoiceMediaGraphVideoQuality,
		pulseToken: number,
	): void {
		if (this.resubscribePulseTokens.get(participantIdentity) !== pulseToken) {
			logger.debug('Skipping stale screen share resubscribe pulse', {participantIdentity});
			return;
		}
		this.pendingResubscribePulses.delete(participantIdentity);
		if (!this.isSubscriptionStillWanted(participantIdentity)) return;
		if (!this.applySubscribeOperations(participantIdentity, publication, enabled, quality)) return;
		this.reportActualChanged(participantIdentity, {
			enabled,
			quality,
			trackSid: publication.trackSid ?? null,
		});
		logger.debug('Screen share resubscribe pulse command applied after republish', {
			participantIdentity,
			trackSid: publication.trackSid,
		});
	}

	private unsubscribePublication(participantIdentity: string): void {
		this.resubscribePulseTokens.delete(participantIdentity);
		this.pendingResubscribePulses.delete(participantIdentity);
		const publications = this.getAllScreenSharePublications(this.getParticipant(participantIdentity));
		let applied = publications.length > 0;
		for (const publication of publications) {
			applied =
				this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
					publication.setSubscribed(false);
				}) && applied;
		}
		if (!applied) return;
		this.reportActualChanged(participantIdentity, {subscribed: false});
		logger.debug('Screen share publications unsubscribed', {participantIdentity});
	}

	private setPublicationEnabled(participantIdentity: string, enabled: boolean): void {
		this.withActiveScreenSharePublications(participantIdentity, (publications) => {
			let applied = true;
			for (const publication of publications) {
				applied =
					this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
						publication.setEnabled(enabled);
					}) && applied;
			}
			if (!applied) return;
			this.reportActualChanged(participantIdentity, {enabled});
			logger.debug('Track enabled state updated', {participantIdentity, enabled});
		});
	}

	private setPublicationQuality(participantIdentity: string, quality: VoiceMediaGraphVideoQuality): void {
		this.withActiveScreenSharePublications(participantIdentity, (publications) => {
			let applied = true;
			for (const publication of publications) {
				applied = this.applyQuality(participantIdentity, publication, quality) && applied;
			}
			if (!applied) return;
			this.reportActualChanged(participantIdentity, {quality});
			logger.debug('Quality updated', {participantIdentity, quality});
		});
	}
}
