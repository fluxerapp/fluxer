// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {PublicationSubscriptionManager} from '@app/features/voice/engine/PublicationSubscriptionManager';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import type {
	VoiceMediaGraphSubscriptionCommand,
	VoiceMediaGraphSubscriptionContext,
	VoiceMediaGraphVideoQuality,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	clearScreenShareViewerDemand,
	resolveScreenSharePublicationVideoRequest,
} from '@app/features/voice/utils/ScreenShareSubscriptionPolicy';
import type {RemoteParticipant, RemoteTrackPublication, Room} from 'livekit-client';
import {VideoQuality} from 'livekit-client';

const logger = new Logger('ScreenShareSubscriptionManager');
const SCREEN_SHARE_REPUBLISH_RESUBSCRIBE_DELAY_MS = 100;

const qualityMap: Record<VoiceMediaGraphVideoQuality, VideoQuality> = {
	low: VideoQuality.LOW,
	medium: VideoQuality.MEDIUM,
	high: VideoQuality.HIGH,
};

const graphQualityMap = new Map<VideoQuality, VoiceMediaGraphVideoQuality>(
	Object.entries(qualityMap).map(([graphQuality, videoQuality]): [VideoQuality, VoiceMediaGraphVideoQuality] => [
		videoQuality,
		graphQuality as VoiceMediaGraphVideoQuality,
	]),
);

export class ScreenShareSubscriptionManager extends PublicationSubscriptionManager {
	protected readonly source = VoiceTrackSource.ScreenShare;
	protected readonly logger = logger;
	protected readonly publicationCommandFailedMessage = 'Screen share publication command failed';

	private reattachPublicationTargets = new Map<string, RemoteTrackPublication>();
	private resubscribePulseTokens = new Map<string, number>();
	private resubscribePulseSequence = 0;
	private pendingResubscribePulses = new Set<string>();

	setRoom(room: Room | null): void {
		clearScreenShareViewerDemand();
		this.update(() => {
			this.room = room;
		});
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
			hasPublication: this.hasPublicationForIdentity(participantIdentity),
			context,
		});
	}

	isSubscribed(participantIdentity: string): boolean {
		return this.getSubscriptionEntry(participantIdentity)?.subscribed ?? false;
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
				hasPublication: publication != null || this.hasPublicationForIdentity(participantIdentity),
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

	protected hasPublicationForIdentity(participantIdentity: string): boolean {
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
	): VoiceMediaGraphVideoQuality | null {
		const request = resolveScreenSharePublicationVideoRequest(publication, qualityMap[quality]);
		const applied = this.runPublicationOperation(participantIdentity, publication, request.operation, () => {
			if (request.operation === 'setVideoDimensions') {
				publication.setVideoDimensions(request.dimensions);
				return;
			}
			publication.setVideoQuality(request.quality);
		});
		if (!applied) return null;
		return graphQualityMap.get(request.quality) ?? null;
	}

	protected applyCommand(command: VoiceMediaGraphSubscriptionCommand): void {
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
	): VoiceMediaGraphVideoQuality | null {
		const subscribedApplied = this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
			publication.setSubscribed(true);
		});
		const enabledApplied = this.runPublicationOperation(participantIdentity, publication, 'setEnabled', () => {
			publication.setEnabled(enabled);
		});
		const appliedQuality = this.applyQuality(participantIdentity, publication, quality);
		if (!subscribedApplied || !enabledApplied) return null;
		return appliedQuality;
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
			let appliedQuality: VoiceMediaGraphVideoQuality | null = null;
			let applied = true;
			for (const publication of publications) {
				const publicationQuality = this.applySubscribeOperations(participantIdentity, publication, enabled, quality);
				if (publicationQuality === null) applied = false;
				else appliedQuality = publicationQuality;
			}
			for (const publication of ScreenSharePublicationMigration.getScreenSharePublicationsToDisable(participant)) {
				this.runPublicationOperation(participantIdentity, publication, 'setSubscribed', () => {
					publication.setSubscribed(false);
				});
			}
			if (!applied) return;
			this.reportActualChanged(participantIdentity, {
				enabled,
				quality: appliedQuality,
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
		const appliedQuality = this.applySubscribeOperations(participantIdentity, publication, enabled, quality);
		if (appliedQuality === null) return;
		this.reportActualChanged(participantIdentity, {
			enabled,
			quality: appliedQuality,
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
			let appliedQuality: VoiceMediaGraphVideoQuality | null = null;
			let applied = true;
			for (const publication of publications) {
				const publicationQuality = this.applyQuality(participantIdentity, publication, quality);
				if (publicationQuality === null) applied = false;
				else appliedQuality = publicationQuality;
			}
			if (!applied) return;
			this.reportActualChanged(participantIdentity, {quality: appliedQuality});
			logger.debug('Quality updated', {participantIdentity, quality: appliedQuality});
		});
	}
}
