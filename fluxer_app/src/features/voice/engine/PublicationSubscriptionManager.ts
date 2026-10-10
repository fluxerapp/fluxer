// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Logger} from '@app/features/platform/utils/AppLogger';
import {Store} from '@app/features/voice/engine/Store';
import {
	selectVoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphSubscriptionCommand,
	type VoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphSubscriptionEvent,
	type VoiceMediaGraphVideoQuality,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import type {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	getScreenShareWatchFailureForPublicationOperation,
	type ScreenSharePublicationOperation,
	ScreenShareWatchErrorCode,
} from '@app/features/voice/state/ScreenShareWatchFailures';
import type {RemoteTrackPublication, Room} from 'livekit-client';

export abstract class PublicationSubscriptionManager extends Store {
	protected room: Room | null = null;
	private observers = new Map<string, IntersectionObserver>();
	private readonly intersectionOptions: IntersectionObserverInit = {
		root: null,
		rootMargin: '50px',
		threshold: [0, 0.1],
	};
	protected abstract readonly source: VoiceTrackSource;
	protected abstract readonly logger: Logger;
	protected abstract readonly publicationCommandFailedMessage: string;

	protected abstract hasPublicationForIdentity(participantIdentity: string): boolean;

	protected abstract applyCommand(command: VoiceMediaGraphSubscriptionCommand): void;

	cleanup(): void {
		this.transition({type: 'subscription.cleanup', source: this.source});
	}

	protected runPublicationOperation(
		participantIdentity: string,
		publication: RemoteTrackPublication,
		operation: ScreenSharePublicationOperation,
		apply: () => void,
	): boolean {
		try {
			apply();
			return true;
		} catch (error) {
			this.logger.error(this.publicationCommandFailedMessage, {
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

	protected reportActualChanged(
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
			source: this.source,
			at: voiceMediaGraphStore.nowMs(),
			...changes,
		});
	}

	protected reportCommandFailed(participantIdentity: string, code: number, reason: string): void {
		voiceMediaGraphStore.transition({
			type: 'subscription.commandFailed',
			participantIdentity,
			source: this.source,
			at: voiceMediaGraphStore.nowMs(),
			code,
			reason,
		});
	}

	protected reportPublicationObserved(participantIdentity: string, trackSid: string | null): void {
		voiceMediaGraphStore.transition({
			type: 'publication.observed',
			participantIdentity,
			source: this.source,
			trackSid,
			at: voiceMediaGraphStore.nowMs(),
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
					source: this.source,
					hasPublication: this.hasPublicationForIdentity(participantIdentity),
					isIntersecting,
				});
				this.logger.debug('Intersection changed', {participantIdentity, isIntersecting});
			}
		}, this.intersectionOptions);
		observer.observe(element);
		return observer;
	}

	protected attachObserver(participantIdentity: string, element: HTMLElement): void {
		try {
			this.observers.set(participantIdentity, this.createObserver(participantIdentity, element));
		} catch (error) {
			this.logger.error('Failed to attach intersection observer', {participantIdentity, error});
			this.reportCommandFailed(
				participantIdentity,
				ScreenShareWatchErrorCode.ObserverAttachFailed,
				'observer-attach-failed',
			);
		}
	}

	protected detachObserver(participantIdentity: string): void {
		const observer = this.observers.get(participantIdentity);
		this.observers.delete(participantIdentity);
		if (!observer) return;
		try {
			observer.disconnect();
		} catch (error) {
			this.logger.error('Failed to detach intersection observer', {participantIdentity, error});
			this.reportCommandFailed(
				participantIdentity,
				ScreenShareWatchErrorCode.ObserverDetachFailed,
				'observer-detach-failed',
			);
		}
	}

	protected getSubscriptionEntry(participantIdentity: string): VoiceMediaGraphSubscriptionEntry | null {
		return selectVoiceMediaGraphSubscriptionEntry(
			voiceMediaGraphStore.getGraphSnapshot(),
			participantIdentity,
			this.source,
		);
	}

	applyReconciledCommand(command: VoiceMediaGraphSubscriptionCommand): void {
		this.applyCommand(command);
	}

	protected transition(event: VoiceMediaGraphSubscriptionEvent): void {
		let commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
		this.update(() => {
			commands = voiceMediaGraphStore.takeSubscriptionCommands(event);
		});
		for (const command of commands) {
			this.applyCommand(command);
		}
	}
}
