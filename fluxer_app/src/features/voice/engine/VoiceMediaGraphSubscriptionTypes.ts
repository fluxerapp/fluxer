// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';

export type VoiceMediaGraphVideoQuality = 'low' | 'medium' | 'high';
export type VoiceMediaGraphSubscriptionContext = 'focused' | 'carousel' | 'hidden';
export type VoiceMediaGraphSubscriptionObservedElement = object | null;

export interface VoiceMediaGraphSubscriptionTarget {
	participantIdentity: string;
	source: VoiceTrackSource;
}

export interface VoiceMediaGraphSubscriptionDesiredState {
	enabled: boolean;
	quality: VoiceMediaGraphVideoQuality;
	context: VoiceMediaGraphSubscriptionContext;
	isIntersecting: boolean;
	observedElement: VoiceMediaGraphSubscriptionObservedElement;
}

interface VoiceMediaGraphSubscriptionActualError {
	code: number;
	reason: string;
	at: number;
}

export interface VoiceMediaGraphSubscriptionActualState {
	subscribed: boolean | null;
	enabled: boolean | null;
	quality: VoiceMediaGraphVideoQuality | null;
	lastCommandAt: number | null;
	lastError: VoiceMediaGraphSubscriptionActualError | null;
}

export interface VoiceMediaGraphSubscriptionPublicationState {
	available: boolean;
	trackSid: string | null;
	observedAt: number | null;
}

export interface VoiceMediaGraphSubscriptionFirstFrameState {
	renderedAt: number | null;
}

export interface VoiceMediaGraphSubscriptionEntry extends VoiceMediaGraphSubscriptionTarget {
	desired: VoiceMediaGraphSubscriptionDesiredState;
	actual: VoiceMediaGraphSubscriptionActualState;
	publication: VoiceMediaGraphSubscriptionPublicationState;
	firstFrame: VoiceMediaGraphSubscriptionFirstFrameState;
	subscribed: boolean;
	publicationAvailable: boolean;
	enabled: boolean;
	quality: VoiceMediaGraphVideoQuality;
	context: VoiceMediaGraphSubscriptionContext;
	isIntersecting: boolean;
	observedElement: VoiceMediaGraphSubscriptionObservedElement;
}

export type VoiceMediaGraphSubscriptionCommand =
	| {
			type: 'subscribePublication';
			participantIdentity: string;
			source: VoiceTrackSource;
			enabled: boolean;
			quality: VoiceMediaGraphVideoQuality;
	  }
	| {
			type: 'resubscribePublication';
			participantIdentity: string;
			source: VoiceTrackSource;
			enabled: boolean;
			quality: VoiceMediaGraphVideoQuality;
	  }
	| {type: 'unsubscribePublication'; participantIdentity: string; source: VoiceTrackSource}
	| {type: 'setPublicationEnabled'; participantIdentity: string; source: VoiceTrackSource; enabled: boolean}
	| {
			type: 'setPublicationQuality';
			participantIdentity: string;
			source: VoiceTrackSource;
			quality: VoiceMediaGraphVideoQuality;
	  }
	| {
			type: 'observeElement';
			participantIdentity: string;
			source: VoiceTrackSource;
			element: NonNullable<VoiceMediaGraphSubscriptionObservedElement>;
	  }
	| {type: 'disconnectObserver'; participantIdentity: string; source: VoiceTrackSource};

export interface VoiceMediaGraphSubscriptionSubscribeEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.subscribe';
	hasPublication: boolean;
	observedElement: VoiceMediaGraphSubscriptionObservedElement;
	quality?: VoiceMediaGraphVideoQuality;
	context?: VoiceMediaGraphSubscriptionContext;
}

interface VoiceMediaGraphSubscriptionUnsubscribeEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.unsubscribe';
}

export interface VoiceMediaGraphSubscriptionReplaceObserverEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.replaceObserver';
	hasPublication: boolean;
	observedElement: VoiceMediaGraphSubscriptionObservedElement;
}

export interface VoiceMediaGraphSubscriptionIntersectionEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.intersection';
	hasPublication: boolean;
	isIntersecting: boolean;
}

export interface VoiceMediaGraphSubscriptionSetEnabledEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.setEnabled';
	hasPublication: boolean;
	enabled: boolean;
}

export interface VoiceMediaGraphSubscriptionSetQualityEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.setQuality';
	hasPublication: boolean;
	quality: VoiceMediaGraphVideoQuality;
}

export interface VoiceMediaGraphSubscriptionSetContextEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.setContext';
	hasPublication: boolean;
	context: VoiceMediaGraphSubscriptionContext;
}

export interface VoiceMediaGraphSubscriptionReattachAfterPublishEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.reattachAfterPublish';
	hasPublication: boolean;
	forceResubscribe?: boolean;
}

interface VoiceMediaGraphSubscriptionPublicationMissingEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.publicationMissing';
}

interface VoiceMediaGraphSubscriptionCleanupEvent {
	type: 'subscription.cleanup';
	source?: VoiceTrackSource;
}

interface VoiceMediaGraphSubscriptionClearCommandsEvent {
	type: 'subscription.clearCommands';
}

export interface VoiceMediaGraphSubscriptionActualChangedEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.actualChanged';
	at: number;
	subscribed?: boolean | null;
	enabled?: boolean | null;
	quality?: VoiceMediaGraphVideoQuality | null;
	trackSid?: string | null;
	streamKey?: string;
	generation?: number;
}

export interface VoiceMediaGraphSubscriptionCommandFailedEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'subscription.commandFailed';
	at: number;
	code: number;
	reason: string;
	streamKey?: string;
	generation?: number;
}

interface VoiceMediaGraphSubscriptionReconcileEvent {
	type: 'subscription.reconcile';
}

export interface VoiceMediaGraphPublicationObservedEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'publication.observed';
	trackSid: string | null;
	at: number;
}

export interface VoiceMediaGraphPublicationLostEvent extends VoiceMediaGraphSubscriptionTarget {
	type: 'publication.lost';
	at: number;
}

export type VoiceMediaGraphSubscriptionEvent =
	| VoiceMediaGraphSubscriptionSubscribeEvent
	| VoiceMediaGraphSubscriptionUnsubscribeEvent
	| VoiceMediaGraphSubscriptionReplaceObserverEvent
	| VoiceMediaGraphSubscriptionIntersectionEvent
	| VoiceMediaGraphSubscriptionSetEnabledEvent
	| VoiceMediaGraphSubscriptionSetQualityEvent
	| VoiceMediaGraphSubscriptionSetContextEvent
	| VoiceMediaGraphSubscriptionReattachAfterPublishEvent
	| VoiceMediaGraphSubscriptionPublicationMissingEvent
	| VoiceMediaGraphSubscriptionCleanupEvent
	| VoiceMediaGraphSubscriptionClearCommandsEvent
	| VoiceMediaGraphSubscriptionActualChangedEvent
	| VoiceMediaGraphSubscriptionCommandFailedEvent
	| VoiceMediaGraphSubscriptionReconcileEvent;
