// SPDX-License-Identifier: AGPL-3.0-or-later

import {VideoQuality} from 'livekit-client';

export interface ScreenShareViewerDemandDimensions {
	width: number;
	height: number;
}

export interface ScreenSharePublicationTarget {
	trackSid?: string | null;
	isEnabled?: boolean;
	isDesired?: boolean;
	isSubscribed?: boolean;
	videoQuality?: VideoQuality;
	setEnabled?: (enabled: boolean) => void;
	setSubscribed?: (subscribed: boolean) => void;
	setVideoQuality?: (quality: VideoQuality) => void;
	setVideoDimensions?: (dimensions: ScreenShareViewerDemandDimensions) => void;
	emitTrackUpdate?: () => void;
}

export type ScreenSharePublicationOperation =
	| 'setEnabled'
	| 'setSubscribed'
	| 'setVideoQuality'
	| 'setVideoDimensions'
	| 'emitTrackUpdate';
export type ScreenSharePublicationErrorHandler = (
	operation: ScreenSharePublicationOperation,
	label: string,
	error: unknown,
) => void;

export interface SyncScreenSharePublicationOptions {
	publication: ScreenSharePublicationTarget | null | undefined;
	label: string;
	shouldSubscribe: boolean;
	shouldEnable?: boolean;
	onError?: ScreenSharePublicationErrorHandler;
}

export interface SyncWatchedScreenSharePublicationsOptions {
	isScreenShare: boolean;
	isOwnScreenShare: boolean;
	userWantsToWatch: boolean;
	videoLocallyDisabled: boolean;
	audioEnabled: boolean;
	videoPublication?: ScreenSharePublicationTarget | null;
	audioPublication?: ScreenSharePublicationTarget | null;
	onError?: ScreenSharePublicationErrorHandler;
}

const SCREEN_SHARE_VIEWER_DEMAND_DEAD_BAND_RATIO = 0.05;
export const SCREEN_SHARE_VIEWER_DEMAND_DEBOUNCE_MS = 500;
const SCREEN_SHARE_VIEWER_DEMAND_MAX_QUALITY = VideoQuality.HIGH;

const viewerDemandByTrackSid = new Map<string, Map<string, ScreenShareViewerDemandDimensions>>();

function resolveScreenShareViewerDemandPixelDensity(devicePixelRatio: number): number {
	if (!Number.isFinite(devicePixelRatio) || devicePixelRatio <= 0) return 1;
	return devicePixelRatio > 2 ? 2 : 1;
}

export function measureScreenShareViewerDemand(
	element: {clientWidth: number; clientHeight: number} | null | undefined,
	devicePixelRatio: number,
): ScreenShareViewerDemandDimensions | null {
	if (!element) return null;
	const density = resolveScreenShareViewerDemandPixelDensity(devicePixelRatio);
	const width = Math.ceil(element.clientWidth * density);
	const height = Math.ceil(element.clientHeight * density);
	if (width <= 0 || height <= 0) return null;
	return {width, height};
}

export function isWithinScreenShareViewerDemandDeadBand(
	applied: ScreenShareViewerDemandDimensions | null,
	next: ScreenShareViewerDemandDimensions,
	ratio: number = SCREEN_SHARE_VIEWER_DEMAND_DEAD_BAND_RATIO,
): boolean {
	if (!applied) return false;
	return (
		Math.abs(next.width - applied.width) <= applied.width * ratio &&
		Math.abs(next.height - applied.height) <= applied.height * ratio
	);
}

function allowsScreenShareViewerDemand(videoQuality: VideoQuality | undefined): boolean {
	return (videoQuality ?? SCREEN_SHARE_VIEWER_DEMAND_MAX_QUALITY) === SCREEN_SHARE_VIEWER_DEMAND_MAX_QUALITY;
}

function hasScreenShareViewerDemandTrack(publication: ScreenSharePublicationTarget): boolean {
	return publication.isSubscribed ?? true;
}

export function clearScreenShareViewerDemand(): void {
	viewerDemandByTrackSid.clear();
}

function selectLargestScreenShareViewerDemand(
	demands: Iterable<ScreenShareViewerDemandDimensions>,
): ScreenShareViewerDemandDimensions | null {
	let largest: ScreenShareViewerDemandDimensions | null = null;
	for (const demand of demands) {
		largest =
			largest === null
				? demand
				: {width: Math.max(largest.width, demand.width), height: Math.max(largest.height, demand.height)};
	}
	return largest;
}

function getScreenShareViewerDemand(trackSid: string | null | undefined): ScreenShareViewerDemandDimensions | null {
	if (!trackSid) return null;
	const demands = viewerDemandByTrackSid.get(trackSid);
	if (!demands) return null;
	return selectLargestScreenShareViewerDemand(demands.values());
}

function setPublicationVideoDimensions(
	publication: ScreenSharePublicationTarget,
	label: string,
	dimensions: ScreenShareViewerDemandDimensions,
	onError?: ScreenSharePublicationErrorHandler,
): void {
	if (typeof publication.setVideoDimensions !== 'function') return;
	try {
		publication.setVideoDimensions(dimensions);
	} catch (error) {
		onError?.('setVideoDimensions', label, error);
	}
}

function applyPublicationViewerDemand(
	publication: ScreenSharePublicationTarget,
	label: string,
	onError?: ScreenSharePublicationErrorHandler,
): void {
	if (!hasScreenShareViewerDemandTrack(publication) || !allowsScreenShareViewerDemand(publication.videoQuality)) return;
	const demand = getScreenShareViewerDemand(publication.trackSid);
	if (!demand) return;
	setPublicationVideoDimensions(publication, label, demand, onError);
}

function restorePublicationVideoQuality(
	publication: ScreenSharePublicationTarget,
	label: string,
	onError?: ScreenSharePublicationErrorHandler,
): void {
	if (typeof publication.setVideoQuality !== 'function') return;
	if (publication.isDesired === false || !allowsScreenShareViewerDemand(publication.videoQuality)) return;
	try {
		publication.setVideoQuality(SCREEN_SHARE_VIEWER_DEMAND_MAX_QUALITY);
	} catch (error) {
		onError?.('setVideoQuality', label, error);
	}
}

export function applyScreenShareViewerDemand({
	publication,
	label,
	viewerKey,
	dimensions,
	onError,
}: {
	publication: ScreenSharePublicationTarget | null | undefined;
	label: string;
	viewerKey: string;
	dimensions: ScreenShareViewerDemandDimensions;
	onError?: ScreenSharePublicationErrorHandler;
}): void {
	const trackSid = publication?.trackSid;
	if (!publication || !trackSid) return;
	let demands = viewerDemandByTrackSid.get(trackSid);
	if (!demands) {
		demands = new Map();
		viewerDemandByTrackSid.set(trackSid, demands);
	}
	demands.set(viewerKey, dimensions);
	applyPublicationViewerDemand(publication, label, onError);
}

export function releaseScreenShareViewerDemand({
	publication,
	label,
	trackSid,
	viewerKey,
	onError,
}: {
	publication: ScreenSharePublicationTarget | null | undefined;
	label: string;
	trackSid: string | null | undefined;
	viewerKey: string;
	onError?: ScreenSharePublicationErrorHandler;
}): void {
	if (!trackSid) return;
	const demands = viewerDemandByTrackSid.get(trackSid);
	if (!demands) return;
	demands.delete(viewerKey);
	if (demands.size === 0) {
		viewerDemandByTrackSid.delete(trackSid);
	}
	if (!publication || publication.trackSid !== trackSid) return;
	if (demands.size === 0) {
		restorePublicationVideoQuality(publication, label, onError);
		return;
	}
	applyPublicationViewerDemand(publication, label, onError);
}

export type ScreenSharePublicationVideoRequest =
	| {operation: 'setVideoQuality'; quality: VideoQuality}
	| {operation: 'setVideoDimensions'; quality: VideoQuality; dimensions: ScreenShareViewerDemandDimensions};

export function resolveScreenSharePublicationVideoRequest(
	publication: ScreenSharePublicationTarget,
	videoQuality: VideoQuality,
): ScreenSharePublicationVideoRequest {
	if (!hasScreenShareViewerDemandTrack(publication) || !allowsScreenShareViewerDemand(videoQuality)) {
		return {operation: 'setVideoQuality', quality: videoQuality};
	}
	const dimensions = getScreenShareViewerDemand(publication.trackSid);
	if (!dimensions) return {operation: 'setVideoQuality', quality: videoQuality};
	return {operation: 'setVideoDimensions', quality: SCREEN_SHARE_VIEWER_DEMAND_MAX_QUALITY, dimensions};
}

function setPublicationEnabled(
	publication: ScreenSharePublicationTarget,
	label: string,
	enabled: boolean,
	forceTrackSettingsUpdate: boolean,
	onError?: ScreenSharePublicationErrorHandler,
): void {
	if (typeof publication.setEnabled !== 'function') return;
	if (publication.isEnabled !== enabled) {
		try {
			publication.setEnabled(enabled);
		} catch (error) {
			onError?.('setEnabled', label, error);
		}
		return;
	}
	if (!forceTrackSettingsUpdate) return;
	if (typeof publication.emitTrackUpdate === 'function') {
		try {
			publication.emitTrackUpdate();
		} catch (error) {
			onError?.('emitTrackUpdate', label, error);
		}
		return;
	}
	try {
		publication.setEnabled(!enabled);
	} catch (error) {
		onError?.('setEnabled', label, error);
	}
	try {
		publication.setEnabled(enabled);
	} catch (error) {
		onError?.('setEnabled', label, error);
	}
}

function setPublicationSubscribed(
	publication: ScreenSharePublicationTarget,
	label: string,
	subscribed: boolean,
	onError?: ScreenSharePublicationErrorHandler,
): void {
	if (typeof publication.setSubscribed !== 'function') return;
	try {
		publication.setSubscribed(subscribed);
	} catch (error) {
		onError?.('setSubscribed', label, error);
	}
}

export function syncScreenSharePublication({
	publication,
	label,
	shouldSubscribe,
	shouldEnable = shouldSubscribe,
	onError,
}: SyncScreenSharePublicationOptions): void {
	if (!publication) return;
	const desired = publication.isDesired ?? publication.isSubscribed ?? false;
	if (!shouldSubscribe) {
		if (typeof publication.setEnabled === 'function' && desired && publication.isEnabled !== false) {
			try {
				publication.setEnabled(false);
			} catch (error) {
				onError?.('setEnabled', label, error);
			}
		}
		if (desired) {
			setPublicationSubscribed(publication, label, false, onError);
		}
		return;
	}
	let didSubscribe = false;
	if (typeof publication.setSubscribed === 'function' && !desired) {
		try {
			publication.setSubscribed(true);
			didSubscribe = true;
		} catch (error) {
			onError?.('setSubscribed', label, error);
		}
	}
	const forceTrackSettingsUpdate = didSubscribe && !shouldEnable && publication.isEnabled === shouldEnable;
	setPublicationEnabled(publication, label, shouldEnable, forceTrackSettingsUpdate, onError);
	applyPublicationViewerDemand(publication, label, onError);
}

export function refreshScreenSharePublicationSubscription({
	publication,
	label,
	shouldEnable = true,
	onError,
}: {
	publication: ScreenSharePublicationTarget | null | undefined;
	label: string;
	shouldEnable?: boolean;
	onError?: ScreenSharePublicationErrorHandler;
}): void {
	if (!publication) return;
	setPublicationSubscribed(publication, label, true, onError);
	setPublicationEnabled(publication, label, shouldEnable, true, onError);
	applyPublicationViewerDemand(publication, label, onError);
}

export function resubscribeScreenSharePublication({
	publication,
	label,
	shouldEnable = true,
	onError,
}: {
	publication: ScreenSharePublicationTarget | null | undefined;
	label: string;
	shouldEnable?: boolean;
	onError?: ScreenSharePublicationErrorHandler;
}): void {
	if (!publication) return;
	const desired = publication.isDesired ?? publication.isSubscribed ?? true;
	if (desired && typeof publication.setEnabled === 'function' && publication.isEnabled !== false) {
		try {
			publication.setEnabled(false);
		} catch (error) {
			onError?.('setEnabled', label, error);
		}
	}
	setPublicationSubscribed(publication, label, false, onError);
	setPublicationSubscribed(publication, label, true, onError);
	setPublicationEnabled(publication, label, shouldEnable, true, onError);
	applyPublicationViewerDemand(publication, label, onError);
}

export function syncWatchedScreenSharePublications({
	isScreenShare,
	isOwnScreenShare,
	userWantsToWatch,
	videoLocallyDisabled,
	audioEnabled,
	videoPublication,
	audioPublication,
	onError,
}: SyncWatchedScreenSharePublicationsOptions): void {
	const canSubscribeRemote = isScreenShare && !isOwnScreenShare;
	const shouldSubscribeVideo = canSubscribeRemote && userWantsToWatch && !videoLocallyDisabled;
	const shouldSubscribeAudio = canSubscribeRemote && userWantsToWatch;
	syncScreenSharePublication({
		publication: videoPublication,
		label: 'screen share video publication',
		shouldSubscribe: shouldSubscribeVideo,
		shouldEnable: shouldSubscribeVideo,
		onError,
	});
	syncScreenSharePublication({
		publication: audioPublication,
		label: 'screen share audio publication',
		shouldSubscribe: shouldSubscribeAudio,
		shouldEnable: shouldSubscribeAudio && audioEnabled,
		onError,
	});
}
