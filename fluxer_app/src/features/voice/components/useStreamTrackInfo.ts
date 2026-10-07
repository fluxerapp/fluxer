// SPDX-License-Identifier: AGPL-3.0-or-later

import {useStoreVersion} from '@app/features/voice/engine/Store';
import {
	mergeVoiceMediaGraphTrackInfo,
	selectVoiceMediaGraphStreamTrackInfo,
	type VoiceMediaGraphNativeStatsTarget,
	type VoiceMediaGraphPartialTrackInfo,
	type VoiceMediaGraphStatsView,
	type VoiceMediaGraphTrackInfo,
} from '@app/features/voice/engine/VoiceMediaGraphStats';
import type {VoiceMediaGraphStatsTrackTarget} from '@app/features/voice/engine/VoiceMediaGraphStatsObservations';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {isTrackReference, type TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {TrackEvent} from 'livekit-client';
import {useEffect, useMemo, useState} from 'react';

export type StreamTrackInfo = VoiceMediaGraphTrackInfo;

interface StreamTrackDimensions {
	width: number;
	height: number;
}

type StreamTrackElementSnapshot =
	| HTMLMediaElement
	| {
			videoWidth?: number;
			videoHeight?: number;
	  };

interface StreamTrackInfoSnapshot {
	attachedElements?: ReadonlyArray<StreamTrackElementSnapshot> | null;
	settings?: MediaTrackSettings | null;
	publicationDimensions?: StreamTrackDimensions | null;
}

type NativeStreamTrackInfoTarget = VoiceMediaGraphNativeStatsTarget;

type UseStreamTrackInfoOptions = NativeStreamTrackInfoTarget;

type PartialStreamTrackInfo = VoiceMediaGraphPartialTrackInfo;

function isPositiveDimension(value: number | undefined): value is number {
	return typeof value === 'number' && Number.isFinite(value) && value > 0;
}

function buildDimensions(width: number | undefined, height: number | undefined): StreamTrackDimensions | null {
	if (!isPositiveDimension(width) || !isPositiveDimension(height)) {
		return null;
	}
	return {width, height};
}

function getLargestAttachedElementDimensions(
	attachedElements?: ReadonlyArray<StreamTrackElementSnapshot> | null,
): StreamTrackDimensions | null {
	if (!attachedElements || attachedElements.length === 0) {
		return null;
	}
	let largest: StreamTrackDimensions | null = null;
	for (const element of attachedElements) {
		const next = buildDimensions(
			'videoWidth' in element ? element.videoWidth : undefined,
			'videoHeight' in element ? element.videoHeight : undefined,
		);
		if (!next) {
			continue;
		}
		if (!largest || next.width * next.height > largest.width * largest.height) {
			largest = next;
		}
	}
	return largest;
}

function resolveStreamTrackInfoSnapshot(snapshot: StreamTrackInfoSnapshot): StreamTrackInfo | null {
	const settingsDimensions = buildDimensions(snapshot.settings?.width, snapshot.settings?.height);
	const publicationDimensions = buildDimensions(
		snapshot.publicationDimensions?.width,
		snapshot.publicationDimensions?.height,
	);
	const dimensions =
		getLargestAttachedElementDimensions(snapshot.attachedElements) ?? settingsDimensions ?? publicationDimensions;
	if (!dimensions) {
		return null;
	}
	return {
		...dimensions,
		fps: Math.round(snapshot.settings?.frameRate ?? 0),
	};
}

function isScreenShareStatsTarget(trackRef: TrackReferenceOrPlaceholder | null, options?: NativeStreamTrackInfoTarget) {
	if (options?.nativeSource != null) {
		return asVoiceTrackSource(options.nativeSource) === VoiceTrackSource.ScreenShare;
	}
	if (!trackRef || !isTrackReference(trackRef)) return false;
	return asVoiceTrackSource(trackRef.source) === VoiceTrackSource.ScreenShare;
}

function mergeStreamTrackInfo(
	primary: StreamTrackInfo | null,
	fallback: PartialStreamTrackInfo | null,
): StreamTrackInfo | null {
	return mergeVoiceMediaGraphTrackInfo(primary, fallback);
}

function resolveStreamTrackStatsInfo(
	view: VoiceMediaGraphStatsView,
	target: VoiceMediaGraphStatsTrackTarget,
): PartialStreamTrackInfo | null {
	return selectVoiceMediaGraphStreamTrackInfo(view, target);
}

function areStreamTrackInfosEqual(left: StreamTrackInfo | null, right: StreamTrackInfo | null): boolean {
	if (left == null || right == null) {
		return left === right;
	}
	return left.width === right.width && left.height === right.height && left.fps === right.fps;
}

export function useStreamTrackInfo(
	trackRef: TrackReferenceOrPlaceholder | null,
	options?: UseStreamTrackInfoOptions,
): StreamTrackInfo | null {
	const [info, setInfo] = useState<StreamTrackInfo | null>(null);
	const graphVersion = useStoreVersion(voiceMediaGraphStore);
	useEffect(() => {
		if (!trackRef || !isTrackReference(trackRef)) {
			setInfo(null);
			return;
		}
		if (asVoiceTrackSource(trackRef.source) !== VoiceTrackSource.ScreenShare) {
			setInfo(null);
			return;
		}
		const publication = trackRef.publication;
		const videoTrack = publication?.videoTrack;
		if (!videoTrack) {
			setInfo(null);
			return;
		}
		const activeVideoTrack = videoTrack;
		function update() {
			const nextInfo = resolveStreamTrackInfoSnapshot({
				attachedElements: activeVideoTrack.attachedElements,
				settings: activeVideoTrack.mediaStreamTrack.getSettings(),
				publicationDimensions: publication?.dimensions,
			});
			setInfo((current) => (areStreamTrackInfosEqual(current, nextInfo) ? current : nextInfo));
		}
		update();
		activeVideoTrack.on(TrackEvent.ElementAttached, update);
		activeVideoTrack.on(TrackEvent.ElementDetached, update);
		activeVideoTrack.on(TrackEvent.VideoDimensionsChanged, update);
		activeVideoTrack.on(TrackEvent.Restarted, update);
		return () => {
			activeVideoTrack.off(TrackEvent.ElementAttached, update);
			activeVideoTrack.off(TrackEvent.ElementDetached, update);
			activeVideoTrack.off(TrackEvent.VideoDimensionsChanged, update);
			activeVideoTrack.off(TrackEvent.Restarted, update);
		};
	}, [trackRef]);
	const statsInfo = useMemo(() => {
		if (!isScreenShareStatsTarget(trackRef, options)) return null;
		const publicationTrackSid = isTrackReference(trackRef) ? trackRef.publication?.trackSid : null;
		const mediaTrackId = isTrackReference(trackRef) ? trackRef.publication?.videoTrack?.mediaStreamTrack.id : null;
		return resolveStreamTrackStatsInfo(voiceMediaGraphStore.graph, {
			trackSid: options?.nativeTrackSid ?? publicationTrackSid,
			trackIdentifier: mediaTrackId,
			participantIdentity: options?.participantIdentity ?? null,
			source: VoiceTrackSource.ScreenShare,
			kind: 'video',
		});
	}, [trackRef, options?.nativeSource, options?.nativeTrackSid, options?.participantIdentity, graphVersion]);
	return mergeStreamTrackInfo(info, statsInfo);
}
