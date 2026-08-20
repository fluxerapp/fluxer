// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	videoElementHasRenderedFrame,
	watchVideoElementRenderedFrame,
} from '@app/features/voice/components/VideoElementFrameState';
import {
	type ScreenShareWatchFailure,
	ScreenShareWatchFailures,
} from '@app/features/voice/state/ScreenShareWatchFailures';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const SCREEN_SHARE_SOURCE = 'screen_share';

interface UseScreenShareWatchFailureOptions {
	enabled: boolean;
	streamKey: string;
	participantIdentity: string;
	participantSid?: string | null;
	trackSid?: string | null;
	hasPublication: boolean;
	isPublicationDesired: boolean;
	hasSubscribedVideo: boolean;
	operationKey?: string | number | null;
	videoRef: React.RefObject<HTMLVideoElement | null>;
}

export interface ScreenShareWatchFailureState {
	failure: ScreenShareWatchFailure | null;
	hasRenderedVideoFrame: boolean;
}

interface NormalizedScreenShareWatchFailureTarget {
	streamKey: string;
	participantIdentity: string;
	source: string;
	participantSid?: string;
	trackSid?: string;
}

function createFailureTarget({
	streamKey,
	participantIdentity,
	participantSid,
	trackSid,
}: Pick<
	UseScreenShareWatchFailureOptions,
	'streamKey' | 'participantIdentity' | 'participantSid' | 'trackSid'
>): NormalizedScreenShareWatchFailureTarget {
	const target: NormalizedScreenShareWatchFailureTarget = {
		streamKey,
		participantIdentity,
		source: SCREEN_SHARE_SOURCE,
	};
	if (participantSid) {
		target.participantSid = participantSid;
	}
	if (trackSid) {
		target.trackSid = trackSid;
	}
	return target;
}

export function useScreenShareWatchFailure({
	enabled,
	streamKey,
	participantIdentity,
	participantSid,
	trackSid,
	operationKey,
	videoRef,
}: UseScreenShareWatchFailureOptions): ScreenShareWatchFailureState {
	const attemptEnabled = enabled && streamKey !== '';
	const target = useMemo(
		() =>
			createFailureTarget({
				streamKey,
				participantIdentity,
				participantSid,
				trackSid,
			}),
		[participantIdentity, participantSid, streamKey, trackSid],
	);
	const watchGeneration = attemptEnabled ? ScreenShareWatchFailures.getWatchGeneration(streamKey) : 0;
	const attemptKey = attemptEnabled
		? operationKey == null
			? `${streamKey}:${watchGeneration}:watch`
			: `${streamKey}:${watchGeneration}:operation:${operationKey}`
		: '';
	const isOperationBuffering = operationKey != null;
	const failure = attemptEnabled ? ScreenShareWatchFailures.getFailure(target) : null;
	const attempt = attemptEnabled ? ScreenShareWatchFailures.getAttempt(streamKey) : null;
	const hasRenderedVideoFrame = attempt?.attemptKey === attemptKey ? attempt.hasRenderedVideoFrame : false;

	useEffect(() => {
		if (!attemptEnabled || !attemptKey) return;
		ScreenShareWatchFailures.ensureAttempt(target, attemptKey);
		return () => {
			ScreenShareWatchFailures.releaseAttempt(target, attemptKey);
		};
	}, [attemptEnabled, attemptKey, target]);

	useEffect(() => {
		if (!attemptEnabled || !attemptKey) return;
		if (isOperationBuffering) return;
		if (hasRenderedVideoFrame) return;
		if (videoElementHasRenderedFrame(videoRef.current)) {
			ScreenShareWatchFailures.markRenderedVideoFrame(target, attemptKey);
			return;
		}
		return watchVideoElementRenderedFrame({
			videoRef,
			onFrame: () => {
				ScreenShareWatchFailures.markRenderedVideoFrame(target, attemptKey);
			},
		});
	}, [attemptEnabled, attemptKey, hasRenderedVideoFrame, isOperationBuffering, target, videoRef]);

	return {
		failure,
		hasRenderedVideoFrame,
	};
}
