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

import {isTrackReference, type TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {useEffect, useMemo} from 'react';
import * as VoiceCallLayoutActionCreators from '~/actions/VoiceCallLayoutActionCreators';

type LayoutMode = 'grid' | 'focus';

interface UsePinnedTrackRefArgs {
	layoutMode: LayoutMode;
	pinnedParticipantIdentity: string | null;
	filteredCameraTracks: Array<TrackReferenceOrPlaceholder>;
	cameraTracksAll: Array<TrackReferenceOrPlaceholder>;
	screenShareTracks: Array<TrackReferenceOrPlaceholder>;
}

function identityOf(track: TrackReferenceOrPlaceholder): string {
	return track.participant?.identity ?? '';
}

function sortByParticipantIdentity(tracks: Array<TrackReferenceOrPlaceholder>) {
	return [...tracks].sort((a, b) => identityOf(a).localeCompare(identityOf(b)));
}

function findByIdentity(
	tracks: Array<TrackReferenceOrPlaceholder>,
	identity: string | null,
): TrackReferenceOrPlaceholder | null {
	if (!identity) return null;
	return tracks.find((t) => identityOf(t) === identity) ?? null;
}

export function usePinnedTrackRef({
	layoutMode,
	pinnedParticipantIdentity,
	filteredCameraTracks,
	cameraTracksAll,
	screenShareTracks,
}: UsePinnedTrackRefArgs) {
	const cameraBase = filteredCameraTracks.length > 0 ? filteredCameraTracks : cameraTracksAll;

	const camerasSorted = useMemo(() => sortByParticipantIdentity(cameraBase), [cameraBase]);
	const screensSorted = useMemo(() => sortByParticipantIdentity(screenShareTracks), [screenShareTracks]);

	const defaultFocusTrack = useMemo<TrackReferenceOrPlaceholder | null>(() => {
		return screensSorted[0] ?? camerasSorted[0] ?? null;
	}, [screensSorted, camerasSorted]);

	const pinnedTrack = useMemo(() => {
		const fromScreens = findByIdentity(screensSorted, pinnedParticipantIdentity);
		if (fromScreens) return fromScreens;
		return findByIdentity(camerasSorted, pinnedParticipantIdentity);
	}, [screensSorted, camerasSorted, pinnedParticipantIdentity]);

	const mainTrack = useMemo<TrackReferenceOrPlaceholder | null>(() => {
		if (layoutMode !== 'focus') return null;
		return pinnedTrack ?? defaultFocusTrack;
	}, [layoutMode, pinnedTrack, defaultFocusTrack]);

	const carouselTracks = useMemo<Array<TrackReferenceOrPlaceholder>>(() => camerasSorted, [camerasSorted]);

	useEffect(() => {
		if (layoutMode !== 'focus') return;
		if (pinnedParticipantIdentity) return;

		if (defaultFocusTrack && isTrackReference(defaultFocusTrack)) {
			const identity = identityOf(defaultFocusTrack);
			if (identity) VoiceCallLayoutActionCreators.setPinnedParticipant(identity);
		}
	}, [layoutMode, pinnedParticipantIdentity, defaultFocusTrack]);

	return {mainTrack, carouselTracks};
}
