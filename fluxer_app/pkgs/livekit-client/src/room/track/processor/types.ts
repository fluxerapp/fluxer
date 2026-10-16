// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type Room from '../../Room.ts';
import type LocalTrack from '../LocalTrack.ts';
import type {Track} from '../Track.ts';

export type ProcessorOptions<T extends Track.Kind> = {
	kind: T;
	track: MediaStreamTrack;
	element?: HTMLMediaElement;
	audioContext?: AudioContext;
	localTrack?: LocalTrack;
};

export interface AudioProcessorOptions extends ProcessorOptions<Track.Kind.Audio> {
	audioContext: AudioContext;
}

export interface VideoProcessorOptions extends ProcessorOptions<Track.Kind.Video> {}

export interface TrackProcessor<T extends Track.Kind, U extends ProcessorOptions<T> = ProcessorOptions<T>> {
	name: string;
	init: (opts: U) => Promise<void>;
	restart: (opts: U) => Promise<void>;
	destroy: () => Promise<void>;
	processedTrack?: MediaStreamTrack;
	onPublish?: (room: Room) => Promise<void>;
	onUnpublish?: () => Promise<void>;
}
