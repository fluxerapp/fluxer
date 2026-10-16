// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceMeshEngine} from '@app/features/voice/engine/mesh/VoiceMeshEngine';
import type {VoiceMeshSource, VoiceMeshTrackInfo} from '@app/features/voice/engine/mesh/VoiceMeshSignal';
import {Track} from 'livekit-client';

export const VOICE_MESH_TRACK_SOURCES: Readonly<Record<VoiceMeshSource, Track.Source>> = {
	microphone: Track.Source.Microphone,
	camera: Track.Source.Camera,
	screen_share: Track.Source.ScreenShare,
	screen_share_audio: Track.Source.ScreenShareAudio,
};

export const VOICE_MESH_TRACK_KINDS: Readonly<Record<VoiceMeshSource, Track.Kind.Audio | Track.Kind.Video>> = {
	microphone: Track.Kind.Audio,
	camera: Track.Kind.Video,
	screen_share: Track.Kind.Video,
	screen_share_audio: Track.Kind.Audio,
};

interface VoiceMeshPublication {
	sid: string;
	muted: boolean;
	width: number;
	height: number;
}

function createSendParameters(
	encodings: ReadonlyArray<RTCRtpEncodingParameters>,
	degradationPreference?: RTCDegradationPreference,
): RTCRtpSendParameters {
	return {
		transactionId: '',
		encodings: encodings.map((encoding) => ({...encoding})),
		codecs: [],
		headerExtensions: [],
		rtcp: {},
		...(degradationPreference !== undefined && {degradationPreference}),
	};
}

export class VoiceMeshSender {
	readonly transport: RTCDtlsTransport | null = null;
	track: MediaStreamTrack | null = null;
	private publication: VoiceMeshPublication | null = null;
	private parameters: RTCRtpSendParameters = createSendParameters([{}]);

	constructor(
		private readonly engine: VoiceMeshEngine,
		readonly src: VoiceMeshSource,
	) {}

	get sid(): string | null {
		return this.publication?.sid ?? null;
	}

	get info(): VoiceMeshTrackInfo | null {
		const publication = this.publication;
		if (!publication) return null;
		return {
			src: this.src,
			sid: publication.sid,
			m: publication.muted,
			...(publication.width > 0 && {w: publication.width}),
			...(publication.height > 0 && {h: publication.height}),
		};
	}

	attach(track: MediaStreamTrack, encodings: ReadonlyArray<RTCRtpEncodingParameters> | undefined): void {
		this.track = track;
		this.parameters = createSendParameters(encodings?.length ? encodings : [{}]);
		void this.engine.syncSender(this.src);
	}

	publish(publication: VoiceMeshPublication): void {
		this.publication = publication;
		void this.engine.syncSender(this.src);
	}

	setMuted(muted: boolean): void {
		if (!this.publication || this.publication.muted === muted) return;
		this.publication = {...this.publication, muted};
		void this.engine.syncSender(this.src);
	}

	unpublish(): void {
		this.publication = null;
		this.track = null;
		void this.engine.syncSender(this.src);
	}

	replaceTrack(track: MediaStreamTrack | null): Promise<void> {
		this.track = track;
		return this.engine.syncSender(this.src);
	}

	getParameters(): RTCRtpSendParameters {
		return createSendParameters(this.parameters.encodings, this.parameters.degradationPreference);
	}

	setParameters(parameters: RTCRtpSendParameters): Promise<void> {
		this.parameters = createSendParameters(parameters.encodings, parameters.degradationPreference);
		return this.engine.syncSender(this.src);
	}

	getStats(): Promise<RTCStatsReport> {
		return this.engine.collectSenderStats(this.src);
	}
}
