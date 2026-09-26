// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';

export interface TrackProcessorReadable<T> {
	readonly readable: ReadableStream<T>;
}

type MediaStreamTrackProcessorInstance = {
	readonly readable: ReadableStream<VideoFrame>;
};
type MediaStreamTrackProcessorConstructor = new (options: {
	track: MediaStreamTrack;
}) => MediaStreamTrackProcessorInstance;

const logger = new Logger('MediaStreamTrackProcessorPolyfill');

export function mediaStreamTrackProcessorSupported(): boolean {
	return 'MediaStreamTrackProcessor' in globalThis;
}

export function videoFrameCaptureSupported(): boolean {
	if (!('VideoFrame' in globalThis)) {
		return false;
	}
	if (!('HTMLVideoElement' in globalThis)) {
		return false;
	}
	return typeof HTMLVideoElement.prototype.requestVideoFrameCallback === 'function';
}

function getMediaStreamTrackProcessorConstructor(): MediaStreamTrackProcessorConstructor | null {
	const candidate: unknown = Reflect.get(globalThis, 'MediaStreamTrackProcessor');
	if (typeof candidate !== 'function') {
		return null;
	}
	return candidate as MediaStreamTrackProcessorConstructor;
}

export function createTrackProcessor<T extends VideoFrame>(track: MediaStreamTrack): TrackProcessorReadable<T> {
	const Processor = getMediaStreamTrackProcessorConstructor();
	if (Processor != null) {
		return new Processor({track}) as unknown as TrackProcessorReadable<T>;
	}
	return {readable: createPolyfilledVideoReadable(track) as ReadableStream<T>};
}

function createPolyfilledVideoReadable(track: MediaStreamTrack): ReadableStream<VideoFrame> {
	return new PolyfilledVideoTrackProcessor(track).createReadable();
}

class PolyfilledVideoTrackProcessor {
	private readonly video: HTMLVideoElement;
	private stopped = false;
	private callbackHandle: number | null = null;
	private streamController: ReadableStreamDefaultController<VideoFrame> | null = null;

	constructor(private readonly track: MediaStreamTrack) {
		this.video = document.createElement('video');
		this.video.muted = true;
		this.video.autoplay = true;
		this.video.playsInline = true;
		this.video.srcObject = new MediaStream([track]);
	}

	createReadable(): ReadableStream<VideoFrame> {
		return new ReadableStream<VideoFrame>({
			start: (controller): void => {
				this.start(controller);
			},
			cancel: (): void => {
				this.stop();
			},
		});
	}

	private readonly handleTrackEnded = (): void => {
		try {
			this.streamController?.close();
		} catch (error) {
			logger.warn('Polyfilled video track processor close failed', {error});
		}
		this.stop();
	};

	private stop(): void {
		if (this.stopped) {
			return;
		}
		this.stopped = true;
		if (this.callbackHandle != null) {
			try {
				this.video.cancelVideoFrameCallback(this.callbackHandle);
			} catch {}
			this.callbackHandle = null;
		}
		try {
			this.track.removeEventListener('ended', this.handleTrackEnded);
		} catch {}
		try {
			this.video.srcObject = null;
		} catch {}
	}

	private reportFailure(error: unknown): void {
		try {
			this.streamController?.error(error);
		} catch {}
		this.stop();
	}

	private start(controller: ReadableStreamDefaultController<VideoFrame>): void {
		this.streamController = controller;
		this.track.addEventListener('ended', this.handleTrackEnded, {once: true});
		this.video
			.play()
			.then(() => {
				this.scheduleNextFrame();
			})
			.catch((error) => {
				if (this.stopped) {
					return;
				}
				this.reportFailure(error);
			});
	}

	private scheduleNextFrame(): void {
		if (this.stopped) {
			return;
		}
		this.callbackHandle = this.video.requestVideoFrameCallback(this.handleFrame);
	}

	private readonly handleFrame: VideoFrameRequestCallback = (_callbackTimeMs, metadata): void => {
		if (this.stopped) {
			return;
		}
		if (this.track.readyState === 'ended') {
			this.handleTrackEnded();
			return;
		}
		const controller = this.streamController;
		if (controller == null) {
			this.reportFailure(new Error('Polyfilled video capture started without a stream controller'));
			return;
		}
		if (controller.desiredSize != null && controller.desiredSize <= 0) {
			this.scheduleNextFrame();
			return;
		}
		this.captureFrame(controller, metadata);
	};

	private captureFrame(
		controller: ReadableStreamDefaultController<VideoFrame>,
		metadata: VideoFrameCallbackMetadata,
	): void {
		const timestamp = Math.round(metadata.mediaTime * 1_000_000);
		let frame: VideoFrame | null = null;
		try {
			frame = new VideoFrame(this.video, {timestamp});
			controller.enqueue(frame);
			frame = null;
		} catch (error) {
			frame?.close();
			this.reportFailure(error);
			return;
		}
		this.scheduleNextFrame();
	}
}
