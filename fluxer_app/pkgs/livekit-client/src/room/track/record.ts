// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import type LocalTrack from './LocalTrack.ts';

const isMediaRecorderAvailable = typeof MediaRecorder !== 'undefined';

class FallbackRecorder extends EventTarget implements MediaRecorder {
	static isTypeSupported(): boolean {
		return false;
	}

	readonly audioBitsPerSecond = 0;
	readonly mimeType = '';
	readonly state: RecordingState = 'inactive';
	readonly stream: MediaStream;
	readonly videoBitsPerSecond = 0;
	ondataavailable: ((this: MediaRecorder, ev: BlobEvent) => unknown) | null = null;
	onerror: ((this: MediaRecorder, ev: Event) => unknown) | null = null;
	onpause: ((this: MediaRecorder, ev: Event) => unknown) | null = null;
	onresume: ((this: MediaRecorder, ev: Event) => unknown) | null = null;
	onstart: ((this: MediaRecorder, ev: Event) => unknown) | null = null;
	onstop: ((this: MediaRecorder, ev: Event) => unknown) | null = null;

	constructor(stream: MediaStream) {
		super();
		this.stream = stream;
		throw new Error('MediaRecorder is not available in this environment');
	}

	pause(): void {
		throw new Error('MediaRecorder is not available in this environment');
	}

	requestData(): void {
		throw new Error('MediaRecorder is not available in this environment');
	}

	resume(): void {
		throw new Error('MediaRecorder is not available in this environment');
	}

	start(): void {
		throw new Error('MediaRecorder is not available in this environment');
	}

	stop(): void {
		throw new Error('MediaRecorder is not available in this environment');
	}
}

const RecorderBase: typeof MediaRecorder = isMediaRecorderAvailable ? MediaRecorder : FallbackRecorder;

function readLegacyBlobByteArray(data: Blob): NonSharedUint8Array | undefined {
	if (!('byteArray' in data)) {
		return undefined;
	}
	return data.byteArray instanceof Uint8Array ? (data.byteArray as NonSharedUint8Array) : undefined;
}

export class LocalTrackRecorder<T extends LocalTrack> extends RecorderBase {
	byteStream: ReadableStream<Uint8Array>;

	constructor(track: T, options?: MediaRecorderOptions) {
		if (!isMediaRecorderAvailable) {
			throw new Error('MediaRecorder is not available in this environment');
		}

		super(new MediaStream([track.mediaStreamTrack]), options);

		let dataListener: (event: BlobEvent) => void;

		let streamController: ReadableStreamDefaultController<Uint8Array> | undefined;

		const isClosed = () => streamController === undefined;

		const onStop = () => {
			this.removeEventListener('dataavailable', dataListener);
			this.removeEventListener('stop', onStop);
			this.removeEventListener('error', onError);
			streamController?.close();
			streamController = undefined;
		};

		const onError = (event: Event) => {
			streamController?.error(event);
			this.removeEventListener('dataavailable', dataListener);
			this.removeEventListener('stop', onStop);
			this.removeEventListener('error', onError);
			streamController = undefined;
		};

		this.byteStream = new ReadableStream({
			start: (controller) => {
				streamController = controller;
				dataListener = async (event: BlobEvent) => {
					let data: NonSharedUint8Array;

					if (event.data.arrayBuffer) {
						const arrayBuffer = await event.data.arrayBuffer();
						data = new Uint8Array(arrayBuffer);
					} else {
						const legacyByteArray = readLegacyBlobByteArray(event.data);
						if (!legacyByteArray) {
							throw new Error('no data available!');
						}
						data = legacyByteArray;
					}

					if (isClosed()) {
						return;
					}
					controller.enqueue(data);
				};
				this.addEventListener('dataavailable', dataListener);
			},
			cancel: () => {
				onStop();
			},
		});

		this.addEventListener('stop', onStop);
		this.addEventListener('error', onError);
	}
}

export function isRecordingSupported(): boolean {
	return isMediaRecorderAvailable;
}
