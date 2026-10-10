// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export interface JsRoutingRule {
	include?: Array<Record<string, string>>;
	exclude?: Array<Record<string, string>>;
	workaround?: Array<Record<string, string>>;
	ignoreDevices?: boolean;
	onlySpeakers?: boolean;
	onlyDefaultSpeakers?: boolean;
}

export interface AudioFrame {
	samples: Float32Array;
	sampleRate: number;
	channels: number;
	timestampUs: number;
}

export interface NativeAudioFrame {
	samples: ArrayBuffer;
	sampleRate: number;
	channels: number;
	timestampUs: number;
}

export declare function pipeWireAvailable(): boolean;

export declare function audioBackend(): 'pipewire' | 'none';

export declare class AudioBridge {
	constructor();

	inventory(fields?: Array<string> | undefined | null): Array<Record<string, string>>;

	apply(rule: JsRoutingRule): boolean;

	release(): void;

	backend(): 'pipewire' | 'none';
}

export declare class DirectAudioCapture {
	constructor();

	start(rule: JsRoutingRule): boolean;

	setRule(rule: JsRoutingRule): boolean;

	setLifecycleCallback(callback: (type: string, message: string) => void): void;

	read(): NativeAudioFrame | null;

	stop(): void;
}

interface ProcessLoopbackEvents {
	on(event: 'frame', listener: (frame: AudioFrame) => void): this;
	on(event: 'error', listener: (error: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'diagnostic', listener: (message: string) => void): this;
	removeListener(event: 'frame', listener: (frame: AudioFrame) => void): this;
	removeListener(event: 'error', listener: (error: Error) => void): this;
	removeListener(event: 'closed', listener: () => void): this;
	removeListener(event: 'diagnostic', listener: (message: string) => void): this;
}

export declare class ProcessLoopback extends EventEmitter implements ProcessLoopbackEvents {
	constructor(targetPid: number, options?: {includeProcessTree?: boolean; ignoreDevices?: boolean});

	constructor(options: {linuxRule: JsRoutingRule});

	start(): void;

	setRoutingRule(target: {linuxRule: JsRoutingRule} | number, options?: {includeProcessTree?: boolean}): boolean;

	stop(): Promise<void>;
}
