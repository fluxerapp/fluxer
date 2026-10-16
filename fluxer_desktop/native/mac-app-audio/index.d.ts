// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export interface MacAppAudioBackendInfo {
	backend: string;
	supported: boolean;
	reason: string;
	minMacosVersion: string;
	minMacosVersionCoreaudio: string;
	detectedMacosVersion?: string;
	sckAvailable: boolean;
	coreaudioAvailable: boolean;
}

export declare function getBackendInfo(): MacAppAudioBackendInfo;

export interface MacBackendAvailability {
	sck?: {
		supported: boolean;
		macosVersion?: string;
	};
	coreaudio?: {
		supported: boolean;
	};
	screenPermission?: string;
	audioPermission?: string;
}

export interface MacApplicationDescriptor {
	pid: number;
	bundleId?: string;
	name: string;
}

export interface ProcessLoopbackOptions {
	excludeSelf?: boolean;
	includeProcessTree?: boolean;
	backend?: 'sck' | 'coreaudio' | 'auto';
	macBackend?: 'sck' | 'coreaudio' | 'auto';
	captureScope?: 'process' | 'system';
	macCaptureScope?: 'process' | 'system';
	scope?: 'process' | 'system';
}

export interface AudioFrame {
	samples: Float32Array;
	sampleRate: number;
	channels: number;
	timestampUs: number;
}

export declare const loadError: Error | null;

export declare function pidFromWindowId(windowId: number): number;

export declare function listAudibleApplications(): Promise<Array<MacApplicationDescriptor>>;

export declare function getBackendAvailability(): Promise<MacBackendAvailability>;

export declare interface ProcessLoopback {
	on(event: 'frame', listener: (frame: AudioFrame) => void): this;
	on(event: 'error', listener: (err: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	off(event: 'frame', listener: (frame: AudioFrame) => void): this;
	off(event: 'error', listener: (err: Error) => void): this;
	off(event: 'closed', listener: () => void): this;
	off(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	emit(event: 'frame', frame: AudioFrame): boolean;
	emit(event: 'error', err: Error): boolean;
	emit(event: 'closed'): boolean;
}

export declare class ProcessLoopback extends EventEmitter {
	constructor(pid: number, options?: ProcessLoopbackOptions);

	start(): Promise<void>;

	stop(): Promise<void>;
}
