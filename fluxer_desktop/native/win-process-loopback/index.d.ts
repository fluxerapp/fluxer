// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export interface ProcessLoopbackOptions {
	includeProcessTree?: boolean;
	captureScope?: 'process' | 'system' | 'session-mixer';
	winCaptureScope?: 'process' | 'system' | 'session-mixer';
	scope?: 'process' | 'system' | 'session-mixer';
	sampleRate?: 48000;
	channels?: 2;
	endpoints?: Array<string>;
}

export interface AudioFrame {
	samples: Float32Array;
	sampleRate: number;
	channels: number;
	timestampUs: bigint;
}

export declare interface ProcessLoopback {
	on(event: 'frame', listener: (frame: AudioFrame) => void): this;
	on(event: 'error', listener: (err: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'started', listener: () => void): this;
	on(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	off(event: 'frame', listener: (frame: AudioFrame) => void): this;
	off(event: 'error', listener: (err: Error) => void): this;
	off(event: 'closed', listener: () => void): this;
	off(event: 'started', listener: () => void): this;
	off(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	emit(event: 'frame', frame: AudioFrame): boolean;
	emit(event: 'error', err: Error): boolean;
	emit(event: 'closed'): boolean;
	emit(event: 'started'): boolean;
}

export declare class ProcessLoopback extends EventEmitter {
	constructor(pid: number, opts?: ProcessLoopbackOptions);

	start(): Promise<void>;

	setEndpoints(endpoints: Array<string>): boolean;

	stop(): void;
}

export declare function isSupported(): boolean;

export interface WinProcessLoopbackBackendInfo {
	backend: string;
	supported: boolean;
	reason: string;
	processSupported: boolean;
	systemSupported: boolean;
	systemExcludesSelf: boolean;
	processIncludeSupported: boolean;
	processExcludeSupported: boolean;
	sessionMixerSupported: boolean;
	systemLoopbackMode: 'process-exclude' | 'session-mixer' | 'unavailable';
	minWindowsBuild: number;
	minWindowsVersionLabel: string;
	detectedWindowsBuild?: number;
}

export declare function getBackendInfo(): WinProcessLoopbackBackendInfo;

export declare function pidFromHwnd(hwnd: bigint): number;

export declare function resolveAudioRootPid(pid: number): number;

export interface PlaybackDeviceSession {
	pid: number;
	rootPid: number;
	exe: string;
	state: 'active' | 'inactive';
	excluded: boolean;
}

export interface PlaybackDevice {
	id: string;
	name: string;
	isDefault: boolean;
	sessions: Array<PlaybackDeviceSession>;
}

export declare function listPlaybackDevices(): Promise<Array<PlaybackDevice>>;
