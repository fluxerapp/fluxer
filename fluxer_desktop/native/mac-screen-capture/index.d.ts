// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export interface MacScreenCaptureBackendInfo {
	backend: string;
	supported: boolean;
	reason: string;
	minMacosVersion: string;
	detectedMacosVersion?: string;
	sckAvailable: boolean;
}

export declare function getBackendInfo(): MacScreenCaptureBackendInfo;

export interface MacScreenCaptureBackendAvailability {
	sck?: {
		supported: boolean;
		macosVersion?: string;
	};
	screenPermission?: string;
}

export declare function getBackendAvailability(): Promise<MacScreenCaptureBackendAvailability>;

export type MacScreenCaptureSourceKind = 'screen' | 'window';

export interface MacScreenCaptureSource {
	kind: MacScreenCaptureSourceKind;
	id: string;
	name: string;
	width: number;
	height: number;
	appName?: string;
	bundleId?: string;
	targetPid?: number;
}

export declare function listSources(): Promise<Array<MacScreenCaptureSource>>;

export type MacScreenRecordingProbeResult = 'granted' | 'denied' | 'timeout' | 'unsupported';

export declare function probeScreenRecordingAccess(): Promise<MacScreenRecordingProbeResult>;

export interface ScreenCaptureRect {
	x: number;
	y: number;
	width: number;
	height: number;
}

export interface ScreenCaptureOptions {
	sourceId: string;
	sourceKind: MacScreenCaptureSourceKind;
	width?: number;
	height?: number;
	frameRate?: number;
	captureId?: string;
	colorRange?: 'full' | 'limited';
	colorSpace?: 'rec709' | 'srgb';
	showCursorClicks?: boolean;
	captureRect?: ScreenCaptureRect;
	frameSinkHandle?: unknown;
	nativeFrameSinkRequired?: boolean;
}

export interface ScreenCaptureStartResult {
	width: number;
	height: number;
	frameRate: number;
	pixelFormat: 'nv12' | 'bgra';
}

export interface FrameSinkDiagnostics {
	accepted: number;
	coalesced: number;
	rejected: number;
	mediaFramesDroppedWithoutSink: number;
}

export declare const loadError: Error | null;

export declare function __setBindingForTests(binding: unknown): void;

export declare interface ScreenCapture {
	on(event: 'error', listener: (err: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'diagnostic', listener: (message?: string) => void): this;
	on(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	off(event: 'error', listener: (err: Error) => void): this;
	off(event: 'closed', listener: () => void): this;
	off(event: 'diagnostic', listener: (message?: string) => void): this;
	off(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	emit(event: 'error', err: Error): boolean;
	emit(event: 'closed'): boolean;
	emit(event: 'diagnostic', message?: string): boolean;
}

export declare class ScreenCapture extends EventEmitter {
	constructor(options: ScreenCaptureOptions);

	start(): Promise<ScreenCaptureStartResult>;

	stop(): Promise<void>;

	attachEncoder(width: number, height: number, frameRate?: number): void;
	detachEncoder(): void;
	isEncoderAttached(): boolean;
	encoderRingFullCount(): number;
	getFrameSinkDiagnostics(): FrameSinkDiagnostics;
}
