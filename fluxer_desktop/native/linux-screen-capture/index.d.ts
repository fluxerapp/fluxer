// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export type LinuxScreenCaptureSourceKind = 'screen' | 'window' | 'game';

export interface LinuxScreenCaptureSource {
	kind: LinuxScreenCaptureSourceKind;
	id: string;
	name: string;
	width: number;
	height: number;
	appName?: string;
	bundleId?: string;
	targetPid?: number;
}

export declare function listSources(): Promise<Array<LinuxScreenCaptureSource>>;

export interface LinuxScreenCaptureCapabilities {
	process: boolean;
	system: boolean;
}

export interface LinuxScreenCaptureAvailability {
	available: boolean;
	backend: 'linux-pipewire-portal';
	reason?: string;
	detail?: string;
	portalVersion?: number;
	capabilities: LinuxScreenCaptureCapabilities;
}

export declare function getAvailability(): Promise<LinuxScreenCaptureAvailability>;

export interface LinuxScreenCaptureBackendInfo {
	backend: 'linux-pipewire-portal';
	supported: boolean;
	reason: string;
	portalVersion?: number;
	pipewireReachable: boolean;
}

export declare function getBackendInfo(): LinuxScreenCaptureBackendInfo;

export interface ScreenCaptureRect {
	x: number;
	y: number;
	width: number;
	height: number;
}

export interface ScreenCaptureOptions {
	sourceId: string;
	sourceKind: LinuxScreenCaptureSourceKind;
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
	pixelFormat: 'nv12';
}

export interface LinuxScreenCaptureDiagnostics {
	backend?: string;
	activeStrategy?: string;
	requestedInjectionMethod?: string;
	injectionMethod?: string;
	lastFallbackReason?: string;
	frameTransport?: 'gpu-dmabuf-requested' | 'host-mapped-cpu-nv12-with-source-dmabuf';
	hostMappedCpuFallback?: boolean;
	sourceDmabufMetadataAvailable?: boolean;
	requestedImportMode?: string;
	importMode?: string;
	mapHost?: boolean;
	noModifiers?: boolean;
	linear?: boolean;
	zeroCopy?: boolean;
	gpuImportAvailable?: boolean;
	deviceUuidAdvertised?: boolean;
	supportedImportModes?: Array<string>;
	clientConnected?: boolean;
	connectedClient?: string;
	connectedPid?: number;
	sourceId?: string;
	sourceKind?: LinuxScreenCaptureSourceKind;
	width?: number;
	height?: number;
	textureFormat?: string;
	textureModifier?: string;
	frameCounter?: number;
	droppedFrameCounter?: number;
	laggedFrameCounter?: number;
	convertQueueDroppedFrameCounter?: number;
	unsupportedFrameCounter?: number;
	lastPresentTimestampUs?: number;
	lastDiagnostic?: string;
	lastAddonError?: string;
}

export declare const loadError: Error | null;

export declare function __setBindingForTests(binding: unknown): void;

export declare interface ScreenCapture {
	on(event: 'error', listener: (err: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'stalled', listener: (message?: string) => void): this;
	on(event: 'diagnostic', listener: (message?: string) => void): this;
	on(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	off(event: 'error', listener: (err: Error) => void): this;
	off(event: 'closed', listener: () => void): this;
	off(event: 'stalled', listener: (message?: string) => void): this;
	off(event: 'diagnostic', listener: (message?: string) => void): this;
	off(event: string | symbol, listener: (...args: Array<unknown>) => void): this;
	emit(event: 'error', err: Error): boolean;
	emit(event: 'closed'): boolean;
	emit(event: 'stalled', message?: string): boolean;
	emit(event: 'diagnostic', message?: string): boolean;
}

export declare class ScreenCapture extends EventEmitter {
	constructor(options: ScreenCaptureOptions);

	start(): Promise<ScreenCaptureStartResult>;

	getDiagnostics(): LinuxScreenCaptureDiagnostics | null;

	stop(): Promise<void>;
}
