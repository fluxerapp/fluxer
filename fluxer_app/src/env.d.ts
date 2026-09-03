// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngineFacade from '@app/features/voice/engine/MediaEngineFacade';
import type {ElectronAPI} from '@app/features/platform/types/Electron';
import type {
	GeolocationResponse,
	InstanceDiscoveryResponse,
} from '@fluxer/instance_bootstrap/src/Types';
import {Buffer} from 'buffer';

type MediaEngineInstance = typeof MediaEngineFacade;
type NodeBufferConstructor = typeof Buffer;

interface FluxerDebugApi {
	getClientInfo?: () => Promise<string>;
	getClientInfoSync?: () => string;
	getClientInfoObject?: () => Promise<unknown>;
	getClientInfoObjectSync?: () => unknown;
	getVoiceSubscriptionDebug?: () => Promise<unknown>;
	getVoiceSubscriptionDebugJson?: () => Promise<string>;
}

type FluxerDebugGlobal = Record<string, unknown> & FluxerDebugApi;

interface FluxerBootstrapGlobal {
	config: {
		releaseChannel: 'stable' | 'canary';
		bootstrapApiEndpoint: string;
		bootstrapApiPublicEndpoint?: string;
	};
	instance: InstanceDiscoveryResponse;
	geoip: GeolocationResponse;
}

declare global {
	interface FilePickerAcceptType {
		description?: string;
		accept: Record<string, Array<string>>;
	}
	interface SaveFilePickerOptions {
		suggestedName?: string;
		excludeAcceptAllOption?: boolean;
		id?: string;
		types?: Array<FilePickerAcceptType>;
	}
	interface CompressionStream extends TransformStream<Uint8Array, Uint8Array> {}
	declare var CompressionStream: {
		prototype: CompressionStream;
		new (format: 'deflate' | 'gzip'): CompressionStream;
	};
	interface DecompressionStream extends TransformStream<Uint8Array, Uint8Array> {}
	declare var DecompressionStream: {
		prototype: DecompressionStream;
		new (format: 'deflate' | 'gzip'): DecompressionStream;
	};
	interface ImportMetaEnv {
		readonly MODE: 'development' | 'production' | 'test';
		readonly DEV: boolean;
		readonly PROD: boolean;
		readonly PUBLIC_BUILD_VERSION?: string;
		readonly PUBLIC_RELEASE_CHANNEL?: 'stable' | 'canary';
		readonly PUBLIC_BOOTSTRAP_API_ENDPOINT?: string;
		readonly PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT?: string;
	}
	interface ImportMetaHot {
		readonly data: Record<string, unknown>;
		accept(deps?: string | ReadonlyArray<string> | (() => void), callback?: () => void): void;
		dispose(callback: (data: Record<string, unknown>) => void): void;
	}
	interface ImportMeta {
		readonly env: ImportMetaEnv;
		readonly hot?: ImportMetaHot;
	}
	interface Navigator {
		userAgentData?: {
			platform?: string;
			mobile?: boolean;
			brands?: Array<{brand: string; version: string}>;
		};
	}
	interface Window {
		__FLUXER_BOOTSTRAP__?: FluxerBootstrapGlobal;
		__FLUXER_DEBUG__?: FluxerDebugGlobal;
		__notificationCleanup?: () => void;
		_mediaEngine?: MediaEngineInstance;
		electron?: ElectronAPI;
		MSStream?: unknown;
		webkitAudioContext?: typeof AudioContext;
		styleMedia: StyleMedia;
		showSaveFilePicker?: (options?: SaveFilePickerOptions) => Promise<FileSystemFileHandle>;
	}
	interface GlobalThis {
		Buffer?: NodeBufferConstructor;
	}
}
