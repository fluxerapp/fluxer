/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ElectronAPI} from '../src-electron/common/types';

type MediaEngineStoreInstance = typeof import('~/stores/voice/MediaEngineFacade').default;
type NodeBufferConstructor = typeof import('buffer').Buffer;

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
		readonly PUBLIC_API_ENDPOINT?: string;
		readonly PUBLIC_API_ENDPOINT_PROXY?: string | null;
		readonly PUBLIC_API_VERSION?: string;
		readonly PUBLIC_GATEWAY_ENDPOINT?: string;
		readonly PUBLIC_MEDIA_PROXY_ENDPOINT?: string;
		readonly PUBLIC_BUILD_SHA?: string;
		readonly PUBLIC_BUILD_NUMBER?: string;
		readonly PUBLIC_BUILD_TIMESTAMP?: string;
		readonly PUBLIC_PROJECT_ENV?: 'stable' | 'canary' | 'development';
		readonly PUBLIC_SENTRY_DSN?: string;
		readonly PUBLIC_SENTRY_PROJECT_ID?: string;
		readonly PUBLIC_SENTRY_PUBLIC_KEY?: string;
		readonly PUBLIC_SENTRY_PROXY_PATH?: string;
		readonly PUBLIC_CAPTCHA_PRIMARY_PROVIDER?: 'hcaptcha' | 'turnstile' | 'none';
		readonly PUBLIC_HCAPTCHA_SITE_KEY?: string;
		readonly PUBLIC_TURNSTILE_SITE_KEY?: string;
		readonly PUBLIC_INVITE_ENDPOINT?: string;
		readonly PUBLIC_GIFT_ENDPOINT?: string;
		readonly PUBLIC_MARKETING_ENDPOINT?: string;
		readonly PUBLIC_ADMIN_ENDPOINT?: string;
		readonly PUBLIC_CDN_ENDPOINT?: string;
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
		__notificationStoreCleanup?: () => void;
		_mediaEngineStore?: MediaEngineStoreInstance;
		electron?: ElectronAPI;
		MSStream?: unknown;
		webkitAudioContext?: typeof AudioContext;
		showSaveFilePicker?: (options?: SaveFilePickerOptions) => Promise<FileSystemFileHandle>;
	}

	interface GlobalThis {
		Buffer?: NodeBufferConstructor;
	}
}

export {};
