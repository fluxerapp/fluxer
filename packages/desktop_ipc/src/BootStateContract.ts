// SPDX-License-Identifier: AGPL-3.0-or-later

export const BOOT_STATE_FILE_VERSION = 2;
export const BOOT_STATE_PAYLOAD_VERSION = 7;
export const BOOT_STATE_FORMAT_META_NAME = 'fluxer-boot-state-format';
export const BOOT_STATE_MAX_PAYLOAD_BYTES = 48 * 1024 * 1024;
export const BOOT_STATE_EXPECTED_ATTRIBUTE = 'data-boot-state';
export const BOOT_STATE_EXPECTED_VALUE = 'expected';
export const RELOAD_COVER_ELEMENT_ID = 'fluxer-reload-cover';

export const DESKTOP_BOOT_STATE_CHANNELS = Object.freeze({
	read: 'desktop-boot-state:read',
	issueWriteToken: 'desktop-boot-state:issue-write-token',
	write: 'desktop-boot-state:write',
	clear: 'desktop-boot-state:clear',
	flushRequested: 'desktop-boot-state:flush-requested',
	flushed: 'desktop-boot-state:flushed',
	captureFrame: 'desktop-boot-state:capture-frame',
	takeFrame: 'desktop-boot-state:take-frame',
	framePainted: 'desktop-boot-state:frame-painted',
	frameDropped: 'desktop-boot-state:frame-dropped',
	replayInput: 'desktop-boot-state:replay-input',
} as const);

export const BOOT_FRAME_MAX_REPLAYED_INPUTS = 256;
export const BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH = 1024;
export const BOOT_FRAME_INPUT_HELD_ATTRIBUTE = 'data-boot-frame-input-held';
export const BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE = 'data-boot-frame-layout-moved';
export const BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE = 'data-boot-frame-wheel-layout-stable';
export const BOOT_FRAME_TEXT_TARGET_SELECTOR = '[data-channel-textarea]';

export type DesktopBootFrameModifier = 'shift' | 'control' | 'alt' | 'meta' | 'isautorepeat';

export type DesktopBootFrameInput =
	| {
			readonly kind: 'key';
			readonly type: 'keyDown' | 'keyUp';
			readonly keyCode: string;
			readonly text: string | null;
			readonly modifiers: ReadonlyArray<DesktopBootFrameModifier>;
	  }
	| {
			readonly kind: 'mouse';
			readonly type: 'mouseDown' | 'mouseUp';
			readonly x: number;
			readonly y: number;
			readonly button: 'left' | 'middle' | 'right';
			readonly clickCount: number;
			readonly modifiers: ReadonlyArray<DesktopBootFrameModifier>;
	  }
	| {
			readonly kind: 'wheel';
			readonly x: number;
			readonly y: number;
			readonly deltaX: number;
			readonly deltaY: number;
			readonly modifiers: ReadonlyArray<DesktopBootFrameModifier>;
	  }
	| {
			readonly kind: 'text';
			readonly text: string;
			readonly modifiers: ReadonlyArray<DesktopBootFrameModifier>;
	  };

const PAYLOAD_MAGIC = [0x46, 0x58, 0x42, 0x50] as const;
const PAYLOAD_FORMAT_HEADER_BYTES = 8;

export function bootStatePayloadFormat(schemaEpoch: number): string {
	return `${BOOT_STATE_PAYLOAD_VERSION}.${schemaEpoch}`;
}

export function readBootStatePayloadFormat(payload: Uint8Array): string | null {
	if (
		payload.byteLength < PAYLOAD_FORMAT_HEADER_BYTES ||
		PAYLOAD_MAGIC.some((byte, index) => payload[index] !== byte)
	) {
		return null;
	}
	const view = new DataView(payload.buffer, payload.byteOffset, PAYLOAD_FORMAT_HEADER_BYTES);
	return `${view.getUint16(4, true)}.${view.getUint16(6, true)}`;
}

export type DesktopBootStateReadResult =
	| {readonly status: 'hit'; readonly payload: Uint8Array; readonly writtenAt: number}
	| {readonly status: 'miss'}
	| {readonly status: 'unavailable'};

export interface DesktopBootStateWriteRequest {
	readonly accountKey: string;
	readonly writeToken: string;
	readonly payload: Uint8Array;
	readonly frameId: string | null;
}

export type DesktopBootStateWriteResult = 'written' | 'revoked' | 'too-large' | 'unavailable' | 'failed';

export interface DesktopBootStateFrameRequest {
	readonly accountKey: string;
	readonly writeToken: string;
	readonly captureAppCheck: boolean;
	readonly systemDark: boolean | null;
}

export const BOOT_FRAME_ID_PATTERN = /^[0-9a-f]{32}$/u;

export type DesktopBootStateFrameResult =
	| {readonly status: 'captured'; readonly frameId: string}
	| {readonly status: 'skipped' | 'revoked' | 'unavailable' | 'failed'};

export interface DesktopBootFrame {
	readonly jpeg: Uint8Array;
	readonly pixelWidth: number;
	readonly pixelHeight: number;
	readonly viewportWidth: number;
	readonly viewportHeight: number;
	readonly systemDark: boolean | null;
}

export interface DesktopBootStateFlushOptions {
	readonly frame: boolean;
}

export interface DesktopBootStateAPI {
	read(accountKey: string): Promise<DesktopBootStateReadResult>;
	issueWriteToken(accountKey: string): Promise<string | null>;
	write(request: DesktopBootStateWriteRequest): Promise<DesktopBootStateWriteResult>;
	clear(accountKey: string): Promise<void>;
	captureFrame(request: DesktopBootStateFrameRequest): Promise<DesktopBootStateFrameResult>;
	onFlushRequested(handler: (options: DesktopBootStateFlushOptions) => Promise<void>): () => void;
}
