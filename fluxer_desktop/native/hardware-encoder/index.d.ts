// SPDX-License-Identifier: AGPL-3.0-or-later

export interface HardwareEncoderCapability {
	available: boolean;
	backend: 'nvenc' | 'videotoolbox' | 'none';
	compiled: boolean;
	runtime: boolean;
	codecs: Array<string>;
	zeroCopy: boolean;
	nativeInputs: Array<'dmabuf' | 'd3d11-texture' | string>;
	reason?: string;
	detail?: string;
}

export declare function isSupported(): boolean;
export declare function getHardwareEncoderCapability(): HardwareEncoderCapability;
export declare function getHardwareEncoderCapabilities(): HardwareEncoderCapability;
