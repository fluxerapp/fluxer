// SPDX-License-Identifier: AGPL-3.0-or-later

export type VideoAccelerationStatus = 'hardware' | 'software' | 'unknown';

const SOFTWARE_VIDEO_IMPLEMENTATION_PREFIXES = [
	'libaom',
	'libvpx',
	'openh264',
	'ffmpeg',
	'dav1d',
	'libdav1d',
	'libgav1',
	'svt',
	'rav1e',
	'x264',
	'x265',
];
const SOFTWARE_VIDEO_IMPLEMENTATION_TERMS = ['software'];
const HARDWARE_VIDEO_IMPLEMENTATION_TERMS = [
	'd3d11',
	'd3d12',
	'dxva',
	'mediafoundation',
	'videotoolbox',
	'vaapi',
	'v4l2',
	'vdpau',
	'nvenc',
	'nvdec',
	'cuvid',
	'cuda',
	'qsv',
	'quick sync',
	'amf',
	'videocore',
];

function isSoftwareVideoImplementation(name: string): boolean {
	const lower = name.toLowerCase();
	return (
		SOFTWARE_VIDEO_IMPLEMENTATION_PREFIXES.some((prefix) => lower.startsWith(prefix)) ||
		SOFTWARE_VIDEO_IMPLEMENTATION_TERMS.some((term) => lower.includes(term))
	);
}

function isHardwareVideoImplementation(name: string): boolean {
	const lower = name.toLowerCase();
	return HARDWARE_VIDEO_IMPLEMENTATION_TERMS.some((term) => lower.includes(term));
}

function classifyVideoAcceleration(
	implementation: string | null | undefined,
	powerEfficient: boolean | null | undefined,
): VideoAccelerationStatus {
	if (implementation) {
		if (isSoftwareVideoImplementation(implementation)) return 'software';
		if (isHardwareVideoImplementation(implementation)) return 'hardware';
	}
	if (powerEfficient === true) return 'hardware';
	if (powerEfficient === false) return 'software';
	return 'unknown';
}

export function classifyVideoEncoderAcceleration(
	implementation: string | null | undefined,
	powerEfficientEncoder: boolean | null | undefined,
): VideoAccelerationStatus {
	return classifyVideoAcceleration(implementation, powerEfficientEncoder);
}

export function classifyVideoDecoderAcceleration(
	implementation: string | null | undefined,
	powerEfficientDecoder: boolean | null | undefined,
): VideoAccelerationStatus {
	return classifyVideoAcceleration(implementation, powerEfficientDecoder);
}
