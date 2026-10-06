// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {VideoCodec} from 'livekit-client';

const BEST_COMPRESSION_USE_IT_ONLY_WHEN_WEBRTC_EXPOSES_DESCRIPTOR = msg({
	message:
		'Best compression. Use it only when WebRTC exposes an encoder and the GPU report says it is hardware-backed.',
	comment:
		'Codec description in the screen share codec policy menu (advanced voice settings). Technical surface; keep WebRTC and GPU as literal proper nouns.',
});
const STRONG_QUALITY_AT_MODERATE_CPU_COST_GOOD_CHROMIUM_DESCRIPTOR = msg({
	message: 'Strong quality at moderate CPU cost. Good Chromium fallback for screen sharing.',
	comment:
		'Codec description in the screen share codec policy menu. Technical surface; keep Chromium as a literal proper noun.',
});
const UNIVERSAL_FALLBACK_LOWEST_QUALITY_BUT_IT_WORKS_EVERYWHERE_DESCRIPTOR = msg({
	message: 'Universal fallback. Lowest quality, but it works everywhere.',
	comment: 'Codec description in the screen share codec policy menu, used for the universal fallback codec.',
});
const HARDWARE_ACCELERATED_ON_MOST_WINDOWS_AND_MACOS_SYSTEMS_DESCRIPTOR = msg({
	message: 'Hardware-accelerated on most Windows and macOS systems. Reliable for older devices.',
	comment: 'Codec description in the screen share codec policy menu. Refers to H.264.',
});
const HEVC_EFFICIENT_AT_LOW_BITRATE_BUT_ONLY_USABLE_DESCRIPTOR = msg({
	message: 'HEVC. Efficient at low bitrate, but only usable when WebRTC exposes a hardware encoder.',
	comment: 'Codec description in the screen share codec policy menu. HEVC is a codec name; keep WebRTC literal.',
});
const YOUR_GPU_ENCODES_THIS_IN_HARDWARE_DESCRIPTOR = msg({
	message: 'Your GPU encodes this in hardware.',
	comment: 'Helper text shown under a codec option indicating the local GPU has a hardware encoder for it.',
});
const YOUR_GPU_HAS_NO_HARDWARE_ENCODER_FOR_THIS_DESCRIPTOR = msg({
	message: '{productName} cannot use a hardware encoder for this, so it will use your CPU.',
	comment:
		'Helper text shown under a codec option indicating no usable hardware encoder path is available and the CPU will be used. productName is the app name.',
});
export const CODEC_DISPLAY_LABEL: Record<VideoCodec, string> = {
	av1: 'AV1',
	vp9: 'VP9',
	vp8: 'VP8',
	h264: 'H.264',
	h265: 'H.265 (HEVC)',
};

export function getSupportedCodecBlurb(codec: VideoCodec, i18n: I18n): string {
	switch (codec) {
		case 'av1':
			return i18n._(BEST_COMPRESSION_USE_IT_ONLY_WHEN_WEBRTC_EXPOSES_DESCRIPTOR);
		case 'vp9':
			return i18n._(STRONG_QUALITY_AT_MODERATE_CPU_COST_GOOD_CHROMIUM_DESCRIPTOR);
		case 'vp8':
			return i18n._(UNIVERSAL_FALLBACK_LOWEST_QUALITY_BUT_IT_WORKS_EVERYWHERE_DESCRIPTOR);
		case 'h264':
			return i18n._(HARDWARE_ACCELERATED_ON_MOST_WINDOWS_AND_MACOS_SYSTEMS_DESCRIPTOR);
		case 'h265':
			return i18n._(HEVC_EFFICIENT_AT_LOW_BITRATE_BUT_ONLY_USABLE_DESCRIPTOR);
	}
}

export function hardwareSuffix(codec: VideoCodec, gpu: HardwareEncodeReport | null, i18n: I18n): string {
	if (!gpu) return '';
	switch (gpu[codec]) {
		case 'hardware':
			return ` ${i18n._(YOUR_GPU_ENCODES_THIS_IN_HARDWARE_DESCRIPTOR)}`;
		case 'software':
			return ` ${i18n._(YOUR_GPU_HAS_NO_HARDWARE_ENCODER_FOR_THIS_DESCRIPTOR, {productName: PRODUCT_NAME})}`;
		case 'unknown':
			return '';
	}
}
