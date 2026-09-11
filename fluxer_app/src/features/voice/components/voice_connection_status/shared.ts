// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const logger = new Logger('VoiceConnectionStatus');

export const AUDIO_PROCESSING_DIRECT_INPUT_RAW_DESCRIPTOR = msg({
	message: 'Audio processing (direct input, raw)',
	comment: 'Tooltip in the voice status popout. Describes the studio / direct-input mic processing profile.',
});
export const AUDIO_PROCESSING_FOCUSED_VOICE_DESCRIPTOR = msg({
	message: 'Audio processing (focused voice)',
	comment: 'Tooltip in the voice status popout. Describes the focused-voice mic processing profile.',
});
export const AUDIO_PROCESSING_CUSTOM_ENHANCED_DESCRIPTOR = msg({
	message: 'Audio processing (custom, enhanced)',
	comment:
		'Tooltip in the voice status popout. Describes the custom mic processing profile with enhanced (DeepFilterNet3) noise suppression.',
});
export const AUDIO_PROCESSING_CUSTOM_DESCRIPTOR = msg({
	message: 'Audio processing (custom)',
	comment:
		'Tooltip in the voice status popout. Describes the custom mic processing profile with browser-built-in noise suppression.',
});
export const AUDIO_PROCESSING_CUSTOM_NO_SUPPRESSION_DESCRIPTOR = msg({
	message: 'Audio processing (custom, no suppression)',
	comment: 'Tooltip in the voice status popout. Describes the custom mic processing profile with no noise suppression.',
});
export const LATENCY_GRAPH_DESCRIPTOR = msg({
	message: 'Latency graph',
	comment: 'Aria label for the latency graph chart in the voice connection status popout.',
});
export const DEVICE_DESCRIPTOR = msg({
	message: 'Device:',
	comment: 'Field label in the voice connection status popout. Shows the connected voice region / device.',
});
export const CURRENT_PING_DESCRIPTOR = msg({
	message: 'Current ping:',
	comment: 'Field label in the voice connection status popout. Shows the latest RTT in ms.',
});
export const MEASURING_DESCRIPTOR = msg({
	message: 'Measuring…',
	comment:
		'Placeholder shown while voice latency is still being sampled. Trailing ellipsis indicates ongoing measurement.',
});
export const AVERAGE_PING_DESCRIPTOR = msg({
	message: 'Average ping:',
	comment: 'Field label in the voice connection status popout. Shows the average RTT in ms over the recent window.',
});
export const ENDPOINT_DESCRIPTOR = msg({
	message: 'Endpoint:',
	comment: 'Field label in the voice connection status popout. Shows the voice server endpoint hostname.',
});
export const COPY_ENDPOINT_DESCRIPTOR = msg({
	message: 'Copy endpoint',
	comment: 'Tooltip / button label that copies the voice endpoint hostname to the clipboard.',
});
export const SESSION_DESCRIPTOR = msg({
	message: 'Session',
	comment: 'Section heading in the voice connection status popout for current call/session debug stats.',
});
export const BANDWIDTH_DESCRIPTOR = msg({
	message: 'Bandwidth',
	comment: 'Section heading in the voice connection status popout for current media bitrate usage.',
});
export const NETWORK_DESCRIPTOR = msg({
	message: 'Network',
	comment: 'Section heading in the voice connection status popout for WebRTC network quality stats.',
});
export const DURATION_DESCRIPTOR = msg({
	message: 'Duration:',
	comment:
		'Field label in the voice connection status popout. Shows how long the current voice session has been connected.',
});
export const PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Participants:',
	comment: 'Field label in the voice connection status popout. Shows the current LiveKit participant count.',
});
export const SEND_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Send:',
	comment: 'Field label in the voice connection status popout. Shows total current outbound media bitrate.',
});
export const RECEIVE_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Receive:',
	comment: 'Field label in the voice connection status popout. Shows total current inbound media bitrate.',
});
export const AUDIO_SEND_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Audio send:',
	comment: 'Field label in the voice connection status popout. Shows current outbound audio bitrate.',
});
export const AUDIO_RECEIVE_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Audio receive:',
	comment: 'Field label in the voice connection status popout. Shows current inbound audio bitrate.',
});
export const VIDEO_SEND_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Video send:',
	comment: 'Field label in the voice connection status popout. Shows current outbound video bitrate.',
});
export const VIDEO_RECEIVE_BANDWIDTH_DESCRIPTOR = msg({
	message: 'Video receive:',
	comment: 'Field label in the voice connection status popout. Shows current inbound video bitrate.',
});
export const AUDIO_PACKET_LOSS_DESCRIPTOR = msg({
	message: 'Audio packet loss:',
	comment: 'Field label in the voice connection status popout. Shows received audio packet loss percentage.',
});
export const VIDEO_PACKET_LOSS_DESCRIPTOR = msg({
	message: 'Video packet loss:',
	comment: 'Field label in the voice connection status popout. Shows received video packet loss percentage.',
});
export const JITTER_DESCRIPTOR = msg({
	message: 'Jitter:',
	comment: 'Field label in the voice connection status popout. Shows audio jitter in milliseconds.',
});
export const OUTGOING_CAPACITY_DESCRIPTOR = msg({
	message: 'Outgoing capacity:',
	comment: 'Field label in the voice connection status popout. Shows WebRTC estimated available outgoing bandwidth.',
});
export const INCOMING_CAPACITY_DESCRIPTOR = msg({
	message: 'Incoming capacity:',
	comment: 'Field label in the voice connection status popout. Shows WebRTC estimated available incoming bandwidth.',
});
export const PUBLISHER_TRANSPORT_DESCRIPTOR = msg({
	message: 'Publisher transport:',
	comment: 'Field label in the voice connection status popout. Shows technical WebRTC publisher ICE transport details.',
});
export const SUBSCRIBER_TRANSPORT_DESCRIPTOR = msg({
	message: 'Subscriber transport:',
	comment:
		'Field label in the voice connection status popout. Shows technical WebRTC subscriber ICE transport details.',
});

function formatNumber(locale: string, value: number, maximumFractionDigits: number): string {
	return getCachedNumberFormat(locale, {maximumFractionDigits}).format(value);
}

function formatUnit(
	locale: string,
	value: number,
	unit: string,
	fallbackUnit: string,
	maximumFractionDigits: number,
): string {
	try {
		return getCachedNumberFormat(locale, {
			style: 'unit',
			unit,
			unitDisplay: 'short',
			maximumFractionDigits,
		}).format(value);
	} catch {
		return `${formatNumber(locale, value, maximumFractionDigits)} ${fallbackUnit}`;
	}
}

export function formatBitrateKbps(kbps: number, locale: string): string {
	const normalizedKbps = Number.isFinite(kbps) && kbps > 0 ? kbps : 0;
	if (normalizedKbps >= 1000) {
		return formatUnit(locale, normalizedKbps / 1000, 'megabit-per-second', 'Mb/s', 1);
	}
	return formatUnit(locale, normalizedKbps, 'kilobit-per-second', 'kb/s', 0);
}

export function formatBitrateBps(bitsPerSecond: number | null | undefined, locale: string): string | null {
	if (typeof bitsPerSecond !== 'number' || !Number.isFinite(bitsPerSecond) || bitsPerSecond <= 0) {
		return null;
	}
	return formatBitrateKbps(bitsPerSecond / 1000, locale);
}

export function formatPacketLossPercent(packetLoss: number, locale: string): string {
	const normalizedPacketLoss = Number.isFinite(packetLoss) && packetLoss > 0 ? packetLoss : 0;
	return getCachedNumberFormat(locale, {
		style: 'percent',
		maximumFractionDigits: 1,
	}).format(normalizedPacketLoss / 100);
}

export function formatMilliseconds(milliseconds: number, locale: string): string {
	const normalizedMilliseconds = Number.isFinite(milliseconds) && milliseconds > 0 ? milliseconds : 0;
	return formatUnit(locale, normalizedMilliseconds, 'millisecond', 'ms', 0);
}

export function getAudioProcessingTooltip(
	i18n: I18n,
	mode: VoiceProcessingMode,
	browserNs: boolean,
	deepFilter: boolean,
): string {
	if (mode === 'studio') return i18n._(AUDIO_PROCESSING_DIRECT_INPUT_RAW_DESCRIPTOR);
	if (mode === 'voice') return i18n._(AUDIO_PROCESSING_FOCUSED_VOICE_DESCRIPTOR);
	if (deepFilter) return i18n._(AUDIO_PROCESSING_CUSTOM_ENHANCED_DESCRIPTOR);
	if (browserNs) return i18n._(AUDIO_PROCESSING_CUSTOM_DESCRIPTOR);
	return i18n._(AUDIO_PROCESSING_CUSTOM_NO_SUPPRESSION_DESCRIPTOR);
}

export function isAudioProcessingActive(mode: VoiceProcessingMode, browserNs: boolean, deepFilter: boolean): boolean {
	return mode === 'voice' || (mode === 'custom' && (browserNs || deepFilter));
}
