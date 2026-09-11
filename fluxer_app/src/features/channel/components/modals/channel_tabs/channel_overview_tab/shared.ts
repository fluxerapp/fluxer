// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRtcRegion} from '@app/features/channel/commands/ChannelCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {getMaxVoiceChannelBitrate} from '@fluxer/constants/src/GuildConstants';
import {VOICE_CHANNEL_BITRATE_DEFAULT, VOICE_CHANNEL_BITRATE_MIN} from '@fluxer/constants/src/LimitConstants';

export interface FormInputs {
	name: string;
	topic?: string;
	url?: string;
	slowmode?: number;
	nsfw_override: boolean | null;
	content_warning_level: number;
	content_warning_text: string;
	bitrate?: number;
	user_limit?: number;
	voice_connection_limit?: number;
	rtc_region: string | null;
}

export const CHANNEL_OVERVIEW_TAB_ID = 'overview';
export const BITRATE_KBPS_MIN = VOICE_CHANNEL_BITRATE_MIN / 1000;
export const BITRATE_KBPS_DEFAULT = VOICE_CHANNEL_BITRATE_DEFAULT / 1000;

export function getMaxBitrateKbps(guildFeatures: Iterable<string> | null | undefined): number {
	return getMaxVoiceChannelBitrate(guildFeatures) / 1000;
}

export function getBitrateKbpsMarkers(maxKbps: number): Array<number> {
	const markers = [BITRATE_KBPS_MIN, BITRATE_KBPS_DEFAULT, 128, 256].filter((marker) => marker < maxKbps);
	markers.push(maxKbps);
	return markers;
}
export const MAX_TOPIC_LENGTH = 1024;

export interface RtcRegionOption extends ComboboxOption<string | null> {
	region: ChannelRtcRegion | null;
}
