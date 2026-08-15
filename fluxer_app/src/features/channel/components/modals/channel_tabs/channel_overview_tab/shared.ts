// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRtcRegion} from '@app/features/channel/commands/ChannelCommands';
import type {TriggerType} from '@app/features/messaging/hooks/useTextareaAutocomplete';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {VOICE_CHANNEL_BITRATE_MAX, VOICE_CHANNEL_BITRATE_MIN} from '@fluxer/constants/src/LimitConstants';

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
export const BITRATE_KBPS_MAX = VOICE_CHANNEL_BITRATE_MAX / 1000;
export const BITRATE_KBPS_MARKERS: ReadonlyArray<number> = [BITRATE_KBPS_MIN, 64, 128, 256, BITRATE_KBPS_MAX];
export const MAX_TOPIC_LENGTH = 1024;
export const TOPIC_AUTOCOMPLETE_TRIGGERS: Array<TriggerType> = ['emoji', 'mention', 'channel'];

export interface RtcRegionOption extends ComboboxOption<string | null> {
	region: ChannelRtcRegion | null;
}
