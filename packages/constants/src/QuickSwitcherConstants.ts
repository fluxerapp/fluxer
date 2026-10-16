// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const QuickSwitcherResultTypes = {
	HEADER: 'header',
	USER: 'user',
	GROUP_DM: 'group_dm',
	TEXT_CHANNEL: 'text_channel',
	VOICE_CHANNEL: 'voice_channel',
	GUILD: 'guild',
	VIRTUAL_GUILD: 'virtual_guild',
	SETTINGS: 'settings',
	LINK: 'link',
} as const;

export type QuickSwitcherResultType = ValueOf<typeof QuickSwitcherResultTypes>;
