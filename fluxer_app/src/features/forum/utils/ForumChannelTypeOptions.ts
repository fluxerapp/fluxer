// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelTypeOption} from '@app/features/channel/utils/ChannelCreateModalUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';

export function getForumChannelTypeOptions(i18n: I18n): Array<ChannelTypeOption> {
	return [
		{
			value: ChannelTypes.GUILD_FORUM,
			name: i18n._(D.FORUM_CHANNEL_DESCRIPTOR),
			desc: i18n._(D.FORUM_CHANNEL_DESC_DESCRIPTOR),
		},
		{
			value: ChannelTypes.GUILD_MEDIA,
			name: i18n._(D.MEDIA_CHANNEL_DESCRIPTOR),
			desc: i18n._(D.MEDIA_CHANNEL_DESC_DESCRIPTOR),
		},
	];
}
