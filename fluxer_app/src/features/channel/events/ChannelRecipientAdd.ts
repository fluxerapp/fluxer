// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface ChannelRecipientAddPayload {
	channel_id: string;
	user: UserPartial;
}

export function handleChannelRecipientAdd(data: ChannelRecipientAddPayload, _context: GatewayHandlerContext): void {
	Channels.handleChannelRecipientAdd({
		channelId: data.channel_id,
		user: data.user,
	});
	QuickSwitcher.recomputeIfOpen();
}
