// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

interface OpenLinkChannelOptions {
	skipGate?: boolean;
}

function openLinkChannelDestination(channel: Channel): void {
	if (!channel.url) return;
	const inviteCode = InviteUtils.findInvite(channel.url);
	if (inviteCode) {
		void InviteCommands.openAcceptModal(inviteCode, currentInstanceTarget());
		return;
	}
	openExternalUrlWithWarning(channel.url);
}

export function openLinkChannel(channel: Channel, options: OpenLinkChannelOptions = {}): boolean {
	if (channel.type !== ChannelTypes.GUILD_LINK || !channel.url) return false;
	if (
		!options.skipGate &&
		GuildMatureContentAgree.shouldShowGate({
			channelId: channel.id,
			guildId: channel.guildId ?? null,
		})
	) {
		return false;
	}
	openLinkChannelDestination(channel);
	return true;
}
