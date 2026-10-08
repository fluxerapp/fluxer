// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Invites from '@app/features/invite/state/Invites';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import Permission from '@app/features/permissions/state/Permission';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import * as PiPCommands from '@app/features/ui/commands/PiPCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import Webhooks from '@app/features/webhook/state/Webhooks';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

export interface ChannelDeletePayload {
	id: string;
	type: number;
	guild_id?: string;
}

export function cleanupChannelLocalState(channel: Channel): void {
	const guildId = channel.guild_id;
	PiPCommands.clearPiPForChannel(channel.id);
	MediaEngine.handleChannelDelete(channel.id);
	Slowmode.deleteChannel(channel.id);
	Drafts.deleteChannelDraft(channel.id);
	SavedMessages.handleChannelDelete(channel);
	ChannelPins.handleChannelDelete(channel);
	Channels.handleChannelDelete({channel});
	Permission.handleChannelDelete(channel.id, guildId);
	GuildReadState.handleChannelDelete(channel.id);
	Invites.handleChannelDelete(channel.id, currentInstanceTarget());
	Webhooks.handleChannelDelete(channel.id);
	ReadStates.handleChannelDelete({channel});
	SelectedChannel.handleChannelDelete(channel);
	MentionFeed.handleChannelDelete(channel);
}

export function handleChannelDelete(data: ChannelDeletePayload, _context: GatewayHandlerContext): void {
	ChannelThreads.handleParentDelete(data.id);
	cleanupChannelLocalState(data as Channel);
	Messages.handleCleanup();
	QuickSwitcher.recomputeIfOpen();
}
