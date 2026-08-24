// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import Messages from '@app/features/messaging/state/MessagingMessages';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Navigation from '@app/features/navigation/state/Navigation';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {type JumpType, JumpTypes} from '@fluxer/constants/src/JumpConstants';

interface MessageJumpOptions {
	flash?: boolean;
	offset?: number;
	returnToMessageId?: string;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
	viewContext?: 'favorites';
}

function resolveGuildId(channelId: string, viewContext: MessageJumpOptions['viewContext']): string {
	if (viewContext === 'favorites') return FAVORITES_GUILD_ID;
	const guildId = Channels.getChannel(channelId)?.guildId;
	return guildId && guildId !== ME ? guildId : ME;
}

function resolveReturnGuildId(
	returnChannelId: string | null | undefined,
	explicitGuildId: string | null | undefined,
): string | null | undefined {
	if (explicitGuildId !== undefined) return explicitGuildId;
	if (!returnChannelId || Navigation.channelId === returnChannelId) return Navigation.guildId;
	return Channels.getChannel(returnChannelId)?.guildId ?? Navigation.guildId;
}

function resolveReturnChannelId(
	returnToMessageId: string | undefined,
	explicitChannelId: string | null | undefined,
): string | null | undefined {
	if (!returnToMessageId) return undefined;
	if (explicitChannelId != null) return explicitChannelId;
	return Navigation.messageId === returnToMessageId ? Navigation.channelId : undefined;
}

function resolveSameChannelReturnChannelId(
	targetChannelId: string,
	returnToMessageId: string | undefined,
	explicitReturnChannelId: string | null | undefined,
): string | undefined {
	const returnChannelId = resolveReturnChannelId(returnToMessageId, explicitReturnChannelId);
	return returnChannelId === targetChannelId ? returnChannelId : undefined;
}

export function goToMessage(channelId: string, messageId: string, options?: MessageJumpOptions): void {
	const isSameChannel = Navigation.channelId === channelId;
	const guildId = resolveGuildId(channelId, options?.viewContext);
	const returnChannelId = resolveSameChannelReturnChannelId(
		channelId,
		options?.returnToMessageId,
		options?.returnChannelId,
	);
	const returnToMessageId = returnChannelId ? options?.returnToMessageId : undefined;
	const returnGuildId = returnToMessageId ? resolveReturnGuildId(returnChannelId, options?.returnGuildId) : undefined;
	const dispatch = {
		messageId,
		flash: options?.flash ?? true,
		offset: options?.offset,
		returnToMessageId,
		returnChannelId,
		returnGuildId,
		jumpType: isSameChannel ? (options?.jumpType ?? JumpTypes.ANIMATED) : JumpTypes.INSTANT,
	};
	if (isSameChannel && Navigation.messageId === messageId) {
		MessageCommands.jumpToMessage({channelId, ...dispatch});
		return;
	}
	Messages.setPendingJumpDispatch(channelId, dispatch);
	NavigationCommands.navigateToMessage(guildId, channelId, messageId, isSameChannel ? 'replace' : 'push');
}

export function parseMessagePath(path: string): {
	channelId: string;
	messageId: string;
} | null {
	const parts = path.split('/').filter(Boolean);
	if (parts.length < 4) return null;
	if (parts[0] !== 'channels') return null;
	const channelId = parts[2];
	const messageId = parts[3];
	if (!channelId || !messageId) return null;
	return {channelId, messageId};
}
