// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import Permission from '@app/features/permissions/state/Permission';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment, MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {useEffect, useState} from 'react';

export interface ThreadStarterPayload {
	content: string;
	nonce: string;
	attachments: Array<MessageAttachment>;
	hasAttachments: boolean;
	stickers: Array<MessageStickerItem>;
	favoriteMemeId?: string;
}

export function sendThreadStarter(thread: Channel, payload: ThreadStarterPayload): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser || !MessageCommands.reserveSend(thread.id, payload.nonce)) return;
	const message = MessageSubmitUtils.createOptimisticMessage(
		{
			content: payload.content,
			channelId: thread.id,
			nonce: payload.nonce,
			currentUser,
			stickers: payload.stickers,
			favoriteMemeId: payload.favoriteMemeId,
		},
		payload.attachments,
	);
	MessageCommands.createOptimistic(thread.id, message.toJSON());
	void MessageCommands.send(thread.id, {
		content: message.content,
		nonce: payload.nonce,
		hasAttachments: payload.attachments.length > 0 || payload.hasAttachments,
		allowedMentions: {replied_user: true},
		flags: message.flags,
		stickers: payload.stickers,
		favoriteMemeId: payload.favoriteMemeId,
	});
}

function useCooldownSeconds(until: number): number {
	const [now, setNow] = useState(() => Date.now());
	useEffect(() => {
		if (until <= Date.now()) return;
		const timer = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(timer);
	}, [until]);
	return Math.max(0, Math.ceil((until - now) / 1000));
}
export function resolveStarterArgs(
	stickersOrTts?: Array<MessageStickerItem> | boolean,
	favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
	maybeFavoriteMemeId?: string,
): {stickers: Array<MessageStickerItem>; favoriteMemeId?: string} {
	if (typeof stickersOrTts === 'boolean') {
		return {
			stickers: Array.isArray(favoriteMemeIdOrStickers) ? favoriteMemeIdOrStickers : [],
			favoriteMemeId: maybeFavoriteMemeId,
		};
	}
	return {
		stickers: stickersOrTts ?? [],
		favoriteMemeId: typeof favoriteMemeIdOrStickers === 'string' ? favoriteMemeIdOrStickers : undefined,
	};
}

export function useThreadCreateCooldownSeconds(parent: Channel): number {
	return useCooldownSeconds(
		Permission.can(Permissions.BYPASS_SLOWMODE, parent) ? 0 : ThreadPanel.getCreateCooldownUntil(parent.id),
	);
}
