// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Channels from '@app/features/channel/state/Channels';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import Permission from '@app/features/permissions/state/Permission';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

const INTENT_STORAGE_KEY = 'Gifts:chatSendIntent';
const INTENT_MAX_AGE_MS = 30 * 60 * 1000;
const CREATED_AT_SKEW_MS = 5 * 60 * 1000;

interface ChatGiftSendIntent {
	readonly channelId: string;
	readonly startedAt: number;
	readonly knownCodes: ReadonlyArray<string>;
}

let delivering = false;

function readIntent(): ChatGiftSendIntent | null {
	const stored = AppStorage.getJSON<Partial<ChatGiftSendIntent>>(INTENT_STORAGE_KEY);
	if (typeof stored?.channelId !== 'string' || typeof stored.startedAt !== 'number') return null;
	if (Date.now() - stored.startedAt > INTENT_MAX_AGE_MS) {
		AppStorage.removeItem(INTENT_STORAGE_KEY);
		return null;
	}
	const knownCodes = Array.isArray(stored.knownCodes)
		? stored.knownCodes.filter((code) => typeof code === 'string')
		: [];
	return {channelId: stored.channelId, startedAt: stored.startedAt, knownCodes};
}

export function beginChatGiftSend(channelId: string): void {
	const startedAt = Date.now();
	AppStorage.setJSON(INTENT_STORAGE_KEY, {channelId, startedAt, knownCodes: []});
	void GiftCommands.fetchUserGifts()
		.then((gifts) => {
			if (readIntent()?.startedAt !== startedAt) return;
			AppStorage.setJSON(INTENT_STORAGE_KEY, {channelId, startedAt, knownCodes: gifts.map((gift) => gift.code)});
		})
		.catch(() => {});
}

export function clearChatGiftSend(): void {
	AppStorage.removeItem(INTENT_STORAGE_KEY);
}

export function hasPendingChatGiftSend(): boolean {
	return readIntent() !== null;
}

function canSendTo(channelId: string): boolean {
	const channel = Channels.getChannel(channelId);
	if (!channel) return false;
	return channel.guildId == null || Permission.can(Permissions.SEND_MESSAGES, channel);
}

export async function deliverPurchasedChatGift(): Promise<boolean> {
	const intent = readIntent();
	const currentUser = Users.getCurrentUser();
	if (intent === null || currentUser === undefined || delivering) return false;
	delivering = true;
	try {
		const known = new Set(intent.knownCodes);
		const gifts = await GiftCommands.fetchUserGifts();
		const purchased = gifts.find(
			(gift) =>
				!known.has(gift.code) &&
				gift.redeemed_at === null &&
				gift.created_by.id === currentUser.id &&
				Date.parse(gift.created_at) >= intent.startedAt - CREATED_AT_SKEW_MS,
		);
		if (!purchased || readIntent()?.startedAt !== intent.startedAt) return false;
		AppStorage.removeItem(INTENT_STORAGE_KEY);
		if (!canSendTo(intent.channelId)) return true;
		const nonce = fromTimestamp(Date.now());
		if (!MessageCommands.reserveSend(intent.channelId, nonce)) return true;
		const message = MessageSubmitUtils.createOptimisticMessage(
			{
				content: `${RuntimeConfig.giftEndpoint}/${purchased.code}`,
				channelId: intent.channelId,
				nonce,
				currentUser,
			},
			[],
		);
		MessageCommands.createOptimistic(intent.channelId, message.toJSON());
		void MessageCommands.send(intent.channelId, {content: message.content, nonce, flags: message.flags});
		return true;
	} catch {
		return false;
	} finally {
		delivering = false;
	}
}
