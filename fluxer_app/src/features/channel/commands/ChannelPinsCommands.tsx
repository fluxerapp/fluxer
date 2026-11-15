// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import ChannelPins from '@app/features/channel/state/ChannelPins';
import Channels from '@app/features/channel/state/Channels';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {PinFailedModal, type PinFailureReason} from '@app/features/messaging/components/alerts/PinFailedModal';
import {http} from '@app/features/platform/transport/RestTransport';
import type {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

interface ApiErrorBody {
	code?: string;
	message?: string;
}

const getApiErrorCode = (error: HttpError): string | undefined => {
	const body = typeof error?.body === 'object' && error.body !== null ? (error.body as ApiErrorBody) : undefined;
	return body?.code;
};
const logger = new Logger('Pins');
const PIN_PAGE_SIZE = 25;
const shouldBlockPinsFetch = (channelId: string): boolean => {
	const channel = Channels.getChannel(channelId);
	if (!channel || channel.isPrivate()) {
		return false;
	}
	return GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null});
};

interface ChannelPinResponse {
	message: Message;
	pinned_at: string;
}

interface ChannelPinsPayload {
	items: Array<ChannelPinResponse>;
	has_more: boolean;
}

function emptyPinsPayload(): ChannelPinsPayload {
	return {items: [], has_more: false};
}

function pinMessages(payload: ChannelPinsPayload): Array<Message> {
	return payload.items.map((pin) => pin.message);
}

function handleBlockedPinsFetch(channelId: string): Array<Message> {
	ChannelPins.handleChannelPinsFetchSuccess(channelId, [], false);
	return [];
}

async function requestChannelPins(channelId: string, before?: string): Promise<ChannelPinsPayload> {
	const response = await http.get<ChannelPinsPayload>(Endpoints.CHANNEL_PINS(channelId), {
		query: before ? {limit: PIN_PAGE_SIZE, before} : {limit: PIN_PAGE_SIZE},
	});
	return response.body ?? emptyPinsPayload();
}

function handlePinsFetchSuccess(channelId: string, payload: ChannelPinsPayload): Array<Message> {
	ChannelPins.handleChannelPinsFetchSuccess(channelId, payload.items, payload.has_more);
	return pinMessages(payload);
}

async function fetchPinsPage(channelId: string, before?: string): Promise<Array<Message>> {
	const payload = await requestChannelPins(channelId, before);
	return handlePinsFetchSuccess(channelId, payload);
}

async function requestPinMutation(action: 'pin' | 'unpin', channelId: string, messageId: string): Promise<void> {
	const endpoint = Endpoints.CHANNEL_PIN(channelId, messageId);
	if (action === 'pin') {
		await http.put(endpoint);
		return;
	}
	await http.delete(endpoint);
}

function showPinFailureModal(error: unknown, isUnpin = false): void {
	const reason = getFailureReason(error as HttpError);
	ModalCommands.push(
		modal(() => (
			<PinFailedModal
				isUnpin={isUnpin}
				reason={reason}
				data-flx="channel.channel-pins-commands.show-pin-failure-modal.pin-failed-modal"
			/>
		)),
	);
}

export async function fetch(channelId: string): Promise<Array<Message>> {
	if (shouldBlockPinsFetch(channelId)) {
		return handleBlockedPinsFetch(channelId);
	}
	ChannelPins.handleFetchPending(channelId);
	try {
		return await fetchPinsPage(channelId);
	} catch (error) {
		logger.error(`Failed to fetch pins for channel ${channelId}:`, error);
		ChannelPins.handleChannelPinsFetchError(channelId);
		return [];
	}
}

export async function loadMore(channelId: string): Promise<Array<Message>> {
	if (shouldBlockPinsFetch(channelId)) {
		return handleBlockedPinsFetch(channelId);
	}
	if (!ChannelPins.getHasMore(channelId) || ChannelPins.getIsLoading(channelId)) {
		return [];
	}
	const before = ChannelPins.getOldestPinnedAt(channelId);
	if (!before) {
		return [];
	}
	ChannelPins.handleFetchPending(channelId);
	try {
		logger.debug(`Loading more pins for channel ${channelId} before ${before}`);
		return await fetchPinsPage(channelId, before);
	} catch (error) {
		logger.error(`Failed to load more pins for channel ${channelId}:`, error);
		ChannelPins.handleChannelPinsFetchError(channelId);
		return [];
	}
}

const getFailureReason = (error: HttpError): PinFailureReason => {
	const errorCode = getApiErrorCode(error);
	if (
		errorCode === APIErrorCodes.CANNOT_SEND_MESSAGES_TO_USER ||
		errorCode === APIErrorCodes.COMMUNITY_MEMBER_DMS_DISABLED
	) {
		return 'dm_restricted';
	}
	return 'generic';
};

export async function pin(channelId: string, messageId: string): Promise<void> {
	try {
		await requestPinMutation('pin', channelId, messageId);
		logger.debug(`Pinned message ${messageId} in channel ${channelId}`);
	} catch (error) {
		logger.error(`Failed to pin message ${messageId} in channel ${channelId}:`, error);
		showPinFailureModal(error);
	}
}

export async function unpin(channelId: string, messageId: string): Promise<void> {
	try {
		await requestPinMutation('unpin', channelId, messageId);
		logger.debug(`Unpinned message ${messageId} from channel ${channelId}`);
	} catch (error) {
		logger.error(`Failed to unpin message ${messageId} from channel ${channelId}:`, error);
		showPinFailureModal(error, true);
	}
}
