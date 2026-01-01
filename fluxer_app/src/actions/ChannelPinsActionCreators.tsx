/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {APIErrorCodes} from '~/Constants';
import {PinFailedModal, type PinFailureReason} from '~/components/alerts/PinFailedModal';
import {Endpoints} from '~/Endpoints';
import http, {type HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Message} from '~/records/MessageRecord';
import ChannelPinsStore from '~/stores/ChannelPinsStore';

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

interface ChannelPinResponse {
	message: Message;
	pinned_at: string;
}
interface ChannelPinsPayload {
	items: Array<ChannelPinResponse>;
	has_more: boolean;
}

export const fetch = async (channelId: string) => {
	ChannelPinsStore.handleFetchPending(channelId);
	try {
		const response = await http.get<ChannelPinsPayload>({
			url: Endpoints.CHANNEL_PINS(channelId),
			query: {limit: PIN_PAGE_SIZE},
		});
		const body = response.body ?? {items: [], has_more: false};
		ChannelPinsStore.handleChannelPinsFetchSuccess(channelId, body.items, body.has_more);
		return body.items.map((pin) => pin.message);
	} catch (error) {
		logger.error(`Failed to fetch pins for channel ${channelId}:`, error);
		ChannelPinsStore.handleChannelPinsFetchError(channelId);
		return [];
	}
};

export const loadMore = async (channelId: string): Promise<Array<Message>> => {
	if (!ChannelPinsStore.getHasMore(channelId) || ChannelPinsStore.getIsLoading(channelId)) {
		return [];
	}

	const before = ChannelPinsStore.getOldestPinnedAt(channelId);
	if (!before) {
		return [];
	}

	ChannelPinsStore.handleFetchPending(channelId);
	try {
		logger.debug(`Loading more pins for channel ${channelId} before ${before}`);
		const response = await http.get<ChannelPinsPayload>({
			url: Endpoints.CHANNEL_PINS(channelId),
			query: {
				limit: PIN_PAGE_SIZE,
				before,
			},
		});
		const body = response.body ?? {items: [], has_more: false};
		ChannelPinsStore.handleChannelPinsFetchSuccess(channelId, body.items, body.has_more);
		return body.items.map((pin) => pin.message);
	} catch (error) {
		logger.error(`Failed to load more pins for channel ${channelId}:`, error);
		ChannelPinsStore.handleChannelPinsFetchError(channelId);
		return [];
	}
};

const getFailureReason = (error: HttpError): PinFailureReason => {
	const errorCode = getApiErrorCode(error);
	if (errorCode === APIErrorCodes.CANNOT_SEND_MESSAGES_TO_USER) {
		return 'dm_restricted';
	}
	return 'generic';
};

export const pin = async (channelId: string, messageId: string): Promise<void> => {
	try {
		await http.put({url: Endpoints.CHANNEL_PIN(channelId, messageId)});
		logger.debug(`Pinned message ${messageId} in channel ${channelId}`);
	} catch (error) {
		logger.error(`Failed to pin message ${messageId} in channel ${channelId}:`, error);
		const reason = getFailureReason(error as HttpError);
		ModalActionCreators.push(modal(() => <PinFailedModal reason={reason} />));
	}
};

export const unpin = async (channelId: string, messageId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.CHANNEL_PIN(channelId, messageId)});
		logger.debug(`Unpinned message ${messageId} from channel ${channelId}`);
	} catch (error) {
		logger.error(`Failed to unpin message ${messageId} from channel ${channelId}:`, error);
		const reason = getFailureReason(error as HttpError);
		ModalActionCreators.push(modal(() => <PinFailedModal isUnpin reason={reason} />));
	}
};
