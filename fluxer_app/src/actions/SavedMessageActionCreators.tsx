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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes} from '~/Constants';
import {MaxBookmarksModal} from '~/components/alerts/MaxBookmarksModal';
import {Endpoints} from '~/Endpoints';
import http, {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {SavedMessageEntryRecord, type SavedMessageEntryResponse} from '~/records/SavedMessageEntryRecord';
import SavedMessagesStore from '~/stores/SavedMessagesStore';

const logger = new Logger('SavedMessages');

export const fetch = async (): Promise<Array<SavedMessageEntryRecord>> => {
	try {
		logger.debug('Fetching saved messages');
		const response = await http.get<Array<SavedMessageEntryResponse>>({url: Endpoints.USER_SAVED_MESSAGES});
		const data = response.body ?? [];
		const entries = data.map(SavedMessageEntryRecord.fromResponse);
		SavedMessagesStore.fetchSuccess(entries);
		logger.debug(`Successfully fetched ${entries.length} saved messages`);
		return entries;
	} catch (error) {
		SavedMessagesStore.fetchError();
		logger.error('Failed to fetch saved messages:', error);
		throw error;
	}
};

export const create = async (i18n: I18n, channelId: string, messageId: string): Promise<void> => {
	try {
		logger.debug(`Saving message ${messageId} from channel ${channelId}`);
		await http.post({url: Endpoints.USER_SAVED_MESSAGES, body: {channel_id: channelId, message_id: messageId}});
		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Added to bookmarks`),
		});
		logger.debug(`Successfully saved message ${messageId}`);
	} catch (error) {
		logger.error(`Failed to save message ${messageId}:`, error);

		if (
			error instanceof HttpError &&
			typeof error.body === 'object' &&
			error.body != null &&
			'code' in error.body &&
			(error.body as {code?: string}).code === APIErrorCodes.MAX_BOOKMARKS
		) {
			ModalActionCreators.push(modal(() => <MaxBookmarksModal />));
			return;
		}

		throw error;
	}
};

export const remove = async (i18n: I18n, messageId: string): Promise<void> => {
	try {
		SavedMessagesStore.handleMessageDelete(messageId);
		logger.debug(`Removing message ${messageId} from saved messages`);
		await http.delete({url: Endpoints.USER_SAVED_MESSAGE(messageId)});
		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Removed from bookmarks`),
		});
		logger.debug(`Successfully removed message ${messageId} from saved messages`);
	} catch (error) {
		logger.error(`Failed to remove message ${messageId} from saved messages:`, error);
		throw error;
	}
};
