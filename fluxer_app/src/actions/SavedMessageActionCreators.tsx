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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {MaxBookmarksModal} from '@app/components/alerts/MaxBookmarksModal';
import {Endpoints} from '@app/Endpoints';
import http from '@app/lib/HttpClient';
import {Logger} from '@app/lib/Logger';
import {type SavedMessageEntry, SavedMessageEntryRecord} from '@app/records/SavedMessageEntryRecord';
import SavedMessagesStore from '@app/stores/SavedMessagesStore';
import UserStore from '@app/stores/UserStore';
import {getApiErrorCode} from '@app/utils/ApiErrorUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const logger = new Logger('SavedMessages');

export async function fetch(): Promise<Array<SavedMessageEntryRecord>> {
	try {
		logger.debug('Fetching saved messages');
		const response = await http.get<Array<SavedMessageEntry>>({url: Endpoints.USER_SAVED_MESSAGES});
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
}

export async function create(i18n: I18n, channelId: string, messageId: string): Promise<void> {
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

		if (getApiErrorCode(error) === APIErrorCodes.MAX_BOOKMARKS) {
			const currentUser = UserStore.currentUser;
			if (!currentUser) {
				throw error;
			}
			ModalActionCreators.push(modal(() => <MaxBookmarksModal user={currentUser} />));
			return;
		}

		throw error;
	}
}

export async function remove(i18n: I18n, messageId: string): Promise<void> {
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
}
