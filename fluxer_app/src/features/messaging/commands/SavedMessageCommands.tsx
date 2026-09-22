// SPDX-License-Identifier: AGPL-3.0-or-later

import {MaxBookmarksModal} from '@app/features/app/components/alerts/MaxBookmarksModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import {SavedMessageEntry, type SavedMessageEntryWire} from '@app/features/messaging/models/SavedMessageEntry';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SAVED_MESSAGES_PAGE_SIZE} from '@fluxer/constants/src/LimitConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ADDED_TO_BOOKMARKS_DESCRIPTOR = msg({
	message: 'Added to bookmarks',
	comment: 'Button or menu action label in the messaging commands. Keep it concise.',
});
const REMOVED_FROM_BOOKMARKS_DESCRIPTOR = msg({
	message: 'Removed from bookmarks',
	comment: 'Button or menu action label in the messaging commands. Keep it concise. Keep the tone plain and specific.',
});
const logger = new Logger('SavedMessages');

interface SaveMessageRequest {
	channel_id: string;
	message_id: string;
}

interface SavedMessagesFetchOptions {
	before?: string;
}

async function requestSavedMessages(options: SavedMessagesFetchOptions = {}): Promise<Array<SavedMessageEntryWire>> {
	const response = await http.get<Array<SavedMessageEntryWire>>(Endpoints.USER_SAVED_MESSAGES, {
		query: {limit: SAVED_MESSAGES_PAGE_SIZE, ...options},
	});
	return response.body ?? [];
}

function savedMessageEntries(data: Array<SavedMessageEntryWire>): Array<SavedMessageEntry> {
	return data.map(SavedMessageEntry.fromResponse);
}

function saveMessageRequest(channelId: string, messageId: string): SaveMessageRequest {
	return {
		channel_id: channelId,
		message_id: messageId,
	};
}

async function requestSavedMessageCreate(channelId: string, messageId: string): Promise<void> {
	await http.post(Endpoints.USER_SAVED_MESSAGES, {body: saveMessageRequest(channelId, messageId)});
}

async function requestSavedMessageRemove(messageId: string): Promise<void> {
	await http.delete(Endpoints.USER_SAVED_MESSAGE(messageId));
}

function showBookmarkToast(children: string): void {
	ToastCommands.createToast({
		type: 'success',
		children,
	});
}

function showMaxBookmarksModal(): boolean {
	const currentUser = Users.currentUser;
	if (!currentUser) {
		return false;
	}
	ModalCommands.push(
		modal(() => (
			<MaxBookmarksModal
				user={currentUser}
				data-flx="messaging.saved-message-commands.show-max-bookmarks-modal.max-bookmarks-modal"
			/>
		)),
	);
	return true;
}

async function runSavedMessagesFetch(
	label: string,
	append: boolean,
	options: SavedMessagesFetchOptions = {},
): Promise<Array<SavedMessageEntry>> {
	const requestId = SavedMessages.handleFetchPending();
	try {
		logger.debug(label);
		const entries = savedMessageEntries(await requestSavedMessages(options));
		SavedMessages.fetchSuccess(requestId, entries, append);
		logger.debug(`Successfully fetched ${entries.length} saved messages`);
		return entries;
	} catch (error) {
		SavedMessages.fetchError(requestId, append);
		logger.error(`${label} failed:`, error);
		throw error;
	}
}

export async function fetch(): Promise<Array<SavedMessageEntry>> {
	return runSavedMessagesFetch('Fetching saved messages', false);
}

export async function loadMore(): Promise<Array<SavedMessageEntry>> {
	const before = SavedMessages.getCursor();
	if (!before || !SavedMessages.getHasMore() || SavedMessages.getIsLoadingMore()) {
		return [];
	}
	return runSavedMessagesFetch(`Loading more saved messages before ${before}`, true, {before});
}

export async function create(i18n: I18n, channelId: string, messageId: string): Promise<void> {
	try {
		logger.debug(`Saving message ${messageId} from channel ${channelId}`);
		await requestSavedMessageCreate(channelId, messageId);
		showBookmarkToast(i18n._(ADDED_TO_BOOKMARKS_DESCRIPTOR));
		InboxCommands.revealBookmarksPopoutForFirstSave();
		logger.debug(`Successfully saved message ${messageId}`);
	} catch (error) {
		logger.error(`Failed to save message ${messageId}:`, error);
		if (failureCode(error) === APIErrorCodes.MAX_BOOKMARKS) {
			if (showMaxBookmarksModal()) return;
		}
		throw error;
	}
}

export async function remove(i18n: I18n, messageId: string): Promise<void> {
	try {
		SavedMessages.handleMessageDelete(messageId);
		logger.debug(`Removing message ${messageId} from saved messages`);
		await requestSavedMessageRemove(messageId);
		showBookmarkToast(i18n._(REMOVED_FROM_BOOKMARKS_DESCRIPTOR));
		logger.debug(`Successfully removed message ${messageId} from saved messages`);
	} catch (error) {
		logger.error(`Failed to remove message ${messageId} from saved messages:`, error);
		throw error;
	}
}
