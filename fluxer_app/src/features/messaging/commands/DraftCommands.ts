// SPDX-License-Identifier: AGPL-3.0-or-later

import Drafts from '@app/features/messaging/state/MessagingDrafts';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {getAppStorageScope, UNAUTHENTICATED_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('Draft');

export type DraftOwner = string | null;

type DraftCommand =
	| {kind: 'create'; channelId: string; content: string; segments?: ReadonlyArray<MentionSegment> | null}
	| {kind: 'delete'; channelId: string};

function dispatchDraftCommand(command: DraftCommand): void {
	if (command.kind === 'create') {
		Drafts.createDraft(command.channelId, command.content, command.segments);
		return;
	}
	Drafts.deleteDraft(command.channelId);
}

export function ownsActiveDrafts(owner: DraftOwner): boolean {
	return getAppStorageScope() === (owner ?? UNAUTHENTICATED_APP_STORAGE_SCOPE);
}

export function createDraft(
	owner: DraftOwner,
	channelId: string,
	content: string,
	segments?: ReadonlyArray<MentionSegment> | null,
): boolean {
	if (!ownsActiveDrafts(owner)) {
		logger.debug(`Dropping a draft for channel ${channelId} owned by another account`);
		return false;
	}
	logger.debug(`Creating draft for channel ${channelId}`);
	dispatchDraftCommand({kind: 'create', channelId, content, segments});
	return true;
}

export function deleteDraft(owner: DraftOwner, channelId: string): boolean {
	if (!ownsActiveDrafts(owner)) {
		logger.debug(`Keeping the draft for channel ${channelId} owned by another account`);
		return false;
	}
	logger.debug(`Deleting draft for channel ${channelId}`);
	dispatchDraftCommand({kind: 'delete', channelId});
	return true;
}
