// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {getUnreadChannels} from '@app/features/app/components/floating/UnreadChannelsContent';
import {CANCEL_DESCRIPTOR, MARK_AS_READ_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {InboxTab} from '@app/features/inbox/state/Inbox';
import Inbox from '@app/features/inbox/state/Inbox';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const MARK_INBOX_AS_READ_DESCRIPTOR = msg({
	message: 'Mark inbox as read?',
	comment: 'Confirmation prompt in the inbox commands.',
});
const THIS_WILL_MARK_ALL_UNREAD_INBOX_CHANNELS_AS_DESCRIPTOR = msg({
	message: 'Mark every unread channel as read.',
	comment: 'Description text in the inbox commands.',
});
const DON_T_ASK_ME_AGAIN_DESCRIPTOR = msg({
	message: "Don't ask me again",
	comment: 'Label in the inbox commands.',
});

export function setTab(tab: InboxTab): void {
	Inbox.setTab(tab);
}

export function closeInboxAndFocusChannelTextarea(channelId: string): void {
	Popout.close('inbox');
	focusChannelTextareaAfterNavigation(channelId);
}

export function revealBookmarksPopoutForFirstSave(): boolean {
	if (MobileLayout.isMobileLayout()) {
		return false;
	}
	if (!Inbox.shouldAutoOpenBookmarksPopoutForFirstSave()) {
		return false;
	}
	const isInboxOpen = Popout.isOpen('inbox');
	const canOpenInbox = isInboxOpen || ComponentBus.hasSubscribers('INBOX_OPEN');
	if (!canOpenInbox) {
		return false;
	}
	Inbox.setTab('bookmarks');
	if (!isInboxOpen) {
		ComponentBus.dispatch('INBOX_OPEN');
	}
	Inbox.markBookmarksPopoutAutoOpenedForFirstSave();
	return true;
}

export function markAllInboxChannelsAsRead(i18n: I18n): void {
	const channelIds = getUnreadChannels().map((channel) => channel.id);
	if (channelIds.length === 0) return;
	if (Inbox.skipMarkAllAsReadConfirmation) {
		void ReadStateCommands.bulkAckChannels(channelIds);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(MARK_INBOX_AS_READ_DESCRIPTOR)}
				description={i18n._(THIS_WILL_MARK_ALL_UNREAD_INBOX_CHANNELS_AS_DESCRIPTOR)}
				primaryText={i18n._(MARK_AS_READ_DESCRIPTOR)}
				secondaryText={i18n._(CANCEL_DESCRIPTOR)}
				checkboxContent={
					<Checkbox data-flx="inbox.inbox-commands.mark-all-inbox-channels-as-read.checkbox">
						{i18n._(DON_T_ASK_ME_AGAIN_DESCRIPTOR)}
					</Checkbox>
				}
				onPrimary={(checkboxChecked = false) => {
					if (checkboxChecked) {
						Inbox.setSkipMarkAllAsReadConfirmation(true);
					}
					void ReadStateCommands.bulkAckChannels(channelIds);
				}}
				data-flx="inbox.inbox-commands.mark-all-inbox-channels-as-read.confirm-modal"
			/>
		)),
	);
}
