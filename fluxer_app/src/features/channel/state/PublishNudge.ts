// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessagingMessages from '@app/features/messaging/state/MessagingMessages';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {AnnouncementPromptsStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

function isPublishCandidate(message: Message): boolean {
	return (
		message.isCurrentUserAuthor() &&
		message.state === MessageStates.SENT &&
		message.type === MessageTypes.DEFAULT &&
		!message.isCrosspostCopy
	);
}

class PublishNudge {
	hidePublishNudge = false;
	dismissedMessageIds = new Set<string>();

	constructor() {
		makeAutoObservable(this, {shouldShow: false}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'announcementPrompts',
			schema: AnnouncementPromptsStateSchema,
			persist: ['hidePublishNudge'],
			toMessage: (s) => ({hidePublishNudge: s.hidePublishNudge}),
			applyMessage: (s, m) => {
				s.hidePublishNudge = m.hidePublishNudge;
			},
		});
	}

	dismiss(messageId: string): void {
		this.dismissedMessageIds.add(messageId);
	}

	hideForever(): void {
		this.hidePublishNudge = true;
	}

	shouldShow(message: Message, canCrosspost: boolean): boolean {
		if (this.hidePublishNudge || !canCrosspost || message.isCrossposted) {
			return false;
		}
		if (!isPublishCandidate(message) || this.dismissedMessageIds.has(message.id)) {
			return false;
		}
		void MessagingMessages.version;
		if (!MessagingMessages.hasNewestMessages(message.channelId)) {
			return false;
		}
		const newestOwn = MessagingMessages.getCachedMessages(message.channelId)?.searchFromNewest(isPublishCandidate);
		return newestOwn?.id === message.id;
	}
}

export default new PublishNudge();
