// SPDX-License-Identifier: AGPL-3.0-or-later

export interface UnreadJumpAnchorInput {
	canBeUnread: boolean;
	canTrackUnreads: boolean;
	hasUnread: boolean;
	oldestUnreadMessageId: string | null;
	ackMessageId: string | null;
}

export interface UnreadJumpAnchor {
	messageId: string;
	offset: number;
}

export function resolveUnreadJumpAnchor(input: UnreadJumpAnchorInput): UnreadJumpAnchor | null {
	if (!input.canBeUnread || !input.canTrackUnreads || !input.hasUnread) {
		return null;
	}
	if (input.oldestUnreadMessageId != null) {
		return {messageId: input.oldestUnreadMessageId, offset: 0};
	}
	if (input.ackMessageId == null) {
		return null;
	}
	return {messageId: input.ackMessageId, offset: 1};
}
