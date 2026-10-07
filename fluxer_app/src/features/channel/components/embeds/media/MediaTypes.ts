// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';

export interface BaseMediaProps {
	nsfw?: boolean;
	channelId?: string;
	messageId?: string;
	attachmentId?: string;
	embedIndex?: number;
	message?: Message;
	contentHash?: string | null;
	onDelete?: (bypassConfirm?: boolean) => void;
}
