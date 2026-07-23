// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import React, {useContext} from 'react';

interface MessagePreviewOverrides {
	usernameColor?: string;
	displayName?: string;
}

export interface MessagePreviewPermissions {
	isDM: boolean;
	canSendMessages: boolean;
	canAddReactions: boolean;
	canEditMessage: boolean;
	canDeleteMessage: boolean;
	canDeleteAttachment: boolean;
	canPinMessage: boolean;
	canForwardMessage: boolean;
	canCreateThread: boolean;
	canSuppressEmbeds: boolean;
	shouldRenderSuppressEmbeds: boolean;
}

export interface MessageViewContextValue {
	channel: Channel;
	message: Message;
	shouldGroup: boolean;
	isHovering: boolean;
	messageDisplayCompact: boolean;
	previewContext?: keyof typeof MessagePreviewContext;
	previewOverrides?: MessagePreviewOverrides;
	previewPermissions?: MessagePreviewPermissions;
	handleDelete: (bypassConfirm?: boolean) => void;
	onPopoutToggle?: (isOpen: boolean) => void;
	readonlyPreview?: boolean;
	onHeadingActivate?: () => void;
}

const MessageViewContext = React.createContext<MessageViewContextValue | null>(null);
export const MessageViewContextProvider = MessageViewContext.Provider;
export const useMessageViewContext = (): MessageViewContextValue => {
	const context = useContext(MessageViewContext);
	if (!context) {
		throw new Error('useMessageViewContext must be used within a MessageViewContextProvider');
	}
	return context;
};
export const useMaybeMessageViewContext = (): MessageViewContextValue | null => useContext(MessageViewContext);
