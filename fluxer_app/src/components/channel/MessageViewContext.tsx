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

import React from 'react';
import type {MessagePreviewContext} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';

export interface MessagePreviewOverrides {
	usernameColor?: string;
	displayName?: string;
}

export interface MessageViewContextValue {
	channel: ChannelRecord;
	message: MessageRecord;
	shouldGroup: boolean;
	isHovering: boolean;
	previewContext?: keyof typeof MessagePreviewContext;
	previewOverrides?: MessagePreviewOverrides;
	handleDelete: (bypassConfirm?: boolean) => void;
	onPopoutToggle?: (isOpen: boolean) => void;
}

const MessageViewContext = React.createContext<MessageViewContextValue | null>(null);

export const MessageViewContextProvider = MessageViewContext.Provider;

export const useMessageViewContext = (): MessageViewContextValue => {
	const context = React.useContext(MessageViewContext);
	if (!context) {
		throw new Error('useMessageViewContext must be used within a MessageViewContextProvider');
	}
	return context;
};

export const useMaybeMessageViewContext = (): MessageViewContextValue | null => React.useContext(MessageViewContext);
