// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createContext, useContext} from 'react';

interface CollapsedMessageVisibilityContextValue {
	isMessageRevealed: (message: Message) => boolean;
}

const defaultValue: CollapsedMessageVisibilityContextValue = {
	isMessageRevealed: () => false,
};
const CollapsedMessageVisibilityContext = createContext<CollapsedMessageVisibilityContextValue>(defaultValue);
export const CollapsedMessageVisibilityProvider = CollapsedMessageVisibilityContext.Provider;

export function useCollapsedMessageVisibility(): CollapsedMessageVisibilityContextValue {
	return useContext(CollapsedMessageVisibilityContext);
}
