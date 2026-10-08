// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {buildMessagePlaintextCopyText} from '@app/features/messaging/utils/MessagePlaintextCopyUtils';
import {buildMessageSelectionCopyText} from '@app/features/messaging/utils/MessageSelectionCopyUtils';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface MessageSelectionCopyOptions {
	getMessagePlaintext: (messageId: string) => string | null;
}

type MessageGetter = (messageId: string) => Message | null | undefined;

function useMessageSelectionCopy<TElement extends HTMLElement>({
	getMessagePlaintext,
}: MessageSelectionCopyOptions): React.ClipboardEventHandler<TElement> {
	return useCallback(
		(event: React.ClipboardEvent<TElement>) => {
			if (!event.clipboardData) {
				return;
			}
			const rootElement = event.currentTarget;
			const selection = rootElement.ownerDocument.defaultView?.getSelection() ?? null;
			const clipboardText = buildMessageSelectionCopyText({
				rootElement,
				selection,
				getMessagePlaintext,
			});
			if (clipboardText === null) {
				return;
			}
			event.preventDefault();
			event.clipboardData.setData('text/plain', clipboardText);
		},
		[getMessagePlaintext],
	);
}

export function useMessageSelectionCopyForMessageGetter<TElement extends HTMLElement>(
	getMessage: MessageGetter,
): React.ClipboardEventHandler<TElement> {
	const {i18n} = useLingui();
	const getMessagePlaintext = useCallback(
		(messageId: string) => {
			const message = getMessage(messageId);
			return message ? buildMessagePlaintextCopyText(message, i18n) : null;
		},
		[getMessage, i18n],
	);
	return useMessageSelectionCopy<TElement>({getMessagePlaintext});
}

export function useMessageSelectionCopyForMessages<TElement extends HTMLElement>(
	messages: ReadonlyArray<Message>,
): React.ClipboardEventHandler<TElement> {
	const messagesById = useMemo(() => {
		const next = new Map<string, Message>();
		for (const message of messages) {
			next.set(message.id, message);
		}
		return next;
	}, [messages]);
	const getMessage = useCallback((messageId: string) => messagesById.get(messageId) ?? null, [messagesById]);
	return useMessageSelectionCopyForMessageGetter<TElement>(getMessage);
}
