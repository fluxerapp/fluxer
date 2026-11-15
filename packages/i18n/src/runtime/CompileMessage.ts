// SPDX-License-Identifier: AGPL-3.0-or-later

import type MessageFormat from '@messageformat/core';

type CompiledMessage = (variables: Record<string, unknown>) => unknown;

const compiledMessagesByFormat = new WeakMap<MessageFormat, Map<string, CompiledMessage>>();

export function compileMessage(messageFormat: MessageFormat, template: string): CompiledMessage {
	let compiledMessages = compiledMessagesByFormat.get(messageFormat);
	if (!compiledMessages) {
		compiledMessages = new Map();
		compiledMessagesByFormat.set(messageFormat, compiledMessages);
	}
	let compiledMessage = compiledMessages.get(template);
	if (!compiledMessage) {
		compiledMessage = messageFormat.compile(template);
		compiledMessages.set(template, compiledMessage);
	}
	return compiledMessage;
}
