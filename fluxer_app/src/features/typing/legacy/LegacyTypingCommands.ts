// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import TypingIndicator from '@app/features/typing/legacy/LegacyTypingIndicator';

const logger = new Logger('Typing');

type TypingMutation = 'start' | 'stop';

async function postTypingIndicator(channelId: string): Promise<void> {
	await http.post(Endpoints.CHANNEL_TYPING(channelId));
}

function logTypingSendFailure(channelId: string, error: unknown): void {
	logger.error(`Failed to send typing indicator to channel ${channelId}:`, error);
}

function updateTypingState(mutation: TypingMutation, channelId: string, userId: string): void {
	if (mutation === 'start') {
		TypingIndicator.startRemoteTyping(channelId, userId);
		return;
	}
	TypingIndicator.stopTyping(channelId, userId);
}

export async function sendTyping(channelId: string): Promise<void> {
	try {
		logger.debug(`Sending typing indicator to channel ${channelId}`);
		await postTypingIndicator(channelId);
		logger.debug(`Successfully sent typing indicator to channel ${channelId}`);
	} catch (error) {
		logTypingSendFailure(channelId, error);
	}
}

export function startTyping(channelId: string, userId: string): void {
	logger.debug(`Starting typing indicator for user ${userId} in channel ${channelId}`);
	updateTypingState('start', channelId, userId);
}

export function stopTyping(channelId: string, userId: string): void {
	logger.debug(`Stopping typing indicator for user ${userId} in channel ${channelId}`);
	updateTypingState('stop', channelId, userId);
}

export function startLocalTyping(channelId: string, userId: string): void {
	logger.debug(`Starting local typing indicator for user ${userId} in channel ${channelId}`);
	TypingIndicator.startLocalTyping(channelId, userId);
}

export function stopLocalTyping(channelId: string, userId: string): void {
	logger.debug(`Stopping local typing indicator for user ${userId} in channel ${channelId}`);
	TypingIndicator.stopLocalTyping(channelId, userId);
}
