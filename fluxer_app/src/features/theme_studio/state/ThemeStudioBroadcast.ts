// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import type {ThemePreferenceSnapshot} from '@app/features/theme/state/Theme';
import {useEffect} from 'react';

const CHANNEL_NAME = 'fluxer-theme-studio';
const logger = new Logger('ThemeStudioBroadcast');

export type ThemeStudioBroadcastInput =
	| {type: 'customThemeCss'; value: string | null}
	| {type: 'themeLibrary'; revision: number}
	| {type: 'themePreference'; snapshot: ThemePreferenceSnapshot}
	| {type: 'studio:opened-popout'}
	| {type: 'studio:closed-popout'}
	| {type: 'studio:close-popout'}
	| {type: 'studio:focus-popout'};
export type ThemeStudioBroadcastMessage = ThemeStudioBroadcastInput & {origin: string};

const senderId = randomUuid();

let channel: BroadcastChannel | null = null;

const listeners = new Set<(message: ThemeStudioBroadcastMessage) => void>();
const ensureChannel = (): BroadcastChannel | null => {
	if (channel) return channel;
	if (typeof BroadcastChannel === 'undefined') {
		logger.debug('BroadcastChannel not supported in this environment');
		return null;
	}
	try {
		channel = new BroadcastChannel(CHANNEL_NAME);
		channel.onmessage = (event: MessageEvent<ThemeStudioBroadcastMessage>) => {
			const message = event.data;
			if (!message || message.origin === senderId) return;
			for (const listener of listeners) {
				try {
					listener(message);
				} catch (error) {
					logger.warn('Listener threw on broadcast message', error);
				}
			}
		};
	} catch (error) {
		logger.warn('Failed to create BroadcastChannel', error);
		channel = null;
	}
	return channel;
};
export const broadcastSenderId = senderId;

export function broadcastThemeStudioMessage(message: ThemeStudioBroadcastInput): void {
	const ch = ensureChannel();
	if (!ch) return;
	try {
		ch.postMessage({...message, origin: senderId});
	} catch (error) {
		logger.warn('Failed to broadcast theme studio message', error);
	}
}

export function subscribeThemeStudioBroadcast(listener: (message: ThemeStudioBroadcastMessage) => void): () => void {
	ensureChannel();
	listeners.add(listener);
	return () => {
		listeners.delete(listener);
	};
}

export function useThemeStudioBroadcast(listener: (message: ThemeStudioBroadcastMessage) => void): void {
	useEffect(() => {
		const dispose = subscribeThemeStudioBroadcast(listener);
		return dispose;
	}, [listener]);
}
