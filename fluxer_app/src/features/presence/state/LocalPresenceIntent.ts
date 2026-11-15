// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountPresenceIntent} from '@app/features/auth/state/AccountStorageContract';

interface LocalPresenceIntentHandler {
	captureIntent(): AccountPresenceIntent | null;
	restoreIntent(intent: AccountPresenceIntent | null | undefined): void;
}

type PendingIntent = {readonly intent: AccountPresenceIntent | null | undefined};

let handler: LocalPresenceIntentHandler | null = null;
let pending: PendingIntent | null = null;

export function captureLocalPresenceIntent(): AccountPresenceIntent | null {
	return handler?.captureIntent() ?? null;
}

export function restoreLocalPresenceIntent(intent: AccountPresenceIntent | null | undefined): void {
	if (handler === null) {
		pending = {intent};
		return;
	}
	handler.restoreIntent(intent);
}

export function registerLocalPresenceIntentHandler(next: LocalPresenceIntentHandler): void {
	handler = next;
	const held = pending;
	pending = null;
	if (held !== null) next.restoreIntent(held.intent);
}
