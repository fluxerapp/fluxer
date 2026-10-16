// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import UserNote from '@app/features/user/state/UserNote';

export interface UserNoteUpdatePayload {
	id: string;
	note?: string | null;
}

export function handleUserNoteUpdate(data: UserNoteUpdatePayload, _context: GatewayHandlerContext): void {
	UserNote.updateUserNote(data.id, data.note ?? '');
}
