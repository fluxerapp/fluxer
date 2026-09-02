// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Room} from 'livekit-client';
import {createContext, useContext} from 'react';

export const VoiceRoomContext = createContext<Room | undefined>(undefined);

export function useMaybeVoiceRoom(): Room | undefined {
	return useContext(VoiceRoomContext);
}
