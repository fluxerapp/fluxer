// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceRoomStore} from '@app/api/infrastructure/VoiceRoomStore';

export type IVoiceRoomStore = Pick<VoiceRoomStore, 'pinRoomServer' | 'getPinnedRoomServer' | 'deleteRoomServer'>;
