// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {logger} from '@app/features/voice/engine/voice_screen_share_manager/shared';

export type VoiceEngineV2AppScreenShareEntryVerdict = 'proceed' | 'share-pending';

export interface VoiceEngineV2AppScreenShareEntryGuardArgs {
	readonly pending: {
		readonly active: boolean;
		readonly debugMessage: string;
		readonly onBlocked?: () => void;
	};
}

export function guardScreenShareEntry(
	args: VoiceEngineV2AppScreenShareEntryGuardArgs,
): VoiceEngineV2AppScreenShareEntryVerdict {
	assert.ok(args !== null && typeof args === 'object', 'screen-share entry guard args must be an object');
	if (args.pending !== undefined) {
		assert.equal(typeof args.pending.active, 'boolean', 'pending guard active flag must be a boolean');
		assert.ok(args.pending.debugMessage.length > 0, 'pending guard debug message must not be empty');
		if (args.pending.active) {
			args.pending.onBlocked?.();
			logger.debug(args.pending.debugMessage);
			return 'share-pending';
		}
	}
	return 'proceed';
}
