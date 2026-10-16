// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {
	ThreadPermissionFlags,
	type ThreadWriteAction,
	threadWriteBlock,
} from '@fluxer/constants/src/ThreadPermissionUtils';

export function assertThreadInteractionAllowed(
	authChannel: AuthenticatedChannel,
	action: ThreadWriteAction,
	{ignoreTimeout = false}: {ignoreTimeout?: boolean} = {},
): void {
	if (!authChannel.thread) return;
	const {actor} = authChannel.thread;
	const effective =
		ignoreTimeout && actor.timedOut
			? {...actor, timedOut: false, permissions: actor.permissions & ~ThreadPermissionFlags.MANAGE_THREADS}
			: actor;
	assertThreadAllowed(threadWriteBlock(action, effective));
}
