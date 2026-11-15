// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {User} from '@app/api/models/User';
import {ATTACHMENT_MAX_SIZE_BOT, ATTACHMENT_MAX_SIZE_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

export function resolveAttachmentMaxFileSize(
	snapshot: LimitConfigSnapshot | null | undefined,
	{user, guildFeatures}: {user: User | null; guildFeatures?: Iterable<string> | null},
): number {
	const ctx = createLimitMatchContext({user, guildFeatures});
	const evaluationContext = guildFeatures ? 'guild' : 'user';
	const maxFileSize = Math.floor(
		resolveLimitSafe(snapshot, ctx, 'max_attachment_file_size', ATTACHMENT_MAX_SIZE_NON_PREMIUM, evaluationContext),
	);
	if (!user?.isBot) {
		return maxFileSize;
	}
	const maxBotFileSize = Math.floor(
		resolveLimitSafe(snapshot, ctx, 'max_bot_attachment_file_size', ATTACHMENT_MAX_SIZE_BOT, evaluationContext),
	);
	return Math.min(maxFileSize, maxBotFileSize);
}
