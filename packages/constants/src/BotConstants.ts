// SPDX-License-Identifier: AGPL-3.0-or-later

import {PublicUserFlags} from '@fluxer/constants/src/UserConstants';

export const BotFlags = {
	FRIENDLY_BOT: PublicUserFlags.FRIENDLY_BOT,
	FRIENDLY_BOT_MANUAL_APPROVAL: PublicUserFlags.FRIENDLY_BOT_MANUAL_APPROVAL,
} as const;
export const BotFlagsDescriptions: Record<keyof typeof BotFlags, string> = {
	FRIENDLY_BOT: 'Bot accepts friend requests from users',
	FRIENDLY_BOT_MANUAL_APPROVAL: 'Bot requires manual approval for friend requests',
};
