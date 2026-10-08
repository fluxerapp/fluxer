// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';

interface TaggedUserLike {
	username: string;
	discriminator: string;
	bot?: boolean | null;
}

const ZERO_DISCRIMINATOR_RE = /^0+$/;

export function shouldShowDiscriminator(
	user: Pick<TaggedUserLike, 'discriminator' | 'bot'>,
	uniqueUsernames: boolean = RuntimeConfig.usesUniqueUsernames,
): boolean {
	if (!uniqueUsernames || user.bot) return true;
	return !ZERO_DISCRIMINATOR_RE.test(user.discriminator);
}

export function formatUserTag(user: TaggedUserLike, uniqueUsernames?: boolean): string {
	return shouldShowDiscriminator(user, uniqueUsernames) ? `${user.username}#${user.discriminator}` : user.username;
}
