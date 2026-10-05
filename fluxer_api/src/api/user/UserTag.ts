// SPDX-License-Identifier: AGPL-3.0-or-later

import {usesUniqueUsernames} from '@app/api/instance/AccountIdentityModeCache';

export const USERNAME_MODE_DISCRIMINATOR = 0;

interface TaggedUser {
	username: string;
	discriminator: number;
	isBot: boolean;
}

export function hasFixedDiscriminator(user: Pick<TaggedUser, 'isBot'>): boolean {
	return !user.isBot && usesUniqueUsernames();
}

export function hidesDiscriminator(user: Pick<TaggedUser, 'discriminator' | 'isBot'>): boolean {
	return user.discriminator === USERNAME_MODE_DISCRIMINATOR && hasFixedDiscriminator(user);
}

export function formatUserTag(user: TaggedUser): string {
	if (hidesDiscriminator(user)) return user.username;
	return `${user.username}#${user.discriminator.toString().padStart(4, '0')}`;
}
