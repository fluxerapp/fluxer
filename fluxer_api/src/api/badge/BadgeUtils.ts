// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import type {BadgeType} from '@fluxer/constants/src/BadgeConstants';
import {UnknownBadgeError} from '@fluxer/errors/src/domains/badge/UnknownBadgeError';

export function mapBadgeIds(badgeIds: ReadonlySet<bigint>): Array<string> | undefined {
	return badgeIds.size > 0 ? Array.from(badgeIds, String) : undefined;
}

export async function resolveBadgeIds(
	instanceConfigRepository: InstanceConfigRepository,
	type: BadgeType,
	badgeIds: ReadonlyArray<bigint>,
): Promise<Set<bigint>> {
	const {badges} = await instanceConfigRepository.getBadgeConfig();
	const known = new Set(badges.filter((badge) => badge.type === type).map((badge) => badge.id));
	if (badgeIds.some((badgeId) => !known.has(badgeId.toString()))) {
		throw new UnknownBadgeError();
	}
	return new Set(badgeIds);
}
