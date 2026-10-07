// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';
import {ForumTagRequiredError} from '@fluxer/errors/src/domains/channel/ForumTagRequiredError';
import {NoTagsAvailableToNonModeratorsError} from '@fluxer/errors/src/domains/channel/NoTagsAvailableToNonModeratorsError';
import {UnknownForumTagError} from '@fluxer/errors/src/domains/channel/UnknownForumTagError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';

export function resolveAppliedTags(params: {
	config: ThreadParentConfig | null;
	requested: ReadonlyArray<bigint>;
	previous: ReadonlyArray<bigint>;
	moderator: boolean;
}): Array<bigint> {
	const available = new Map((params.config?.availableTags ?? []).map((tag) => [tag.id, tag]));
	const stale = new Set(params.previous.filter((id) => !available.has(id)));
	const requested = [...new Set(params.requested)].filter((id) => !stale.has(id));
	for (const id of requested) {
		if (!available.has(id)) throw new UnknownForumTagError();
	}
	const previous = new Set(params.previous.filter((id) => available.has(id)));
	if (!params.moderator) {
		const changed = [
			...requested.filter((id) => !previous.has(id)),
			...[...previous].filter((id) => !requested.includes(id)),
		];
		if (changed.some((id) => available.get(id)?.moderated)) throw new MissingPermissionsError();
	}
	const requireTag = ((params.config?.flags ?? 0) & ChannelFlags.REQUIRE_TAG) !== 0;
	if (requireTag && requested.length === 0) {
		if (!params.moderator && ![...available.values()].some((tag) => !tag.moderated)) {
			throw new NoTagsAvailableToNonModeratorsError();
		}
		throw new ForumTagRequiredError();
	}
	return requested;
}
