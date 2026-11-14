// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ContentWarningChannelLike,
	computeEffectiveChannelNsfw,
	computeEffectiveContentWarning,
} from '@app/api/channel/utils/EffectiveContentWarning';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {FollowTargetContentWarningRequiredError} from '@fluxer/errors/src/domains/channel/FollowTargetContentWarningRequiredError';
import {FollowTargetNotAgeRestrictedError} from '@fluxer/errors/src/domains/channel/FollowTargetNotAgeRestrictedError';

type ContentWarningGuildLike = Parameters<typeof computeEffectiveChannelNsfw>[2];

export type CrosspostContentRuleResult = 'ok' | 'nsfw' | 'content_warning';

export interface CrosspostContentRuleInput {
	source: ContentWarningChannelLike;
	sourceParent: ContentWarningChannelLike | null;
	sourceGuild: ContentWarningGuildLike;
	target: ContentWarningChannelLike;
	targetParent: ContentWarningChannelLike | null;
	targetGuild: ContentWarningGuildLike;
}

export function checkCrosspostContentRules(input: CrosspostContentRuleInput): CrosspostContentRuleResult {
	const sourceNsfw = computeEffectiveChannelNsfw(input.source, input.sourceParent, input.sourceGuild);
	const targetNsfw = computeEffectiveChannelNsfw(input.target, input.targetParent, input.targetGuild);
	if (sourceNsfw && !targetNsfw) {
		return 'nsfw';
	}
	const sourceWarning = computeEffectiveContentWarning(input.source, input.sourceParent, input.sourceGuild);
	if (sourceWarning.level !== ContentWarningLevel.CONTENT_WARNING) {
		return 'ok';
	}
	if (targetNsfw) {
		return 'ok';
	}
	const targetWarning = computeEffectiveContentWarning(input.target, input.targetParent, input.targetGuild);
	return targetWarning.level === ContentWarningLevel.CONTENT_WARNING ? 'ok' : 'content_warning';
}

export function assertCrosspostContentRules(input: CrosspostContentRuleInput): void {
	const result = checkCrosspostContentRules(input);
	if (result === 'nsfw') {
		throw new FollowTargetNotAgeRestrictedError();
	}
	if (result === 'content_warning') {
		throw new FollowTargetContentWarningRequiredError();
	}
}
