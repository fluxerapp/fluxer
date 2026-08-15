// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as CodeLinkUtils from '@app/features/messaging/utils/CodeLinkUtils';

const OFFICIAL_GIFT_URL_BASES = Object.freeze([
	'https://fluxer.app/gift',
	'https://canary.fluxer.app/gift',
	'https://web.fluxer.app/gift',
	'https://web.canary.fluxer.app/gift',
	'https://fluxer.gift',
	'https://fluxer.gift/gift',
]);
const GIFT_CONFIG: CodeLinkUtils.CodeLinkConfig = {
	path: 'gift',
	get urlBases() {
		if (RuntimeConfig.isSelfHosted()) {
			return [RuntimeConfig.giftUrlBase];
		}
		return [RuntimeConfig.giftUrlBase, ...OFFICIAL_GIFT_URL_BASES];
	},
};

export function findGifts(content: string | null): Array<string> {
	return CodeLinkUtils.findCodes(content, GIFT_CONFIG);
}

export function findSpoileredGifts(content: string | null): Array<CodeLinkUtils.CodeLinkMatch> {
	return CodeLinkUtils.findSpoileredCodeMatches(content, GIFT_CONFIG);
}
