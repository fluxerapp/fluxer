// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentBus} from '@app/features/platform/utils/ComponentBus';

export interface ChannelComposerDismissalRequest {
	channelId: string;
}

export function requestChannelComposerAffordanceDismissal(channelId: string): boolean {
	const result = ComponentBus.dispatchToFirstResult(
		'TEXTAREA_DISMISS_AFFORDANCE',
		{channelId},
		(candidate) => candidate === true,
	);
	return result === true;
}
