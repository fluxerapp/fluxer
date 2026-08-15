// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';

export interface ChannelComposerDismissalRequest {
	channelId: string;
}

export function requestChannelComposerAffordanceDismissal(channelId: string): boolean {
	const result = ComponentDispatch.dispatchToFirstResult(
		'TEXTAREA_DISMISS_AFFORDANCE',
		{channelId},
		(candidate) => candidate === true,
	);
	return result === true;
}
