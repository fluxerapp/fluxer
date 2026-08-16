// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {useEffect, useRef} from 'react';

export function useChannelSearchVisibility(channelId: string | null, visible: boolean): void {
	const previousVisibilityRef = useRef({channelId, visible});
	useEffect(() => {
		const previousVisibility = previousVisibilityRef.current;
		previousVisibilityRef.current = {channelId, visible};
		if (!channelId) return;
		if (previousVisibility.channelId === channelId && previousVisibility.visible === visible) return;
		ComponentDispatch.dispatch('LAYOUT_RESIZED', {channelId});
	}, [channelId, visible]);
}
