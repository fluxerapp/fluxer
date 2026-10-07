// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCallback, useRef, useState} from 'react';

export interface MessagesBottomBarVisibilityState {
	channelId: string | null;
	visible: boolean;
}

interface MessagesBottomBarVisibilityReport {
	channelId: string;
	visible: boolean;
}

const initialMessagesBottomBarVisibilityState: MessagesBottomBarVisibilityState = {
	channelId: null,
	visible: false,
};

function getMessagesBottomBarVisibleForChannel(state: MessagesBottomBarVisibilityState, channelId: string): boolean {
	return state.channelId === channelId && state.visible;
}

function getNextMessagesBottomBarVisibilityState(
	state: MessagesBottomBarVisibilityState,
	report: MessagesBottomBarVisibilityReport,
	activeChannelId: string,
): MessagesBottomBarVisibilityState {
	if (report.channelId !== activeChannelId) return state;
	if (state.channelId === report.channelId && state.visible === report.visible) return state;
	return {
		channelId: report.channelId,
		visible: report.visible,
	};
}

export function useMessagesBottomBarVisibility(channelId: string) {
	const activeChannelIdRef = useRef(channelId);
	activeChannelIdRef.current = channelId;
	const [state, setState] = useState<MessagesBottomBarVisibilityState>(initialMessagesBottomBarVisibilityState);
	const onBottomBarVisibilityChange = useCallback(
		(visible: boolean) => {
			setState((currentState) =>
				getNextMessagesBottomBarVisibilityState(
					currentState,
					{
						channelId,
						visible,
					},
					activeChannelIdRef.current,
				),
			);
		},
		[channelId],
	);
	return {
		hasMessagesBottomBar: getMessagesBottomBarVisibleForChannel(state, channelId),
		onBottomBarVisibilityChange,
	};
}
