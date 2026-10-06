// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchMachineState} from '@app/features/channel/components/SearchStateMachine';
import {Channel} from '@app/features/channel/models/Channel';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';

export type {
	SearchMachineEvent,
	SearchMachineSnapshot,
	SearchMachineState,
	SearchMachineStateError,
	SearchMachineStateIdle,
	SearchMachineStateIndexing,
	SearchMachineStateLoading,
	SearchMachineStateSuccess,
} from '@app/features/channel/components/SearchStateMachine';

export const cloneMessageRecord = (message: Message): Message => {
	return new Message(message.toJSON(), {skipUserCache: true});
};
export const cloneMessageResults = (messages: Array<Message>): Array<Message> => {
	return messages.map(cloneMessageRecord);
};
export const cloneChannelRecord = (channel: Channel): Channel => {
	return new Channel(channel.toJSON(), {instanceId: channel.instanceId});
};
export const cloneChannelResults = (channels: Array<Channel>): Array<Channel> => {
	return channels.map(cloneChannelRecord);
};
export const cloneMachineState = (machineState: SearchMachineState): SearchMachineState => {
	if (machineState.status !== 'success') {
		return machineState;
	}
	return {
		...machineState,
		channels: cloneChannelResults(machineState.channels),
		results: cloneMessageResults(machineState.results),
	};
};
export const areSegmentsEqual = (current: Array<SearchSegment>, next: Array<SearchSegment>): boolean => {
	if (current.length !== next.length) {
		return false;
	}
	for (let index = 0; index < current.length; index += 1) {
		const a = current[index];
		const b = next[index];
		if (
			a.type !== b.type ||
			a.filterKey !== b.filterKey ||
			a.id !== b.id ||
			a.displayText !== b.displayText ||
			a.start !== b.start ||
			a.end !== b.end
		) {
			return false;
		}
	}
	return true;
};
