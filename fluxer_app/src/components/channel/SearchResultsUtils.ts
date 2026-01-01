/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {MessageRecord} from '~/records/MessageRecord';
import type {SearchSegment} from '~/utils/SearchSegmentManager';

export interface SearchMachineStateIdle {
	status: 'idle';
}

export interface SearchMachineStateLoading {
	status: 'loading';
}

export interface SearchMachineStateIndexing {
	status: 'indexing';
	pollCount: number;
}

export interface SearchMachineStateSuccess {
	status: 'success';
	results: Array<MessageRecord>;
	total: number;
	hitsPerPage: number;
	page: number;
}

export interface SearchMachineStateError {
	status: 'error';
	error: string;
}

export type SearchMachineState =
	| SearchMachineStateIdle
	| SearchMachineStateLoading
	| SearchMachineStateIndexing
	| SearchMachineStateSuccess
	| SearchMachineStateError;

export const cloneMessageRecord = (message: MessageRecord): MessageRecord => {
	return new MessageRecord(message.toJSON(), {skipUserCache: true});
};

export const cloneMessageResults = (messages: Array<MessageRecord>): Array<MessageRecord> => {
	return messages.map(cloneMessageRecord);
};

export const cloneMachineState = (machineState: SearchMachineState): SearchMachineState => {
	if (machineState.status !== 'success') {
		return machineState;
	}

	return {
		...machineState,
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
