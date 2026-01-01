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

import {makeAutoObservable} from 'mobx';
import type {ScheduledMessagePayload, ScheduledMessageRecord} from '~/records/ScheduledMessageRecord';

interface ScheduledMessageEditState {
	scheduledMessageId: string;
	channelId: string;
	payload: ScheduledMessagePayload;
	scheduledLocalAt: string;
	timezone: string;
}

class ScheduledMessageEditorStore {
	private state: ScheduledMessageEditState | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	startEditing(record: ScheduledMessageRecord): void {
		this.state = {
			scheduledMessageId: record.id,
			channelId: record.channelId,
			payload: record.payload,
			scheduledLocalAt: record.scheduledLocalAt,
			timezone: record.timezone,
		};
	}

	stopEditing(): void {
		this.state = null;
	}

	isEditingChannel(channelId: string): boolean {
		return this.state?.channelId === channelId;
	}

	getEditingState(): ScheduledMessageEditState | null {
		return this.state;
	}
}

export default new ScheduledMessageEditorStore();
