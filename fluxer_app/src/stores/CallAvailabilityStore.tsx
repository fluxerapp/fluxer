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

import {makeAutoObservable, observable} from 'mobx';

class CallAvailabilityStore {
	unavailableCalls: Set<string> = observable.set();

	constructor() {
		makeAutoObservable(
			this,
			{
				unavailableCalls: false,
			},
			{autoBind: true},
		);
	}

	setCallAvailable(channelId: string): void {
		if (this.unavailableCalls.has(channelId)) {
			this.unavailableCalls.delete(channelId);
		}
	}

	setCallUnavailable(channelId: string): void {
		if (!this.unavailableCalls.has(channelId)) {
			this.unavailableCalls.add(channelId);
		}
	}

	handleCallAvailability(channelId: string, unavailable = false): void {
		if (unavailable) {
			this.setCallUnavailable(channelId);
		} else {
			this.setCallAvailable(channelId);
		}
	}

	get totalUnavailableCalls(): number {
		return this.unavailableCalls.size;
	}

	isCallUnavailable(channelId: string): boolean {
		return this.unavailableCalls.has(channelId);
	}
}

export default new CallAvailabilityStore();
