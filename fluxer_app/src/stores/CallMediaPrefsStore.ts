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

interface CallScopedPrefs {
	disabledVideoByIdentity: Record<string, boolean>;
}

class CallMediaPrefsStore {
	private byCall: Record<string, CallScopedPrefs> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private ensure(callId: string): CallScopedPrefs {
		return (this.byCall[callId] ||= {disabledVideoByIdentity: {}});
	}

	isVideoDisabled(callId: string, identity: string): boolean {
		return !!this.byCall[callId]?.disabledVideoByIdentity[identity];
	}

	setVideoDisabled(callId: string, identity: string, disabled: boolean): void {
		const scope = this.ensure(callId);
		scope.disabledVideoByIdentity = {
			...scope.disabledVideoByIdentity,
			[identity]: disabled,
		};
	}

	clearForCall(callId: string): void {
		delete this.byCall[callId];
	}
}

export default new CallMediaPrefsStore();
export {CallMediaPrefsStore};
