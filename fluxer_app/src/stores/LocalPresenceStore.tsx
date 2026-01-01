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
import type {StatusType} from '~/Constants';
import {StatusTypes} from '~/Constants';
import {
	type CustomStatus,
	customStatusToKey,
	type GatewayCustomStatusPayload,
	normalizeCustomStatus,
	toGatewayCustomStatus,
} from '~/lib/customStatus';
import IdleStore from '~/stores/IdleStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserSettingsStore from '~/stores/UserSettingsStore';

type Presence = Readonly<{
	status: StatusType;
	since: number;
	afk: boolean;
	mobile: boolean;
	custom_status: GatewayCustomStatusPayload | null;
}>;

class LocalPresenceStore {
	status: StatusType = StatusTypes.ONLINE;

	since: number = 0;

	customStatus: CustomStatus | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	updatePresence(): void {
		const userStatus = UserSettingsStore.status;
		const idleSince = IdleStore.getIdleSince();

		const effectiveStatus = userStatus === StatusTypes.ONLINE && idleSince > 0 ? StatusTypes.IDLE : userStatus;

		const normalizedCustomStatus = normalizeCustomStatus(UserSettingsStore.getCustomStatus());
		this.customStatus = normalizedCustomStatus ? {...normalizedCustomStatus} : null;
		this.status = effectiveStatus;
		this.since = idleSince;
	}

	getStatus(): StatusType {
		return this.status;
	}

	getPresence(): Presence {
		const isMobile = MobileLayoutStore.isMobileLayout();
		const idleSince = IdleStore.getIdleSince();
		const afkTimeout = UserSettingsStore.getAfkTimeout();

		const timeSinceLastActivity = idleSince > 0 ? Date.now() - idleSince : 0;
		const afk = !isMobile && timeSinceLastActivity > afkTimeout * 1000;

		return {
			status: this.status,
			since: this.since,
			afk,
			mobile: isMobile,
			custom_status: toGatewayCustomStatus(this.customStatus),
		};
	}

	get presenceFingerprint(): string {
		return `${this.status}|${customStatusToKey(this.customStatus)}`;
	}
}

export default new LocalPresenceStore();
