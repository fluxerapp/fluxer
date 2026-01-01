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
import {makePersistent} from '~/lib/MobXPersistence';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GeoIPStore from '~/stores/GeoIPStore';
import UserStore from '~/stores/UserStore';

export enum NSFWGateReason {
	NONE = 0,
	GEO_RESTRICTED = 1,
	AGE_RESTRICTED = 2,
	CONSENT_REQUIRED = 3,
}

class GuildNSFWAgreeStore {
	agreedChannelIds: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'GuildNSFWAgreeStore', ['agreedChannelIds']);
	}

	agreeToChannel(channelId: string): void {
		if (!this.agreedChannelIds.includes(channelId)) {
			this.agreedChannelIds.push(channelId);
		}
	}

	reset(): void {
		this.agreedChannelIds = [];
	}

	hasAgreedToChannel(channelId: string): boolean {
		return this.agreedChannelIds.includes(channelId);
	}

	getGateReason(channelId: string): NSFWGateReason {
		const mockReason = DeveloperOptionsStore.mockNSFWGateReason;
		if (mockReason !== 'none') {
			switch (mockReason) {
				case 'geo_restricted':
					return NSFWGateReason.GEO_RESTRICTED;
				case 'age_restricted':
					return NSFWGateReason.AGE_RESTRICTED;
				case 'consent_required':
					return NSFWGateReason.CONSENT_REQUIRED;
			}
		}

		const countryCode = GeoIPStore.countryCode;
		const regionCode = GeoIPStore.regionCode;
		const ageRestrictedGeos = GeoIPStore.ageRestrictedGeos;

		if (countryCode) {
			const isAgeRestricted = ageRestrictedGeos.some((geo) => {
				if (geo.countryCode !== countryCode) return false;
				if (geo.regionCode === null) return true;
				return geo.regionCode === regionCode;
			});

			if (isAgeRestricted) {
				return NSFWGateReason.GEO_RESTRICTED;
			}
		}

		const currentUser = UserStore.getCurrentUser();
		if (currentUser && !currentUser.nsfwAllowed) {
			return NSFWGateReason.AGE_RESTRICTED;
		}

		if (!this.hasAgreedToChannel(channelId)) {
			return NSFWGateReason.CONSENT_REQUIRED;
		}

		return NSFWGateReason.NONE;
	}

	shouldShowGate(channelId: string): boolean {
		return this.getGateReason(channelId) !== NSFWGateReason.NONE;
	}
}

export default new GuildNSFWAgreeStore();
