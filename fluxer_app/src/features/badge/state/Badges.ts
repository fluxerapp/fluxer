// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {BadgeType} from '@fluxer/constants/src/BadgeConstants';
import type {
	BadgeResponse,
	BadgesResponse,
	BadgesUpdateDispatchData,
	BuiltinBadgeIconsResponse,
	UserBadgesUpdateDispatchData,
} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('Badges');

const DEFAULT_BUILTIN_ICONS: BuiltinBadgeIconsResponse = {
	staff: null,
	premium: null,
	verified: null,
	partnered: null,
	discoverable: null,
};

class Badges {
	version = 0;
	badges: ReadonlyMap<string, BadgeResponse> = new Map();
	builtinIcons: BuiltinBadgeIconsResponse = DEFAULT_BUILTIN_ICONS;
	userBadges: ReadonlyMap<string, ReadonlyArray<string>> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		const badges = typeof window !== 'undefined' ? window.__FLUXER_BOOTSTRAP__?.instance.badges : undefined;
		if (badges) this.setBadges(badges);
	}

	setBadges({version, badges, builtin_icons}: BadgesResponse): void {
		this.version = version;
		this.badges = new Map(badges.map((badge) => [badge.id, badge]));
		this.builtinIcons = builtin_icons;
	}

	handleBadgesUpdate({version, badges, deleted_badge_ids, builtin_icons}: BadgesUpdateDispatchData): void {
		const next = new Map(this.badges);
		for (const badge of badges ?? []) next.set(badge.id, badge);
		for (const badgeId of deleted_badge_ids ?? []) next.delete(badgeId);
		this.version = version;
		this.badges = next;
		if (builtin_icons) this.builtinIcons = {...this.builtinIcons, ...builtin_icons};
	}

	handleUserBadgesUpdate({user_id, badges}: UserBadgesUpdateDispatchData): void {
		this.userBadges = new Map(this.userBadges).set(user_id, badges);
	}

	async handleGatewayReady(version: number | undefined): Promise<void> {
		this.userBadges = new Map();
		if (version === undefined || version === this.version) return;
		try {
			const response = await http.get<BadgesResponse>(Endpoints.BADGES);
			if (response.body) this.setBadges(response.body);
		} catch (error) {
			logger.warn('Failed to refresh badges:', error);
		}
	}

	resolve(type: BadgeType, badgeIds: ReadonlyArray<string> | undefined): Array<BadgeResponse> {
		return (badgeIds ?? [])
			.flatMap((badgeId) => {
				const badge = this.badges.get(badgeId);
				return badge?.type === type ? [badge] : [];
			})
			.sort((a, b) => a.position - b.position);
	}
}

export default new Badges();
