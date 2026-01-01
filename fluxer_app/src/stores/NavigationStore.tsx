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

import {action, makeAutoObservable} from 'mobx';
import {ME} from '~/Constants';
import type {Router} from '~/lib/router';
import NavigationCoordinator from '~/navigation/NavigationCoordinator';
import type {NavState} from '~/navigation/routeParser';

type ChannelId = string;
type GuildId = string;

class NavigationStore {
	guildId: GuildId | null = null;
	channelId: ChannelId | null = null;
	messageId: string | null = null;
	private router: Router | null = null;
	private unsubscribe: (() => void) | null = null;
	currentLocation: URL | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	@action
	initialize(router: Router): void {
		this.router = router;
		this.updateFromRouter();

		this.unsubscribe = this.router.subscribe(() => {
			this.updateFromRouter();
		});
	}

	@action
	private updateFromRouter(): void {
		if (!this.router) return;

		const state = this.router.getState();
		this.currentLocation = state.location;
		const match = state.matches[state.matches.length - 1];

		if (!match) {
			this.guildId = null;
			this.channelId = null;
			this.messageId = null;
			this.updateCoordinator();
			return;
		}

		const params = match.params;
		this.guildId = (params.guildId as GuildId) ?? null;
		this.channelId = (params.channelId as ChannelId) ?? null;
		this.messageId = (params.messageId as string) ?? null;
		this.updateCoordinator();
	}

	get pathname(): string {
		return this.currentLocation?.pathname ?? '';
	}

	get search(): string {
		return this.currentLocation?.search ?? '';
	}

	get hash(): string {
		return this.currentLocation?.hash ?? '';
	}

	@action
	private updateCoordinator(): void {
		const guildId = this.guildId;
		let context: NavState['context'];

		if (guildId === ME || !guildId) {
			context = {kind: 'dm'};
		} else if (guildId === '@favorites') {
			context = {kind: 'favorites'};
		} else {
			context = {kind: 'guild', guildId};
		}

		const navState: NavState = {
			context,
			channelId: this.channelId,
			messageId: this.messageId,
		};

		NavigationCoordinator.applyRoute(navState);
	}

	destroy(): void {
		if (this.unsubscribe) {
			this.unsubscribe();
			this.unsubscribe = null;
		}
		this.router = null;
	}
}

export default new NavigationStore();
