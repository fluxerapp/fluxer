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
import * as RouterUtils from '~/utils/RouterUtils';
import type {NavState} from './routeParser';

type ViewContext = {kind: 'dm'} | {kind: 'favorites'} | {kind: 'guild'; guildId: string};

class NavigationStore {
	context: ViewContext = {kind: 'dm'};
	channelId: string | null = null;
	messageId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	@action
	setState(state: NavState): void {
		this.context = state.context;
		this.channelId = state.channelId;
		this.messageId = state.messageId;
	}

	@action
	reset(): void {
		this.context = {kind: 'dm'};
		this.channelId = null;
		this.messageId = null;
	}
}

class NavigationCoordinator {
	private readonly store: NavigationStore;

	constructor() {
		this.store = new NavigationStore();
	}

	get currentContext(): ViewContext {
		return this.store.context;
	}

	get currentChannelId(): string | null {
		return this.store.channelId;
	}

	get currentMessageId(): string | null {
		return this.store.messageId;
	}

	@action
	applyRoute(state: NavState): void {
		this.store.setState(state);
	}

	@action
	navigateToGuild(guildId: string, channelId?: string, messageId?: string): void {
		const state: NavState = {
			context: {kind: 'guild', guildId},
			channelId: channelId ?? null,
			messageId: messageId ?? null,
		};

		this.applyRoute(state);

		if (messageId) {
			RouterUtils.transitionTo(`/channels/${guildId}/${channelId}/${messageId}`);
		} else if (channelId) {
			RouterUtils.transitionTo(`/channels/${guildId}/${channelId}`);
		} else {
			RouterUtils.transitionTo(`/channels/${guildId}`);
		}
	}

	@action
	navigateToDM(channelId?: string, messageId?: string): void {
		const state: NavState = {
			context: {kind: 'dm'},
			channelId: channelId ?? null,
			messageId: messageId ?? null,
		};

		this.applyRoute(state);

		if (messageId && channelId) {
			RouterUtils.transitionTo(`/channels/@me/${channelId}/${messageId}`);
		} else if (channelId) {
			RouterUtils.transitionTo(`/channels/@me/${channelId}`);
		} else {
			RouterUtils.transitionTo('/channels/@me');
		}
	}

	@action
	navigateToFavorites(channelId?: string, messageId?: string): void {
		const state: NavState = {
			context: {kind: 'favorites'},
			channelId: channelId ?? null,
			messageId: messageId ?? null,
		};

		this.applyRoute(state);

		if (messageId && channelId) {
			RouterUtils.transitionTo(`/channels/@favorites/${channelId}/${messageId}`);
		} else if (channelId) {
			RouterUtils.transitionTo(`/channels/@favorites/${channelId}`);
		} else {
			RouterUtils.transitionTo('/channels/@favorites');
		}
	}

	@action
	reset(): void {
		this.store.reset();
	}
}

const navigationCoordinator = new NavigationCoordinator();

export type {ViewContext, NavState};
export {NavigationStore, NavigationCoordinator};
export default navigationCoordinator;
