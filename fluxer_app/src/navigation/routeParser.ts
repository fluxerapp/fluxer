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

import {ME} from '~/Constants';
import type {ViewContext} from './NavigationCoordinator';

interface NavState {
	context: ViewContext;
	channelId: string | null;
	messageId: string | null;
}

interface LocationLike {
	pathname: string;
}

function parseNavState(location: LocationLike): NavState {
	const pathname = location.pathname;
	const segments = pathname.split('/').filter(Boolean);

	if (segments.length < 2 || segments[0] !== 'channels') {
		return {
			context: {kind: 'dm'},
			channelId: null,
			messageId: null,
		};
	}

	const guildId = segments[1];
	const channelId = segments[2] ?? null;
	const messageId = segments[3] ?? null;

	let context: ViewContext;

	if (guildId === ME) {
		context = {kind: 'dm'};
	} else if (guildId === '@favorites') {
		context = {kind: 'favorites'};
	} else {
		context = {kind: 'guild', guildId};
	}

	return {
		context,
		channelId,
		messageId,
	};
}

function normalizeRoute(location: LocationLike): string | null {
	const state = parseNavState(location);

	if (state.context.kind === 'dm' && !state.channelId) {
		return `/channels/@me`;
	}

	if (state.context.kind === 'favorites' && !state.channelId) {
		return `/channels/@favorites`;
	}

	if (state.context.kind === 'guild' && !state.channelId) {
		return `/channels/${state.context.guildId}`;
	}

	return null;
}

export type {NavState};
export {parseNavState, normalizeRoute};
