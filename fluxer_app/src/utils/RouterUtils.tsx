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

import {Logger} from '~/lib/Logger';
import {createBrowserHistory, type HistoryAdapter} from '~/lib/router';

const logger = new Logger('RouterUtils');

export const history: HistoryAdapter | null = createBrowserHistory();

export const transitionTo = (path: string) => {
	logger.info('transitionTo', path);
	if (history) {
		const current = history.getLocation().url.pathname;
		if (current === path) return;
		history.push(new URL(path, window.location.origin));
	}
};

export const replaceWith = (path: string) => {
	logger.info('replaceWith', path);
	if (history) {
		const current = history.getLocation().url.pathname;
		if (current === path) return;
		history.replace(new URL(path, window.location.origin));
	}
};

export const getHistory = () => history;
