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

import path from 'node:path';
import {app} from 'electron';
import {BUILD_CHANNEL, type BuildChannel} from './build-channel.js';

interface UserDataPaths {
	readonly channel: BuildChannel;
	readonly directoryName: string;
	readonly base: string;
}

interface ChannelStorageDirectoryMap {
	stable: string;
	canary: string;
}

const channelStorageDirectoryMap: ChannelStorageDirectoryMap = {
	stable: 'fluxer',
	canary: 'fluxercanary',
};

function resolveUserDataPaths(channel: BuildChannel): {directoryName: string; base: string} {
	const directoryName = channelStorageDirectoryMap[channel];
	const appDataPath = app.getPath('appData');
	const base = path.join(appDataPath, directoryName);

	return {
		directoryName,
		base,
	};
}

export function configureUserDataPath(): UserDataPaths {
	const channel = BUILD_CHANNEL;
	const {directoryName, base} = resolveUserDataPaths(channel);
	app.setPath('userData', base);

	return {
		channel,
		directoryName,
		base,
	};
}
