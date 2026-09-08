// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupServer} from 'msw/node';
import {createBunnyEdgeHandlers} from './handlers/BunnyEdgeHandlers';
import {createIpInfoLookupHandler} from './handlers/IpInfoHandlers';
import {createNcmecHandlers} from './handlers/NcmecHandlers';
import {createOnionooDetailsHandler} from './handlers/OnionooHandlers';
import {createOpenNsfwHandlers} from './handlers/OpenNsfwHandlers';
import {createPwnedPasswordsRangeHandler} from './handlers/PwnedPasswordsHandlers';

export const server = setupServer(
	...createBunnyEdgeHandlers(),
	...createNcmecHandlers(),
	...createOpenNsfwHandlers(),
	createIpInfoLookupHandler(),
	createOnionooDetailsHandler(),
	createPwnedPasswordsRangeHandler(),
);
