// SPDX-License-Identifier: AGPL-3.0-or-later

import {createNcmecHandlers} from '@app/api/test/msw/handlers/NcmecHandlers';
import {createOnionooDetailsHandler} from '@app/api/test/msw/handlers/OnionooHandlers';
import {createOpenNsfwHandlers} from '@app/api/test/msw/handlers/OpenNsfwHandlers';
import {createPwnedPasswordsRangeHandler} from '@app/api/test/msw/handlers/PwnedPasswordsHandlers';
import {setupServer} from 'msw/node';

export const server = setupServer(
	...createNcmecHandlers(),
	...createOpenNsfwHandlers(),
	createOnionooDetailsHandler(),
	createPwnedPasswordsRangeHandler(),
);
