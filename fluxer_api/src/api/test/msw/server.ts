// SPDX-License-Identifier: AGPL-3.0-or-later

import {createOnionooDetailsHandler} from '@app/api/test/msw/handlers/OnionooHandlers';
import {createOpenNsfwHandlers} from '@app/api/test/msw/handlers/OpenNsfwHandlers';
import {createPwnedPasswordsRangeHandler} from '@app/api/test/msw/handlers/PwnedPasswordsHandlers';
import {setupServer} from 'msw/node';

export const server = setupServer(
	...createOpenNsfwHandlers(),
	createOnionooDetailsHandler(),
	createPwnedPasswordsRangeHandler(),
);
