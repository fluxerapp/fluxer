// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPwnedPasswordsRangeHandler} from '@app/api/test/msw/handlers/PwnedPasswordsHandlers';
import {setupServer} from 'msw/node';

export const server = setupServer(createPwnedPasswordsRangeHandler());
