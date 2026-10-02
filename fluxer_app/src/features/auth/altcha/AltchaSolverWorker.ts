// SPDX-License-Identifier: AGPL-3.0-or-later

import {deriveKey} from 'altcha-lib/algorithms/web/pbkdf2';
import {handler} from 'altcha-lib/workers/shared';

handler({deriveKey});
