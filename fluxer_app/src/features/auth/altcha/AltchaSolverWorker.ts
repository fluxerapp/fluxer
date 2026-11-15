// SPDX-License-Identifier: AGPL-3.0-or-later

import {deriveKey} from '@app/features/auth/altcha/AltchaKeyDerivation';
import {handler} from 'altcha-lib/workers/shared';

handler({deriveKey});
