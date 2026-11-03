// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {Config} from '@app/api/Config';

export const OPERATOR_PASS_QUOTE_SECRET_LABEL = 'fluxer-operator-pass-quote-v1';
export const OPERATOR_PASS_UNSUBSCRIBE_SECRET_LABEL = 'fluxer-operator-pass-unsubscribe-v1';
export const OPERATOR_PASS_FOLLOWUP_SECRET_LABEL = 'fluxer-operator-pass-waitlist-followup-v1';

export function deriveOperatorPassSecret(label: string): string {
	return createHmac('sha256', Config.auth.sudoModeSecret).update(label).digest('hex');
}
