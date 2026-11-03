// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {User} from '@app/api/models/User';
import {isAccountKind} from '@fluxer/constants/src/EnforcementReasons';

export function isDecisionExpired(decision: EnforcementDecisionRow, now: number = Date.now()): boolean {
	return (
		decision.status === 'active' &&
		(decision.kind === 'suspension' || decision.kind === 'feature_limit') &&
		decision.ends_at !== null &&
		decision.ends_at.getTime() < now
	);
}

export function isDecisionReplaced(decision: EnforcementDecisionRow, subject: User | null): boolean {
	return (
		decision.status === 'active' &&
		isAccountKind(decision.kind) &&
		subject?.enforcementDecisionId !== decision.decision_id
	);
}
