// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readExpressionInfoCardAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {ExpressionInfoCardAssignmentResponse} from '@fluxer/schema/src/domains/experiment/ExpressionInfoCardSchemas';

class ExpressionInfoCardRolloutSelector {
	get assignment(): ExpressionInfoCardAssignmentResponse {
		return readExpressionInfoCardAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const ExpressionInfoCardRollout = new ExpressionInfoCardRolloutSelector();

export default ExpressionInfoCardRollout;
