// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readTypingIndicatorReworkAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	isTypingIndicatorReworkTargeted,
	type TypingIndicatorReworkAssignmentResponse,
} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';

class TypingIndicatorReworkRolloutSelector {
	get assignment(): TypingIndicatorReworkAssignmentResponse {
		return readTypingIndicatorReworkAssignment(ExperimentAssignments.response);
	}

	get usesRollingTyping(): boolean {
		return isTypingIndicatorReworkTargeted(this.assignment);
	}
}

export const TypingIndicatorReworkRollout = new TypingIndicatorReworkRolloutSelector();

export default TypingIndicatorReworkRollout;
