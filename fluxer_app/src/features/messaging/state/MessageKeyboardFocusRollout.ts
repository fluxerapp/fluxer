// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readMessageKeyboardFocusAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {MessageKeyboardFocusAssignmentResponse} from '@fluxer/schema/src/domains/experiment/MessageKeyboardFocusSchemas';

class MessageKeyboardFocusRolloutSelector {
	get assignment(): MessageKeyboardFocusAssignmentResponse {
		return readMessageKeyboardFocusAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const MessageKeyboardFocusRollout = new MessageKeyboardFocusRolloutSelector();

export default MessageKeyboardFocusRollout;
