// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readGuildActivityLogPresentationAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {GuildActivityLogPresentationAssignmentResponse} from '@fluxer/schema/src/domains/experiment/GuildActivityLogPresentationSchemas';

class GuildActivityLogPresentationRolloutSelector {
	get assignment(): GuildActivityLogPresentationAssignmentResponse {
		return readGuildActivityLogPresentationAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const GuildActivityLogPresentationRollout = new GuildActivityLogPresentationRolloutSelector();

export default GuildActivityLogPresentationRollout;
