// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readGuildHeaderCollapseAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	type GuildHeaderCollapseAssignmentResponse,
	isGuildHeaderCollapseTargeted,
} from '@fluxer/schema/src/domains/experiment/GuildHeaderCollapseSchemas';

class GuildHeaderCollapseRolloutSelector {
	get assignment(): GuildHeaderCollapseAssignmentResponse {
		return readGuildHeaderCollapseAssignment(ExperimentAssignments.response);
	}

	get collapsesOnScroll(): boolean {
		return isGuildHeaderCollapseTargeted(this.assignment);
	}
}

export const GuildHeaderCollapseRollout = new GuildHeaderCollapseRolloutSelector();

export default GuildHeaderCollapseRollout;
