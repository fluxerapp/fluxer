// SPDX-License-Identifier: AGPL-3.0-or-later

import {openGuildWhenAvailable} from '@app/features/guild/commands/GuildVisitCommands';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {OPERATOR_PASS_LABS_INVITE_CODE} from '@app/features/premium/utils/OperatorPassUtils';

export type OpenOperatorsResult = 'opened' | 'banned';

function openLabsInvite(): void {
	void InviteCommands.openAcceptModal(OPERATOR_PASS_LABS_INVITE_CODE, currentInstanceTarget());
}

export async function openOperatorsChannel(): Promise<OpenOperatorsResult> {
	const labs = OperatorPassHoldings.passes?.labs ?? (await OperatorPassCommands.fetchPasses()).labs;
	if (labs.member && labs.guild_id) {
		NavigationCommands.selectChannel(labs.guild_id, labs.operators_channel_id ?? undefined);
		return 'opened';
	}
	const joined = await OperatorPassCommands.joinLabs();
	if (joined.status === 'banned') return 'banned';
	if (joined.status === 'joined' && joined.guild_id) {
		await openGuildWhenAvailable(joined.guild_id, joined.operators_channel_id ?? undefined);
		return 'opened';
	}
	openLabsInvite();
	return 'opened';
}
