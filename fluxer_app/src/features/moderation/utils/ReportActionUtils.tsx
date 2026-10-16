// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {type ReportFlowContext, ReportFlowModal} from '@app/features/moderation/components/report_flow/ReportFlowModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';

function openReportModal(context: ReportFlowContext, onFinish?: () => void): void {
	ModalCommands.push(
		modal(() => (
			<ReportFlowModal
				context={context}
				onFinish={onFinish}
				data-flx="moderation.report-action-utils.open-report-modal.report-flow-modal"
			/>
		)),
	);
}

export function openReportMessageModal(message: Message, options: {onFinish?: () => void} = {}): void {
	openReportModal({type: 'message', message}, options.onFinish);
}

export function openReportUserProfileModal(params: {user: User; guildId?: string}): void {
	openReportModal({type: 'user', user: params.user, guildId: params.guildId});
}
