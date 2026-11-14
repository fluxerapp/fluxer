// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {Trans} from '@lingui/react/macro';
import {EnvelopeSimpleIcon} from '@phosphor-icons/react';
import type React from 'react';

export const ReportStepUnavailable: React.FC = () => (
	<StatusSlate
		Icon={EnvelopeSimpleIcon}
		title={<Trans>Email reports aren't available here</Trans>}
		description={
			<Trans>
				This instance doesn't use email, so it can't take reports through this form. Report content from inside the app,
				or contact the administrators of this instance.
			</Trans>
		}
		data-flx="moderation.report.report-step-unavailable.status-slate"
	/>
);
