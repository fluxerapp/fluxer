// SPDX-License-Identifier: AGPL-3.0-or-later

import {URGENT_BANNER_DESCRIPTOR} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import {InfoAlert} from '@app/features/ui/info_alert/InfoAlert';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

export const ReportFlowBanner: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<InfoAlert data-flx="moderation.report-flow.report-flow-banner.banner">
			{i18n._(URGENT_BANNER_DESCRIPTOR)}
		</InfoAlert>
	);
};
