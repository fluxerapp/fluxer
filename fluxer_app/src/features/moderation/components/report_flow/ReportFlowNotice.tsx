// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/moderation/components/report_flow/ReportFlowThankYou.module.css';
import type {ReportFlowNotice as ReportFlowNoticeData} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type React from 'react';

export const ReportFlowNotice: React.FC<{notice: ReportFlowNoticeData}> = ({notice}) => (
	<p className={styles.body} data-flx="moderation.report-flow.report-flow-notice.body">
		{notice.body}
	</p>
);
