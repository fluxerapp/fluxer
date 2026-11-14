// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/moderation/components/report_flow/ReportFlowBanner.module.css';
import {URGENT_BANNER_DESCRIPTOR} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {useLingui} from '@lingui/react/macro';
import {InfoIcon} from '@phosphor-icons/react';
import type React from 'react';

export const ReportFlowBanner: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<div className={styles.banner} role="note" data-flx="moderation.report-flow.report-flow-banner.banner">
			<InfoIcon
				className={styles.icon}
				weight="fill"
				size={remFromPx(20)}
				aria-hidden={true}
				data-flx="moderation.report-flow.report-flow-banner.icon"
			/>
			<p className={styles.text} data-flx="moderation.report-flow.report-flow-banner.text">
				{i18n._(URGENT_BANNER_DESCRIPTOR)}
			</p>
		</div>
	);
};
