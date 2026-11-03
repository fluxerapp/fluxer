// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import type {ReportType} from '@app/features/moderation/components/report/ReportTypes';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

const REPORT_TYPE_DESCRIPTOR = msg({
	message: 'Report type',
	comment:
		'Button or menu action label in the moderation report step selection. Keep it concise. Keep the tone plain and specific.',
});

interface Props {
	reportTypeOptions: ReadonlyArray<RadioOption<ReportType>>;
	selectedType: ReportType | null;
	onSelect: (type: ReportType) => void;
}

export const ReportStepSelection: React.FC<Props> = ({reportTypeOptions, selectedType, onSelect}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.card} data-flx="moderation.report.report-step-selection.card">
			<header className={styles.cardHeader} data-flx="moderation.report.report-step-selection.card-header">
				<p className={styles.eyebrow} data-flx="moderation.report.report-step-selection.eyebrow">
					<Trans>Step 1</Trans>
				</p>
				<h1 className={styles.title} data-flx="moderation.report.report-step-selection.title">
					<Trans>Report illegal content</Trans>
				</h1>
				<p className={styles.description} data-flx="moderation.report.report-step-selection.description">
					<Trans>Select what you want to report.</Trans>
				</p>
			</header>
			<div className={styles.cardBody} data-flx="moderation.report.report-step-selection.card-body">
				<RadioGroup<ReportType>
					options={reportTypeOptions}
					value={selectedType}
					onChange={onSelect}
					aria-label={i18n._(REPORT_TYPE_DESCRIPTOR)}
					data-flx="moderation.report.report-step-selection.radio-group.select"
				/>
			</div>
		</div>
	);
};
