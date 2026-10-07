// SPDX-License-Identifier: AGPL-3.0-or-later

import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {ReportFlowBanner} from '@app/features/moderation/components/report_flow/ReportFlowBanner';
import {
	REPORT_CATEGORY_DESCRIPTOR,
	REPORT_DISCLAIMER_NO_LINK_DESCRIPTOR,
} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import styles from '@app/features/moderation/components/report_flow/ReportFlowSummary.module.css';
import {getReportFlowAnswerLabels} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import type {ReportFlowResponse, ReportFlowStep} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

interface ReportFlowSummaryProps {
	flow: ReportFlowResponse;
	steps: ReadonlyArray<ReportFlowStep>;
	urgent: boolean;
	previewHeading?: string;
	preview?: React.ReactNode;
}

export const ReportFlowAnswerList: React.FC<{flow: ReportFlowResponse; steps: ReadonlyArray<ReportFlowStep>}> = ({
	flow,
	steps,
}) => (
	<ol className={styles.answers} data-flx="moderation.report-flow.report-flow-summary.report-flow-answer-list.answers">
		{getReportFlowAnswerLabels(flow, steps).map((label, index) => (
			<li
				key={`${index}:${label}`}
				className={styles.answer}
				data-flx="moderation.report-flow.report-flow-summary.report-flow-answer-list.answer"
			>
				<span
					className={styles.answerText}
					data-flx="moderation.report-flow.report-flow-summary.report-flow-answer-list.answer-text"
				>
					{label}
				</span>
			</li>
		))}
	</ol>
);

export const ReportFlowSummary: React.FC<ReportFlowSummaryProps> = ({flow, steps, urgent, previewHeading, preview}) => {
	const {i18n} = useLingui();
	const guidelinesUrl = flow.guidelines_url;
	return (
		<div className={styles.container} data-flx="moderation.report-flow.report-flow-summary.container">
			<p className={styles.disclaimer} data-flx="moderation.report-flow.report-flow-summary.disclaimer">
				{guidelinesUrl !== null ? (
					<Trans comment="Report flow: good-faith statement on the summary screen. The linked text opens the community guidelines.">
						Only report what you honestly believe breaks the rules. Misusing reports goes against our{' '}
						<ExternalLink href={guidelinesUrl} data-flx="moderation.report-flow.report-flow-summary.external-link">
							Community Guidelines
						</ExternalLink>
						.
					</Trans>
				) : (
					i18n._(REPORT_DISCLAIMER_NO_LINK_DESCRIPTOR)
				)}
			</p>
			{urgent && <ReportFlowBanner data-flx="moderation.report-flow.report-flow-summary.report-flow-banner" />}
			{preview !== undefined && (
				<section className={styles.section} data-flx="moderation.report-flow.report-flow-summary.section">
					{previewHeading !== undefined && (
						<h4 className={styles.heading} data-flx="moderation.report-flow.report-flow-summary.heading">
							{previewHeading}
						</h4>
					)}
					{preview}
				</section>
			)}
			<section className={styles.section} data-flx="moderation.report-flow.report-flow-summary.section--2">
				<h4 className={styles.heading} data-flx="moderation.report-flow.report-flow-summary.heading--2">
					{i18n._(REPORT_CATEGORY_DESCRIPTOR)}
				</h4>
				<ReportFlowAnswerList
					flow={flow}
					steps={steps}
					data-flx="moderation.report-flow.report-flow-summary.report-flow-answer-list"
				/>
			</section>
		</div>
	);
};
