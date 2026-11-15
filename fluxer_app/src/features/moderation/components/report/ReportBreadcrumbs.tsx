// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import type {FlowStep} from '@app/features/moderation/components/report/ReportTypes';
import {Trans} from '@lingui/react/macro';
import clsx from 'clsx';
import type React from 'react';

interface Props {
	current: FlowStep;
	hasSelection: boolean;
	hasEmail: boolean;
	hasTicket: boolean;
	hasAnswers: boolean;
	onSelect: (step: FlowStep) => void;
}

const STEP_ORDER: Array<FlowStep> = ['selection', 'email', 'verification', 'reason', 'details'];
export const ReportBreadcrumbs: React.FC<Props> = ({
	current,
	hasSelection,
	hasEmail,
	hasTicket,
	hasAnswers,
	onSelect,
}) => {
	const isEnabled = (step: FlowStep) => {
		if (step === 'selection') return true;
		if (step === 'email') return hasSelection;
		if (step === 'verification') return hasEmail;
		if (step === 'reason') return hasTicket;
		if (step === 'details') return hasTicket && hasAnswers;
		return false;
	};
	const labelMap: Record<FlowStep, React.ReactNode> = {
		selection: <Trans>Choose</Trans>,
		email: <Trans>Email</Trans>,
		verification: <Trans>Code</Trans>,
		reason: (
			<Trans comment="DSA report page: step indicator label for the step where the reporter picks why they are reporting.">
				Reason
			</Trans>
		),
		details: <Trans>Details</Trans>,
		complete: <Trans>Done</Trans>,
	};
	return (
		<div className={styles.breadcrumbs} data-flx="moderation.report.report-breadcrumbs.breadcrumbs">
			{STEP_ORDER.map((step, index) => {
				const active = current === step;
				const clickable = !active && isEnabled(step);
				const button = (
					<button
						key={step}
						type="button"
						className={clsx(styles.breadcrumbStep, active && styles.breadcrumbActive)}
						disabled={!clickable}
						onClick={() => clickable && onSelect(step)}
						data-flx="moderation.report.report-breadcrumbs.breadcrumb-step.button"
					>
						<span className={styles.breadcrumbNumber} data-flx="moderation.report.report-breadcrumbs.breadcrumb-number">
							{index + 1}
						</span>
						<span className={styles.breadcrumbLabel} data-flx="moderation.report.report-breadcrumbs.breadcrumb-label">
							{labelMap[step]}
						</span>
					</button>
				);
				if (index === STEP_ORDER.length - 1) return button;
				return (
					<span
						key={step}
						className={styles.breadcrumbGroup}
						data-flx="moderation.report.report-breadcrumbs.breadcrumb-group"
					>
						{button}
						<span
							className={styles.breadcrumbSeparator}
							aria-hidden="true"
							data-flx="moderation.report.report-breadcrumbs.breadcrumb-separator"
						>
							›
						</span>
					</span>
				);
			})}
		</div>
	);
};
