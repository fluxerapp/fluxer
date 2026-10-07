// SPDX-License-Identifier: AGPL-3.0-or-later

import {ReportFlowBanner} from '@app/features/moderation/components/report_flow/ReportFlowBanner';
import {ReportFlowChecklist} from '@app/features/moderation/components/report_flow/ReportFlowChecklist';
import {ReportFlowOptionList} from '@app/features/moderation/components/report_flow/ReportFlowOptionList';
import styles from '@app/features/moderation/components/report_flow/ReportFlowScreenView.module.css';
import {
	getReportFlowCheckedItems,
	getReportFlowScreenKind,
	type ReportFlowWalk,
} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import type {ReportFlowOption, ReportFlowScreen} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type React from 'react';

interface ReportFlowScreenViewProps {
	screen: ReportFlowScreen;
	walk: ReportFlowWalk;
	onChooseOption: (option: ReportFlowOption) => void;
	onToggleItem: (itemId: string) => void;
	previewHeading?: string;
	preview?: React.ReactNode;
}

export const ReportFlowScreenView: React.FC<ReportFlowScreenViewProps> = ({
	screen,
	walk,
	onChooseOption,
	onToggleItem,
	previewHeading,
	preview,
}) => {
	const kind = getReportFlowScreenKind(screen);
	return (
		<div className={styles.container} data-flx="moderation.report-flow.report-flow-screen-view.container">
			{preview !== undefined && (
				<section className={styles.section} data-flx="moderation.report-flow.report-flow-screen-view.section">
					{previewHeading !== undefined && (
						<h4 className={styles.heading} data-flx="moderation.report-flow.report-flow-screen-view.heading">
							{previewHeading}
						</h4>
					)}
					{preview}
				</section>
			)}
			{screen.urgent && (
				<ReportFlowBanner data-flx="moderation.report-flow.report-flow-screen-view.report-flow-banner" />
			)}
			{kind === 'checklist' && screen.checklist !== null && (
				<ReportFlowChecklist
					items={screen.checklist.items}
					checked={getReportFlowCheckedItems(walk, screen.id)}
					onToggle={onToggleItem}
					ariaLabel={screen.title}
					data-flx="moderation.report-flow.report-flow-screen-view.report-flow-checklist"
				/>
			)}
			{kind !== 'checklist' && screen.options.length > 0 && (
				<section className={styles.section} data-flx="moderation.report-flow.report-flow-screen-view.section--2">
					{screen.options_heading !== null && (
						<h4 className={styles.heading} data-flx="moderation.report-flow.report-flow-screen-view.heading--2">
							{screen.options_heading}
						</h4>
					)}
					<ReportFlowOptionList
						options={screen.options}
						onChoose={onChooseOption}
						data-flx="moderation.report-flow.report-flow-screen-view.report-flow-option-list"
					/>
				</section>
			)}
		</div>
	);
};
