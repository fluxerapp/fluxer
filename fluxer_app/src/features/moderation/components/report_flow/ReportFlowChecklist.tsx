// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/moderation/components/report_flow/ReportFlowChecklist.module.css';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ReportFlowChecklistItem} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type React from 'react';

interface ReportFlowChecklistProps {
	items: ReadonlyArray<ReportFlowChecklistItem>;
	checked: ReadonlySet<string>;
	onToggle: (itemId: string) => void;
	ariaLabel: string;
}

export const ReportFlowChecklist: React.FC<ReportFlowChecklistProps> = ({items, checked, onToggle, ariaLabel}) => (
	<ul className={styles.list} aria-label={ariaLabel} data-flx="moderation.report-flow.report-flow-checklist.list">
		{items.map((item) => {
			const isChecked = checked.has(item.id);
			return (
				<li key={item.id} className={styles.item} data-flx="moderation.report-flow.report-flow-checklist.item">
					<FocusRing offset={-2} data-flx="moderation.report-flow.report-flow-checklist.focus-ring">
						<button
							type="button"
							role="checkbox"
							aria-checked={isChecked}
							className={styles.row}
							onClick={() => onToggle(item.id)}
							data-flx="moderation.report-flow.report-flow-checklist.row.toggle.button"
						>
							<Checkbox
								checked={isChecked}
								readOnly
								size={20}
								aria-hidden={true}
								data-flx="moderation.report-flow.report-flow-checklist.checkbox"
							/>
							<span className={styles.text} data-flx="moderation.report-flow.report-flow-checklist.text">
								<span className={styles.label} data-flx="moderation.report-flow.report-flow-checklist.label">
									{item.label}
								</span>
								{item.description !== null && (
									<span
										className={styles.description}
										data-flx="moderation.report-flow.report-flow-checklist.description"
									>
										{item.description}
									</span>
								)}
							</span>
						</button>
					</FocusRing>
				</li>
			);
		})}
	</ul>
);
