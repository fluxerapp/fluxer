// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPENS_IN_NEW_TAB_DESCRIPTOR} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import styles from '@app/features/moderation/components/report_flow/ReportFlowOptionList.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ReportFlowOption} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, CaretRightIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import type React from 'react';

interface ReportFlowOptionListProps {
	options: ReadonlyArray<ReportFlowOption>;
	onChoose: (option: ReportFlowOption) => void;
	disabled?: boolean;
}

export const ReportFlowOptionList: React.FC<ReportFlowOptionListProps> = ({options, onChoose, disabled = false}) => {
	const {i18n} = useLingui();
	return (
		<ul className={styles.list} data-flx="moderation.report-flow.report-flow-option-list.list">
			{options.map((option) => {
				const isLink = option.outcome.type === 'link';
				return (
					<li key={option.id} className={styles.item} data-flx="moderation.report-flow.report-flow-option-list.item">
						<FocusRing offset={-2} data-flx="moderation.report-flow.report-flow-option-list.focus-ring">
							<button
								type="button"
								className={styles.row}
								onClick={() => onChoose(option)}
								disabled={disabled}
								data-flx="moderation.report-flow.report-flow-option-list.row.choose.button"
							>
								<span className={styles.label} data-flx="moderation.report-flow.report-flow-option-list.label">
									{option.label}
								</span>
								{isLink ? (
									<>
										<span className={styles.srOnly} data-flx="moderation.report-flow.report-flow-option-list.sr-only">
											{i18n._(OPENS_IN_NEW_TAB_DESCRIPTOR)}
										</span>
										<ArrowSquareOutIcon
											className={styles.icon}
											weight="bold"
											size={remFromPx(18)}
											aria-hidden={true}
											data-flx="moderation.report-flow.report-flow-option-list.icon"
										/>
									</>
								) : (
									<CaretRightIcon
										className={clsx(styles.icon, styles.chevron)}
										weight="bold"
										size={remFromPx(18)}
										aria-hidden={true}
										data-flx="moderation.report-flow.report-flow-option-list.icon--2"
									/>
								)}
							</button>
						</FocusRing>
					</li>
				);
			})}
		</ul>
	);
};
