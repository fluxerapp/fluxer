// SPDX-License-Identifier: AGPL-3.0-or-later

import {NEXT_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import type {ReportFlowStatus} from '@app/features/moderation/components/report/ReportTypes';
import {BACK_DESCRIPTOR, LOAD_FAILED_DESCRIPTOR} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import {ReportFlowNotice} from '@app/features/moderation/components/report_flow/ReportFlowNotice';
import {ReportFlowScreenView} from '@app/features/moderation/components/report_flow/ReportFlowScreenView';
import {
	backReportFlowWalk,
	canContinueReportFlowChecklist,
	canGoBackInReportFlow,
	chooseReportFlowOption,
	continueReportFlowChecklist,
	continueReportFlowInfo,
	getReportFlowScreen,
	getReportFlowScreenKind,
	getReportFlowStepKey,
	type ReportFlowWalk,
	startReportFlowWalk,
	toggleReportFlowItem,
} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {ReportFlowOption, ReportFlowResponse} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

const WHAT_ARE_YOU_REPORTING_DESCRIPTOR = msg({
	message: "What's the reason for your report?",
	comment: 'DSA report page: title of the reason step while its questions load or when they could not be loaded.',
});

interface Props {
	flow: ReportFlowResponse | null;
	flowStatus: ReportFlowStatus;
	walk: ReportFlowWalk | null;
	onWalkChange: (walk: ReportFlowWalk) => void;
	onBack: () => void;
	onRetryLoad: () => void;
	onStartOver: () => void;
}

export const ReportStepReason: React.FC<Props> = ({
	flow,
	flowStatus,
	walk,
	onWalkChange,
	onBack,
	onRetryLoad,
	onStartOver,
}) => {
	const {i18n} = useLingui();
	const ready = flowStatus === 'loaded' && flow !== null && walk !== null;
	const screen = ready ? getReportFlowScreen(flow, walk.screenId) : null;
	const notice =
		ready && walk.phase === 'notice' ? (flow.notices.find((entry) => entry.id === walk.noticeId) ?? null) : null;
	const kind = screen !== null ? getReportFlowScreenKind(screen) : null;
	const handleChooseOption = (option: ReportFlowOption) => {
		if (!ready) return;
		if (option.outcome.type === 'link') {
			if (option.outcome.url !== null) void openExternalUrl(option.outcome.url);
			return;
		}
		onWalkChange(chooseReportFlowOption(flow, walk, option.id));
	};
	const handleToggleItem = (itemId: string) => {
		if (ready) onWalkChange(toggleReportFlowItem(flow, walk, itemId));
	};
	const handleNext = () => {
		if (!ready) return;
		onWalkChange(kind === 'checklist' ? continueReportFlowChecklist(flow, walk) : continueReportFlowInfo(flow, walk));
	};
	const handleBack = () => {
		if (ready && canGoBackInReportFlow(walk)) {
			onWalkChange(backReportFlowWalk(walk));
			return;
		}
		onBack();
	};
	const handleRestart = () => {
		if (flow !== null) onWalkChange(startReportFlowWalk(flow));
	};
	let title = i18n._(WHAT_ARE_YOU_REPORTING_DESCRIPTOR);
	let description: string | null = null;
	if (notice !== null) {
		title = notice.title;
	} else if (screen !== null && walk?.phase === 'screen') {
		title = screen.title;
		description = screen.subtitle;
	}
	const backButton = (
		<Button
			variant="secondary"
			fitContent
			type="button"
			onClick={handleBack}
			data-flx="moderation.report.report-step-reason.button.back"
		>
			{i18n._(BACK_DESCRIPTOR)}
		</Button>
	);
	const renderBody = (): React.ReactNode => {
		if (flowStatus === 'unavailable') {
			return (
				<>
					<div
						className={styles.errorBox}
						role="alert"
						aria-live="polite"
						data-flx="moderation.report.report-step-reason.error-box"
					>
						{i18n._(LOAD_FAILED_DESCRIPTOR)}
					</div>
					<div className={styles.actionRow} data-flx="moderation.report.report-step-reason.action-row">
						<Button
							fitContent
							type="button"
							onClick={onRetryLoad}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-reason.action-button.retry-load"
						>
							{i18n._(TRY_AGAIN_DESCRIPTOR)}
						</Button>
						{backButton}
					</div>
				</>
			);
		}
		if (!ready) {
			return <Spinner data-flx="moderation.report.report-step-reason.spinner" />;
		}
		if (notice !== null) {
			return (
				<>
					<ReportFlowNotice notice={notice} data-flx="moderation.report.report-step-reason.report-flow-notice" />
					<div className={styles.actionRow} data-flx="moderation.report.report-step-reason.action-row--2">
						<Button
							fitContent
							type="button"
							onClick={handleRestart}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-reason.action-button.restart"
						>
							<Trans>Start over</Trans>
						</Button>
					</div>
				</>
			);
		}
		if (screen === null || walk.phase !== 'screen') return null;
		return (
			<>
				<SteppedCarousel
					step={getReportFlowStepKey(walk)}
					steps={[getReportFlowStepKey(walk)]}
					direction={walk.direction}
					focusOnStepChange
					data-flx="moderation.report.report-step-reason.stepped-carousel"
				>
					<ReportFlowScreenView
						screen={screen}
						walk={walk}
						onChooseOption={handleChooseOption}
						onToggleItem={handleToggleItem}
						data-flx="moderation.report.report-step-reason.report-flow-screen-view"
					/>
				</SteppedCarousel>
				<div className={styles.actionRow} data-flx="moderation.report.report-step-reason.action-row--3">
					{kind !== 'choice' && (
						<Button
							fitContent
							type="button"
							onClick={handleNext}
							disabled={kind === 'checklist' && !canContinueReportFlowChecklist(flow, walk)}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-reason.action-button.next"
						>
							{i18n._(NEXT_DESCRIPTOR)}
						</Button>
					)}
					{backButton}
				</div>
			</>
		);
	};
	return (
		<div className={styles.card} data-flx="moderation.report.report-step-reason.card">
			<header className={styles.cardHeader} data-flx="moderation.report.report-step-reason.card-header">
				<p className={styles.eyebrow} data-flx="moderation.report.report-step-reason.eyebrow">
					<Trans>Step 4</Trans>
				</p>
				<h1 className={styles.title} data-flx="moderation.report.report-step-reason.title">
					{title}
				</h1>
				{description !== null && (
					<p className={styles.description} data-flx="moderation.report.report-step-reason.description">
						{description}
					</p>
				)}
			</header>
			<div className={styles.cardBody} data-flx="moderation.report.report-step-reason.card-body">
				{renderBody()}
			</div>
			{notice === null && (
				<footer className={styles.footerLinks} data-flx="moderation.report.report-step-reason.footer-links">
					<p className={styles.linkRow} data-flx="moderation.report.report-step-reason.link-row">
						<FocusRing offset={-2} data-flx="moderation.report.report-step-reason.focus-ring.start-over">
							<button
								type="button"
								className={styles.linkButton}
								onClick={onStartOver}
								data-flx="moderation.report.report-step-reason.link-button.start-over"
							>
								<Trans>Start over</Trans>
							</button>
						</FocusRing>
					</p>
				</footer>
			)}
		</div>
	);
};
