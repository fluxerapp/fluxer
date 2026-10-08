// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import {
	NEXT_DESCRIPTOR,
	REPORT_MESSAGE_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {buildMessageJumpLink} from '@app/features/messaging/utils/MessageLinkUtils';
import {type ReportFlowSubmissionTarget, submitReportFlow} from '@app/features/moderation/commands/ReportFlowCommands';
import {
	ALREADY_REPORTED_MESSAGE_DESCRIPTOR,
	ALREADY_REPORTED_PROFILE_DESCRIPTOR,
	BACK_DESCRIPTOR,
	CLAIM_AND_VERIFY_TO_REPORT_DESCRIPTOR,
	CLAIM_TO_REPORT_DESCRIPTOR,
	DONE_DESCRIPTOR,
	FINISH_ACCOUNT_SETUP_DESCRIPTOR,
	FINISH_ACCOUNT_SETUP_FIRST_DESCRIPTOR,
	FLOW_OUTDATED_DESCRIPTOR,
	LOAD_FAILED_DESCRIPTOR,
	RATE_LIMITED_DESCRIPTOR,
	REPORT_SUMMARY_SUBTITLE_DESCRIPTOR,
	REPORT_SUMMARY_TITLE_DESCRIPTOR,
	SELECTED_MESSAGE_DESCRIPTOR,
	SELECTED_USER_DESCRIPTOR,
	SUBMIT_FAILED_DESCRIPTOR,
	SUBMIT_REPORT_DESCRIPTOR,
	THANK_YOU_NO_REPORT_TITLE_DESCRIPTOR,
	THANK_YOU_TITLE_DESCRIPTOR,
} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import styles from '@app/features/moderation/components/report_flow/ReportFlowModal.module.css';
import {ReportFlowNotice} from '@app/features/moderation/components/report_flow/ReportFlowNotice';
import {
	ReportFlowMessagePreview,
	ReportFlowUserPreview,
} from '@app/features/moderation/components/report_flow/ReportFlowPreview';
import {ReportFlowScreenView} from '@app/features/moderation/components/report_flow/ReportFlowScreenView';
import {ReportFlowSummary} from '@app/features/moderation/components/report_flow/ReportFlowSummary';
import {ReportFlowThankYou} from '@app/features/moderation/components/report_flow/ReportFlowThankYou';
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
	isReportFlowWalkUrgent,
	markReportFlowSent,
	type ReportFlowWalk,
	startReportFlowWalk,
	toggleReportFlowItem,
} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import ReportFlows from '@app/features/moderation/state/ReportFlows';
import {REPORT_USER_PROFILE_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {canSubmitReport, showReportRestrictionDialog} from '@app/features/moderation/utils/ReportVerificationGate';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import Relationships from '@app/features/relationship/state/Relationships';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {User} from '@app/features/user/models/User';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {ReportFlowOption, ReportFlowResponse} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

export type ReportFlowContext = {type: 'message'; message: Message} | {type: 'user'; user: User; guildId?: string};

interface ReportFlowSession {
	flow: ReportFlowResponse;
	productName: string;
	walk: ReportFlowWalk;
}

interface ReportFlowFrame {
	loadFailed: boolean;
	flow: ReportFlowResponse | null;
	walk: ReportFlowWalk | null;
}

const SURFACE = 'in_app';

function toSubmissionTarget(context: ReportFlowContext): ReportFlowSubmissionTarget {
	switch (context.type) {
		case 'message':
			return {type: 'message', channelId: context.message.channelId, messageId: context.message.id};
		case 'user':
			return {type: 'user', userId: context.user.id, guildId: context.guildId};
	}
}

function getReportedUser(context: ReportFlowContext): User | null {
	const user = context.type === 'message' ? context.message.author : context.user;
	if (context.type === 'message' && context.message.webhookId) return null;
	if (user.id === Authentication.currentUserId || user.system) return null;
	return user;
}

function resolveLinkUrl(url: string, context: ReportFlowContext): string {
	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		return url;
	}
	if (!parsed.pathname.replace(/\/+$/, '').endsWith('/report')) return url;
	if (context.type === 'message') {
		const channel = Channels.getChannel(context.message.channelId);
		parsed.searchParams.set('type', 'message');
		parsed.searchParams.set(
			'message_link',
			buildMessageJumpLink({
				guildId: channel?.guildId ?? context.message.guildId,
				channelId: context.message.channelId,
				messageId: context.message.id,
			}),
		);
	} else {
		parsed.searchParams.set('type', 'user');
		parsed.searchParams.set('user_id', context.user.id);
	}
	return parsed.toString();
}

function getContextGuildId(context: ReportFlowContext): string | null | undefined {
	if (context.type === 'user') return context.guildId;
	return Channels.getChannel(context.message.channelId)?.guildId ?? context.message.guildId;
}

function isTerminalSubmitFailure(code: string | undefined): boolean {
	return (
		code === APIErrorCodes.CONFLICT ||
		code === APIErrorCodes.UNKNOWN_MESSAGE ||
		code === APIErrorCodes.UNKNOWN_USER ||
		code?.startsWith('CANNOT_REPORT_') === true
	);
}

function describeSubmitFailure(i18n: I18n, context: ReportFlowContext, error: unknown): string {
	const code = failureCode(error);
	if (code === APIErrorCodes.CONFLICT) {
		return context.type === 'message'
			? i18n._(ALREADY_REPORTED_MESSAGE_DESCRIPTOR)
			: i18n._(ALREADY_REPORTED_PROFILE_DESCRIPTOR);
	}
	if (
		code === APIErrorCodes.RATE_LIMITED ||
		(error instanceof HttpError && error.status === HttpStatus.TOO_MANY_REQUESTS)
	) {
		return i18n._(RATE_LIMITED_DESCRIPTOR);
	}
	if (
		code === APIErrorCodes.REPORT_BANNED ||
		code === APIErrorCodes.UNKNOWN_MESSAGE ||
		code === APIErrorCodes.UNKNOWN_USER ||
		code?.startsWith('CANNOT_REPORT_')
	) {
		return failureMessage(error) ?? i18n._(SUBMIT_FAILED_DESCRIPTOR);
	}
	return i18n._(SUBMIT_FAILED_DESCRIPTOR);
}

interface ReportFlowModalProps {
	context: ReportFlowContext;
	onFinish?: () => void;
}

export const ReportFlowModal: React.FC<ReportFlowModalProps> = observer(({context, onFinish}) => {
	const {i18n} = useLingui();
	const onFinishRef = useRef(onFinish);
	const finishedRef = useRef(false);
	const submissionRef = useRef<Promise<void> | null>(null);
	const finishFlow = useCallback(() => {
		if (finishedRef.current) return;
		finishedRef.current = true;
		onFinishRef.current?.();
	}, []);
	useEffect(
		() => () => {
			const submission = submissionRef.current;
			if (submission === null) {
				finishFlow();
				return;
			}
			void submission.then(finishFlow, finishFlow);
		},
		[finishFlow],
	);
	const locale = i18n.locale;
	const targetType = context.type;
	const loadState = ReportFlows.getState(targetType, SURFACE, locale);
	const [session, setSession] = useState<ReportFlowSession | null>(null);
	const [submitting, setSubmitting] = useState(false);
	const [submitNote, setSubmitNote] = useState<string | null>(null);
	const [submitClosed, setSubmitClosed] = useState(false);
	const [blockTarget] = useState(() => {
		const user = getReportedUser(context);
		return user !== null && !Relationships.isBlocked(user.id) ? user : null;
	});
	const accountReady = canSubmitReport();
	if (loadState?.status === 'loaded' && session?.flow !== loadState.flow) {
		const keepWalk = session !== null && session.flow.revision_hash === loadState.flow.revision_hash;
		setSession({
			flow: loadState.flow,
			productName: loadState.productName,
			walk: keepWalk ? session.walk : startReportFlowWalk(loadState.flow),
		});
		if (!keepWalk) {
			setSubmitNote(null);
			setSubmitClosed(false);
		}
	}
	const view = loadState?.status === 'loaded' ? session : null;
	const flow = view?.flow ?? null;
	const walk = view?.walk ?? null;
	const stepKey = walk !== null ? getReportFlowStepKey(walk) : `load:${loadState?.status ?? 'loading'}`;
	const frame: ReportFlowFrame = {loadFailed: loadState?.status === 'error', flow, walk};
	const [shownStepKey, setShownStepKey] = useState(stepKey);
	const shownFrameRef = useRef(frame);
	const shown = shownStepKey === stepKey ? frame : shownFrameRef.current;
	useLayoutEffect(() => {
		shownFrameRef.current = shown;
	});
	useEffect(() => {
		void ReportFlows.revalidate(targetType, SURFACE, locale);
	}, [targetType, locale]);
	const closeModal = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const updateWalk = useCallback((update: (flow: ReportFlowResponse, walk: ReportFlowWalk) => ReportFlowWalk) => {
		setSubmitNote(null);
		setSession((current) => {
			const base = shownFrameRef.current;
			if (current === null || current.walk.phase === 'thanks' || base.flow !== current.flow || base.walk === null) {
				return current;
			}
			return {...current, walk: update(current.flow, base.walk)};
		});
	}, []);
	const handleChooseOption = useCallback(
		(option: ReportFlowOption) => {
			if (option.outcome.type === 'link') {
				if (option.outcome.url !== null) void openExternalUrl(resolveLinkUrl(option.outcome.url, context));
				return;
			}
			updateWalk((flow, walk) => chooseReportFlowOption(flow, walk, option.id));
		},
		[context, updateWalk],
	);
	const handleToggleItem = useCallback(
		(itemId: string) => updateWalk((flow, walk) => toggleReportFlowItem(flow, walk, itemId)),
		[updateWalk],
	);
	const handleBack = useCallback(() => {
		setSubmitClosed(false);
		updateWalk((_flow, walk) => backReportFlowWalk(walk));
	}, [updateWalk]);
	const handleNext = useCallback(() => {
		updateWalk((flow, walk) => {
			const screen = getReportFlowScreen(flow, walk.screenId);
			if (screen === null) return walk;
			return getReportFlowScreenKind(screen) === 'checklist'
				? continueReportFlowChecklist(flow, walk)
				: continueReportFlowInfo(flow, walk);
		});
	}, [updateWalk]);
	const handleRetryLoad = useCallback(() => {
		void ReportFlows.load(targetType, SURFACE, locale);
	}, [targetType, locale]);
	const handleSubmit = useCallback(async () => {
		if (session === null || session.walk.phase !== 'summary') return;
		if (!canSubmitReport()) {
			showReportRestrictionDialog();
			return;
		}
		setSubmitting(true);
		setSubmitNote(null);
		const submission = submitReportFlow(toSubmissionTarget(context), session.flow, session.walk.steps);
		submissionRef.current = submission;
		try {
			await submission;
			finishFlow();
			updateWalk((_flow, walk) => markReportFlowSent(walk));
		} catch (error) {
			const code = failureCode(error);
			if (code === APIErrorCodes.REPORT_FLOW_OUTDATED || code === APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS) {
				ToastCommands.createToast({type: 'info', children: i18n._(FLOW_OUTDATED_DESCRIPTOR)});
				setSession(null);
				void ReportFlows.restart(targetType, SURFACE, locale);
				return;
			}
			if (
				code === APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED ||
				code === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS
			) {
				showReportRestrictionDialog();
				return;
			}
			setSubmitClosed(isTerminalSubmitFailure(code));
			setSubmitNote(describeSubmitFailure(i18n, context, error));
		} finally {
			if (submissionRef.current === submission) submissionRef.current = null;
			setSubmitting(false);
		}
	}, [context, finishFlow, i18n, locale, session, targetType, updateWalk]);
	const fallbackTitle =
		context.type === 'message' ? i18n._(REPORT_MESSAGE_DESCRIPTOR) : i18n._(REPORT_USER_PROFILE_DESCRIPTOR);
	const previewHeading =
		context.type === 'message' ? i18n._(SELECTED_MESSAGE_DESCRIPTOR) : i18n._(SELECTED_USER_DESCRIPTOR);
	const preview =
		context.type === 'message' ? (
			<ReportFlowMessagePreview
				message={context.message}
				data-flx="moderation.report-flow.report-flow-modal.report-flow-message-preview"
			/>
		) : (
			<ReportFlowUserPreview
				user={context.user}
				guildId={context.guildId}
				data-flx="moderation.report-flow.report-flow-modal.report-flow-user-preview"
			/>
		);
	const screen = flow !== null && walk !== null ? getReportFlowScreen(flow, walk.screenId) : null;
	const notice =
		flow !== null && walk?.phase === 'notice'
			? (flow.notices.find((entry) => entry.id === walk.noticeId) ?? null)
			: null;
	const shownFlow = shown.flow;
	const shownWalk = shown.walk;
	const shownScreen =
		shownFlow !== null && shownWalk !== null ? getReportFlowScreen(shownFlow, shownWalk.screenId) : null;
	let title = fallbackTitle;
	let subtitle: string | null = null;
	if (shownFlow !== null && shownWalk !== null) {
		switch (shownWalk.phase) {
			case 'screen':
				title = shownScreen?.title ?? fallbackTitle;
				subtitle = shownScreen?.subtitle ?? null;
				break;
			case 'summary':
				title = i18n._(REPORT_SUMMARY_TITLE_DESCRIPTOR);
				subtitle = i18n._(REPORT_SUMMARY_SUBTITLE_DESCRIPTOR);
				break;
			case 'notice':
				title = shownFlow.notices.find((entry) => entry.id === shownWalk.noticeId)?.title ?? fallbackTitle;
				break;
			case 'thanks':
				title = shownWalk.reportSent
					? i18n._(THANK_YOU_TITLE_DESCRIPTOR)
					: i18n._(THANK_YOU_NO_REPORT_TITLE_DESCRIPTOR);
				break;
		}
	}
	const renderBody = (): React.ReactNode => {
		if (view === null || flow === null || walk === null) {
			if (loadState?.status === 'error') {
				return (
					<p className={styles.loadError} data-flx="moderation.report-flow.report-flow-modal.render-body.load-error">
						{i18n._(LOAD_FAILED_DESCRIPTOR)}
					</p>
				);
			}
			return (
				<div className={styles.loading} data-flx="moderation.report-flow.report-flow-modal.render-body.loading">
					<Spinner data-flx="moderation.report-flow.report-flow-modal.render-body.spinner" />
				</div>
			);
		}
		switch (walk.phase) {
			case 'screen': {
				if (screen === null) return null;
				const isStart = walk.steps.length === 0 && screen.id === flow.start_screen_id;
				return (
					<ReportFlowScreenView
						screen={screen}
						walk={walk}
						onChooseOption={handleChooseOption}
						onToggleItem={handleToggleItem}
						previewHeading={isStart && context.type === 'user' ? previewHeading : undefined}
						preview={isStart ? preview : undefined}
						data-flx="moderation.report-flow.report-flow-modal.render-body.report-flow-screen-view"
					/>
				);
			}
			case 'summary':
				return (
					<div className={styles.body} data-flx="moderation.report-flow.report-flow-modal.render-body.body">
						<ReportFlowSummary
							flow={flow}
							steps={walk.steps}
							urgent={isReportFlowWalkUrgent(flow, walk)}
							previewHeading={previewHeading}
							preview={preview}
							data-flx="moderation.report-flow.report-flow-modal.render-body.report-flow-summary"
						/>
						{!accountReady && (
							<div
								className={styles.accountNotice}
								data-flx="moderation.report-flow.report-flow-modal.render-body.account-notice"
							>
								<span
									className={styles.accountNoticeTitle}
									data-flx="moderation.report-flow.report-flow-modal.render-body.account-notice-title"
								>
									{i18n._(FINISH_ACCOUNT_SETUP_FIRST_DESCRIPTOR)}
								</span>
								<span
									className={styles.accountNoticeText}
									data-flx="moderation.report-flow.report-flow-modal.render-body.account-notice-text"
								>
									{i18n._(
										RuntimeConfig.usesUsernameSignIn
											? CLAIM_TO_REPORT_DESCRIPTOR
											: CLAIM_AND_VERIFY_TO_REPORT_DESCRIPTOR,
									)}
								</span>
								<div
									className={styles.accountNoticeActions}
									data-flx="moderation.report-flow.report-flow-modal.render-body.account-notice-actions"
								>
									<Button
										variant="secondary"
										small
										fitContent
										onClick={showReportRestrictionDialog}
										data-flx="moderation.report-flow.report-flow-modal.render-body.button.show-report-restriction-dialog"
									>
										{i18n._(FINISH_ACCOUNT_SETUP_DESCRIPTOR)}
									</Button>
								</div>
							</div>
						)}
						{submitNote !== null && (
							<p
								className={styles.submitNote}
								role="alert"
								data-flx="moderation.report-flow.report-flow-modal.render-body.submit-note"
							>
								{submitNote}
							</p>
						)}
					</div>
				);
			case 'notice':
				return notice !== null ? (
					<ReportFlowNotice
						notice={notice}
						data-flx="moderation.report-flow.report-flow-modal.render-body.report-flow-notice"
					/>
				) : null;
			case 'thanks':
				return (
					<ReportFlowThankYou
						reportSent={walk.reportSent}
						productName={view.productName}
						blockTarget={blockTarget}
						endedOnStart={walk.screenId === flow.start_screen_id}
						guildId={getContextGuildId(context)}
						channelId={context.type === 'message' ? context.message.channelId : undefined}
						data-flx="moderation.report-flow.report-flow-modal.render-body.report-flow-thank-you"
					/>
				);
		}
	};
	const renderFooter = (): React.ReactNode => {
		if (shownFlow === null || shownWalk === null) {
			if (!shown.loadFailed) return null;
			return (
				<Modal.Footer stretchButtons data-flx="moderation.report-flow.report-flow-modal.render-footer.modal-footer">
					<Button
						onClick={handleRetryLoad}
						data-flx="moderation.report-flow.report-flow-modal.render-footer.button.retry-load"
					>
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			);
		}
		const backButton = canGoBackInReportFlow(shownWalk) ? (
			<Button
				variant="secondary"
				onClick={handleBack}
				disabled={submitting}
				data-flx="moderation.report-flow.report-flow-modal.render-footer.button.back"
			>
				{i18n._(BACK_DESCRIPTOR)}
			</Button>
		) : null;
		switch (shownWalk.phase) {
			case 'screen': {
				if (shownScreen === null) return null;
				const kind = getReportFlowScreenKind(shownScreen);
				if (kind === 'choice') {
					return backButton !== null ? (
						<Modal.Footer
							stretchButtons
							data-flx="moderation.report-flow.report-flow-modal.render-footer.modal-footer--2"
						>
							{backButton}
						</Modal.Footer>
					) : null;
				}
				const nextButton = (
					<Button
						onClick={handleNext}
						disabled={kind === 'checklist' && !canContinueReportFlowChecklist(shownFlow, shownWalk)}
						data-flx="moderation.report-flow.report-flow-modal.render-footer.button.next"
					>
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				);
				if (backButton === null)
					return (
						<Modal.Footer
							stretchButtons
							data-flx="moderation.report-flow.report-flow-modal.render-footer.modal-footer--3"
						>
							{nextButton}
						</Modal.Footer>
					);
				return (
					<Modal.Footer
						className={styles.splitFooter}
						data-flx="moderation.report-flow.report-flow-modal.render-footer.split-footer"
					>
						{backButton}
						{nextButton}
					</Modal.Footer>
				);
			}
			case 'summary':
				return (
					<Modal.Footer
						className={styles.splitFooter}
						data-flx="moderation.report-flow.report-flow-modal.render-footer.split-footer--2"
					>
						{backButton}
						{submitClosed ? (
							<Button
								onClick={closeModal}
								data-flx="moderation.report-flow.report-flow-modal.render-footer.button.close-modal"
							>
								{i18n._(DONE_DESCRIPTOR)}
							</Button>
						) : (
							<Button
								variant="danger"
								onClick={handleSubmit}
								submitting={submitting || walk?.phase === 'thanks'}
								disabled={!accountReady}
								data-flx="moderation.report-flow.report-flow-modal.render-footer.button.submit"
							>
								{i18n._(SUBMIT_REPORT_DESCRIPTOR)}
							</Button>
						)}
					</Modal.Footer>
				);
			case 'notice':
			case 'thanks':
				return (
					<Modal.Footer
						stretchButtons
						data-flx="moderation.report-flow.report-flow-modal.render-footer.modal-footer--4"
					>
						<Button
							onClick={closeModal}
							data-flx="moderation.report-flow.report-flow-modal.render-footer.button.close-modal--2"
						>
							{i18n._(DONE_DESCRIPTOR)}
						</Button>
					</Modal.Footer>
				);
		}
	};
	const dir = getLocaleDirection(locale);
	return (
		<Modal.Root size="small" centered data-flx="moderation.report-flow.report-flow-modal.modal-root">
			<div
				className={styles.directionScope}
				dir={dir}
				data-flx="moderation.report-flow.report-flow-modal.direction-scope"
			>
				<Modal.Header title={title} data-flx="moderation.report-flow.report-flow-modal.modal-header">
					{subtitle !== null && (
						<p className={styles.subtitle} data-flx="moderation.report-flow.report-flow-modal.subtitle">
							{subtitle}
						</p>
					)}
				</Modal.Header>
				<Modal.Content className={styles.content} dir={dir} data-flx="moderation.report-flow.report-flow-modal.content">
					<SteppedCarousel
						step={stepKey}
						steps={[stepKey]}
						direction={(walk?.direction ?? 1) * (dir === 'rtl' ? -1 : 1)}
						focusOnStepChange
						onStepShown={setShownStepKey}
						data-flx="moderation.report-flow.report-flow-modal.stepped-carousel"
					>
						{renderBody()}
					</SteppedCarousel>
				</Modal.Content>
				{renderFooter()}
			</div>
		</Modal.Root>
	);
});
